# Data Wrangling for ML Project

# Libraries
library(data.table)
library(stringr)
library(priceR)
library(zeallot)
library(corrplot)
library(ggplot2)

# Source Files
source('./params.R')
source('./name-wrangling.R')
source('./price-wrangling.R')
source('./wrangle-consignor-names.R')
source('./load-clean-outings.R')
source('./build-summary-stats.R')
source('./generate-consignor-scores.R')

catalogController <- build_catalog_controller()

yearlingCatalogs <- catalogController[flagYearling == TRUE]

# Choose to use only sales that sell exclusively yearlings 
# Ones in mixed sales are low quality 
yearlingCatalogs <- yearlingCatalogs[Category == 'Yearlings']

# Create some additional variables 
yearlingCatalogs[, salesHouse := str_extract(SavedAs, "[^_]+")]

# Year on year there are the same sales, better horses expected at certain sales
# Need to make sure names line up year on year
yearlingCatalogs[, saleName := str_sub(str_remove(str_remove(SavedAs, ".xlsx"), '.xls'), end=-3)]
# Looks good 
yearlingCatalogs[, .N, by = .(saleName)]

# Now want to load all catalogs and create 1 data table 
salesResults <- combine_catalogs(fileVector = yearlingCatalogs$FileName, filePath = SALE_CATALOGUES_PATH)

# Match across the decided saleNames
z <- match(salesResults$FileName, yearlingCatalogs$FileName)
salesResults$saleName <- yearlingCatalogs$saleName[z]
salesResults$saleDate <- yearlingCatalogs$Date[z]

# Rename year to foalingYear
names(salesResults)[names(salesResults) == 'Year'] <- 'foalingYear'
# Match format
salesResults[, foalingYear := as.numeric(foalingYear)]
salesResults[foalingYear < 1000, foalingYear := foalingYear + 2000]

# Get prices for all horses available in GBP
salesResults <- wrangle_prices(as.data.table(salesResults))

# What do I actually want to use as the final price? 
# Probably the vendor buyback price ? 

missingData <- salesResults[, lapply(.SD, function(x) sum(is.na(x))), by = .(Sale)]

# Dont get name and country for goffs, this is fine
# Need foaling years for goffs, this is just missed out for a large number of the sales..

unique(salesResults[is.na(foalingYear)]$FileName)
# There are 7 goffs sales without the foaling year. These will have to be dropped

# 1) Choose pricing to use
# 2) consignor wrangling

# Parse all the consignor names
salesResults$ParsedConsignor <- wrangle_consignor_names(salesResults$Consignor)

# Summarise and generate additional variable
consignorSummary <- as.data.table(table(salesResults$ParsedConsignor))
z <- match(salesResults$ParsedConsignor, consignorSummary$V1)
salesResults$totalNoConsigned <- consignorSummary$N[z]
# Probably want to split these into buckets ?

salesResults[, SIRESTRIP.SUFFIX := paste0(SIRESTRIP, '.', SIRESUFFIX)]
salesResults[, DAMSTRIP.SUFFIX := paste0(DAMSTRIP, '.', DAMSUFFIX)]

salesResults[, saleDate := as.Date(saleDate)]

salesResults[, premiumSale := 0]
salesResults[saleName %in% PREMIUM_SALES, premiumSale := 1]
salesResults[, saleYear := year(saleDate)]

# Function that takes in the damstrip and suffix and uses this to create a summary of
# the outings 
cleanOutings <- load_clean_outings()
cleanSires <- load_clean_sires()
cleanFoals <- load_clean_foals()

# For each sale date calculate the sire and dam statistics 
# in terms of progeny performance

totalSummary <- data.table()
for (aDate in unique(salesResults$saleDate)) {
  
  todaysResults <- salesResults[saleDate == aDate]
  
  print('Running aggregation for:')
  print(unique(todaysResults$FileName))
  
  sireSummary <- build_summary_stats(aDate, 
                                     cleanOutings, 
                                     allSires = cleanSires,
                                     allFoals = cleanFoals,
                                     SireStrip.Suffix = todaysResults$SIRESTRIP.SUFFIX)
  
  damSummary <- build_summary_stats(aDate, 
                                    cleanOutings, 
                                    DamStrip.Suffix = todaysResults$DAMSTRIP.SUFFIX)
  
  # Then bind back up all the results to salesResults 
  todaysSummary <- merge(todaysResults, 
                         sireSummary, 
                         by.x = 'SIRESTRIP.SUFFIX', 
                         by.y = 'SIRESTRIP.SUFFIX_PROG_SIRE', 
                         all.x = TRUE)
  
  todaysSummary <- merge(todaysSummary, 
                         damSummary, 
                         by.x = 'DAMSTRIP.SUFFIX', 
                         by.y = 'DAMSTRIP.SUFFIX_PROG_DAM', 
                         all.x = TRUE)
  
  # Also run a sire function here
  
  # Match across some sire statistics 
  # Sire stats have to be lagged by 2 
  # 2 years between a cover and yearling being sold 
  sireYear <- year(as.Date(aDate, origin = '1970-01-01')) - 2
  sireInfo <- cleanSires[coverYear == sireYear]
  
  z <- match(todaysSummary$SIRESTRIP.SUFFIX, sireInfo$SIRESTRIP.SUFFIX)
  todaysSummary$coverYear_SIRE <- sireInfo$coverYear[z]
  todaysSummary$coverFee.GBP_SIRE <- round(sireInfo$Price.GBP[z], 2)
  todaysSummary$age_SIRE <- sireInfo$age[z]
  todaysSummary$coverNum_SIRE <- sireInfo$coverNum[z]
  todaysSummary$priceDiff.GBP_lag1_SIRE <- sireInfo$coverFeeDiff_lag.GBP[z]
  
  if (NROW(totalSummary) > 0) {
    totalSummary <- rbindlist(list(totalSummary, todaysSummary))
  } else {
    totalSummary <- todaysSummary
  }
}

# convert runners and winners to a percentage for sire 
totalSummary[, winPctRF_PROG_SIRE := round(100*winnersRF_PROG_SIRE/runnersRF_PROG_SIRE, 1)]

# Looks to be quite a few NAs - CHECK
# Remove all infinities
# Instead of -inf I want NA
totalSummary[sapply(totalSummary, is.infinite)] <- NA
totalSummary[sapply(totalSummary, is.na)] <- NA

# There are quite a few Sires for which we do not have information
# I am going to ignore these and not train my model on them as this is the approach 
# which I would take in real life

totalSummary_clean <- drop_empty_rows(totalSummary)
consignorScore <- generate_consignor_scores(totalSummary_clean, cleanOutings)

z <- match(totalSummary_clean$ParsedConsignor, consignorScore$ParsedConsignor)
totalSummary_clean$consignorScore <- consignorScore$consignorScore[z]

# Look at the percentage of dams / sires which we have the correct stats for
totalSummary_clean[, lapply(.SD, function(i) mean(i, na.rm = T)), .SDcols = is.numeric]
colSums(is.na(totalSummary_clean))

colnames(totalSummary_clean)

correlationTestCols <- c("ChosenPrice.GBP", "runnersRF_PROG_SIRE", "winnersRF_PROG_SIRE", 
                        "wnrs2yo_PROG_SIRE", "earlyWnrs_PROG_SIRE", "lateWnrs_PROG_SIRE", 
                        "win2yoPct_PROG_SIRE", "earlyWnrsPct_PROG_SIRE", "lateWnrsPct_PROG_SIRE", 
                        "PATwnrs_PROG_SIRE", "PATplcd_PROG_SIRE", "BTcount_PROG_SIRE", "RPRmax_PROG_SIRE", 
                        "RPRavg_PROG_SIRE", "RPRmin_PROG_SIRE", "tripAvg_PROG_SIRE", "foalMedianPrice_PROG_SIRE", 
                        "foalSaleCount_PROG_SIRE", "foalSoldPct_PROG_SIRE", "coverYear_SIRE",             
                        "coverFee.GBP_SIRE", "age_SIRE", "coverNum_SIRE", "runnersRF_PROG_DAM",           
                        "winnersRF_PROG_DAM", "wnrs2yo_PROG_DAM", "earlyWnrs_PROG_DAM",          
                        "lateWnrs_PROG_DAM", "win2yoPct_PROG_DAM", "earlyWnrsPct_PROG_DAM",      
                        "lateWnrsPct_PROG_DAM", "PATwnrs_PROG_DAM", "PATplcd_PROG_DAM",            
                        "BTcount_PROG_DAM", "RPRmax_PROG_DAM", "RPRavg_PROG_DAM",              
                        "RPRmin_PROG_DAM", "tripAvg_PROG_DAM", "RPRmax_DAM",                   
                        "OJCmax_DAM", "runs_DAM", "winner_DAM", "wnr2yo_DAM", 
                        "earlyWnr_DAM", "lateWnr_DAM", "PATwnr_DAM", "PATplc_DAM",
                        "BTyes_DAM", "consignorScore")

# Have a look at the correlation for some of my new columns

# Do some correlation tests on the columns
res <- cor(na.omit(totalSummary_clean[, ..correlationTestCols]))
priceCor <- as.data.table(res)[1]
corrplot(res, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45, cl.cex =0.1)

# From this it is clear that there are a number of variables with next to no correlation
# These are the ones that we are likely going to drop from the model 

# Use the log of prices

saveRDS(totalSummary_clean, './data/totalSummary_clean.RDS')

# Export tidy variables for matlab algo's 
# Do I need to use logs of prices for naive bayes and random forest?

priceCorT <- data.table(colnames(priceCor), t(priceCor))
colnames(priceCorT) <- c('Variable', 'Coefficient')
priceCorTSig <- priceCorT[Coefficient > 0.2]
useVars <- priceCorTSig$Variable

useVars <- append(useVars, c('premiumSale', 'consignorScore', 
                             'BTpct_PROG_DAM', 'BTpct_PROG_SIRE', 
                             'priceDiff.GBP_lag1_SIRE', 'RPR100pct_PROG_DAM', 
                             'RPR100pct_PROG_SIRE', 'winPctRF_PROG_SIRE'))

# replot with useVars
res <- cor(na.omit(totalSummary_clean[, ..useVars]))
priceCor <- as.data.table(res)[1]
corrplot(res, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45, cl.cex =0.1)

# Add unique identifier and saleDate then save
useVars <- append(c('SIRESTRIP.DAMSTRIP.BIRTHYEAR', 'saleDate', 'coverNum_SIRE'), useVars)

# Drop the non useful ones 
dropVars <- c('consignorScore', 'BTcount_PROG_DAM', 'BTcount_PROG_SIRE', 
              'PATwnrs_PROG_SIRE', 'PATwnrs_PROG_DAM', 
              'winnersRF_PROG_SIRE', 'BTpct_PROG_DAM')

useVars <- useVars[useVars %!in% dropVars]

continous_data_final <- totalSummary_clean[, ..useVars]

colnames(continous_data_final) <-  c("SIRESTRIP.DAMSTRIP.BIRTHYEAR", "SaleDate", "CoverNumSire", "ChosenPriceGbp", 
                             "RunnersRfProgSire", "LateWnrsProgSire", "PatPlcdProgSire", "RprAvgProgSire",
                             "TripAvgProgSire", "FoalMedianPriceProgSire", "CoverFeeGbpSire", "PatPlcdProgDam", 
                             "RprAvgProgDam", "PremiumSale", "BtPctProgSire", "PriceDiffGbpLag1Sire", "Rpr100PctProgDam", 
                             "Rpr100PctProgSire", "WinPctRfProgSire")

# % LateWnrsProgSire buckets 
# % 0 zero 0 
# % 1 low <50 
# % 2 high >50
# 
# % Price Diff GBP Lag1 Sire
# % -1 decrease <0 
# % 0 zero 0 
# % 1 increase 1
# 
# % Rpr100PctProjDam
# % 0 zero 0 
# % 1 low <20
# % 2 high >20

# Fix a few of my variables 
continous_data_final[, LateWnrsProgSireBINS := 2]
continous_data_final[LateWnrsProgSire < 50, LateWnrsProgSireBINS := 1]
continous_data_final[LateWnrsProgSire == 0, LateWnrsProgSireBINS := 0]
continous_data_final[is.na(LateWnrsProgSire), LateWnrsProgSireBINS := 0]

# Price Diff
continous_data_final[PriceDiffGbpLag1Sire == 0, PriceDiffGbpLag1SireBINS := 0]
continous_data_final[is.na(PriceDiffGbpLag1Sire), PriceDiffGbpLag1SireBINS := 0]
continous_data_final[PriceDiffGbpLag1Sire < 0, PriceDiffGbpLag1SireBINS := -1]
continous_data_final[PriceDiffGbpLag1Sire > 0, PriceDiffGbpLag1SireBINS := 1]

# Rpr100PctProjDam
continous_data_final[, Rpr100PctProgDamBINS := 2]
continous_data_final[Rpr100PctProgDam < 40, Rpr100PctProgDamBINS := 1]
continous_data_final[Rpr100PctProgDam == 0, Rpr100PctProgDamBINS := 0]
continous_data_final[is.na(Rpr100PctProgDam), Rpr100PctProgDamBINS := 0]

useVars <- c("SIRESTRIP.DAMSTRIP.BIRTHYEAR", "SaleDate", "CoverNumSire", "ChosenPriceGbp", 
            "RunnersRfProgSire", "LateWnrsProgSireBINS", "PatPlcdProgSire", "RprAvgProgSire",
            "TripAvgProgSire", "FoalMedianPriceProgSire", "CoverFeeGbpSire", "PatPlcdProgDam", 
            "RprAvgProgDam", "PremiumSale", "BtPctProgSire", "PriceDiffGbpLag1SireBINS", "Rpr100PctProgDamBINS", 
            "Rpr100PctProgSire", "WinPctRfProgSire")

continous_data_final_save <- continous_data_final[, ..useVars]

write.csv(continous_data_final_save, './data/ml-vars.csv', row.names = FALSE)


# Some summary stats
print('Number of sales: ')
length(unique(salesResults$FileName))
print('Number of horses')
NROW(continous_data_final)
print('Start Date')
min(salesResults$saleDate)
print('End date')
max(salesResults$saleDate)
print('Max Price')
max(continous_data_final$ChosenPriceGbp)
print('Min Price')
min(continous_data_final$ChosenPriceGbp)
print('Median Price')
median(continous_data_final$ChosenPriceGbp)

# Including Ratings

# Maximum rating achieved by each horse 
# Horses that never run are assigned 30 as max rating
maxRatings <- cleanOutings[year(ODATE) > 2016, list(RPRmax = max(ORF, na.rm=T), fill = 30), 
                           by = .(SIRESTRIP.DAMSTRIP.BIRTHYEAR)]

# I am going to have to cut out 2021 and 2022 sales as these horses wont yet have achieved ratings
ratings_data <- continous_data_final_save

z <- match(ratings_data$SIRESTRIP.DAMSTRIP.BIRTHYEAR, maxRatings$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
ratings_data$RprMax <- maxRatings$RPRmax[z]
ratings_data[sapply(ratings_data, is.infinite)] <- NA

ratings_data <- ratings_data[year(SaleDate) < 2021 & !is.na(RprMax)]

write.csv(ratings_data, './data/ratings-data.csv', row.names = FALSE)


