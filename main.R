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
  
  
  if (NROW(totalSummary) > 0) {
    totalSummary <- rbindlist(list(totalSummary, todaysSummary))
  } else {
    totalSummary <- todaysSummary
  }
  
}

# Looks to be quite a few NAs - CHECK
# Remove all infinities
# Instead of -inf I want NA
totalSummary[sapply(totalSummary, is.infinite)] <- NA
totalSummary[sapply(totalSummary, is.na)] <- NA

# There are quite a few Sires for which we do not have information
# I am going to ignore these and not train my model on them as this is the approach 
# which I would take in real life

# Speak to Jason about this 
totalSummary_clean <- drop_empty_rows(totalSummary)

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
                        "BTyes_DAM")

# Do some correlation tests on the columns
res <- cor(na.omit(totalSummary_clean[, ..correlationTestCols]))
priceCor <- as.data.table(res)[1]
corrplot(res, type = "upper", order = "original", 
         tl.col = "black", tl.srt = 45, cl.cex =0.1)

# From this it is clear that there are a number of variables with next to no correlation
# These are the ones that we are likely going to drop from the model 

# Use the log of prices

# saveRDS(totalSummary_clean, './data/totalSummary_clean.RDS')

# Deriving a consignor score 
consignorScore <- totalSummary_clean[, c('SIRESTRIP.DAMSTRIP.BIRTHYEAR', 'ChosenPrice.GBP', 'ParsedConsignor', 'saleDate')]
consignorScore[, saleYear := year(saleDate)]
# I have six years of sales but 2020 foals are the latest that will have made it to the track 
# going to use the first 2.5 and second 2.5 to derive coefficients 
# This means we can use 2021 yearlings
unique(consignorScore$saleYear)

# Need to get the median rating that all horses achieved on the track 
progenySummary <- cleanOutings[, list(RPRmax = max(ORF, na.rm=T),
                                        OJCmax = max(OJC, na.rm=T),
                                        runs = sum(run, na.rm=T),
                                        winner = pmin(1, sum(win, na.rm=T)),
                                        wnr2yo = pmin(1, sum(win2yo, na.rm=T)),
                                        earlyWnr = pmin(1, sum(earlyWin, na.rm = T)),
                                        lateWnr  = pmin(1, sum(lateWin, na.rm = T))), 
                                 by = .(SIRESTRIP.DAMSTRIP.BIRTHYEAR)]

# Instead of -inf I want NA
progenySummary[RPRmax < 0, RPRmax := NA]
progenySummary[OJCmax < 0, OJCmax := NA]

z <- match(consignorScore$SIRESTRIP.DAMSTRIP.BIRTHYEAR, progenySummary$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
consignorScore$RPRmax <- progenySummary$RPRmax[z]
consignorScore[, horseNumber := 1]

# Can only use rows which we have these values for
consignorScore <- na.omit(consignorScore, cols = c('RPRmax', 'ChosenPrice.GBP'))
unique(consignorScore[, saleYear])

# Split into buckets
consignorScore <- consignorScore[order(saleDate)]
consignorScore[, ID := .I]
consignorScore[ID < NROW(consignorScore)/2, dateBucket := 1]
consignorScore[ID >= NROW(consignorScore)/2, dateBucket := 2]

# Carry out a linear regression for both buckets 
consignorScore_1 <- consignorScore[dateBucket == 1]
consignorScore_2 <- consignorScore[dateBucket == 2]

# Regression Function 


# Summarise by consignor 
consignorSummary_1 <- consignorScore_1[, list(meanRPRmax = round(mean(RPRmax, na.rm=T), 2),
                                          medianRPRmax = round(median(RPRmax, na.rm=T), 2), 
                                          meanSalePrice = round(mean(ChosenPrice.GBP, na.rm=T), 2), 
                                          medianSalePrice = round(median(ChosenPrice.GBP, na.rm = T), 2), 
                                          totalSold = sum(horseNumber)), 
                                   by = .(ParsedConsignor)]

consignorSummary_2 <- consignorScore_2[, list(meanRPRmax = round(mean(RPRmax, na.rm=T), 2),
                                              medianRPRmax = round(median(RPRmax, na.rm=T), 2), 
                                              meanSalePrice = round(mean(ChosenPrice.GBP, na.rm=T), 2), 
                                              medianSalePrice = round(median(ChosenPrice.GBP, na.rm = T), 2), 
                                              totalSold = sum(horseNumber)), 
                                       by = .(ParsedConsignor)]

consignorSummary_1[sapply(consignorSummary_1, is.infinite)] <- NA
consignorSummary_1[sapply(consignorSummary_1, is.na)] <- NA
consignorSummary_2[sapply(consignorSummary_2, is.infinite)] <- NA
consignorSummary_2[sapply(consignorSummary_2, is.na)] <- NA

# Regression between log(price) and medianRPRmax

# this is predicted line comparing only chosen variables
plot_1 <- ggplot(data = consignorSummary_1[totalSold > 5], aes(x = log(medianSalePrice), y = medianRPRmax)) + 
                 geom_point(color='black') +
                 geom_smooth(method = "lm", se = FALSE)

plot_1         

res_1 <- lm((log(consignorSummary_1[totalSold > 5]$medianSalePrice) ~ consignorSummary_1[totalSold > 5]$medianRPRmax))


plot_2 <- ggplot(data = consignorSummary_2[totalSold > 5], aes(x = log(medianSalePrice), y = medianRPRmax)) + 
                 geom_point(color='black') +
                 geom_smooth(method = "lm", se = FALSE)

plot_2

res_2 <- lm((log(consignorSummary_2[totalSold > 5]$medianSalePrice) ~ consignorSummary_2[totalSold > 5]$medianRPRmax))

print(paste0('Intercept 1: ', round(res_1[["coefficients"]][["(Intercept)"]], 4)))
print(paste0('Intercept 2: ', round(res_2[["coefficients"]][["(Intercept)"]], 4)))
print(paste0('Coefficient 1: ', round(res_1[["coefficients"]][["consignorSummary_1[totalSold > 5]$medianRPRmax"]], 4)))
print(paste0('Coefficient 2: ', round(res_2[["coefficients"]][["consignorSummary_2[totalSold > 5]$medianRPRmax"]], 4)))

# The idea being if a consignor is consistently selling 
# horses better than expected then they are are considered a good consignor






