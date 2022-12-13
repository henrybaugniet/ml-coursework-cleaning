# Data Wrangling for ML Project

# Libraries
library(data.table)
library(stringr)
library(priceR)
library(zeallot)

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
outings <- load_clean_outings()

# For each sale date calculate the sire and dam statistics 
# in terms of progeny performance

totalSummary <- data.table()
for (aDate in unique(salesResults$saleDate)) {
  
  todaysResults <- salesResults[saleDate == aDate]
  # Should I be doing total number consigned as all consigned prior to date?
  # Progeny summary by sire ...
  
  # Would be good to get some stats about the dam and sire themselves
  # ESPECIALLY THE DAM
  
  sireSummary <- build_summary_stats(aDate, 
                                     outings, 
                                     SireStrip.Suffix = todaysResults$SIRESTRIP.SUFFIX)
  
  damSummary <- build_summary_stats(aDate, 
                                    outings, 
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
totalSummary[sapply(totalSummary, is.infinite)] <- NA_real_

# Look at the percentage of dams / sires which we have the correct stats for
totalSummary[, lapply(.SD, function(i) mean(i, na.rm = T)), .SDcols = is.numeric]
colSums(is.na(totalSummary))

# There are quite a few Sires for which we do not have information
# I am going to ignore these and not train my model on them as this is the approach 
# which I would take in real life

# Speak to Jason about this 


