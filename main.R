# Data Wrangling for ML Project

# Libraries
library(data.table)
library(stringr)
library(priceR)

# Source Files
source('./params.R')
source('./name-wrangling.R')
source('./price-wrangling.R')

# Results Data UK
load(RI_DAILY_PATH)

# Catalogs SS
readxl::excel_sheets(file.path(SALE_CATALOGUES_PATH, "README.xlsx"))

goffsUK <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                   sheet = 'GoffsUK', 
                   trim_ws = TRUE, 
                   col_names = TRUE)

goffs <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                              sheet = 'Goffs', 
                              trim_ws = TRUE, 
                              col_names = TRUE)

arqana <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                              sheet = 'Arqana', 
                              trim_ws = TRUE, 
                              col_names = TRUE)

tattersallsNewmarket <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                              sheet = 'TattersallsNewmarket', 
                              trim_ws = TRUE, 
                              col_names = TRUE)

tattersallsAscot <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                              sheet = 'TattersallsAscot', 
                              trim_ws = TRUE, 
                              col_names = TRUE)

tattersallsIreland <- readxl::read_excel(file.path(SALE_CATALOGUES_PATH, "README.xlsx"), 
                              sheet = 'TattersallsIreland', 
                              trim_ws = TRUE, 
                              col_names = TRUE)

catalogController <- as.data.table(rbindlist(list(goffsUK, 
                                                  goffs, 
                                                  arqana, 
                                                  tattersallsNewmarket, 
                                                  tattersallsAscot, 
                                                  tattersallsIreland)))

# Tag sales that contain yearlings in any column (to check I havent mislabeled any)
catalogController[, flagYearling := rowSums(sapply(catalogController, grepl, pattern = 'Yearling', fixed = TRUE)) > 0]
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
salesResults <- wrangle_prices(salesResults)

# What do I actually want to use as the final price? 
# Probably the vendor buyback price ? 

missingData <- salesResults[, lapply(.SD, function(x) sum(is.na(x))), by = .(Sale)]

# Dont get name and country for goffs, this is fine
# Need foaling years for goffs, this is just missed out for a large number of the sales..

unique(salesResults[is.na(foalingYear)]$FileName)
# There are 7 goffs sales without the foaling year. These will have to be dropped




