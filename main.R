# Data Wrangling for ML Project

# Libraries
library(data.table)
library(stringr)

# Source Files
source('./params.R')
source('./name-wrangling.R')

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

# Wrangle the prices

# Withdrawn Tag
salesResults[, withdrawnTag := numeric()]
# Vendor Buyback Tag
salesResults[, vendorBuybackTag := numeric()]
salesResults[, vendorBuybackPrice.GBP := numeric()]
# Not Sold Tag
salesResults[, notSoldTag := numeric()]
salesResults[, notSoldPrice.GBP:= numeric()]
# Post Sale Tag
salesResults[, postSaleTag := numeric()]
salesResults[, postSalePrice.GBP := numeric()]

# GoffsUK 

# Withdrawn 
# Purchaser contains "withdrawn" 
salesResults[Sale == "GoffsUK", withdrawnTag := 0]
salesResults[Sale == "GoffsUK" &
             is.na(Price.GBP) & 
             str_detect(Purchaser, regex('Withdrawn', ignore_case = T)), 
             withdrawnTag := 1]

# Vendor Buyback 
# Purchaser contains "vendor"
salesResults[Sale == "GoffsUK", vendorBuybackTag := 0]
salesResults[Sale == "GoffsUK" &
             str_detect(Purchaser, regex('Vendor', ignore_case = T)), 
             vendorBuybackTag := 1]
salesResults[Sale == "GoffsUK" & 
             vendorBuybackTag == 1, 
             vendorBuybackPrice.GBP := as.numeric(str_extract(Purchaser, "\\d+"))]

# Not Sold 
# Purchaser contains "n.sold" | "Not Sold"
salesResults[Sale == "GoffsUK", notSoldTag := 0]
salesResults[Sale == "GoffsUK" &
             (str_detect(Purchaser, regex('Not Sold', ignore_case = T)) |
              str_detect(Purchaser, regex('N.Sold', ignore_case = T))), 
             notSoldTag := 1]
salesResults[Sale == "GoffsUK" &
             notSoldTag == 1, 
             notSoldPrice.GBP := as.numeric(str_extract(Purchaser, "\\d+"))]

# Post Sale
# Purchaser contains "(PS)"
salesResults[Sale == "GoffsUK", postSaleTag := 0]
salesResults[Sale == "GoffsUK" &
             str_detect(Purchaser,  "\\(PS\\)"), 
             postSaleTag := 1]
salesResults[Sale == "GoffsUK" &
             postSaleTag == 1,
             postSalePrice.GBP := as.numeric(Price.GBP)]

# Check rows that aren't accounted for 
salesResults[Sale == "GoffsUK" &
             withdrawnTag == 0 &
             is.na(vendorBuybackPrice.GBP) &
             is.na(notSoldPrice.GBP) &
             is.na(postSalePrice.GBP) &
             is.na(Price.GBP)]

# Only horses that are not sold and dont have a not sold price ...  
# Eventually remove not sold no price and withdrawn horses





