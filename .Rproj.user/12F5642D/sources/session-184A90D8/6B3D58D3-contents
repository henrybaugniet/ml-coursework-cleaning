# Data Wrangling for ML Project

# Libraries
library(data.table)

# Source Files
source('./params.R')

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

# Tag sales that contain yearlings .. 
catalogController[, flagYearling := rowSums(sapply(catalogController, grepl, pattern = 'Yearling', fixed = TRUE)) > 0]
yearlingCatalogs <- catalogController[flagYearling == TRUE]




