# Parameters

# File Paths
RI_DAILY_PATH <- file.path(Sys.getenv("RRDIR"), "RI_daily", 'Data', 'allRISetUp.RData')
SALE_CATALOGUES_PATH <- file.path(Sys.getenv("RRDIR"), "RawCataloguesRepo")

STRIP_VARS <- c('SIRESTRIP', 'DAMSTRIP', 'SIRESTRIP.DAMSTRIP.BIRTHYEAR')

DAM_VARS <- c('DAMSTRIP', 'DAMSUFFIX', 'DAMSTRIP.BREEDING')

siblingVars <- c('HID', 'HNAME', 'HSUFFIX', 'HSIREID', 
                 'HDAMID', 'HFOALDATE', 'HSEX')

catalogKeepVars <-  c('HNAME', 'HSUFFIX', 'HSEX','Year.x' ,'SIRESTRIP.DAMSTRIP.BIRTHYEAR.x', 
                      'CATALOG.SALENAME', 'SaleDate', 'CATALOG.COMPANY', 'CATALOG.HORSETYPE', 
                      'Header1', 'Header2', 'SaleLocation', "Lot", 'provisionalName', "Year.y", 
                      "Consignor", 'pedigreeURL', 'SaleBlurb', 'HorseName') 

catalogNeatNames <- c("tgtHorse.Name", 'tgtHorse.Breeding', 'tgtHorse.Sex' , "tgtHorse.Birthyear", "tgtHorse.SIRESTRIP.DAMSTRIP.BIRTHYEAR", 
                      "CATALOG.Salename", 'CATALOG.SaleDate', "CATALOG.Company", "CATALOG.Horsetype",             
                      "SALE.Header1", "SALE.Header2", 'SALE.Location', "SALE.Lot", "SALE.provisionalName", "SALE.Birthyear",                        
                      "SALE.Consignor", "SALE.pedigreeURL", 'SALE.Blurb', 'SALE.HorseName')

resultsKeepVars <- c('HNAME', 'HSUFFIX', 'HSEX','Year.x' ,'SIRESTRIP.DAMSTRIP.BIRTHYEAR.x', 'CATALOG.SALENAME', 'SaleDate','CATALOG.COMPANY', 'CATALOG.HORSETYPE', "Lot", 'provisionalName',                        
                     "Year.y",  "Purchaser", 'Price.GBP', 'pedigreeURL', 'MESSAGE')

resultsNeatNames <- c("tgtHorse.Name", 'tgtHorse.Breeding', 'tgtHorse.Sex' , "tgtHorse.Birthyear", "tgtHorse.SIRESTRIP.DAMSTRIP.BIRTHYEAR", "CATALOG.Salename", 'CATALOG.SaleDate', "CATALOG.Company", "CATALOG.Horsetype",             
                      "SALE.Lot", "SALE.provisionalName", "SALE.Birthyear", 'SALE.Purchaser', 'SALE.Price.GBP',                        
                      "SALE.URL", 'MESSAGE')
