# Parameters

# File Paths
RI_DAILY_PATH <- file.path(Sys.getenv("RRDIR"), "RI_daily", 'Data', 'allRISetUp.RData')
SALE_CATALOGUES_PATH <- file.path(Sys.getenv("RRDIR"), "RawCataloguesRepo")

STRIP_VARS <- c('SIRESTRIP', 'DAMSTRIP', 'SIRESTRIP.DAMSTRIP.BIRTHYEAR')

DAM_VARS <- c('DAMSTRIP', 'DAMSUFFIX', 'DAMSTRIP.BREEDING')

CLASS_RANGE      = c( 0, 3, 7 )
CLASS_LABEL      = c("Class 1-3", "Class 4-7")
CLASS_BIG_RANGE   = c( 0, 1, 2, 3, 4, 5, 6, 7 )
CLASS_BIG_LABEL   = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5", "Class 6", "Class 7")

EARLY_MONTH <- 7