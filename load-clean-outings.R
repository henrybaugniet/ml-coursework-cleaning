###################
#
#
# load_clean_outings
#
# Henry Baugniet  
#
# 12/12/2022
#
###################

# Catalogs SS
build_catalog_controller <- function() {
  
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
  
  return(catalogController)
}

load_clean_sires <- function(){
  
  load(SIRE_DATA_PATH)
  
  allsires[, SIRESTRIP.SUFFIX := paste0(sStrip, '.', sSuffix)]
  
  # Filter out Danish 
  allsires <- na.omit(allsires, cols = c('cur', 'coverFee', 'coverYear'))
  allsires <- allsires[cur %in% c('USD', 'EUR', 'GBP', 'JPY', 'AUD')]
  allsires <- allsires[as.numeric(coverYear) > 2010 & as.numeric(coverYear) < 2023]
  allsires[, coverFee := as.numeric(coverFee)]
  allsires[, cur := as.character(cur)]
  
  # Using the start of July as date for conversion 
  allsires[, Price.GBP := priceR::convert_currencies(price_start = coverFee, 
                                                     from = cur, 
                                                     to = 'GBP', 
                                                     date = as.Date(paste0(coverYear, '-07-01')))]
  
  # Get the lag price in GBP 
  allsires[!is.na(coverFee.lag) & 
           !is.na(cur.lag) & 
           cur.lag %in% c('USD', 'EUR', 'GBP', 'JPY', 'AUD'), 
           Price.GBP_lag1 := priceR::convert_currencies(price_start = coverFee.lag, 
                                                        from = cur.lag, 
                                                        to = 'GBP', 
                                                        date = as.Date(paste0(coverYear, '-07-01')))]
  
  # I am not changing exchange rate because I it is important the diff stays as zero 
  # if the price doesnt change.
  allsires[, coverFeeDiff_lag.GBP := Price.GBP - Price.GBP_lag1]
  
  return(allsires)
}

load_clean_foals <- function() {
  
  load(FOAL_DATA_PATH)
  
  foalPrices[, SIRESTRIP.SUFFIX := paste0(sStrip, '.', sSuffix)]
  
  return(foalPrices)
  
}

# Now that we have sales data we want to get dam, sire and progeny stats
load_clean_outings <- function(){
  
  # Results Data UK
  load(RI_DAILY_PATH)
  
  # Clean up Outings, only wanting flat races
  outings <- RIouting[OFJ == 'F']
  races <- RIrace[RFJ == 'F']
  
  # delete non-numeric chars from the ratings
  outings$ORF   <- as.character(outings$ORF)
  outings$ORF   <- gsub("\\?","",outings$ORF)
  outings$ORF   <- gsub("\\+","",outings$ORF)
  outings$ORF   <- as.numeric(outings$ORF)
  z              <- which(outings$ORF==0)
  outings$ORF[z]<- NA
  
  outings$OJC   <-as.character(outings$OJC)
  outings$OJC   <-gsub("\\?","",outings$OJC)
  outings$OJC   <-gsub("\\+","",outings$OJC)
  outings$OJC   <-as.numeric(outings$OJC)
  z              <-which(outings$OJC==0)
  outings$OJC[z]<-NA
  
  outings$OSPEED   <-as.character(outings$OSPEED)
  outings$OSPEED   <-gsub("\\?","",outings$OSPEED)
  outings$OSPEED   <-gsub("\\+","",outings$OSPEED)
  outings$OSPEED   <-as.numeric(outings$OSPEED)
  z                 <-which(outings$OSPEED==0)
  outings$OSPEED[z]<-NA
  
  # delete D from OPOS
  outings$OPOS     <-as.character(outings$OPOS)
  outings$OPOS     <-gsub("D","", outings$OPOS)
  outings$OPOS     <-gsub("d","", outings$OPOS)
  
  # convert to integer
  outings$OPOS    <- as.integer(outings$OPOS)
  
  # set up identifiers using YOB
  outingsHead <- as.data.table(head(outings))
  
  # Horse
  z <- match(outings$OHORSEID, RIhorse$HID)
  outings$HorseName <- RIhorse$HNAME[z]
  outings$HorseStrip <- RIhorse$HSTRIP[z]
  outings$HorseSuffix <- RIhorse$HSUFFIX[z]
  outings$HorseFoalingDate <- RIhorse$HFOALDATE[z]
  # Dam / Sire
  outings$SireID <- RIhorse$HSIREID[z]
  outings$DamID <- RIhorse$HDAMID[z]
  # Dam Sire
  outings$DamSireName <- RIhorse$HDAMSIRE[z]
  outings$DamSireName <- RIhorse$HDAMSIRE[z]
  outings$DamSireSuffix <- RIhorse$HDSIRESUFF[z]
  outings$DamSireStrip <- RIhorse$HDSSTRIP[z]
  
  # Sire Name
  z <- match(outings$SireID, RIsire$SHID)
  outings$SireName <- RIsire$SHNAME[z]
  outings$SireStrip <- RIsire$SHSTRIP[z]
  outings$SireSuffix <- RIsire$SHSUFFIX[z]
  outings$SireFoalingYear <- RIsire$SHFOALDATE[z]
  
  # Dam Name
  z <- match(outings$DamID, RIdam$DHID)
  outings$DamName <- RIdam$DHNAME[z]
  outings$DamStrip <- RIdam$DHSTRIP[z]
  outings$DamSuffix <- RIdam$DHSUFFIX[z]
  outings$DamFoalingYear <- RIdam$DHFOALDATE[z]
  
  # Useful Race Variables
  z <- match(outings$ORACEID, races$RID)
  outings$RaceClass <- races$RCLASS[z]
  outings$Pattern <- races$RPATTERN[z]
  
  # Format distance
  # first set distance in furlongs in a new variable
  outings$Distance       <- round(outings$ODISTANCE/2200,1)*10
  
  # cut into Class categories
  outings$classCat   <- NA_character_
  outings$classCat   <- cut(as.numeric(outings$RaceClass), CLASS_RANGE, CLASS_LABEL)
  # and set NA to ovs
  outings[is.na(classCat)]$classCat <- "Overseas"
  
  outings$classBigCat   <- NA_character_
  outings$classBigCat   <- cut( as.numeric(outings$RaceClass), CLASS_BIG_RANGE, CLASS_BIG_LABEL)
  # and set NA to ovs
  outings[is.na(classBigCat)]$classBigCat <- "Overseas"
  
  # Set up win and place markers 
  # assign win/run/place variables, set place for BLACK TYPE & BOOKMAKER definitions
  outings$win      <- 0
  outings$place    <- 0
  
  z                 <- which(outings$OPOS==1)
  outings$win[z]   <- 1
  outings$run      <- 1
  
  z                 <- which((outings$OPOS <= 3))
  outings$place[z] <- 1
  
  # set  PTN win/run/place vars
  outings$PATwin <- NA
  outings$PATrun <- NA
  outings$PATplc <- NA
  
  # set the pattern vars then the mdn vars
  z                  <- which((outings$Pattern != "NOT"))
  outings$PATrun[z] <- 1
  outings$PATwin[z] <- 0
  outings$PATplc[z] <- 0
  
  z                  <- which((outings$Pattern != "NOT") & (outings$OPOS == 1))
  outings$PATwin[z] <- 1
  z                  <- which((outings$Pattern != "NOT") & (outings$place == 1))
  outings$PATplc[z] <- 1
  
  # 2yo winning outing 
  outings[, win2yo := 0]
  outings[OAGE == 2 & win == 1, win2yo := 1]
  
  # set early and late win marker, 0 if not a 2yo winner by end July, 1 if yes
  outings[, earlyWin := 0]
  outings[ OAGE == 2 & win == 1 & month(ODATE) <= EARLY_MONTH, earlyWin := 1]
  
  outings[, lateWin := 0]
  outings[, outingRank := frank(ODATE, ties.method = 'dense'), by = .(OHORSEID)]
  outings[, winRank := 0]
  outings[ win == 1, winRank := frank(ODATE, ties.method = 'dense'), by = .(OHORSEID)]
  outings[ OAGE > 2 & win == 1 & winRank == 1 & month(ODATE) > EARLY_MONTH, lateWin := 1]
  
  # Rank best trip per horse 
  outings[, tripRank := frank(ORF, ties.method = 'dense'), by = OHORSEID]
  
  # add BT tag
  outings[, BTyes := 0]
  outings[ PATplc == 1 | ORF >= RATING_BT, BTyes := 1 ]
  
  #SIRE and DAMSTRIP
  outings[, SIRESTRIP.SUFFIX := paste0(SireStrip, '.', SireSuffix)]
  outings[, DAMSTRIP.SUFFIX := paste0(DamStrip, '.', DamSuffix)]
  outings[, HORSESTRIP.SUFFIX := paste0(HorseStrip, '.', HorseSuffix)]
  
  # Make sure that using Date format for the date
  outings[, ODATE := as.Date(ODATE)]
  outings[, SIRESTRIP.DAMSTRIP.BIRTHYEAR := paste0(SireStrip, '.', DamStrip, '.', year(HorseFoalingDate))]
  
  return (outings)
  
}