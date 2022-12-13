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
  
  return (outings)
  
}