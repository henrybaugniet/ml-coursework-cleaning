###################
#
# build-summary-stats.R
#
#
# Henry Baugniet 
#
# 12/12/2022
#
###################

# Summary statistics
build_summary_stats <- function(startDate, 
                              outings,
                              allSires = NULL,
                              allFoals = NULL,
                              SireStrip.Suffix = NULL, 
                              DamStrip.Suffix = NULL) {
  
  todaysOutings <- outings[ODATE < startDate]
  
  if (length(SireStrip.Suffix) > 0) {
    
    progenyOutings <- todaysOutings[SIRESTRIP.SUFFIX %in% SireStrip.Suffix]
    
    # add best trip tag
    progenyOutings[, tripBest := 0]
    progenyOutings[, bestTripRank := min(tripRank, ties.method = 'dense'), by = OHORSEID]
    progenyOutings[bestTripRank == tripRank, tripBest := 1]
    
    # Sumarry stats for the progeny 
    # There are some outing level stats that have to be done each time
    progenySummary <- progenyOutings[, list(RPRmax = max(ORF, na.rm=T),
                                            OJCmax = max(OJC, na.rm=T),
                                            runs = sum(run, na.rm=T),
                                            winner = pmin(1, sum(win, na.rm=T)),
                                            wnr2yo = pmin(1, sum(win2yo, na.rm=T)),
                                            earlyWnr = pmin(1, sum(earlyWin, na.rm = T)),
                                            lateWnr  = pmin(1, sum(lateWin, na.rm = T)),
                                            PATwnr = pmin(1, sum(PATwin, na.rm=T)),
                                            PATplc = pmin(1, sum(PATplc, na.rm=T))), 
                                     by = .(OHORSEID, SIRESTRIP.SUFFIX)]
    
    # Instead of -inf I want NA
    progenySummary[RPRmax < 0, RPRmax := NA_real_]
    progenySummary[OJCmax < 0, OJCmax := NA_real_]
    
    # add BT tag
    progenySummary[, BTyes := 0]
    progenySummary[ PATplc == 1 | RPRmax >= RATING_BT, BTyes := 1 ]
    
    # Best Trip by horse
    z <- match(progenySummary$OHORSEID, progenyOutings[tripBest == 1]$OHORSEID)
    progenySummary$tripBest <- progenyOutings$Distance[z]
    
    # Summary stats for the parent
    parentSummary   <- progenySummary[, list(runnersRF = .N, 
                                             winnersRF = sum(winner, na.rm=T),
                                             wnrs2yo = sum(wnr2yo, na.rm=T),
                                             earlyWnrs = sum(earlyWnr, na.rm = T),
                                             lateWnrs = sum(lateWnr, na.rm=T),
                                             win2yoPct = round(100*sum(wnr2yo, na.rm=T)/sum(winner, na.rm=T),1),
                                             earlyWnrsPct = round(100*sum(earlyWnr, na.rm=T)/sum(winner, na.rm=T),1),
                                             lateWnrsPct = round(100*sum(lateWnr, na.rm=T)/sum(winner, na.rm=T),1),
                                             PATwnrs = sum(PATwnr, na.rm=T),
                                             PATplcd = sum(PATplc, na.rm=T),
                                             BTcount = sum(BTyes, na.rm=T),
                                             RPRmax  = max(RPRmax, na.rm=T),
                                             RPRavg  = round(mean(RPRmax, na.rm=T),0),
                                             RPRmin  = min(RPRmax, na.rm=T),
                                             tripAvg = round(mean(tripBest, na.rm=T),0))
                                      , by = list(SIRESTRIP.SUFFIX)]
    
    # Foal year also lagged by one
    foalYear <- year(as.Date(startDate, origin = '1970-01-01')) - 1
    foalInfo <- allFoals[saleYear == foalYear]
    
    # median
    z <- match(parentSummary$SIRESTRIP.SUFFIX, foalInfo$SIRESTRIP.SUFFIX)
    parentSummary$foalMedianPrice <- foalInfo$median[z]
    parentSummary$foalSaleCount <- foalInfo$totalCount[z]
    parentSummary$foalSoldPct <- foalInfo$soldPercent[z]
    
    colnames(parentSummary) <-  paste(colnames(parentSummary), "PROG", sep = "_")
    
    sireYear <- year(as.Date(startDate, origin = '1970-01-01')) - 1
    sireInfo <- allSires[coverYear == sireYear]
    
    z <- match(parentSummary$SIRESTRIP.SUFFIX_PROG, sireInfo$SIRESTRIP.SUFFIX)
    parentSummary$coverYear <- sireInfo$coverYear[z]
    parentSummary$coverFee.GBP <- sireInfo$Price.GBP[z]
    parentSummary$age <- sireInfo$age[z]
    parentSummary$coverNum <- sireInfo$coverNum[z]
    
    colnames(parentSummary) <-  paste(colnames(parentSummary), "SIRE", sep = "_")
    
  }
  
  if (length(DamStrip.Suffix) > 0) {
    
    progenyOutings <- todaysOutings[DAMSTRIP.SUFFIX %in% DamStrip.Suffix]
    
    # add best trip tag
    progenyOutings[, tripBest := 0]
    progenyOutings[, bestTripRank := min(tripRank, ties.method = 'dense'), by = OHORSEID]
    progenyOutings[bestTripRank == tripRank, tripBest := 1]
    
    # Sumarry stats for the progeny 
    # There are some outing level stats that have to be done each time
    progenySummary <- progenyOutings[, list(RPRmax = max(ORF, na.rm=T),
                                            OJCmax = max(OJC, na.rm=T),
                                            runs = sum(run, na.rm=T),
                                            winner = pmin(1, sum(win, na.rm=T)),
                                            wnr2yo = pmin(1, sum(win2yo, na.rm=T)),
                                            earlyWnr = pmin(1, sum(earlyWin, na.rm = T)),
                                            lateWnr  = pmin(1, sum(lateWin, na.rm = T)),
                                            PATwnr = pmin(1, sum(PATwin, na.rm=T)),
                                            PATplc = pmin(1, sum(PATplc, na.rm=T))), 
                                     by = .(OHORSEID, DAMSTRIP.SUFFIX)]
    
    # Instead of -inf I want NA
    progenySummary[RPRmax < 0, RPRmax := NA_real_]
    progenySummary[OJCmax < 0, OJCmax := NA_real_]
    
    # add BT tag
    progenySummary[, BTyes := 0]
    progenySummary[ PATplc == 1 | RPRmax >= RATING_BT, BTyes := 1 ]
    
    # Best Trip by horse
    z <- match(progenySummary$OHORSEID, progenyOutings[tripBest == 1]$OHORSEID)
    progenySummary$tripBest <- progenyOutings$Distance[z]
    
    # Summary stats for the parent
    parentSummary   <- progenySummary[, list(runnersRF = .N, 
                                             winnersRF = sum(winner, na.rm=T),
                                             wnrs2yo = sum(wnr2yo, na.rm=T),
                                             earlyWnrs = sum(earlyWnr, na.rm = T),
                                             lateWnrs = sum(lateWnr, na.rm=T),
                                             win2yoPct = round(100*sum(wnr2yo, na.rm=T)/sum(winner, na.rm=T),1),
                                             earlyWnrsPct = round(100*sum(earlyWnr, na.rm=T)/sum(winner, na.rm=T),1),
                                             lateWnrsPct = round(100*sum(lateWnr, na.rm=T)/sum(winner, na.rm=T),1),
                                             PATwnrs = sum(PATwnr, na.rm=T),
                                             PATplcd = sum(PATplc, na.rm=T),
                                             BTcount = sum(BTyes, na.rm=T),
                                             RPRmax  = max(RPRmax, na.rm=T),
                                             RPRavg  = round(mean(RPRmax, na.rm=T),0),
                                             RPRmin  = min(RPRmax, na.rm=T),
                                             tripAvg = round(mean(tripBest, na.rm=T),0))
                                      , by = list(DAMSTRIP.SUFFIX)]
    
    colnames(parentSummary) <-  paste(colnames(parentSummary), "PROG", sep = "_")
    
    # Also want some statistics on the dam at the track 
    damOutings <- todaysOutings[HORSESTRIP.SUFFIX %in% DamStrip.Suffix]
    
    damSummary <- damOutings[, list(RPRmax = max(ORF, na.rm=T),
                               OJCmax = max(OJC, na.rm=T),
                               runs = sum(run, na.rm=T),
                               winner = pmin(1, sum(win, na.rm=T)),
                               wnr2yo = pmin(1, sum(win2yo, na.rm=T)),
                               earlyWnr = pmin(1, sum(earlyWin, na.rm = T)),
                               lateWnr  = pmin(1, sum(lateWin, na.rm = T)),
                               PATwnr = pmin(1, sum(PATwin, na.rm=T)),
                               PATplc = pmin(1, sum(PATplc, na.rm=T))), 
                               by = .(HORSESTRIP.SUFFIX)]
    
    # Instead of -inf I want NA
    damSummary[RPRmax < 0, RPRmax := NA_real_]
    damSummary[OJCmax < 0, OJCmax := NA_real_]
    
    # add BT tag
    damSummary[, BTyes := 0]
    damSummary[ PATplc == 1 | RPRmax >= RATING_BT, BTyes := 1 ]
    
    parentSummary <- merge(parentSummary, 
                      damSummary, 
                      by.x = 'DAMSTRIP.SUFFIX_PROG', 
                      by.y = 'HORSESTRIP.SUFFIX', 
                      all.x = TRUE)
    
    colnames(parentSummary) <-  paste(colnames(parentSummary), "DAM", sep = "_")
    
  }
  
  return (parentSummary)
    
}
  