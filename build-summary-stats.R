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


build_summary_stats <- function(startDate, 
                              outings,
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
    
    colnames(parentSummary) <-  paste(colnames(parentSummary), "PROG_SIRE", sep = "_")
    
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
  