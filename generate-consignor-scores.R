###################
#
# generate-consignor-scores.R
# 
# Henry Baugniet 
# 
# 14-12-2022
#
#
###################

# This function also contains some analysis that I carried out to 
# find the correct paramenters to derive a consignor score

# Deriving a consignor score 

# The idea being if a consignor is consistently selling 
# horses better than expected then they are are considered a good consignor

generate_consignor_scores <- function(totalSummary_clean, cleanOutings){
  
  consignorScore <- totalSummary_clean[, c('SIRESTRIP.DAMSTRIP.BIRTHYEAR', 'ChosenPrice.GBP', 'ParsedConsignor', 'saleDate')]
  consignorScore[, saleYear := year(saleDate)]
  # I have six years of sales but 2020 foals are the latest that will have made it to the track 
  # going to use the first 2.5 and second 2.5 to derive coefficients 
  # This means we can use 2021 yearlings
  unique(consignorScore$saleYear)
  
  # Need to get the median rating that all horses achieved on the track 
  progenySummary <- cleanOutings[, list(RPRmax = max(ORF, na.rm=T),
                                        OJCmax = max(OJC, na.rm=T),
                                        runs = sum(run, na.rm=T),
                                        winner = pmin(1, sum(win, na.rm=T)),
                                        wnr2yo = pmin(1, sum(win2yo, na.rm=T)),
                                        earlyWnr = pmin(1, sum(earlyWin, na.rm = T)),
                                        lateWnr  = pmin(1, sum(lateWin, na.rm = T))), 
                                 by = .(SIRESTRIP.DAMSTRIP.BIRTHYEAR)]
  
  # Instead of -inf I want NA
  progenySummary[RPRmax < 0, RPRmax := NA]
  progenySummary[OJCmax < 0, OJCmax := NA]
  
  z <- match(consignorScore$SIRESTRIP.DAMSTRIP.BIRTHYEAR, progenySummary$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
  consignorScore$RPRmax <- progenySummary$RPRmax[z]
  consignorScore[, horseNumber := 1]
  
  # Can only use rows which we have these values for
  consignorScore <- na.omit(consignorScore, cols = c('RPRmax', 'ChosenPrice.GBP'))
  unique(consignorScore[, saleYear])
  
  # Split into buckets
  consignorScore <- consignorScore[order(saleDate)]
  consignorScore[, ID := .I]
  consignorScore[ID < NROW(consignorScore)/2, dateBucket := 1]
  consignorScore[ID >= NROW(consignorScore)/2, dateBucket := 2]
  
  # Carry out a linear regression for both buckets 
  consignorScore_1 <- consignorScore[dateBucket == 1]
  consignorScore_2 <- consignorScore[dateBucket == 2]
  
  # Summarise by consignor 
  consignorSummary_1 <- consignorScore_1[, list(meanRPRmax = round(mean(RPRmax, na.rm=T), 2),
                                                medianRPRmax = round(median(RPRmax, na.rm=T), 2), 
                                                meanSalePrice = round(mean(ChosenPrice.GBP, na.rm=T), 2), 
                                                medianSalePrice = round(median(ChosenPrice.GBP, na.rm = T), 2), 
                                                totalSold = sum(horseNumber)), 
                                         by = .(ParsedConsignor)]
  
  consignorSummary_2 <- consignorScore_2[, list(meanRPRmax = round(mean(RPRmax, na.rm=T), 2),
                                                medianRPRmax = round(median(RPRmax, na.rm=T), 2), 
                                                meanSalePrice = round(mean(ChosenPrice.GBP, na.rm=T), 2), 
                                                medianSalePrice = round(median(ChosenPrice.GBP, na.rm = T), 2), 
                                                totalSold = sum(horseNumber)), 
                                         by = .(ParsedConsignor)]
  
  consignorSummary_1[sapply(consignorSummary_1, is.infinite)] <- NA
  consignorSummary_1[sapply(consignorSummary_1, is.na)] <- NA
  consignorSummary_2[sapply(consignorSummary_2, is.infinite)] <- NA
  consignorSummary_2[sapply(consignorSummary_2, is.na)] <- NA
  
  # Regression between log(price) and medianRPRmax
  MIN_CONSIGNED <- 10
  
  # OLM
  
  # this is predicted line comparing only chosen variables
  plot_1 <- ggplot(data = consignorSummary_1[totalSold > MIN_CONSIGNED], aes(x = log(medianSalePrice), y = medianRPRmax)) + 
    geom_point(color='black') +
    geom_smooth(method = "lm", se = FALSE)
  
  plot_1
  
  res_1 <- lm((consignorSummary_1[totalSold > MIN_CONSIGNED]$medianRPRmax ~ log(consignorSummary_1[totalSold > MIN_CONSIGNED]$medianSalePrice)))
  
  plot_2 <- ggplot(data = consignorSummary_2[totalSold > MIN_CONSIGNED], 
                   aes(x = log(medianSalePrice), y = medianRPRmax)) + 
    geom_point(color='black') +
    geom_smooth(method = "lm", se = FALSE)
  
  plot_2
  
  res_2 <- lm(consignorSummary_2[totalSold > MIN_CONSIGNED]$medianRPRmax ~ (log(consignorSummary_2[totalSold > MIN_CONSIGNED]$medianSalePrice)))
  
  print(paste0('Intercept 1: ', round(res_1[["coefficients"]][["(Intercept)"]], 4)))
  print(paste0('Intercept 2: ', round(res_2[["coefficients"]][["(Intercept)"]], 4)))
  print(paste0('Coefficient 1: ', round(res_1[["coefficients"]][["log(consignorSummary_1[totalSold > MIN_CONSIGNED]$medianSalePrice)"]], 4)))
  print(paste0('Coefficient 2: ', round(res_2[["coefficients"]][["log(consignorSummary_2[totalSold > MIN_CONSIGNED]$medianSalePrice)"]], 4)))
  
  # As we can see that the coefficients are stable I will use the coefficient 
  # from the whole dataset in the model
  
  MIN_CONSIGNED <- 15
  
  consignorSummary <- consignorScore[, list(meanRPRmax = round(mean(RPRmax, na.rm=T), 2),
                                            medianRPRmax = round(median(RPRmax, na.rm=T), 2), 
                                            meanSalePrice = round(mean(ChosenPrice.GBP, na.rm=T), 2), 
                                            medianSalePrice = round(median(ChosenPrice.GBP, na.rm = T), 2), 
                                            totalSold = sum(horseNumber)), 
                                     by = .(ParsedConsignor)]
  
  consignorSummary[sapply(consignorSummary, is.infinite)] <- NA
  consignorSummary[sapply(consignorSummary, is.na)] <- NA
  
  
  # this is predicted line comparing only chosen variables
  plot_whole <- ggplot(data = consignorSummary[totalSold > MIN_CONSIGNED], aes(x = log(medianSalePrice), y = medianRPRmax)) + 
    geom_point(color='black') +
    geom_smooth(method = "lm", se = FALSE)
  
  plot_whole
  
  x <- log(consignorSummary[totalSold > MIN_CONSIGNED]$medianSalePrice)
  y <- consignorSummary[totalSold > MIN_CONSIGNED]$medianRPRmax
  
  res_whole <- lm(y ~ x)
  
  print(paste0('Intercept Whole: ', round(res_whole[["coefficients"]][["(Intercept)"]], 4)))
  print(paste0('Coefficient Whole: ', round(res_whole[["coefficients"]][["x"]], 4)))
  
  # Now need to use the equation to produce a consignor score?
  c <- res_whole[["coefficients"]][["(Intercept)"]]
  m <- res_whole[["coefficients"]][["x"]]
  
  # This can be calculated as the difference between the medianRPR max and the 
  # medianRPR max that our equation calculates 
  consignorSummary[MIN_CONSIGNED <= totalSold, consignorScore := (m*log(medianSalePrice) + c)]
  consignorSummary[, consignorScore := (m*log(medianSalePrice) + c) - log(medianSalePrice)]
  
  consignorSummary[, medianSalePrice_log := log(medianSalePrice)]
  consignorSummary[, predictedRPRmax := (m*medianSalePrice_log + c)]
  consignorSummary[totalSold > MIN_CONSIGNED, consignorScore := medianRPRmax - predictedRPRmax]
  consignorSummary[totalSold <= MIN_CONSIGNED, consignorScore := 0]
  
  # These look reasonable with 20 as the min consigned 
  unique(consignorSummary$consignorScore)
  
  # This is not the correct way to be doing it ...
  # Should still be operating on the aggregated consignor level, just need to work 
  # out how to give each a score 
  
  return(consignorSummary)
  
}
