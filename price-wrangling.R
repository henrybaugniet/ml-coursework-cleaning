###################
#
# price-wrangling.R
#
# Henry Baugniet
#
# 05/12/2022
#
#
###################


wrangle_prices <- function(df){
  
  # Wrangle the prices
  df[, Price.GBP := as.numeric(Price.GBP)]
  df[, Price.EU := as.numeric(Price.EU)]
  
  # Withdrawn Tag
  df[, withdrawnTag := numeric()]
  # Vendor Buyback Tag
  df[, vendorBuybackTag := numeric()]
  df[, vendorBuybackPrice.GBP := numeric()]
  # Not Sold Tag
  df[, notSoldTag := numeric()]
  df[, notSoldPrice.GBP:= numeric()]
  # Post Sale Tag
  df[, postSaleTag := numeric()]
  df[, postSalePrice.GBP := numeric()]
  
  # GoffsUK 
  
  # Withdrawn 
  # Purchaser contains "withdrawn" 
  df[Sale == "GoffsUK", withdrawnTag := 0]
  df[Sale == "GoffsUK" &
                 is.na(Price.GBP) & 
                 str_detect(Purchaser, regex('Withdrawn', ignore_case = T)), 
               withdrawnTag := 1]
  
  # Vendor Buyback 
  # Purchaser contains "vendor"
  df[Sale == "GoffsUK", vendorBuybackTag := 0]
  df[Sale == "GoffsUK" &
                 str_detect(Purchaser, regex('Vendor', ignore_case = T)), 
               vendorBuybackTag := 1]
  df[Sale == "GoffsUK" & 
                 vendorBuybackTag == 1, 
               vendorBuybackPrice.GBP := as.numeric(str_extract(Purchaser, "\\d+"))]
  
  # Not Sold 
  # Purchaser contains "n.sold" | "Not Sold"
  df[Sale == "GoffsUK", notSoldTag := 0]
  df[Sale == "GoffsUK" &
                 (str_detect(Purchaser, regex('Not Sold', ignore_case = T)) |
                    str_detect(Purchaser, regex('N.Sold', ignore_case = T))), 
               notSoldTag := 1]
  df[Sale == "GoffsUK" &
                 notSoldTag == 1, 
               notSoldPrice.GBP := as.numeric(str_extract(Purchaser, "\\d+"))]
  
  # Post Sale
  # Purchaser contains "(PS)"
  df[Sale == "GoffsUK", postSaleTag := 0]
  df[Sale == "GoffsUK" &
                 str_detect(Purchaser,  "\\(PS\\)"), 
               postSaleTag := 1]
  df[Sale == "GoffsUK" &
                 postSaleTag == 1,
               postSalePrice.GBP := Price.GBP]
  
  # Only horses that are not sold and don't have a not sold price ...  
  # Eventually remove not sold no price and withdrawn horses
  
  # Some prices are in EUR
  unique(df[!is.na(Price.EU)]$Sale)
  
  # Goffs: Original prices in Euros 
  df[Sale == "Goffs",
               Price.GBP := priceR::convert_currencies(price_start = Price.EU, 
                                                       from = 'EUR', 
                                                       to = 'GBP', 
                                                       date = saleDate)]
  
  # Withdrawn 
  # Purchaser contains "withdrawn" 
  df[Sale == "Goffs", withdrawnTag := 0]
  df[Sale == "Goffs" &
                 is.na(Price.EU) & 
                 str_detect(Purchaser, regex('Withdrawn', ignore_case = T)), 
               withdrawnTag := 1]
  
  # Vendor Buyback 
  # Purchaser contains "vendor"
  df[Sale == "Goffs", vendorBuybackTag := 0]
  df[Sale == "Goffs" &
                 str_detect(Purchaser, regex('Vendor', ignore_case = T)), 
               vendorBuybackTag := 1]
  df[Sale == "Goffs" & 
                 vendorBuybackTag == 1, 
               vendorBuybackPrice.GBP := priceR::convert_currencies(price_start = as.numeric(str_extract(Purchaser, "\\d+")), 
                                                                    from = 'EUR', 
                                                                    to = 'GBP', 
                                                                    date = saleDate)]
  
  # Not Sold 
  # Purchaser contains "n.sold" | "Not Sold"
  df[Sale == "Goffs", notSoldTag := 0]
  df[Sale == "Goffs" &
                 (str_detect(Purchaser, regex('Not Sold', ignore_case = T)) |
                    str_detect(Purchaser, regex('N.Sold', ignore_case = T))), 
               notSoldTag := 1]
  df[Sale == "Goffs" &
                 notSoldTag == 1, 
               notSoldPrice.GBP := priceR::convert_currencies(price_start = as.numeric(str_extract(Purchaser, "\\d+")), 
                                                              from = 'EUR', 
                                                              to = 'GBP', 
                                                              date = saleDate)]
  
  # Post Sale
  # Purchaser contains "(PS)"
  df[Sale == "Goffs", postSaleTag := 0]
  df[Sale == "Goffs" &
                 str_detect(Purchaser,  "\\(PS\\)"), 
               postSaleTag := 1]
  df[Sale == "Goffs" &
                 postSaleTag == 1,
               postSalePrice.GBP := Price.GBP]
  
  # Tattersalls UK  
  
  # Withdrawn 
  # Purchaser contains "Lot Withdrawn" 
  df[Sale %in% c('TNew', 'TAsc'), withdrawnTag := 0]
  df[Sale %in% c('TNew', 'TAsc') &
                 is.na(Price.GBP) & 
                 str_detect(Purchaser, regex('Lot Withdrawn', ignore_case = T)), 
               withdrawnTag := 1]
  
  # Vendor Buyback 
  # Purchaser contains "vendor"
  df[Sale %in% c('TNew', 'TAsc'), vendorBuybackTag := 0]
  df[Sale %in% c('TNew', 'TAsc') &
                 str_detect(Purchaser, regex('Vendor', ignore_case = T)), 
               vendorBuybackTag := 1]
  df[Sale %in% c('TNew', 'TAsc') & 
                 vendorBuybackTag == 1, 
               vendorBuybackPrice.GBP := Price.GBP]
  
  
  # Not Sold 
  # Purchaser contains "n.sold" | "Not Sold"
  df[Sale %in% c('TNew', 'TAsc'), notSoldTag := 0]
  df[Sale %in% c('TNew', 'TAsc') &
                 str_detect(Purchaser, regex('Lot Not Sold', ignore_case = T)), 
               notSoldTag := 1]
  df[Sale %in% c('TNew', 'TAsc') &
                 notSoldTag == 1, 
               notSoldPrice.GBP := Price.GBP]
  
  # Post Sale
  # Purchaser contains "(PS)"
  df[Sale %in% c('TNew', 'TAsc'), postSaleTag := 0]
  df[Sale %in% c('TNew', 'TAsc') &
                 str_detect(Purchaser,  "\\(P.S.\\)"), 
               postSaleTag := 1]
  df[Sale %in% c('TNew', 'TAsc') &
                 postSaleTag == 1,
               postSalePrice.GBP := Price.GBP]
  
  # Tattersalls IRE 
  
  df[Sale == "TIre",
               Price.GBP := priceR::convert_currencies(price_start = Price.EU, 
                                                       from = 'EUR', 
                                                       to = 'GBP', 
                                                       date = saleDate)]
  
  # Withdrawn 
  # Purchaser contains "Lot Withdrawn" 
  df[Sale == 'TIre', withdrawnTag := 0]
  df[Sale == 'TIre' &
                 is.na(Price.EU) & 
                 str_detect(Purchaser, regex('Lot Withdrawn', ignore_case = T)), 
               withdrawnTag := 1]
  
  # Vendor Buyback 
  # Purchaser contains "vendor"
  df[Sale == 'TIre', vendorBuybackTag := 0]
  df[Sale == 'TIre' &
                 str_detect(Purchaser, regex('Vendor', ignore_case = T)), 
               vendorBuybackTag := 1]
  df[Sale == 'TIre' & 
                 vendorBuybackTag == 1, 
               vendorBuybackPrice.GBP := Price.GBP]
  
  
  # Not Sold 
  # Purchaser contains "n.sold" | "Not Sold"
  df[Sale == 'TIre', notSoldTag := 0]
  df[Sale == 'TIre' &
                 str_detect(Purchaser, regex('Lot Not Sold', ignore_case = T)), 
               notSoldTag := 1]
  df[Sale == 'TIre' &
                 notSoldTag == 1, 
               notSoldPrice.GBP := Price.GBP]
  
  # Post Sale
  # Purchaser contains "(PS)"
  df[Sale == 'TIre', postSaleTag := 0]
  df[Sale == 'TIre' &
                 str_detect(Purchaser,  "\\(P.S.\\)"), 
               postSaleTag := 1]
  df[Sale == 'TIre' &
                 postSaleTag == 1,
               postSalePrice.GBP := Price.GBP]
  
  
  # Arqana
  
  # Vendu = Sold
  # Racheté = Vendor Buyback 
  # Amiable = Private Sale
  # Absent = Withdrawn 
  # Retiré = Withdrawn 
  # Not Sold = Non Vendu 
  
  df[Sale == "Arq" & 
                 Price.EU == 0, Price.EU := NA_real_]
  
  df[Sale == "Arq",
               Price.GBP := priceR::convert_currencies(price_start = Price.EU, 
                                                       from = 'EUR', 
                                                       to = 'GBP', 
                                                       date = saleDate)]
  # Withdrawn 
  # Purchaser contains "Lot Withdrawn" 
  df[Sale == 'Arq', withdrawnTag := 0]
  df[Sale == 'Arq' &
                 is.na(Price.EU) & 
                 (str_detect(Purchaser, regex('Absent', ignore_case = T)) |
                    str_detect(Purchaser, regex('Retiré', ignore_case = T))), 
               withdrawnTag := 1]
  
  # Vendor Buyback 
  # Purchaser contains "vendor"
  df[Sale == 'Arq', vendorBuybackTag := 0]
  df[Sale == 'Arq' &
                 str_detect(Purchaser, regex('Racheté', ignore_case = T)), 
               vendorBuybackTag := 1]
  df[Sale == 'Arq' & 
                 vendorBuybackTag == 1, 
               vendorBuybackPrice.GBP := Price.GBP]
  
  
  # Purchaser contains "Non Vendu"
  df[Sale == 'Arq', notSoldTag := 0]
  df[Sale == 'Arq' &
                 str_detect(Purchaser, regex('Non Vendu', ignore_case = T)), 
               notSoldTag := 1]
  df[Sale == 'Arq' &
                 notSoldTag == 1, 
               notSoldPrice.GBP := Price.GBP]
  
  
  # Post Sale
  # Purchaser contains "(PS)"
  df[Sale == 'Arq', postSaleTag := 0]
  df[Sale == 'Arq' &
                 str_detect(Purchaser,  "Amiable"), 
               postSaleTag := 1]
  df[Sale == 'Arq' &
                 postSaleTag == 1,
               postSalePrice.GBP := Price.GBP]
  
  
  # Finally
  
  # Check for lacking prices
  print(paste0('Droppping ',
               NROW(df[(withdrawnTag == 0 &
                    is.na(vendorBuybackPrice.GBP) &
                    is.na(notSoldPrice.GBP) &
                    is.na(postSalePrice.GBP) &
                    is.na(Price.GBP))]), 
               ' horses not sold'))
  
  df <- df[!(withdrawnTag == 0 &
          is.na(vendorBuybackPrice.GBP) &
          is.na(notSoldPrice.GBP) &
          is.na(postSalePrice.GBP) &
          is.na(Price.GBP))]
  
  # Check for withdrawn horses
  print(paste0('Droppping ',
               NROW(df[(withdrawnTag == 1)]), 
               ' withdrawn horses'))
  
  df <- df[!(withdrawnTag == 1)]
  
  return(df)
  
}