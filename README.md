# Yearling Sale Data Wrangling 

### This repo contains code to wrangle data for ML yearling project. 

main.R
- Top script produces wrangled data when run 

build-summary-stats.R
- Various functions for aggregating summary statistics 

generate-consignor-score.R
- Attempt to generate a score for each consignor based on the performance of 
horses that they sold when they reach the track 
- This did not prove to have any correlation with Price, though may do with performance in 
future studies 

load-clean-outings.R
- function for loading up outings data. 

name-wrangling.R
- Deals with the assimilating sales data to be uniform

params.R
- File paths and global paramters

price-wrangling.R
- Prices across sales vary in currency and exchange rate. This file contains 
functions to deal with this. 

wrangle-consignor-names.R
- This allows for the assimilation of names where the same party sells are horse under 
different names. 