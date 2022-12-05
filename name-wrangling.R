###################
#
# helperFunctions.R
#
# Henry Baugniet 
# 
#
#
#
###################

# Form sire and damstrip 
# A function to add a sire and dam strip 

strip_maker <- function(myData = 'test'){
  
  myData <- as.data.table(myData)
  
  # Converts any dogdy names 
  names(myData) <- iconv(names(myData), from="UTF-8", to="ASCII//TRANSLIT")
  names(myData) <- str_replace_all(names(myData), '[`]', '')
  
  # Converts French Birth to Year
  if (any(grepl('Date de naissance', names(myData)))){
    
    #Change DOB to year
    birthIndex <- grep('Date', names(myData))
    colnames(myData)[birthIndex] <- 'Year'
    
    myData <- myData[, Year := as.Date(Year)]
    myData <- myData[, Year := as.numeric(format(Year, '%Y'))]
  }
  
  # Dam Breeding Suffix
  # Arqana
  if (any(grepl('Suffixe Mere', names(myData)))){
    damSuffixIndex <- grep('Suffixe Mere', names(myData))
    colnames(myData)[damSuffixIndex] <- 'DAMSUFFIX'
  }
  
  # Sire Breeding Suffix
  # Arqana
  if (any(grepl('Suffixe Pere', names(myData)))){
    sireSuffixIndex <- grep('Suffixe Pere', names(myData))
    colnames(myData)[sireSuffixIndex] <- 'SIRESUFFIX'
  }
  
  # DAMSTRIP
  if (!is.null(myData$Dam)){
    myData$Dam <- str_remove(myData$Dam, "(?=\\().*?(?<=\\))")
    myData$DAMSTRIP <- toupper(gsub('\\s+', '', myData$Dam))
    myData[, DAMSTRIP := gsub('[[:punct:]]+','',DAMSTRIP)]
  }
  # This will work for arqana
  else if(!is.null(myData$Mere)){
    myData$Mere <- str_remove(myData$Mere, "(?=\\().*?(?<=\\))")
    myData$DAMSTRIP <- toupper(gsub('\\s+', '', myData$Mere))
    myData[, DAMSTRIP := gsub('[[:punct:]]+','',DAMSTRIP)]
  }
  
  #SIRESTRIP
  if (!is.null(myData$Sire)){
    myData$Sire <- str_remove(myData$Sire, "(?=\\().*?(?<=\\))")
    myData$SIRESTRIP <- toupper(gsub('\\s+', '', myData$Sire))
    myData[, SIRESTRIP := gsub('[[:punct:]]+','',SIRESTRIP)]
  }
  
  # This will work for arqana
  else if(!is.null(myData$Pere)){
    myData$Pere <- str_remove(myData$Pere, "(?=\\().*?(?<=\\))")
    myData$SIRESTRIP <- toupper(gsub('\\s+', '', myData$Pere))
    myData[, SIRESTRIP := gsub('[[:punct:]]+','', SIRESTRIP)]
  }
  
  myData$SIRESTRIP.DAMSTRIP.BIRTHYEAR <- str_c(myData$SIRESTRIP, '.', myData$DAMSTRIP, '.', myData$Year)
  
  return(myData)
}


# This is a function that will produce the same format of data.table for each 
# different sale file. 
read_catalog <- function(fileName = 'Arq_BUP17.xlsx', filePath = '/Downloads'){
  
  if (str_detect(fileName, 'Arq')){
    
    # I am assuming all have LOT, NOM and DOB
    
    # Skip one row as we don't want it 
    myXLFile <- readxl::read_excel(file.path(filePath, fileName), skip = 1)
    
    # In Case first row has already been deleted
    if (is.null(myXLFile$Nom)){ 
      myXLFile <- readxl::read_excel(file.path(filePath, fileName))
    }
    
    #Removes statistiques at bottom
    myXLFile <- head(myXLFile, -4)
    myXLFile <- as.data.table(myXLFile)
    
    #Stripmaking 
    myXLFile <- strip_maker(myXLFile)
    
    #Get Strip for matching 
    freshDataTable <- myXLFile[, ..STRIP_VARS]
    freshDataTable <- as.data.table(freshDataTable)
    
    #Add in Sale Name and File Name 
    acronym <- str_extract(fileName, "[^_]+")
    freshDataTable <- freshDataTable[, Sale := acronym]
    freshDataTable <- freshDataTable[, FileName := fileName]
    
    # Match by STRIP then add NA in if isn't null
    z <- match(freshDataTable$SIRESTRIP.DAMSTRIP.BIRTHYEAR, myXLFile$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
    
    # Lot 
    if (!is.null(myXLFile$Lot)){
      freshDataTable$Lot <- myXLFile$Lot[z]
    }
    else{
      freshDataTable$Lot <- NA
    }
    
    # Name
    if (!is.null(myXLFile$Nom)){
      freshDataTable$Name <- myXLFile$Nom[z]
    }
    else{
      freshDataTable$Name <- NA
    }
    
    
    # Country 
    if (!is.null(myXLFile$Suffixe)){
      freshDataTable$Country <- myXLFile$Suffixe[z]
    }
    else{
      freshDataTable$Country <- NA
    }
    
    # Year
    if (!is.null(myXLFile$Year)){
      freshDataTable$Year <- myXLFile$Year[z]
    }
    else{
      freshDataTable$Year <- NA
    }
    
    # Colour
    if (!is.null(myXLFile$Couleur)){
      freshDataTable$Colour <- myXLFile$Couleur[z]
    }
    else{
      freshDataTable$Colour <- NA
    }
    
    #Sire
    if (any(grepl('Pere', names(myXLFile)))){
      
      sireIndex <- grep('Pere', names(myXLFile))
      colnames(myXLFile)[sireIndex] <- 'Sire'
      
      freshDataTable$Sire <- myXLFile$Sire[z]
    }
    else{
      freshDataTable$Sire <- NA
    }
    
    # Sire Suffix
    if(!is.null(myXLFile$SIRESUFFIX)){
      freshDataTable$SIRESUFFIX <- myXLFile$SIRESUFFIX
    }
    else{
      freshDataTable$SIRESUFFIX <- NA
    }
    
    # Dam
    if (any(grepl('Mere', names(myXLFile)))){
      
      damIndex <- grep('Mere', names(myXLFile))
      colnames(myXLFile)[damIndex] <- 'Dam'
      
      freshDataTable$Dam <- myXLFile$Dam[z]
    }
    else{
      freshDataTable$Dam <- NA
    }
    
    # Dam Suffix
    if(!is.null(myXLFile$DAMSUFFIX)){
      freshDataTable$DAMSUFFIX <- myXLFile$DAMSUFFIX
    }
    else{
      freshDataTable$DAMSUFFIX <- NA
    }
    
    # Consignor
    if (!is.null(myXLFile$Vendeur)){
      freshDataTable$Consignor <- myXLFile$Vendeur[z]
    }
    else {
      freshDataTable$Consignor <- NA
    }
    
    # Stabling
    if (any(grepl('Box', names(myXLFile)))){
      
      birthIndex <- grep('Box', names(myXLFile))
      colnames(myXLFile)[birthIndex] <- 'Stabling'
      
      freshDataTable$Stabling <- myXLFile$Stabling[z]
    }
    else{
      freshDataTable$Stabling <- NA
    }
    
    # Purchaser
    if (!is.null(myXLFile$Acheteur)){
      freshDataTable$Purchaser <- myXLFile$Acheteur[z]
    }
    else {
      freshDataTable$Purchaser <- NA
    }
    
    # Price.GBP
    freshDataTable$Price.GBP <- NA
    
    # Price.EU
    if (any(grepl('Encheres', names(myXLFile)))){
      
      priceIndex <- grep('Encheres', names(myXLFile))
      colnames(myXLFile)[priceIndex] <- 'Price.EU'
      
      freshDataTable$Price.EU <- myXLFile$Price.EU[z]
    }
    else{
      freshDataTable$Price.EU <- NA
    }
    
    # Not completely sure that this works but check if Arq become client
    if (!is.null(myXLFile$Name)){
      freshDataTable$HorseName <- myXLFile$Name[z]
    }
    else {
      freshDataTable$HorseName <- NA
    }
    
    freshDataTable$Strip <- myXLFile$SIRESTRIP.DAMSTRIP.YEAR
    
  }
  
  else if (str_detect(fileName, 'Goffs')){
    
    myXLFile <- readxl::read_excel(file.path(filePath, fileName))
    myXLFile <- as.data.table(myXLFile)
    
    # Add country to goffs format
    myXLFile$Country <- str_extract(myXLFile$Name, "(?<=\\().+?(?=\\))")
    myXLFile$Name <- str_remove(myXLFile$Name, "(?=\\().*?(?<=\\))")
    
    # Add DAMSUFFIX to goffs format
    myXLFile$DAMSUFFIX <- str_extract(myXLFile$Dam, "(?<=\\().+?(?=\\))")
    myXLFile$Dam <- str_remove(myXLFile$Dam, "(?=\\().*?(?<=\\))")
    
    # Add SIRESUFFIX to goffs format
    myXLFile$SIRESUFFIX <- str_extract(myXLFile$Sire, "(?<=\\().+?(?=\\))")
    myXLFile$Sire <- str_remove(myXLFile$Sire, "(?=\\().*?(?<=\\))")
    
    #Stripmaking 
    myXLFile <- strip_maker(myXLFile)
    
    #Get Strip for matching 
    freshDataTable <- myXLFile[, ..STRIP_VARS]
    freshDataTable <- as.data.table(freshDataTable)
    
    #Add in Sale Name
    acronym <- str_extract(fileName, "[^_]+")
    freshDataTable <- freshDataTable[, Sale := acronym]
    freshDataTable <- freshDataTable[, FileName := fileName]
    
    # Match by STRIP then add NA in if isn't null
    z <- match(freshDataTable$SIRESTRIP.DAMSTRIP.BIRTHYEAR, myXLFile$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
    
    
    # Lot 
    if (!is.null(myXLFile$Lot)){
      freshDataTable$Lot <- myXLFile$Lot[z]
    }
    else{
      freshDataTable$Lot <- NA
    }
    
    # Name
    if (!is.null(myXLFile$Name)){
      freshDataTable$Name <- myXLFile$Name[z]
    }
    else{
      freshDataTable$Name <- NA
    }
    
    # Country 
    if (!is.null(myXLFile$Country)){
      freshDataTable$Country <- myXLFile$Country[z]
    }
    else{
      freshDataTable$Country <- NA
    }
    
    # Year
    if (!is.null(myXLFile$Year)){
      freshDataTable$Year <- myXLFile$Year[z]
    }
    else{
      freshDataTable$Year <- NA
    }
    
    # Colour
    if (!is.null(myXLFile$Colour)){
      freshDataTable$Colour <- myXLFile$Colour[z]
    }
    else{
      freshDataTable$Colour <- NA
    }
    
    #Sire
    if (!is.null(myXLFile$Sire)){
      freshDataTable$Sire <- myXLFile$Sire[z]
    }
    else{
      freshDataTable$Sire <- NA
    }
    
    # Sire Suffix
    if(!is.null(myXLFile$SIRESUFFIX)){
      freshDataTable$SIRESUFFIX <- myXLFile$SIRESUFFIX
    }
    else{
      freshDataTable$SIRESUFFIX <- NA
    }
    
    # Dam
    if (!is.null(myXLFile$Dam)){
      freshDataTable$Dam <- myXLFile$Dam[z]
    }
    else{
      freshDataTable$Dam <- NA
    }
    
    # Dam Suffix
    if(!is.null(myXLFile$DAMSUFFIX)){
      freshDataTable$DAMSUFFIX <- myXLFile$DAMSUFFIX
    }
    else{
      freshDataTable$DAMSUFFIX <- NA
    }
    
    # Consignor
    if (!is.null(myXLFile$Consignor)){
      freshDataTable$Consignor <- myXLFile$Consignor[z]
    }
    else {
      freshDataTable$Consignor <- NA
    }
    
    # Stabling
    if (!is.null(myXLFile$Stabling)){
      freshDataTable$Stabling <- myXLFile$Stabling[z]
    }
    else{
      freshDataTable$Stabling <- NA
    }
    
    # Purchaser
    if (!is.null(myXLFile$Purchaser)){
      freshDataTable$Purchaser <- myXLFile$Purchaser[z]
    }
    else {
      freshDataTable$Purchaser <- NA
    }
    
    # Price.GBP
    if (any(grepl('Price', names(myXLFile)))){
      
      priceIndex <- grep('Price', names(myXLFile))
      colnames(myXLFile)[priceIndex] <- 'Price'
      
      freshDataTable$Price.GBP <- myXLFile$Price[z]
    }
    else {
      freshDataTable$Price.GBP <- NA
    }
    
    # Price.EU
    freshDataTable$Price.EU <- NA
    
    if (!is.null(myXLFile$Name)){
      freshDataTable$HorseName <- myXLFile$Name[z]
    }
    else {
      freshDataTable$HorseName <- NA
    }
    
    
  }
  
  else if(str_detect(fileName, 'TAsc')| str_detect(fileName, 'TNew')| 
          str_detect(fileName, 'TChel')| str_detect(fileName, 'TIre')){
    
    myXLFile <- readxl::read_excel(file.path(filePath, fileName))
    myXLFile <- as.data.table(myXLFile)
    
    # Add country to tatts format
    myXLFile$Country <- str_extract(myXLFile$Name, "(?<=\\().+?(?=\\))")
    myXLFile$Name <- str_remove(myXLFile$Name, "\\(.*")
    
    # Add DAMSUFFIX to tatts format
    myXLFile$DAMSUFFIX <- str_extract(myXLFile$Dam, "(?<=\\().+?(?=\\))")
    myXLFile$Dam <- str_remove(myXLFile$Dam, "(?=\\().*?(?<=\\))")
    
    # Add SIRESUFFIX to tatts format
    myXLFile$SIRESUFFIX <- str_extract(myXLFile$Sire, "(?<=\\().+?(?=\\))")
    myXLFile$Sire <- str_remove(myXLFile$Sire, "(?=\\().*?(?<=\\))")
    
    #Stripmaking 
    myXLFile <- strip_maker(myXLFile)
    
    #Get Strip for matching 
    freshDataTable <- myXLFile[, ..STRIP_VARS]
    freshDataTable <- as.data.table(freshDataTable)
    
    #Add in Sale Name
    acronym <- str_extract(fileName, "[^_]+")
    freshDataTable <- freshDataTable[, Sale := acronym]
    freshDataTable <- freshDataTable[, FileName := fileName]
    
    # Match by STRIP then add NA in if isn't null
    z <- match(freshDataTable$SIRESTRIP.DAMSTRIP.BIRTHYEAR, myXLFile$SIRESTRIP.DAMSTRIP.BIRTHYEAR)
    
    # Lot 
    if (!is.null(myXLFile$Lot)){
      freshDataTable$Lot <- myXLFile$Lot[z]
    }
    else{
      freshDataTable$Lot <- NA
    }
    
    # Name
    if (!is.null(myXLFile$Name)){
      freshDataTable$Name <- myXLFile$Name[z]
    }
    else{
      freshDataTable$Name <- NA
    }
    
    # Country 
    if (!is.null(myXLFile$Country)){
      freshDataTable$Country <- myXLFile$Country[z]
    }
    else{
      freshDataTable$Country <- NA
    }
    
    # Year
    if (!is.null(myXLFile$Year)){
      freshDataTable$Year <- myXLFile$Year[z]
    }
    else{
      freshDataTable$Year <- NA
    }
    
    # Colour
    if (!is.null(myXLFile$Colour)){
      freshDataTable$Colour <- myXLFile$Colour[z]
    }
    else{
      freshDataTable$Colour <- NA
    }
    
    #Sire
    if (!is.null(myXLFile$Sire)){
      freshDataTable$Sire <- myXLFile$Sire[z]
    }
    else{
      freshDataTable$Sire <- NA
    }
    
    # Dam
    if (!is.null(myXLFile$Dam)){
      freshDataTable$Dam <- myXLFile$Dam[z]
    }
    else{
      freshDataTable$Dam <- NA
    }
    
    # Dam Suffix
    if(!is.null(myXLFile$DAMSUFFIX)){
      freshDataTable$DAMSUFFIX <- myXLFile$DAMSUFFIX
    }
    else{
      freshDataTable$DAMSUFFIX <- NA
    }
    
    # Sire Suffix
    if(!is.null(myXLFile$SIRESUFFIX)){
      freshDataTable$SIRESUFFIX <- myXLFile$SIRESUFFIX
    }
    else{
      freshDataTable$SIRESUFFIX <- NA
    }
    
    # Consignor
    if (!is.null(myXLFile$Consignor)){
      freshDataTable$Consignor <- myXLFile$Consignor[z]
    }
    else {
      freshDataTable$Consignor <- NA
    }
    
    # Stabling
    if (!is.null(myXLFile$Stabling)){
      freshDataTable$Stabling <- myXLFile$Stabling[z]
    }
    else{
      freshDataTable$Stabling <- NA
    }
    
    # Purchaser
    if (!is.null(myXLFile$Purchaser)){
      freshDataTable$Purchaser <- myXLFile$Purchaser[z]
    }
    else {
      freshDataTable$Purchaser <- NA
    }
    
    # Price.GBP
    if (any(grepl('Price', names(myXLFile)))){
      
      priceIndex <- grep('Price', names(myXLFile))
      colnames(myXLFile)[priceIndex] <- 'Price'
      
      freshDataTable$Price.GBP <- myXLFile$Price[z]
    }
    else {
      freshDataTable$Price.GBP <- NA
    }
    
    # Price.EU
    freshDataTable$Price.EU <- NA
    
    if (!is.null(myXLFile$Name)){
      freshDataTable$HorseName <- myXLFile$Name[z]
    }
    else {
      freshDataTable$HorseName <- NA
    }
    
    
  }
  
  # Form DAMSTRIP.BREEDING
  freshDataTable$DAMSTRIP.BREEDING <- str_c(freshDataTable$DAMSTRIP, '.', freshDataTable$DAMSUFFIX)
  
  return(freshDataTable)
}

# This is a function that will combine all the xls/xlsx files in a list myExcelFilesNames 
# into one big list. 

combine_catalogs <- function(fileVector = c('Arq_Yearlings22.xlsx', 'Goffs_Yearlings22.xls'), filePath = '/Downloads') {
  
  for (name in fileVector){
    
    myLonelyFile <- read_catalog(fileName = name, filePath = filePath)
    
    if (!exists('myCombinedXL')){
      myCombinedXL <- myLonelyFile
    }
    
    else {
      myCombinedXL <- list(myCombinedXL, myLonelyFile) 
      myCombinedXL <- rbindlist(myCombinedXL, use.names = TRUE, fill = TRUE)
    }
    
  }
  return(myCombinedXL)
}
