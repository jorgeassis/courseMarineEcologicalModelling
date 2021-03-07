## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Functions
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

packages.to.use <- c("dismo","sp","rgdal","rgeos","raster","ggplot2","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","robis","sdmpredictors")

# packages.to.use <- c("ENMeval","plotROC","RColorBrewer","devtools","shiny","robis","mapproj","knitr","sf","worms","RCurl","RJSONIO","sp","rgdal","rgeos","raster","geosphere","ggplot2","gridExtra","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","rgbif","dismo","SDMTools","SDMtune","sdmpredictors")

options(warn=-1)

for(package in packages.to.use) {
  
  if( ! "rnaturalearthhires" %in% rownames(installed.packages()) ) { devtools::install_github("ropensci/rnaturalearthhires")  }
  if( ! "SDMTools" %in% rownames(installed.packages()) ) { devtools::install_github('dbahrdt/SDMTools@ignore_invalid')  }
  
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package , type = "source" ) }

  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  
  library(package, character.only = TRUE)

}

options(warn=0)

## -----------------------------------------------------------------------------------------------

getOccurrencesObis <- function(taxa) {
  
  result <- occurrence(taxa)
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(as.character(result$decimalLongitude)),Lat=as.numeric(as.character(result$decimalLatitude)),Depth=as.numeric(as.character(result$depth)),dateYear=as.numeric(as.character(result$year)),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

getOccurrencesGBIF <- function(taxa) {
  
  taxa <- unlist(strsplit(taxa, " "))
  
  if( length(length(taxa)) == 1) { result <- gbif(taxa[1],taxa[2]) }
  if( length(length(taxa)) == 2) { result <- gbif(taxa[1],taxa[2]) }
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(as.character(result$lon)),Lat=as.numeric(as.character(result$lat)),Depth=as.numeric(result$depth),dateYear=as.numeric(result$year),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

removeOverLand <- function(spobj1,lonName,latName) {
  
  spobj1 <- spobj1[which(!is.na(spobj1[,lonName])),] 
  spobj2 <- ne_countries(scale = 110)
  
  if(class(spobj1) == "data.frame" ) {
    
    spobj1 <- SpatialPointsDataFrame(spobj1[,c(lonName,latName)], data=spobj1)
    
  }
  
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(spobj2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  overLand <- which( is.na(over(spobj1,spobj2)[,1] ))
  cat("Removing",length(overLand),"records over Land")
  overLand <- which( ! is.na(over(spobj1,spobj2)[,1] ))
  spobj1 <- spobj1[-overLand,]
  spobj1 <- as.data.frame(spobj1)[,c(lonName,latName)]
  return(spobj1)
  
}

## -----------------------------------------------------------------------------------------------

whichOverPolygon <- function(spobj1,spobj2) {
  
  if(class(spobj1) == "data.frame") {
    spobj1 <- spobj1[complete.cases(spobj1),]
    spobj1 <- SpatialPoints(spobj1)
    
  }
  
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(spobj2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  which( ! is.na(over(spobj1,spobj2) ))
  
}

## -----------------------------------------------------------------------------------------------

removeNA <- function(records,lonName,latName) {
  
  cat("Removing",sum( is.na( records[,lonName] ) ),"NA records")
  records <- records[which( ! is.na(records[,lonName]) ), ]
  return(records)
  
}

## -----------------------------------------------------------------------------------------------

removeDuplicated <- function(records,lonName,latName) {
  
  cat("Removing",length(which( duplicated( records[,c(lonName,latName)] ) )),"NA records")
  records <- records[ which( ! duplicated( records[,c(lonName,latName)] ) ), ]
  
  return(records)
  
}
