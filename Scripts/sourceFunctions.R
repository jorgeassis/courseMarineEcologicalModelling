## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Functions
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

packages.to.use <- c("devtools","mapproj","knitr","sf","worms","RCurl","RJSONIO","sp","rgdal","rgeos","raster","geosphere","ggplot2","gridExtra","rnaturalearth","rnaturalearthdata","leaflet","rgbif","dismo","sdmpredictors")

options(warn=-1)

for(package in packages.to.use) {
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  if( ! "rnaturalearthhires" %in% rownames(installed.packages()) ) { devtools::install_github("ropensci/rnaturalearthhires") 
 }
  library(package, character.only = TRUE)
}

options(warn=0)

## -----------------------------------------------------------------------------------------------

getCoordinates <- function(address) {
  
  construct.geocode.url <- function(x, return.call = "json", sensor = "false") {
    
    root <- "http://www.mapquestapi.com/geocoding/v1/"
    u <- paste(root, "address?key=mpIx2AWq4Lj9R0mDbW1hNWrPe1Jju4X9&location=", x, sep = "")
    return(URLencode(u))
  }
  
  coords <- data.frame()
  
  options(warn=-1)
  
  for(address.i in address) {
  
    x <- NA
    u <- construct.geocode.url(address.i)
    doc <- getURL(u)
    
    tryCatch({
      x <- fromJSON(doc,simplify = FALSE)
    }, error=function(e){ error <- TRUE })
    
    if( is.na(x) | is.na(address.i) ) { lat <- NA ; lng <- NA }
    
    if(!  is.na(x) ) { 
      
      lat <- x$results[[1]]$locations[[1]]$latLng$lat
      lng <- x$results[[1]]$locations[[1]]$latLng$lng
      
      }

    coords <- rbind(coords,data.frame(Lon=lng,Lat=lat))
    
  }

  options(warn=0)
  
  return(coords)
  
}

## -----------------------------------------------------------------------------------------------
