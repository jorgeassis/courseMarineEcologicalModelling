## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Functions
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

packages.to.use <- c("devtools","shiny","robis","mapproj","knitr","sf","worms","RCurl","RJSONIO","sp","rgdal","rgeos","raster","geosphere","ggplot2","gridExtra","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","rgbif","dismo","sdmpredictors")

options(warn=-1)

for(package in packages.to.use) {
  
  if( ! "rnaturalearthhires" %in% rownames(installed.packages()) ) { devtools::install_github("ropensci/rnaturalearthhires")  }
  if( ! "robis" %in% rownames(installed.packages()) ) { devtools::install_github("iobis/robis")  }

  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package , type = "source" ) }

  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  
  library(package, character.only = TRUE)

}

options(warn=0)

## -----------------------------------------------------------------------------------------------

defineRegion <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),c(lonName,latName)] 
  
  ui <- fluidPage(leafletOutput("mymap",height=500))
  
  server <- function(input, output) {
    
    output$mymap <- renderLeaflet(
      leaflet() %>%
        addProviderTiles("Esri.OceanBasemap",group = "Ocean Basemap") %>%
        
        addCircles(lng=records[,lonName], lat=records[,latName] , weight = 3,color="Black",radius = 6) %>%
        
        addDrawToolbar(
          targetGroup='draw')  
      
    )
    
    observeEvent(input$mymap_draw_new_feature,{
      feature <<- input$mymap_draw_new_feature
      
      print(feature)
      
    })
    
  }
  
  return(shinyApp(ui = ui, server = server))
  
}
  
## -----------------------------------------------------------------------------------------------

selectRecords <- function(records,lonName,latName) {
  
  records <- records[which(!is.na(records[,lonName])),] 
  
  nPoints <- length(feature$geometry$coordinates[[1]])
  sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })
  poly <- spPolygons(t(sapply(1:nPoints,function(x) { unlist(feature$geometry$coordinates[[1]][[x]]) })))
  
  spobj1 <- SpatialPointsDataFrame(records[,c(lonName,latName)], data=records)
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(poly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  records <- records[as.numeric(which( ! is.na(over(spobj1,poly) ))),]
  
  return(records)
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
  
  crs(spobj1) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  crs(spobj2) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  which( ! is.na(over(spobj2,spobj1)[,1] ))
  
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
## -----------------------------------------------------------------------------------------------

getOccurrencesObis <- function(taxa) {
  
  result <- occurrence(taxa)
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(result$decimalLongitude),Lat=as.numeric(result$decimalLatitude),Depth=as.numeric(result$depth),dateYear=as.numeric(result$year),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

getOccurrencesGBIF <- function(taxa) {
  
  taxa <- unlist(strsplit(taxa, " "))
  
  if( length(length(taxa)) == 1) { result <- gbif(taxa[1],taxa[2]) }
  if( length(length(taxa)) == 2) { result <- gbif(taxa[1],taxa[2]) }
  result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(result$lon),Lat=as.numeric(result$lat),Depth=as.numeric(result$depth),dateYear=as.numeric(result$year),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
  return(result)
}

## -----------------------------------------------------------------------------------------------

decimals <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

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

getLocation <- function(coordLon,coordLat) {
  
  construct.geocode.url <- function(coordLon,coordLat, return.call = "json", sensor = "false") {
    
    root <- "http://www.mapquestapi.com/geocoding/v1/reverse?key=mpIx2AWq4Lj9R0mDbW1hNWrPe1Jju4X9&"
    u <- paste(root, "location=", coordLat,",",coordLon,"", sep = "") #&includeRoadMetadata=true&includeNearestIntersection=true
    return(URLencode(u))
  }
  
  u <- construct.geocode.url(coordLon,coordLat)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  
  
  return(x$results[[1]]$locations[[1]]$adminArea1)
  
}

## -----------------------------------------------------------------------------------------------
