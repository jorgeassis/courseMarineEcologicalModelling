## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Functions
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

packages.to.use <- c("plyr","devtools","Rcpp","ENMeval","plotROC","dismo","sp","rgdal","rgeos","raster","ggplot2","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","robis","sdmpredictors","SDMtune", "RColorBrewer")

# "SDMTools"
# "RColorBrewer","ENMeval","enmSdm"
# packages.to.use <- c("ENMeval","plotROC","RColorBrewer","devtools","shiny","robis","mapproj","knitr","sf","worms","RCurl","RJSONIO","sp","rgdal","rgeos","raster","geosphere","ggplot2","gridExtra","rnaturalearth","rnaturalearthdata","leaflet","leaflet.extras","rgbif","dismo","SDMtune","sdmpredictors")

options(warn=-1)

for(package in packages.to.use) {
  
  if( ! "devtools" %in% rownames(installed.packages()) ) { install.packages( package ) }

  if( ! "rnaturalearthhires" %in% rownames(installed.packages()) ) { devtools::install_github("ropensci/rnaturalearthhires")  }
  
  # if( ! "SDMTools" %in% rownames(installed.packages()) ) { devtools::install_github('dbahrdt/SDMTools@ignore_invalid')  }

  # if( ! "SDMtune" %in% rownames(installed.packages()) ) { detach("dismo", unload=TRUE); remove.packages("dismo"); install.packages("dismo" , type = "source" ); install.packages( package , type = "source" )  }
  
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package ) }
  if( ! package %in% rownames(installed.packages()) ) { install.packages( package , type = "source" ) }

  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  
  library(package, character.only = TRUE)

}

options(warn=0)

## --------------

print("All packages are correctly installed and loaded")

## -----------------------------------------------------------------------------------------------


produceModel <- function(myRasterLayers,myRecords) {
  
  options(warn=-1)
  spobj1 <- records
  spobj1 <- spobj1[which(!is.na(spobj1[,1])),] 
  spobj1 <- spobj1[which(!is.na(spobj1[,2])),] 
  spobj2 <- subset(myRasterLayers,1)
  spobj2 <- crop(spobj2,extent(c(min(spobj1[,1])-1,max(spobj1[,1])+1,min(spobj1[,2])-1,max(spobj1[,2])+1)))
  
  toCorrect <- which(is.na(raster::extract(spobj2,spobj1[c(1,2)])))
  overLand <- 0
  dist <- 25
  
  if( length(toCorrect) > 0) {
    
    spobj2Cells <- Which(!is.na(spobj2),cells=T)
    corrected <- xyFromCell(spobj2,spobj2Cells)
    
    for(i in 1:length(toCorrect)) {
      
      dists <- spDistsN1(as.matrix(corrected),as.matrix(spobj1[,c(1,2)])[toCorrect[i],],longlat = TRUE)
      
      if( min(dists) <= dist){ 
        closest <- which.min(spDistsN1(as.matrix(corrected),as.matrix(spobj1[,c(1,2)])[toCorrect[i],],longlat = TRUE))
        spobj1[toCorrect[i],1] <- corrected[closest,1]
        spobj1[toCorrect[i],2] <- corrected[closest,2]
      }
      
      if(min(dists) > dist){ 
        spobj1[toCorrect[i],1] <- NA
        spobj1[toCorrect[i],2] <- NA
        overLand <- overLand + 1
      }
    }
    
  }
  
  records <- spobj1[which(!is.na(spobj1[,1])),] 
  
  # generate pseudo absences
  pseudoAbs <- pseudoAbsences(myRasterLayers,records,n=1000)
  plot(pseudoAbs)
  points(records, col="red")
  
  ## -----------------------
  # 04. fit a model with best hyperparameters
  
  # extract environmental values and make a data.frame with PA information
  
  p <- records
  a <- pseudoAbs
  m <- subset(myRasterLayers,1)
  
  p.i <- raster::extract(m,p)
  p <- p[which(!is.na(p.i)),]
  
  p <- xyFromCell(m,cellFromXY(m,p))
  p <- unique(p)
  
  a.i <- raster::extract(m,a)
  a <- a[which(!is.na(a.i)),]
  
  a <- xyFromCell(m,cellFromXY(m,a))
  a <- unique(a)
  
  modelData <- prepareSWD(species = "Model species", p = p, a = a, env = myRasterLayers)
  
  # Generate cross validation folds
  folds <- getBlocks(modelData)
  
  # fit a BRT model with cross-validation and 
  model <- train("BRT", modelData, folds = folds)
  
  # given a set of possible hyperparameter values for BRT
  h <- list(interaction.depth = c(1,2,3,4) , shrinkage = c(0.01,0.001) )
  
  # test all possible combinations of hyperparameter values
  exp1 <- gridSearch(model, hypers = h, metric = "auc")
  
  # fit a BRT model to the dataset with the best hyperparameter values
  model <- train("BRT", modelData, folds = folds , interaction.depth=exp1@results[which.max(exp1@results$test_AUC),"interaction.depth"], shrinkage=exp1@results[which.max(exp1@results$test_AUC),"shrinkage"] )
  
  options(warn=0)
  
  return(model)
}

## -----------------------------------------------------------------------------------------------

stackDirectory <- function(directory) {
  
  library(raster)
  files <- list.files(directory, pattern="tif", full.names = TRUE )
  files <- files[!grepl("xml",files)]
  if(length(files) == 0) { cat("Found 0 files in directory\n")}
  if(length(files) != 0) { cat("Found",length(files),"files in directory\n")}
  return(stack(files))
  
}

## -----------------------------------------------------------------------------------------------

getOccurrencesObis <- function(taxa) {
  
  resultStruct <- data.frame(scientificName=NA,locality=NA,decimalLongitude=NA,decimalLatitude=NA,depth=NA,year=NA,month=NA,dat=NA,stringsAsFactors = FALSE)
  result <- occurrence(taxa)
  
  if(nrow(result) > 0) {
    result <- rbind.fill(resultStruct,result)
    result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(as.character(result$decimalLongitude)),Lat=as.numeric(as.character(result$decimalLatitude)),Depth=as.numeric(as.character(result$depth)),dateYear=as.numeric(as.character(result$year)),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
    result <- result[-1,]
  }

  return(result)
}

## -----------------------------------------------------------------------------------------------

getOccurrencesGBIF <- function(taxa) {
  
  taxa <- unlist(strsplit(taxa, " "))
  
  resultStruct <- data.frame(scientificName=NA,locality=NA,decimalLongitude=NA,decimalLatitude=NA,depth=NA,year=NA,month=NA,dat=NA,stringsAsFactors = FALSE)
  
  if( length(length(taxa)) == 1) { result <- gbif(taxa[1],taxa[2]) }
  if( length(length(taxa)) == 2) { result <- gbif(taxa[1],taxa[2]) }
  
  if( nrow(result) > 0) {
    result <- rbind.fill(resultStruct,result)
    result <- data.frame(Name=result$scientificName,Locality=result$locality,Lon=as.numeric(as.character(result$lon)),Lat=as.numeric(as.character(result$lat)),Depth=as.numeric(as.character(result$depth)),dateYear=as.numeric(as.character(result$year)),dateMonth=as.numeric(result$month),dateDay=as.numeric(result$day),stringsAsFactors = FALSE)
    result <- result[-1,]
  }
  
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

removeOverLandDist <- function(spobj1,lonName,latName,dist=9) {
  
  options(warn=-1)
  spobj1 <- spobj1[which(!is.na(spobj1[,lonName])),] 
  spobj1 <- spobj1[which(!is.na(spobj1[,latName])),] 
  spobj2 <- sdmpredictors::load_layers("BO_bathymean")
  spobj2 <- crop(spobj2,extent(c(min(spobj1[,lonName])-1,max(spobj1[,lonName])+1,min(spobj1[,latName])-1,max(spobj1[,latName])+1)))
  
  toCorrect <- which(is.na(raster::extract(spobj2,spobj1[c(lonName,latName)])))
  overLand <- 0
  
  if( length(toCorrect) > 0) {
    
    spobj2Cells <- Which(!is.na(spobj2),cells=T)
    corrected <- xyFromCell(spobj2,spobj2Cells)

    for(i in 1:length(toCorrect)) {
      
      dists <- spDistsN1(as.matrix(corrected),as.matrix(spobj1[,c(lonName,latName)])[toCorrect[i],],longlat = TRUE)
      
      if( min(dists) <= dist){ 
        closest <- which.min(spDistsN1(as.matrix(corrected),as.matrix(spobj1[,c(lonName,latName)])[toCorrect[i],],longlat = TRUE))
        spobj1[toCorrect[i],lonName] <- corrected[closest,1]
        spobj1[toCorrect[i],latName] <- corrected[closest,2]
      }
      
      if(min(dists) > dist){ 
        spobj1[toCorrect[i],lonName] <- NA
        spobj1[toCorrect[i],latName] <- NA
        overLand <- overLand + 1
      }
    }
    
  }

  options(warn=0)
  
  cat("Removing",overLand,"records over Land")
  
  spobj1 <- spobj1[which(!is.na(spobj1[,lonName])),] 
  return(spobj1)

}

## -----------------------------------------------------------------------------------------------

removeOverOffshore <- function(spobj1,lonName,latName,intertidalmask = "Data/RasterLayers/CoastLine.tif") {
  
  options(warn=-1)
  spobj1 <- spobj1[which(!is.na(spobj1[,lonName])),] 
  spobj1 <- spobj1[which(!is.na(spobj1[,latName])),] 
  spobj2 <- raster(intertidalmask)
  
  toCorrect <- which(is.na( raster::extract(spobj2,spobj1[,c(lonName,latName)]) ))

  if( length(toCorrect) > 0) {
    
    spobj1 <- spobj1[-toCorrect,]

  }
  
  options(warn=0)
  
  cat("Removing",length(toCorrect),"offshore records")
  
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
  
  over <- names(which( ! is.na(over(spobj1,spobj2) )))
  return(over)
}

## -----------------------------------------------------------------------------------------------

prepareModelData <- function(p,a,env) {
  
  m <- subset(env,1)
  
  p.i <- extract(m,p)
  p <- p[which(!is.na(p.i)),]
  
  p <- xyFromCell(m,cellFromXY(m,p))
  p <- unique(p)
  
  a.i <- extract(m,a)
  a <- a[which(!is.na(a.i)),]
  
  a <- xyFromCell(m,cellFromXY(m,a))
  a <- unique(a)
  
  return(prepareSWD(species = "Model species", p = p, a = a, env = env))
  
}

## -----------------------------------------------------------------------------------------------

getBlocks <- function(modelData) {
  
  get.block(modelData@coords[modelData@pa == 1,], modelData@coords[modelData@pa == 0,])
  
}
  
## -----------------------------------------------------------------------------------------------

removeNA <- function(records,lonName,latName) {
  
  cat("Removing",sum( is.na( records[,lonName] ) ),"NA records")
  records <- records[which( ! is.na(records[,lonName]) ), ]
  records <- records[which( ! is.na(records[,latName]) ), ]
  return(records)
  
}

## -----------------------------------------------------------------------------------------------

removeDuplicated <- function(records,lonName,latName) {
  
  cat("Removing",length(which( duplicated( records[,c(lonName,latName)] ) )),"duplicated records")
  records <- records[ which( ! duplicated( records[,c(lonName,latName)] ) ), ]
  
  return(records)
  
}

## -----------------------------------------------------------------------------------------------

trainGLM <- function(modelData) {
  
  data <- modelData@data
  data <- data.frame(PA=modelData@pa,modelData@data)
  model <- glm( paste0("PA ~ ",paste(colnames(modelData@data),collapse = " + ")) , family="binomial", data=data)
  
  return(model)
  
}

## -----------------------------------------------------------------------------------------------

pseudoAbsences <- function(rasters,records,n) {
  
  shape <- subset(rasters,1)
  nonNACells <- Which(!is.na(shape), cells=TRUE) 
  sink.points <- xyFromCell(shape, nonNACells)
  
  # Removes those closer than paDist
  
  sink.points.poly <- as.data.frame(records)
  coordinates( sink.points.poly ) <- c( "Lon", "Lat" )
  proj4string( sink.points.poly ) <- CRS( "+proj=longlat +datum=WGS84" )
  
  sink.points.poly <- gBuffer( sink.points.poly, width=50 / 111.699, byid=TRUE )
  # plot(sink.points.poly)
  
  sink.points.pts <- as.data.frame(sink.points)
  colnames( sink.points.pts ) <- c( "Lon", "Lat" )
  coordinates( sink.points.pts ) <- c( "Lon", "Lat" )
  proj4string( sink.points.pts ) <- CRS( "+proj=longlat +datum=WGS84" )
  
  to.remove.id <- sp::over(sink.points.pts,sink.points.poly)
  
  if( class(to.remove.id) == "integer" ) { to.keep <- which(is.na(to.remove.id)) }

  if( class(to.remove.id) == "data.frame" ) { 
    
    if(ncol(to.remove.id) > 1) { to.keep <- which(is.na(to.remove.id[,1])) }
    if(ncol(to.remove.id) == 1) { to.keep <- which(is.na(to.remove.id)) }
    
  }
  
  absences <- sink.points[to.keep,]
  absences <- as.data.frame(absences)
  
  absences.n <- sample( 1:nrow(absences) , min(n,nrow(absences)) , replace=FALSE)
  absences <- absences[absences.n,]
  colnames(absences) <- c("Lon","Lat")
  
  return(absences)
  
}

## -----------------------------------------------------------------------------------------------

backgroundInformation <- function(rasters,n) {
  
  shape <- subset(rasters,1)
  nonNACells <- Which(!is.na(shape), cells=TRUE) 
  sink.points <- xyFromCell(shape, nonNACells)
  
  absences <- sample( 1:nrow(sink.points) , min(n,nrow(sink.points)) , replace=FALSE)
  absences <- sink.points[absences,]
  colnames(absences) <- c("Lon","Lat")
  
  return(absences)
  
}

## -----------------------------------------------------------------------------------------------

trainBRT <- function(data, distribution = "bernoulli", n.trees = 1000,
                     interaction.depth = 1, shrinkage = 0.1,
                     bag.fraction = 0.5 ) {
  
  result <- SDMmodel(data = data)
  
  df <- data@data
  df <- cbind(pa = data@pa, df)

  if( exists("monotonicity") ) {
    
    model <- gbm::gbm(pa ~ ., data = df, distribution = distribution,
                      n.trees = n.trees, interaction.depth = interaction.depth,
                      shrinkage = shrinkage, bag.fraction = bag.fraction, var.monotone=monotonicity[,which(colnames(monotonicity) %in% colnames(df))])
    
  }
  if( ! exists("monotonicity")) {
    
    model <- gbm::gbm(pa ~ ., data = df, distribution = distribution,
                      n.trees = n.trees, interaction.depth = interaction.depth,
                      shrinkage = shrinkage, bag.fraction = bag.fraction)
    
  }
  
  model_object <- BRT(n.trees = n.trees, distribution = distribution,
                      interaction.depth = interaction.depth,
                      shrinkage = shrinkage, bag.fraction = bag.fraction,
                      model = model)

  result@model <- model_object
  
  return(result)
}

environment(trainBRT) <- asNamespace('SDMtune')
assignInNamespace("trainBRT", trainBRT, ns = "SDMtune")

## -----------------------------------------------------------------------------------------------

getAUC <- function(model, test = NULL) {
  
  return(SDMtune::auc(model, test = test))
  
}

## -----------------------------------------------------------------------------------------------

thresholdMaxTSS <- function(model) {
  
  r <- getAccuracy(model,threshold = seq(0,1,by=0.01))
  
  plot(r$threshold,r$sensitivity,type="l", xlab="Threshold",ylab="Performance" , lty=1, lwd=1 )
  lines(r$threshold,r$specificity , lty=1, lwd=4 , col= "gray")
  
  val <- unlist(r$threshold)[which.max( unlist(r$sensitivity) + unlist(r$specificity) )]
  
  abline(v=val , lty=3, lwd=0.7 )
  legend(0.7, 0.9, legend=c("Sensitivity", "Specificity"),col=c("black", "gray"), lty=1, lwd=c(1,4) , cex=1)
  
  return(val)
  
}

## -----------------------------------------------------------------------------------------------

getAccuracy <- function(model,threshold=0.5) {
  
  predMain <- predict(model, model@data@data, type=c("logistic")) 
  acc <- data.frame()
  
  for(threshold.i in threshold ) {
    
    pred <- predMain
    pred[pred >= threshold.i] <- 1
    pred[pred < threshold.i] <- 0
    
    confusionDF <- data.frame(obs=model@data@pa,pred)
    sensitivity <- sum(apply(confusionDF[confusionDF$obs == 1,],1,function(x) { ifelse(x[1] == 1 & x[2] == 1 , 1 , 0 ) })) / sum(confusionDF$obs == 1)
    specificity <- sum(apply(confusionDF[confusionDF$obs == 0,],1,function(x) { ifelse(x[1] == 0 & x[2] == 0 , 1 , 0 ) })) / sum(confusionDF$obs == 0)
    
    acc <- rbind(acc,data.frame(threshold=threshold.i,tss=specificity+sensitivity-1,sensitivity=sensitivity,specificity=specificity))
    
  }
 return(acc)
  
}
