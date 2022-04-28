# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.1
# The main objective of this challenge is to plot a shapefile with the European distribution of a seagrass species. The plot must include the landmass shapefile of the world.

library(raster)

data1 <- "Data/vectorShapefiles/seagrass/cymodoceaNodosa.shp"
data2 <- "Data/vectorShapefiles/globalLandmass/world.shp"

?shapefile

records <- shapefile(data1)
world <- shapefile(data2)

plot(world)
plot(records)

plot(world)
plot(records, add=TRUE,col="red")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.2
# The main objective of this challenge is to crop a global map to the Azores islands region and plot it.

library(raster)
library(rnaturalearth)

world <- ne_countries(scale = 'medium')

# Crop with extent

regionAzores <- extent(-36,-18,34,42)

azores <- crop(world,regionAzores)
plot(azores)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.3
# The main objective of this challenge is to plot the centroids of the European no-Take MPA.

library(raster)
library(rgeos)
library(sp)

data1 <- "Data/vectorShapefiles/noTakeMPA/MPA.shp"
data2 <- "Data/vectorShapefiles/coastlinesMediterranean/coastlinesMed.shp"

mpa <- shapefile(data1)
mediterranean <- shapefile(data2)

centroids <- gCentroid(mpa, byid=TRUE)

plot(mediterranean)
plot(centroids,add=TRUE,col="red")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.4
# The main objective of this challenge is to determine and plot the more distant European no-Take MPA.

# Getting the centroid of a polygon
longitude <- c(-15,45,45,-15)
latitude <- c(30,30,47.5,47.5)
lonlat <- cbind(longitude, latitude)
region <- spPolygons(lonlat)

data1 <- "Data/vectorShapefiles/globalLandmass/world.shp"
data2 <- "Data/vectorShapefiles/noTakeMPA/MPA.shp"

coastlines <- shapefile(data1)
coastlinesMed <- crop(coastlines,region)

map <- shapefile(data2)

mpaCentroids <- gCentroid(mpa , byid=TRUE)

distanceMatrix <- spDists(mpaCentroids,mpaCentroids,longlat = TRUE)
dim(distanceMatrix)

max(distanceMatrix)
which(distanceMatrix == max(distanceMatrix) , arr.ind = TRUE)

plot(coastlinesMed, main = "Maximum air distance between European no-Take MPA", col="gray",axes = TRUE)
points(mpaCentroids[c(14,134),],col="red",pch=16)

# Intermediate points on a great circle
greatCircle <- gcIntermediate(mpaCentroids[14,], mpaCentroids[134,], n=100, addStartEnd=TRUE)
lines(greatCircle)
text(x=12.25, y=39, paste0(round(max(distanceMatrix)), "km"), font=4)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.5.1 & 2.5.2
# The main objective of this challenge is to plot the smallest no-Take MPA in Europe.

longitude <- c(-15,45,45,-15)
latitude <- c(30,30,47.5,47.5)
lonlat <- cbind(longitude, latitude)
region <- spPolygons(lonlat)

data1 <- "Data/vectorShapefiles/globalLandmass/world.shp"
data2 <- "Data/vectorShapefiles/noTakeMPA/MPA.shp"

coastlines <- shapefile(data1)
coastlinesMed <- crop(coastlines,region)

map <- shapefile(data2)

mpaAreas <- raster::area(mpa)
which.min(mpaAreas)
mpa$name[120]

plot(coastlinesMed, main = "The smaller no-Take MPA in European coastlines", col="gray", border="gray",axes = TRUE)
plot(mpa[120,],col="Black",add=TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.6
# The main objective of this challenge is to make two plots of the global maximum and minimum SST.

library(raster)

maxSST <- raster("Data/rasterLayers/Present Temperature LtMax.tif")

plot(maxSST, main ="Maximum Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.7
# The main objective of this challenge is to plot of the global range of sea surface temperatures.

library(raster)

maxSST <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")

rageSST <- maxSST - minSST
plot(rageSST, main ="Range of Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.8
# The main objective of this challenge is to plot the minimum sea surface temperatures in Europe

library(raster)

longitude <- c(-15,45,45,-15)
latitude <- c(30,30,47.5,47.5)
region <- extent(-15,45,30,47.5)

minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")
minSSTEurope <- crop(minSST,region)
plot(minSSTEurope, main ="Minimum European Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.9
# The main objective of this challenge is to reclassify a set of environmental rasters and plot the potential ecological niche of a marine forest species in European coastlines using a mechanistic approach (i.e., process based model).

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Git/courseMarineEcologicalModelling/Sessions [P]")

# Suitable conditions set by: 
# Light > 5; 
# Salinity > 10; 
# Max Temp < 20.5; 
# Min Temp > 5

library(raster)

light <- raster("Data/rasterLayers/Present Light at bottom.tif")
salinity <- raster("Data/rasterLayers/Present Salinity LtMin.tif")
maxsst <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minsst <- raster("Data/rasterLayers/Present Temperature LtMin.tif")

lightConditions <- data.frame(from = c(0,5) , to=c(5,+Inf) , reclassValue=c(0,1))
lightConditions
lightConditions <- reclassify(light,lightConditions)

salinityConditions <- data.frame(from = c(0,10) , to=c(10,+Inf) , reclassValue=c(0,1))
salinityConditions <- reclassify(salinity,salinityConditions)

maxsstConditions <- data.frame(from = c(-Inf,20.5) , to=c(20.5,+Inf) , reclassValue=c(1,0))
maxsstConditions <- reclassify(maxsst,maxsstConditions)

minsstConditions <- data.frame(from = c(-Inf,5) , to=c(5,+Inf) , reclassValue=c(0,1))
minsstConditions <- reclassify(minsst,minsstConditions)

niche <- lightConditions * salinityConditions * maxsstConditions * minsstConditions

# or use the function calc with a stack of Rasters
# niche <- calc(stack(lightConditions , salinityConditions , maxsstConditions , minsstConditions), prod)

europeanExtent <- c(-30,40,20,75)
niche <- crop(niche,europeanExtent)

plot(niche, main = "The ecological niche of Saccorhiza polyschides",axes = TRUE,legend=FALSE, col=c("azure3","black"))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.10
# The main objective of this individual assignment is to **extract** the depth range used (values) by a mediterranean coral and plot it as an histogram.

data1 <- "Data/rasterLayers/BathymetryDepthMean.tif"
data2 <- "Data/dataBases/Paramuricea_clavata.csv"

bathymetry <- raster(data1)
occurrences <- read.csv(data2,sep=";")

colnames(occurrences)

depthsUsed <- extract(bathymetry,occurrences[,c("Lon","Lat")])
hist(depthsUsed,breaks=100)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.11
# The main objective of this individual assignment is to **extract** the depth range used (values) by a mediterranean coral and plot it as an histogram.

data1 <- "Data/rasterLayers/Present Nitrate LtMin.tif"
data2 <- "Data/rasterLayers/Present Salinity LtMin.tif"
data3 <- "Data/rasterLayers/Present Temperature LtMax.tif"
data4 <- "Data/rasterLayers/Present Temperature LtMin.tif"
data5 <- "Data/dataBases/Paramuricea_clavata.csv"

data1 <- raster(data1)
data2 <- raster(data2)
data3 <- raster(data3)
data4 <- raster(data4)

environment <- stack(data1,data2,data3,data4)
occurrences <- read.csv(data5,sep=";")

colnames(occurrences)

environmentUsed <- extract(environment,occurrences[,c("Lon","Lat")])
summary(environmentUsed)
