# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.1
# The main objetive of this challenge is to plot a shapefile with the European distribution of a seagrass species. The plot must include the landmass shapefile of the world.

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

# better map
plot(world, main = "European distribution of a seagrass species", col="#E5E5E5" , border="#959595", axes=TRUE)
points(records, pch=16, col="Black")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.2
# The main objetive of this challenge is to crop a global map to the Azores islands region and plot it.

library(raster)
library(rnaturalearth)

world <- ne_countries(scale = 'medium')

# Crop with extent

regionAzores <- extent(-36,-18,34,42)

azores <- crop(world,regionAzores)
plot(azores)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.3
# The main objetive of this challenge is to plot the centroids of the European no-Take MPA.

library(raster)
library(rgeos)
library(sp)

data1 <- "Data/vectorShapefiles/noTakeMPA/MPA.shp"
data2 <- "Data/vectorShapefiles/coastlinesMediterranean/coastlinesMed.shp"

mpa <- shapefile(data1)
mediterranean <- shapefile(data2)

centroids <- gCentroid(mpa, byid=TRUE)

plot(mediterranean)
plot(mpa,add=TRUE,col="red")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.4
# The main objetive of this challenge is to determine and plot the more distant European no-Take MPA.

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
# Challenge 3.5
# The main objetive of this challenge is to plot the smallest no-Take MPA in Europe.

longitude <- c(-15,45,45,-15)
latitude <- c(30,30,47.5,47.5)
lonlat <- cbind(longitude, latitude)
region <- spPolygons(lonlat)

data1 <- "Data/vectorShapefiles/globalLandmass/world.shp"
data2 <- "Data/vectorShapefiles/noTakeMPA/MPA.shp"

coastlines <- shapefile(data1)
coastlinesMed <- crop(coastlines,region)

map <- shapefile(data2)

mpaAreas <- area(mpa)
which.min(mpaAreas)
mpa$name[120]

plot(coastlinesMed, main = "The smaller no-Take MPA in European coastlines", col="gray",axes = TRUE)
points(mpa[120,],col="Red",pch=16)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.6
# The main objetive of this challenge is to make two plots of the global maximum and minimum SST.

library(raster)

maxSST <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")

plot(maxSST, main ="Maximum Sea Surface Temperatures")
plot(minSST, main ="Minimum Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.7
# The main objetive of this challenge is to plot of the global range of sea surface temperatures.

library(raster)

maxSST <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")

rageSST <-maxSST - minSST
plot(rageSST, main ="Range of Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.8
# The main objetive of this challenge is to plot the minimum sea surface temperatures in Europe

library(raster)

longitude <- c(-15,45,45,-15)
latitude <- c(30,30,47.5,47.5)
region <- cbind(longitude,latitude)
region <- spPolygons(region)

minSST <- raster("Data/rasterLayers/Present Temperature LtMin.tif")
minSSTEurope <- crop(minSST,region)
plot(minSSTEurope, main ="Minimum European Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.9
# The main objetive of this challenge is to reclassify a set of environmental rasters and plot the potential ecological niche of a marine forest species in European coastlines using a mechanistic approach (i.e., process based model).

# Physiological information for Saccorhiza polyschides:
# Constrained by Light < 5, Salinity < 10, Max. Temp. > 20.5 and Min. Temp. min < 5
# ** Lobban & Wynne, 1981 Botanical Monographs; Assis et al., 2017 Global Change Biology

library(rnaturalearth)
library(raster)

longitude <- c(-15,40,40,-15)
latitude <- c(25,25,70.5,70.5)
lonlat <- cbind(longitude, latitude)
region <- spPolygons(lonlat)

world <- ne_countries(scale = 'medium')
landmassMed <- crop(world,region)

light <- raster("Data/rasterLayers/Present Light at bottom.tif")
salinity <- raster("Data/rasterLayers/Present Salinity LtMin.tif")
maxsst <- raster("Data/rasterLayers/Present Temperature LtMax.tif")
minsst <- raster("Data/rasterLayers/Present Temperature LtMin.tif")

environmentData <- stack(light,salinity,maxsst,minsst)
environmentData <- crop(environmentData,landmassMed)
plot(environmentData)
names(environmentData)

niche <- calc(environmentData, function(x){ ifelse( x[[1]] >= 5 & x[[2]] >= 10 & x[[3]] <= 20.5 & x[[4]] >= 5 , 1 , NA ) } )

plot(landmassMed, main = "The ecological niche of Saccorhiza polyschides", col="gray", border="gray",axes = TRUE)
plot(niche, col="black",add = TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.10
# The main objetive of this individual assignment is to determine the area of loss landmasses anticipated in a business as usual climate scenario for the Mediteranean Sea.

dtmPresent <- raster("data/rasterLayers/dtmEurope.tif")
dtmFuture <- raster("data/rasterLayers/dtmEurope.tif")

dtmPresent[dtmPresent <= 0] <- NA
dtmPresent[dtmPresent > 0] <- 1

dtmFuture[dtmFuture <= 1] <- NA
dtmFuture[dtmFuture > 1] <- 1

myLandmassPresent <- rasterToPolygons(dtmPresent)
myLandmassFuture <- rasterToPolygons(dtmFuture, fun=NULL, n=8, digits=12, dissolve=TRUE)

(area(myLandmassFuture) / 1000000 ) - ( area(myLandmassPresent) / 1000000 )

plot(myLandmassPresent, main = "Loss of European coastlines by 2100 [RCP85]", col="Black", border="Black",axes = TRUE)
plot(myLandmassFuture, col="gray", border="gray",add = TRUE)


