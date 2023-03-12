# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.1
# The main objective of this challenge is to plot a shapefile file with the distribution of a coral species.
# The plot must include the landmass shapefile of the world.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
distributionRecords <- shapefile("Data/vectorShapefiles/seagrass/cymodoceaNodosa.shp")

plot(world)
plot(distributionRecords)

plot(world)
plot(distributionRecords, add=TRUE, col="red")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.2
# The main objective of this challenge is to crop a global map to the Azores islands region and plot it.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

# Crop with extent

regionAzores <- extent(-36, -18, 34, 42)

azores <- crop(world,regionAzores)
plot(azores)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.3
# The main objective of this challenge is to make one plot of the global SST.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

SST <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMax.tif")

plot(SST, main ="Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.4
# The main objective of this challenge is to crop a global SST layers to the extent of Western Africa.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

presentSST <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMax.tif")

westAfricaExtent <- extent(-30, 22, -37.5, 37)
presentSSTWAfrica <- crop(presentSST,westAfricaExtent)
plot(presentSSTWAfrica, main="West Africa warming" )

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.5
# The main objective of this challenge is to plot the global range of sea surface temperatures.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

SSTMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMax.tif")
SSTMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMin.tif")

SSTRange <- SSTMax - SSTMin

plot(SSTRange )

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.6
# The main objective of this challenge is to plot the average sea surface temperatures of the European coastlines.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

library(raster)

SSTMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMax.tif")
SSTMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred LtMin.tif")

SST <- stack(SSTMax,SSTMin)
SSTMean <- calc(SST,mean)
plot(SSTMean)

europeanExtent <- extent(-15, 45, 30, 47.5)
europeanSSTMean <- crop(SSTMean,europeanExtent)
plot(europeanSSTMean, main="European average sea surface temperatures" )

