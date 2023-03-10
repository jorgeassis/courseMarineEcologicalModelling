# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.1
# The main objective of this challenge is to plot a shapefile with the distribution of a seagrass species. The plot must include the landmass shapefile of the world.

library(raster)

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
distributionRecords <- read.table("Data/dataBases/MergedCoordinates.csv", sep = ";", header = TRUE)

plot(world)
plot(distributionRecords)

plot(world)
points(distributionRecords, col="red")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.2
# The main objective of this challenge is to crop a global map to the Azores islands region and plot it.

library(raster)

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

# Crop with extent

regionAzores <- extent(-36, -18, 34, 42)

azores <- crop(world,regionAzores)
plot(azores)
plot(azores, axes=TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.3
# The main objective of this challenge is to make one plot of the global SST.

library(raster)

SST <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred Mean.tif")

plot(SST, main ="Sea Surface Temperatures")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 2.4
# The main objective of this challenge is to crop a global SST layers to the extent of Western Africa.

library(raster)

presentSST <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Surface Pred Mean.tif")

westAfricaExtent <- extent(-30, 22, -37.5, 36)
presentSSTWAfrica <- crop(presentSST,westAfricaExtent)
plot(presentSSTWAfrica, main="West Africa warming" )
