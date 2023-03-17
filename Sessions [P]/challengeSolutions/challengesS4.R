# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.1
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

mediterraneanExtent <- extent(-15, 37.5, 30, 50)
landmassEur <- crop(world, mediterraneanExtent)

plot(landmassEur, col="gray", border="gray" )
points( presences, col="Black" , pch= 19)

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditionsPresent <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
envConditionsPresent <- crop(envConditionsPresent, europeanExtent)

# plot environmental layers
plot(envConditionsPresent)

# generate pseudo-absences
background <- backgroundInformation(envConditionsPresent, n = 10000)

# plot presences and background
plot(landmassEur, col="gray", border="gray" )
points( background, col="red" , pch= 19)
points( presences, col="Black" , pch= 19)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditionsPresent)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData)

# predict with Maxent to raster stack
map <- predict(model, envConditionsPresent, type = "logistic")

# plot prediction of suitable habitats
plot(map, col = brewer.pal(n = 11, name = "YlOrRd"), main = "Suitable habitats for the coral P. clavata")
points(presences,col="black", pch=20, cex=0.5)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.2
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data and transfer the model to different climate conditions.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

mediterraneanExtent <- extent(-15, 37.5, 30, 50)
landmassEur <- crop(world, mediterraneanExtent)

plot(landmassEur, col="gray", border="gray" )
points( presences, col="Black" , pch= 19)

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditionsPresent <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
envConditionsPresent <- crop(envConditionsPresent, europeanExtent)

# plot environmental layers
plot(envConditionsPresent)

# generate pseudo-absences
background <- backgroundInformation(envConditionsPresent, n = 10000)

# plot presences and background
plot(landmassEur, col="gray", border="gray" )
points( background, col="red" , pch= 19)
points( presences, col="Black" , pch= 19)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditionsPresent)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData)

# predict with Maxent to raster stack
mapPresent <- predict(model, envConditionsPresent, type = "logistic")

# Load raster layers of future conditions
tempMin <- raster("Data/rasterLayers/Climate/RCP85/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/RCP85/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/RCP85/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/RCP85/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditionsRCP85 <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
envConditionsRCP85 <- crop(envConditionsRCP85, europeanExtent)

# predict with Maxent to raster stack
mapFuture <- predict(model, envConditionsRCP85, type = "logistic")

# plot prediction of suitable habitats
plot(mapFuture, col = brewer.pal(n = 11, name = "YlOrRd"), main = "Future suitable habitats for the coral P. clavata")

# plot difference in habitat suitability
difference <- mapFuture - mapPresent
plot(difference, main = "Difference in habitat suitablity (RCP85)", col = brewer.pal(n = 11, name = "RdBu"))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.3
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data and transfer the model to a different geographic regions

source("sourceFunctions.R")

# load occurrence records
presences <- getOccurrencesGBIF("Undaria pinnatifida")
presences <- presences[, c("Lon", "Lat")]

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

plot(world, col="gray", border="gray" )
points( presences, col="Black" , pch= 19)

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
nitrate <- raster("Data/rasterLayers/Climate/Present/Nitrate Surface Pred Mean.tif")
envConditionsPresent <- stack(tempMin, tempMax, nitrate)

# mask layers to the coastal region, and then rename
coastline <- raster("Data/rasterLayers/CoastLine.tif")
envConditionsPresent <- mask(envConditionsPresent, coastline)

# crop and plot layers to the native extent
nativeExtent <- extent(115, 155, 20, 50)
envConditionsPresentNative <- crop(envConditionsPresent, nativeExtent)
plot(envConditionsPresentNative)

# crop layers to the Invasive European extent
europeanExtent <- extent(-15, 40, 25, 75)
envConditionsPresentInvasive <- crop(envConditionsPresent, europeanExtent)
plot(envConditionsPresentInvasive)

# generate background information
background <- backgroundInformation(envConditionsPresentNative, n = 10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditionsPresentNative)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = NULL)

# predict with Maxent to the different regions
mapNative <- predict(model, envConditionsPresentNative, type = c("logistic"))
mapEurope <- predict(model, envConditionsPresentInvasive, type = c("logistic"))

# plot suitable habitats for native extent
plot(mapNative, main = "Native suitable habitats of Undaria pinnatifida", col = brewer.pal(n = 11, name = "YlOrRd"))

# plot suitable habitats for invasive extent
plot(mapEurope, main = "Invasive suitable habitats of Undaria pinnatifida", col = brewer.pal(n = 11, name = "YlOrRd"))
points(presences,col="black", pch=20, cex=0.5)
