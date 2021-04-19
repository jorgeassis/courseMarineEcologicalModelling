# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 6.1
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# load layers of sea surface temperatures and nutrients
envConditions <- load_layers(c("BO2_tempmin_bdmean", "BO2_tempmax_bdmean", "BO2_dissoxmean_bdmean", "BO2_ppmean_bdmean"))

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
envConditions <- crop(envConditions, europeanExtent)

# plot environmental layers
plot(envConditions)

# generate pseudo-absences
background <- backgroundInformation(envConditions, n = 10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditions)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData)

# predict with Maxent to raster stack
map <- predict(model, envConditions, type = "logistic")

# plot prediction of suitable habitats
plot(map, col = brewer.pal(n = 11, name = "YlOrRd"), main = "Suitable habitats for the coral P. clavata")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 6.2
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data and transfer the model to different climate conditions.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# list future layers
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)

# load layers of sea temperatures
envConditions <- load_layers(c("BO2_tempmin_bdmean", "BO2_tempmax_bdmean"))

# load future layers of sea temperatures
envConditionsFuture <- load_layers(c("BO2_RCP26_2100_tempmin_bdmean", "BO2_RCP26_2100_tempmax_bdmean"))

# compare names
names(envConditions)
names(envConditionsFuture)

# correct names
names(envConditionsFuture) <- c("BO2_tempmin_bdmean","BO2_tempmax_bdmean")

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
envConditions <- crop(envConditions, europeanExtent)
envConditionsFuture <- crop(envConditionsFuture, europeanExtent)

# plot environmental layers
plot(envConditions)

# plot future environmental layers
plot(envConditionsFuture)

# generate pseudo-absences
background <- backgroundInformation(envConditions, n = 10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditions)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData)

# predict with Maxent to raster stack
map <- predict(model, envConditions, type = "logistic")

# predict with Maxent to raster stack
mapFuture <- predict(model, envConditionsFuture, type = "logistic")

# plot prediction of suitable habitats
plot(map, col = brewer.pal(n = 11, name = "YlOrRd"), main = "Suitable habitats for the coral P. clavata")

# plot future prediction of suitable habitats
plot(mapFuture, col = brewer.pal(n = 11, name = "YlOrRd"), main = "Suitable habitats for the coral P. clavata")

# plot difference in habitat suitability
difference <- mapFuture - map
plot(difference, main = "Difference in habitat suitablity (RCP26)", col = brewer.pal(n = 11, name = "RdBu"))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 6.3
# The main objetive of this challenge is to fit a MaxEnt model to your own occurrence and environmental data and transfer the model to a different geographic regions

source("sourceFunctions.R")

# load occurrence records
presences <- getOccurrencesGBIF("Undaria pinnatifida")
presences <- presences[, c("Lon", "Lat")]

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# load layers of sea temperatures
envConditions <- load_layers(c("BO2_lightbotmean_bdmean", "BO2_tempmin_bdmean", "BO2_tempmax_bdmean", "BO2_nitratemean_bdmean"))

# crop layers to the native extent
nativeExtent <- extent(115, 155, 20, 50)
envConditionsNative <- crop(envConditions, nativeExtent)

# crop layers to the Invasive European extent
europeanExtent <- extent(-15, 40, 25, 75)
envConditionsEurope <- crop(envConditions, europeanExtent)

# Plot environmental conditions of the native range
plot(envConditionsNative)

# Plot environmental conditions of the invasive range
plot(envConditionsEurope)

# generate pseudo-absences
background <- backgroundInformation(envConditionsNative, n = 10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, background, envConditionsNative)

# fit a Maxent model to the dataset
model <- train("Maxnet", modelData, folds = NULL)

# predict with Maxent to the native extent
mapNative <- predict(model, envConditionsNative, type = c("logistic"))

# predict with Maxent to the inavasive extent
mapEurope <- predict(model, envConditionsEurope, type = c("logistic"))

# plot suitable habitats for native extent
plot(mapNative, main = "Native suitable habitats of Undaria pinnatifida", col = brewer.pal(n = 11, name = "YlOrRd"))

# plot suitable habitats for invasive extent
plot(mapEurope, main = "Invasive suitable habitats of Undaria pinnatifida", col = brewer.pal(n = 11, name = "YlOrRd"))
