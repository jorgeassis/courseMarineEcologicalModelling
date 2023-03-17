# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.1
# The main objective of this challenge is to fit a BRT model and assess its predictive performance with AUC without cross-validation.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditions <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
envConditions <- crop(envConditions, europeanExtent)

# plot environmental layers
plot(envConditions)

# generate pseudo absences
absences <- pseudoAbsences(envConditions,presences,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, absences, envConditions)

# fit a BRT model to the dataset
model <- train("BRT", modelData)

# determine the threshold-independent AUC
getAUC(model)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.2
# The main objective of this challenge is to fit a BRT model and assess its predictive performance with AUC and cross-validation.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditions <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
envConditions <- crop(envConditions, europeanExtent)

# plot environmental layers
plot(envConditions)

# generate pseudo absences
absences <- pseudoAbsences(envConditions,presences,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, absences, envConditions)

# fit a BRT model to the dataset
model <- train("BRT", modelData)

# determine the threshold-independent AUC
getAUC(model)

# Cross validation with 4 blocks partitioning
folds2 <- getBlocks(modelData)

# fit a BRT model to the dataset
model <- train("BRT", modelData, folds = folds2)

# determine AUC
getAUC(model, test = TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.3
# The main objective of this challenge is to evaluate a model by inspecting variable relative importance and response curves.

source("sourceFunctions.R")

# read occurrence records
presences <- read.csv("Data/dataBases/Paramuricea_clavata.csv", sep = ";")
presences <- presences[, c("Lon", "Lat")]

# load layers of sea surface temperatures and nutrients
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditions <- stack(tempMin, tempMax, oxygen, productivity)

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
envConditions <- crop(envConditions, europeanExtent)

# plot environmental layers
plot(envConditions)

# generate pseudo absences
absences <- pseudoAbsences(envConditions,presences,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, absences, envConditions)

# fit a BRT model to the dataset
model <- train("BRT", modelData)

# calculate the variable relative importance with permutations
viModel <- varImp(model, permut = 5)
viModel
plotVarImp(viModel)

plotResponse(model, var = "OceanTemperature.Benthic.Mean.Pred.LtMin", type = "logistic", color = "Black")
plotResponse(model, var = "DissolvedMolecularOxygen.Benthic.Mean.Pred.Mean", type = "logistic", color = "Black")


