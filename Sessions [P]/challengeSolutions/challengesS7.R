# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 7.1
# The main objective of this challenge is to fit a BRT model and assess its predictive performance with AUC without cross-validation.

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
# Challenge 7.2
# The main objective of this challenge is to fit a BRT model and assess its predictive performance with AUC and cross-validation.

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

# generate pseudo absences
absences <- pseudoAbsences(envConditions,presences,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, absences, envConditions)

# fit a BRT model to the dataset
model <- train("BRT", modelData)

# determine the threshold-independent AUC
getAUC(model)

# Cross validation with 4 blocks partitioning
folds2 <- get.block(presences, absences)

# fit a BRT model to the dataset
model <- train("BRT", modelData, folds = folds2)

# determine AUC
getAUC(model, test = TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 7.3
# The main objective of this challenge is to fit a BRT model and reclassify a predictive surface (e.g., present) by using a threshold maximizing the sum of sensitivity and specificity.

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

# generate pseudo absences
absences <- pseudoAbsences(envConditions,presences,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences, absences, envConditions)

# fit a BRT model to the dataset
model <- train("BRT", modelData)

# determine the threshold-independent AUC
getAUC(model)

# predict with BRT to raster stack
map <- predict(model, envConditions, type = c("logistic"))
plot(map, main = "Predictive model")

threshold <- thresholdMaxTSS(model)
threshold

# determine model performance
getAccuracy(model, threshold = threshold)

reclassMatrix <- data.frame(from = c(0, threshold), to = c(threshold, 1), value = c(0, 1))
reclassMatrix

# apply threshold to reclassify the predictive surface
map <- reclassify(map, rcl = reclassMatrix)

plot(map, main = "Reclassified predictive model")


# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 7.4
# The main objective of this challenge is to fit a BRT model and reclassify a predictive surface (e.g., present) by using a threshold maximizing the sum of sensitivity and specificity.

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

plotResponse(model, var = "BO2_ppmean_bdmean", type = "logistic", color = "Black")
plotResponse(model, var = "BO2_tempmin_bdmean", type = "logistic", color = "Black")


