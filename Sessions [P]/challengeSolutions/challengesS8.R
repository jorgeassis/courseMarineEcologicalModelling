# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 8.1
# The main objetive of this challenge is to fit a MaxEnt model and tune proper hyperparameter selection.

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

# generate background information
background <- backgroundInformation(envConditions,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,background,envConditions) 

#train a model with threshold feature
folds <- getBlocks(modelData)
model <- train("Maxnet", modelData, folds = folds, fc = "tp")

# given a set of possible hyperparameter values (regularization)
h <- list(reg = seq(1, 10, 1))

# train models with all the possible combinations of hyperparameters
exp1 <- gridSearch(model, hypers = h, metric = "auc")
exp1@results

which.max(exp1@results$test_AUC)

model <- train("Maxnet", modelData, folds = folds, fc = "tp", reg = 4)
plotResponse(model, var = "BO2_dissoxmean_bdmean", type = "logistic", color = "Black")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 8.2
# The main objetive of this challenge is to fit a MaxEnt model and remove variables with low importance.

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

# generate background information
background <- backgroundInformation(envConditions,n=10000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,background,envConditions) 

#train a model with threshold feature
folds <- getBlocks(modelData)
model <- train("Maxnet", modelData, folds = folds, fc = "tp")

# determine relative variable contribution and performance as AUC
viModel <- varImp(model, permut = 5)
plotVarImp(viModel)

# reduce model complexity by dropping variable at a time and remove all variables that have a permutation importance lower than 5%.
reducedModel <- reduceVar(model, th = 5, metric = "auc", permut = 5)

# determine relative variable contribution and performance as AUC of the reduced model
viModel <- varImp(reducedModel, permut = 5)
plotVarImp(viModel)
