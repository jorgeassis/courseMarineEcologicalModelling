## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Recipes
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
## Recipe 2
# The main objetive of this Recipe is to fit a BRT **model** to biodiversity data predict distributions for the present and the future.

# 01. read environmental layers
# 02. crop environmental layers to the extent of the study region
# 03. generate pseudoAbsences for BRT
# 04. prepare data for modelling
# 05. train model with cross-validation
# 06. test all possible combinations of hyperparameter values
# 07. fit a BRT model to the dataset with the best hyperparameter values
# 08. determine relative variable contribution
# 09. determine performance as AUC
# 10. reduce model complexity by dropping variabale at a time
# 11. determine the final relative variable contribution
# 12. determine the final performance as AUC
# 13. inspect response curves
# 14. predict with BRT to raster stacks

source("sourceFunctions.R")

## -----------------------
# 01. open final records

# load clean occurrence data with two columns only for Lon and Lat (follow Recipe 1)
presences <- read.csv("Data/myFile.csv", sep = ";")

## -----------------------
# 02. load and crop environmental layers

# load layers
layerCodes <- list_layers(datasets = "Bio-ORACLE")
View(layerCodes)

# Select a set of predictors
environmentalConditions <- load_layers(c("BO2_tempmin_bdmean", "BO2_tempmax_bdmean", "BO2_dissoxmean_bdmean", "BO2_ppmean_bdmean"))

# crop layers to the European extent
myExtent <- c(-35,30,-10,70.5)
europeanExtent <- extent(myExtent)
environmentalConditions <- crop(environmentalConditions,europeanExtent)

# plot predictors
plot(environmentalConditions)

# crop predictors to intertidal region (along shore) if that is the case
maskCoastLine <- raster("Data/RasterLayers/CoastLine.tif")
maskCoastLine <- crop(maskCoastLine,environmentalConditions)
environmentalConditions <- mask(environmentalConditions,maskCoastLine)

# crop for maximum potential distribution
bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
bathymetry <- crop(bathymetry,subset(environmentalConditions,1))
bathymetry <- mask(bathymetry,subset(environmentalConditions,1))
bathymetry[bathymetry < -80 ] <- NA
environmentalConditions <- mask(environmentalConditions,bathymetry)

# plot predictors
plot(environmentalConditions)

## -----------------------
# 03. generate pseudo-absences

# generate pseudo-absences
pseudoAbs <- pseudoAbsences(environmentalConditions,presences,n=1000)

# Plot pseudo-absences and presences
myExtent <- c(-35,30,-10,70.5)
myRegion <- crop(world,extent(myExtent))
plot(myRegion,col="gray",border="gray")
points(presences,col="red")
points(pseudoAbs,col="blue")

## -----------------------
# 04. fit a model with best hyperparameters

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,pseudoAbs,environmentalConditions) 

# Generate cross validation folds
folds <- getBlocks(modelData)

# define monotonicity constrains (-1 for negative, +1 for positive, 0 for non-monotonicity)
monotonicity = data.frame(BO2_tempmin_bdmean=+1,BO2_tempmax_bdmean=-1,BO2_dissoxmean_bdmean=+1,BO2_ppmean_bdmean=+1)
monotonicity

# fit a BRT model with cross-validation and 
model <- train("BRT", modelData, folds = folds)

# given a set of possible hyperparameter values for BRT
h <- list(interaction.depth = c(1,2,3,4) , shrinkage = c(0.1,0.01,0.001) )

# test all possible combinations of hyperparameter values
exp1 <- gridSearch(model, hypers = h, metric = "auc")
plot(exp1)
exp1@results
exp1@results[which.max(exp1@results$test_AUC),]

# fit a BRT model to the dataset with the best hyperparameter values
model <- train("BRT", modelData, folds = folds , interaction.depth=BEST_VAL_HERE, shrinkage=BEST_VAL_HERE )
getAUC(model, test = TRUE)

## -----------------------
# 05. assess for variable contribution and response functions

# determine relative variable contribution
viModel <- varImp(model, permut = 5)
viModel

# reduce model complexity by dropping one variable at a time
reducedModel <- reduceVar(model, th = 5, metric = "auc", permut = 5)

# determine the final performance as AUC
getAUC(reducedModel, test = TRUE)

# determine the final relative variable contribution
viModel <- varImp(reducedModel, permut = 5)
viModel
plotVarImp(viModel)

# inspect response curves
plotResponse(reducedModel, var = "BO2_tempmin_bdmean", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")

## -----------------------
# 06. predict to produce maps

# predict with BRT to raster stack
mapPresent <- predict(reducedModel, environmentalConditions, type=c("logistic"))
plot(mapPresent)

# determine the threshold maximizing the sum of sensitivity and specificity
threshold <- thresholdMaxTSS(reducedModel)
threshold

# generate a reclassification table
thresholdConditions <- data.frame(from = c(0,threshold) , to=c(threshold,1) , reclassValue=c(0,1))
thresholdConditions

# apply threshold to reclassify the predictive surface
mapPresentReclass <- reclassify(mapPresent, rcl = thresholdConditions)
plot(mapPresentReclass)

## -----------------------
# 07. model performance

# determine model performance
getAccuracy(reducedModel,threshold = threshold)
getAUC(reducedModel, test = TRUE)

## -----------------------
# 08. predict to the future

# load layers of sea surface temperatures for the future
environmentalConditionsRCP26 <- load_layers(c("BO2_RCP26_2100_tempmin_bdmean","BO2_RCP26_2100_tempmax_bdmean"))

# change layer names for them to match
names(environmentalConditionsRCP26) <- c("BO2_tempmin_bdmean","BO2_tempmax_bdmean")

# add external layer (from file) if that is the case
newLayer <- raster("file")
names(newLayer) <- "match_name_to_bio_oracle"
environmentalConditionsRCP26 <- stack(environmentalConditionsRCP26,newLayer)

# crop layers to the European extent
environmentalConditionsRCP26 <- crop(environmentalConditionsRCP26,europeanExtent)

# crop to intertidal region (along shore) if that is the case
maskCoastLine <- raster("Data/RasterLayers/CoastLine.tif")
maskCoastLine <- crop(maskCoastLine,environmentalConditionsRCP26)
environmentalConditionsRCP26 <- mask(environmentalConditionsRCP26,maskCoastLine)

# predict with BRT to raster stack
mapRCP26 <- predict(reducedModel, environmentalConditionsRCP26, type=c("logistic"))
plot(mapRCP26)

# apply threshold to reclassify the future predictive surface
mapRCP26Reclass <- reclassify(mapRCP26, rcl = thresholdConditions)
plot(mapRCP26Reclass)

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------