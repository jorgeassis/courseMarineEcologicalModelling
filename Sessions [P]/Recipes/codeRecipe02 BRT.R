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
# 03. generate background information for BRT
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

# load clean occurrence data (follow Recipe 1)
presences <- YOUR_DATA
  
# load environmental layers
layerCodes <- list_layers(datasets = "Bio-ORACLE")
View(layerCodes)
environmentalConditions <- load_layers(c("BO2_tempmax_bdmean","BO2_tempmin_bdmean"))

# crop layers to the European extent
europeanExtent <- extent(-15, 40, 25, 50)
environmentalConditions <- crop(environmentalConditions,europeanExtent)

# generate pseudo absences
pseudoAbsences <- pseudoAbsences(environmentalConditions,presences,n=1000)

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,pseudoAbsences,environmentalConditions) 

# Generate cross validation folds
folds <- get.block(presences, background)

# fit a BRT model with cross-validation and monotonicity constrains
monotonicity = data.frame(BO2_tempmax_bdmean=0,BO2_tempmin_bdmean=0)
monotonicity
model <- train("BRT", modelData, folds = folds)

# given a set of possible hyperparameter values for BRT
h <- list(interaction.depth = c(1,2,3,4) , shrinkage = c(0.1,0.01,0.001) )

# test all possible combinations of hyperparameter values
exp1 <- gridSearch(model, hypers = h, metric = "auc")
plot(exp1)
exp1@results
which.max(exp1@results$test_AUC)

# fit a BRT model to the dataset with the best hyperparameter values
model <- train("BRT", modelData, folds = folds , interaction.depth=BEST_HERE, shrinkage=BEST_HERE )
getAUC(model, test = TRUE)

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
plotResponse(reducedModel, var = "BO2_tempmax_bdmean", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")

# predict with BRT to raster stack
mapPresent <- predict(reducedModel, environmentalConditions, type=c("logistic"))
plot(mapPresent)

# determine the threshold maximizing the sum of sensitivity and specificity
threshold <- thresholdMaxTSS(reducedModel)
threshold

# determine model performance
getAccuracy(reducedModel,threshold = threshold)
getAUC(reducedModel, test = TRUE)

# generate a reclassification table
thresholdConditions <- data.frame(from = c(0,threshold) , to=c(threshold,1) , reclassValue=c(0,1))
thresholdConditions

# apply threshold to reclassify the predictive surface
mapPresentReclass <- reclassify(mapPresent, rcl = thresholdConditions)
plot(mapPresentReclass)

# load layers of sea surface temperatures for the future
environmentalConditionsRCP26 <- load_layers(c("BO2_RCP26_2100_tempmin_bdmean","BO2_RCP26_2100_tempmax_bdmean"))

# change layer names for them to match
names(environmentalConditionsRCP26) <- c("BO2_tempmin_bdmean","BO2_tempmax_bdmean")

# crop layers to the European extent
environmentalConditionsRCP26 <- crop(environmentalConditionsRCP26,europeanExtent)

# predict with BRT to raster stack
mapRCP26 <- predict(reducedModel, environmentalConditionsRCP26, type=c("logistic"))
plot(mapRCP26)

# apply threshold to reclassify the future predictive surface
mapRCP26Reclass <- reclassify(mapRCP26, rcl = thresholdConditions)
plot(mapRCP26Reclass)

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------