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

# 01. read occurrence data and environmental layers
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

## -----------------------
# 00. Set the working directory and read the main functions

setwd("...")
source("sourceFunctions.R")

## -----------------------
# 01. open final records

# load clean occurrence data with two columns only for Lon and Lat (follow Recipe 1)
presences <- read.csv("Data/myFile.csv", sep = ";")

## -----------------------
# 02. load and crop environmental layers

# load layers
tempMin <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/Present/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/Present/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/Present/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
environmentalConditions <- stack(tempMin, tempMax, oxygen, productivity)

# Give simpler names
names(environmentalConditions) <- c("tempMin", "tempMax", "oxygen", "productivity")

# crop layers to the European extent
myExtent <- extent(-15, 37.5, 30, 50)
environmentalConditions <- crop(environmentalConditions,myExtent)

# plot predictors
plot(environmentalConditions)

# mask predictors to intertidal region (along shore) if that is the case
maskCoastLine <- raster("Data/RasterLayers/CoastLine.tif")
maskCoastLine <- crop(maskCoastLine,environmentalConditions)
environmentalConditions <- mask(environmentalConditions,maskCoastLine)
  
# plot predictors
plot(environmentalConditions)

## -----------------------
# 03. generate pseudo-absences

# generate pseudo-absences
pseudoAbs <- pseudoAbsences(environmentalConditions,presences,n=1000)

# Plot pseudo-absences and presences
world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
myRegion <- crop(world, myExtent)

plot(myRegion,col="gray",border="gray")
points( pseudoAbs, col="blue" , pch= 19)
points( presences, col="red" , pch= 19)

## -----------------------
# 04. fit a model with best hyperparameters

# extract environmental values and make a data.frame with PA information
modelData <- prepareModelData(presences,pseudoAbs,environmentalConditions) 

# Generate cross validation folds
folds <- getBlocks(modelData)

# define monotonicity constrains (-1 for negative, +1 for positive, 0 for non-monotonicity)
monotonicity = data.frame(tempMin=+1,tempMax=-1,oxygen=+1,productivity=+1)
monotonicity

# fit a BRT model with cross-validation and 
model <- train("BRT", modelData, folds = folds)

# given a set of possible hyperparameter values for BRT
h <- list(interaction.depth = c(1,2,3,4) , shrinkage = c(0.1,0.01,0.001) )

# test all possible combinations of hyperparameter values
exp1 <- gridSearch(model, hypers = h, metric = "auc")
plot(exp1)
exp1@results

# fit a BRT model to the dataset with the best hyperparameter values
model <- train("BRT", modelData, folds = folds , interaction.depth=2, shrinkage=0.010 )

## -----------------------
# 05. assess for variable contribution and response functions

# determine relative variable contribution
viModel <- varImp(model, permut = 5)
viModel

# reduce model complexity by dropping one variable at a time
reducedModel <- reduceVar(model, th = 5, metric = "auc", permut = 5)

# determine the final relative variable contribution
viModel <- varImp(reducedModel, permut = 5)
viModel
plotVarImp(viModel)

# inspect response curves
plotResponse(reducedModel, var = "tempMax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")
plotResponse(reducedModel, var = "oxygen", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")

## -----------------------
# 06. model performance

# determine the threshold maximizing the sum of sensitivity and specificity
threshold <- thresholdMaxTSS(reducedModel)

# determine model performance
getAccuracy(reducedModel,threshold = threshold)
getAUC(reducedModel, test = TRUE)

## -----------------------
# 07. predict to produce maps

# predict with BRT to raster stack
mapPresent <- predict(reducedModel, environmentalConditions, type=c("logistic"))
plot(mapPresent)

## -----------------------
# 08. predict to the future

# load layers of the future
tempMin <- raster("Data/rasterLayers/Climate/RCP85/OceanTemperature Benthic Mean Pred LtMin.tif")
tempMax <- raster("Data/rasterLayers/Climate/RCP85/OceanTemperature Benthic Mean Pred LtMax.tif")
oxygen <- raster("Data/rasterLayers/Climate/RCP85/DissolvedMolecularOxygen Benthic Mean Pred Mean.tif")
productivity <- raster("Data/rasterLayers/Climate/RCP85/TotalPrimaryProductionPhyto Benthic Mean Pred Mean.tif")

# stack layers to generate a unique object
envConditionsRCP85 <- stack(tempMin, tempMax, oxygen, productivity)

# Give simpler names
names(envConditionsRCP85) <- c("tempMin", "tempMax", "oxygen", "productivity")

# crop layers to the European extent
envConditionsRCP85 <- crop(envConditionsRCP85,myExtent)

plot(envConditionsRCP85)

# mask predictors to intertidal region (along shore) if that is the case
maskCoastLine <- raster("Data/RasterLayers/CoastLine.tif")
maskCoastLine <- crop(maskCoastLine,myExtent)
envConditionsRCP85 <- mask(envConditionsRCP85,maskCoastLine)

plot(envConditionsRCP85)

# predict with BRT to raster stack
mapRCP85 <- predict(reducedModel, envConditionsRCP85, type=c("logistic"))
plot(mapRCP85)

# plot difference in habitat suitability
difference <- mapRCP85 - mapPresent
plot(difference, main = "Difference in habitat suitablity (RCP85)", col = brewer.pal(n = 11, name = "RdBu"))

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------