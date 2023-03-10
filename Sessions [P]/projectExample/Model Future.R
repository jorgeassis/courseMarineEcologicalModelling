## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
# The main objetive of this Recipe is to fit a BRT **model** to biodiversity data predict distributions for the present and the future.

## -----------------------
## -----------------------

# Set the working directory to where the data is located
setwd(".../projectExample")

# Load main functions
source("sourceFunctions.R")

## -----------------------
# 01. open final records

# load clean occurrence data with two columns only for Lon and Lat (follow Recipe 1)
records <- read.csv("Data/mycleanRecords.csv", sep = ";")
View(records)

## -----------------------
# 02. open and crop landmass

# Read polygon defining global landmasses
world <- shapefile("Data/globalLandmass/world.shp")

# Define my extent
myExtent <- extent(-20,47.5,10,60)

# Crop polygon to my region
myRegion <- crop(world,myExtent)
plot(myRegion,col="gray",border="gray")

# plot my records
points(records, pch=20, col="Black")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 02. open and crop environmental layers

# Load layers from directory
environmentPresent <- stackDirectory('Data/rastersPresent/')
plot(environmentPresent)

# mask to coastLine [if needed!] and plot
maskCoastal <- raster("Data/CoastLine.tif")
plot(maskCoastal, col="black")
environmentPresent <- mask(environmentPresent,maskCoastal)
plot(environmentPresent)

# crop and plot layers
environmentPresent <- crop(environmentPresent,myExtent)
plot(environmentPresent)

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
## -----------------------
# 03. produce distribution model

# produce model with raster layers and distribution records
model <- produceModel(myRasterLayers=environmentPresent,myRecords=records)

# Get the performance of the model with AUC, it ranges from 0 to 1. 
# Models <0.5 are considered no better than random
# Models > 0.75 are considered good. 
# Models >0.9 are considered exceptional

getAUC(model, test = TRUE)

# Plot the relative importance of variables
importanceVariables <- varImp(model, permut = 5)
importanceVariables
plotVarImp(importanceVariables)

# Save image file to PNG or PDF [Plots panel -> Export]

# Produce response curves
names(environmentPresent)
plotResponse(model, var = "OceanTemperature.Benthic.Mean.Pred.LtMin", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")
plotResponse(model, var = "TotalPrimaryProductionPhyto.Benthic.Mean.Pred.Mean", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 06. predict to produce maps

# predict with BRT to raster stack
mapPresent <- predict(model, environmentPresent, type=c("logistic"))
plot(mapPresent)

plot(mapPresent)
points(records, pch=20, col="Black")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 08. predict to the future RCP26

# Load layers
environmentRCP26 <- stackDirectory('Data/rastersFutureRCP26/')
plot(environmentRCP26)

# mask to coastLine [if needed!!] and plot
maskCoastal <- raster("Data/CoastLine.tif")
plot(maskCoastal, col="black")

environmentRCP26 <- mask(environmentRCP26,maskCoastal)
plot(environmentRCP26)

# crop and plot layers
environmentRCP26 <- crop(environmentRCP26,myExtent)
plot(environmentRCP26)

mapRCP26 <- predict(model, environmentRCP26, type=c("logistic"))
plot(mapRCP26)

# Save image file to PNG or PDF [Plots panel -> Export]

plot(mapRCP26 - mapPresent)

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 08. predict to the future RCP85

# Load layers
environmentRCP85 <- stackDirectory('Data/rastersFutureRCP85/')
plot(environmentRCP85)

# mask to coastLine [if needed!!] and plot
maskCoastal <- raster("Data/CoastLine.tif")
plot(maskCoastal, col="black")

environmentRCP85 <- mask(environmentRCP85,maskCoastal)
plot(environmentRCP85)

# crop and plot layers
environmentRCP85 <- crop(environmentRCP85,myExtent)
plot(environmentRCP85)

mapRCP85 <- predict(model, environmentRCP85, type=c("logistic"))
plot(mapRCP85)
plot(mapRCP85 - mapPresent)

## ----------------------

par(mfrow=c(1,3))
plot(mapPresent)
plot(mapRCP26 - mapPresent)
plot(mapRCP85 - mapPresent)
dev.off()

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------