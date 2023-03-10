## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
## Recipe 2
# The main objetive of this Recipe is to fit a BRT **model** to biodiversity data predict distributions for the native and invasive regions

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

# Load layers
environmentPresent <- stackDirectory('Data/rastersPresent/')
plot(environmentPresent)

# mask to coastLine [if needed!!] and plot
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
plotResponse(model, var = "OceanTemperature.Benthic.Mean.Pred.LtMax", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")
plotResponse(model, var = "DissolvedMolecularOxygen.Benthic.Mean.Pred.Mean", type = "logistic", only_presence = FALSE, marginal = FALSE, rug = FALSE, color="Black")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 04. predict to produce maps

# predict with BRT to raster stack
mapNative <- predict(model, environmentPresent, type=c("logistic"))
plot(mapNative)

plot(mapNative)
points(records, pch=20, col="Black")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 05. predict to produce maps [invasive region]

myExtent <- extent(100,180,-52,-10)

# Crop polygon to my region
myRegion <- crop(world,myExtent)
plot(myRegion,col="gray",border="gray")

# Save image file to PNG or PDF [Plots panel -> Export]

## -----------------------
# 06. open and crop environmental layers

# Load layers
environmentPresent <- stackDirectory('Data/rastersPresent/')
plot(environmentPresent)

# mask to coastLine [if needed!!] and plot
maskCoastal <- raster("Data/CoastLine.tif")
plot(maskCoastal, col="black")

environmentPresent <- mask(environmentPresent,maskCoastal) 
plot(environmentPresent)

# crop and plot layers
environmentPresent <- crop(environmentPresent,myExtent)
plot(environmentPresent)

## -----------------------
## -----------------------
# 07. predict to produce maps [invasive region]

# predict with BRT to raster stack
mapInvasive <- predict(model, environmentPresent, type=c("logistic"))
plot(mapInvasive)

## ----------------------

par(mfrow=c(1,2))
plot(mapNative)
plot(mapInvasive)
dev.off()

