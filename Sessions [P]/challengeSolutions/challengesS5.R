# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.1
# The main objective of this challenge is to plot the maximum sea surface temperature in Europe for the year 2100 under two contrasting scenarios of climate change (RCP26 and RCP85)

source("sourceFunctions.R")

# Bio-ORACLE layer:
# Sea water temperature (maximum)

# European coastline extent
# xmin = -15
# xmax = 40
# ymin = 25
# ymax = 70.5

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# list future layers
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)

# open layers
SSTRCP26 <- load_layers("BO2_RCP26_2100_templtmax_ss")
SSTRCP85 <- load_layers("BO2_RCP85_2100_templtmax_ss")

# crop layers for the extent

SSTRCP26 <- crop(SSTRCP26,extent(-15,40,25,70.5))
SSTRCP85 <- crop(SSTRCP85,extent(-15,40,25,70.5))

# plot layers

plot(SSTRCP26, main="Maximum sea surface temperature; 2100 RCP26")
plot(SSTRCP85, main="Maximum sea surface temperature; 2100 RCP85")

SSTRCP <- stack(SSTRCP26,SSTRCP85)
plot(SSTRCP, main="Maximum sea surface temperature; 2100 different conditions")

# plot layers with fixed scale and a redish gradient

min(getValues(SSTRCP26),na.rm=T)
max(getValues(SSTRCP85),na.rm=T)

plot(SSTRCP26, main="Maximum sea surface temperature; 2100 RCP26", zlim=c(6.4,34.6) , col= rev(heat.colors(100)))
plot(SSTRCP85, main="Maximum sea surface temperature; 2100 RCP85", zlim=c(6.4,34.6) , col= rev(heat.colors(100)))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.2
# The main objective of this challenge is to plot the difference in global maximum sea surface temperature between the present and the year 2100 under two contrasting scenarios of climate change (RCP26 and RCP85)

source("sourceFunctions.R")

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# list future layers
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)

# open layers
SSTPresent <- load_layers("BO2_templtmax_ss")
SSTRCP26 <- load_layers("BO2_RCP26_2100_templtmax_ss")
SSTRCP85 <- load_layers("BO2_RCP85_2100_templtmax_ss")

SSTRCP26Diff <- SSTRCP26 - SSTPresent
SSTRCP85Diff <- SSTRCP85 - SSTPresent

# plot layers

plot(SSTRCP26Diff, main="Difference in maximum sea surface temperature; 2100 RCP26")
plot(SSTRCP85Diff, main="Difference in maximum sea surface temperature; 2100 RCP85")

# plot layers with fixed scale and a redish gradient

min(getValues(SSTRCP26Diff),na.rm=T)
max(getValues(SSTRCP85Diff),na.rm=T)

plot(SSTRCP26Diff, main="Difference in maximum sea surface temperature; 2100 RCP26", zlim=c(-1.2,10.9) , col= rev(heat.colors(100)))
plot(SSTRCP85Diff, main="Difference in maximum sea surface temperature; 2100 RCP85", zlim=c(-1.2,10.9) , col= rev(heat.colors(100)))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.3
# The main objective of this challenge is to plot the difference in sea ice thickness between the present and the year 2100 under two contrasting scenarios of climate change (RCP26 and RCP85).

source("sourceFunctions.R")

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# list future layers
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)

# open layers
IcePresent <- load_layers("BO2_icethickmean_ss")
IceRCP26 <- load_layers("BO2_RCP26_2100_icethickmean_ss")
IceRCP85 <- load_layers("BO2_RCP85_2100_icethickmean_ss")

IceRCP26Diff <- IceRCP26 - IcePresent
IceRCP85Diff <- IceRCP85 - IcePresent

# plot layers

plot(IceRCP26Diff, main="Difference in sea ice; 2100 RCP26")
plot(IceRCP85Diff, main="Difference in sea ice; 2100 RCP85")

# plot layers with fixed scale and a redish gradient

min(getValues(IceRCP26Diff),na.rm=T)
max(getValues(IceRCP85Diff),na.rm=T)

plot(IceRCP26Diff, main="Difference in sea ice; 2100 RCP26", zlim=c(-5.8,1.77) , col= heat.colors(100))
plot(IceRCP85Diff, main="Difference in sea ice; 2100 RCP85", zlim=c(-5.8,1.77) , col= heat.colors(100))

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.4
# The main objective of this challenge is to plot the difference in sea temperature between the surface and bottom layers of the ocean.

source("sourceFunctions.R")

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# list future layers
BOLayersFuture <- list_layers_future(datasets = "Bio-ORACLE")
View(BOLayersFuture)

# load layers
bottomTemp <- load_layers("BO2_tempmean_bdmean")
surfaceTemp <- load_layers("BO2_tempmean_ss")

tempDiff <- surfaceTemp - bottomTemp

# plot layers

plot(tempDiff, main="Difference in sea temperature")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.5
# The main objective of this assignment is to extract the environmental use (values) of the mediterranean coral Paramuricea clavata for the present. Use GBIF data for this assignment.

source("sourceFunctions.R")

# Bio-ORACLE layers:
# Dissolved oxygen concentration (mean at mean depth)
# Sea water temperature (maximum at mean depth)
# Sea water temperature (minimum at mean depth)
# Primary production (mean at mean depth)
# Sea water salinity (mean at mean depth)

records <- getOccurrencesGBIF("Paramuricea clavata")

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# load layers
environmentLayers <- load_layers(c("BO2_dissoxmean_bdmean","BO2_tempmax_bdmean","BO2_tempmin_bdmean","BO2_ppmean_bdmean","BO2_salinitymean_bdmean"))

# extract values from layers
environmentUse <- extract(environmentLayers,records[,c("Lon","Lat")])
head(environmentUse)
summary(environmentUse)

# making an histogram of a particular variable

hist(environmentUse[,"BO2_tempmax_bdmean"],breaks=25)
hist(environmentUse[,"BO2_dissoxmean_bdmean"],breaks=25)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.6
# The main objective of this challenge is to reclassify a set of environmental rasters and plot the potential habitat of a marine forest species (Saccorhiza polyschides) in European coastlines for the present and the future (year 2100 RCP 85) using a mechanistic approach (i.e., process based model). Compare ranges between time periods.

source("sourceFunctions.R")

# list present layers
BOLayers <- list_layers(datasets = "Bio-ORACLE")
View(BOLayers)

# develop reclassification matrices
lightConditions <- data.frame(from = c(0,5) , to=c(5,+Inf) , reclassValue=c(0,1))
lightConditions
salinityConditions <- data.frame(from = c(0,10) , to=c(10,+Inf) , reclassValue=c(0,1))
salinityConditions
maxsstConditions <- data.frame(from = c(-Inf,20.5) , to=c(20.5,+Inf) , reclassValue=c(1,0))
maxsstConditions
minsstConditions <- data.frame(from = c(-Inf,5) , to=c(5,+Inf) , reclassValue=c(0,1))
minsstConditions

# load layers for the present
lightPresent <- load_layers("BO2_lightbotmean_bdmax")
salinityPresent <- load_layers("BO2_salinitymean_ss")
maxsstPresent <- load_layers("BO2_tempmax_ss")
minsstPresent <- load_layers("BO2_tempmin_ss")

# load layers for the future (RCP85 year 2100)
salinityFuture <- load_layers("BO2_RCP85_2100_salinitymean_ss")
maxsstFuture <- load_layers("BO2_RCP85_2100_tempmax_ss")
minsstFuture <- load_layers("BO2_RCP85_2100_tempmin_ss")

# reclassify rasters for the suitable ecological domain
lightPresentSuitable <- reclassify(lightPresent,lightConditions)
salinityPresentSuitable <- reclassify(salinityPresent,salinityConditions)
maxsstPresentSuitable <- reclassify(maxsstPresent,maxsstConditions)
minsstPresentSuitable <- reclassify(minsstPresent,minsstConditions)

salinityFutureSuitable <- reclassify(salinityFuture,salinityConditions)
maxsstFutureSuitable <- reclassify(maxsstFuture,maxsstConditions)
minsstFutureSuitable <- reclassify(minsstFuture,minsstConditions)

# multiply all layers to meet the condition of niche suitability
nichePresent <- lightPresentSuitable * salinityPresentSuitable * maxsstPresentSuitable * minsstPresentSuitable
nicheFuture <- lightPresentSuitable * salinityFutureSuitable * maxsstFutureSuitable * minsstFutureSuitable

# crop layers
europeanExtent <- c(-30,40,20,75)
nichePresent <- crop(nichePresent,europeanExtent)
nicheFuture <- crop(nicheFuture,europeanExtent)

plot(nichePresent, main = "The ecological niche of Saccorhiza polyschides",axes = TRUE)
plot(nicheFuture, main = "The ecological niche of Saccorhiza polyschides",axes = TRUE)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.7
# The collinearity (high correlation) between your variables and region of interest.

source("sourceFunctions.R")

environmentLayers <- load_layers(c("BO2_lightbotmean_bdmax","BO2_salinitymean_ss","BO2_tempmax_ss","BO2_tempmin_ss"))

# crop layers
europeanExtent <- c(-30,40,20,75)
environmentLayers <- crop(environmentLayers,europeanExtent)

pairs(environmentLayers)
