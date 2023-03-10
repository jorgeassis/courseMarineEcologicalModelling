# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.1
# The main objective of this challenge is to collect and plot biodiversity records from GBIF - http://gbif.org - and iNaturalist - https://inaturalist.org/.

source("sourceFunctions.R")

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)

plot(world , border="gray" , col="gray" )
points( records, col="Black" , pch= 19)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.2
# The main objective of this challenge is to collect and merge biodiversity data from different sources (inaturalist and GBIF) for a species of your interest.

source("sourceFunctions.R")

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)

mediterraneanExtent <- extent(-15, 37.5, 30, 50)
landmassEur <- crop(world, mediterraneanExtent)

plot(landmassEur)
points( records, col="Black" , pch= 19)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.3
# The main objetive of this challenge is to clean records and produce a plot.

source("sourceFunctions.R")

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)

mediterraneanExtent <- extent(-22.45,43.12,25.86,47.26)
landmassEur <- crop(world, mediterraneanExtent)

plot(landmassEur)
points( records, col="Black" , pch= 20)

regionOfInterest <- drawPoly()

pointsInRegion <- whichOverPolygon(records,regionOfInterest)
records <- records[pointsInRegion,]

plot(landmassEur)
points( records, col="Black" , pch= 19)

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------