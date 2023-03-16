# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.1
# The main objective of this challenge is to plot biodiversity records from a file, for a given region.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

source("sourceFunctions.R")

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)

europeanExtent <- extent(-15, 45, 30, 47.5)
europeanCoastlines <- crop(world,europeanExtent)
plot(europeanCoastlines, main="Distribution of Paramuricea clavata", col="gray", border="gray", axes=TRUE )
points( records, col="Black" , pch= 19, cex=0.5)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 3.2
# The main objetive of this challenge is to clean records and produce a plot.

setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

source("sourceFunctions.R")

world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")
records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)

mediterraneanExtent <- extent(-15, 37.5, 30, 50)
landmassEur <- crop(world, mediterraneanExtent)

plot(landmassEur, col="gray", border="gray" )
points( records, col="Black" , pch= 19)

regionOfInterest <- drawPoly()
pointsInRegion <- whichOverPolygon(records,regionOfInterest)
records <- records[pointsInRegion,]

plot(landmassEur, main="Clean distribution of Paramuricea clavata", col="gray", border="gray", axes=TRUE )
points( records, col="Black" , pch= 19, cex=0.5)

write.csv(records, file="myCleanRecords.csv")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------