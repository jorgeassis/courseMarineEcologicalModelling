## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Pack of Recipes
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
## Recipe 1
# The main objetive of this Recipe is to **collect** biodiversity data from GBIF in R, **clean records**, produce a **plot** and **save** records for latter use.

# 01. open records from external file
# 02. remove NAs and duplicated records
# 03. identify and remove records on land
# 04. remove records outside the know distribution of the species
# 06. plot records with the functions plot() and ggplot()
# 07. Save records with the function write.table()

## -----------------------
## -----------------------

# Set the working directory to where the data is located
setwd("~/Dropbox/Tutoring/Classes & Courses/Ecological Niche Modelling and Climate Change/Main Contents/Sessions P")

# Load main functions
source("sourceFunctions.R")

## -----------------------
# 01. get records

records <- read.table('Data/dataBases/MergedCoordinates.csv',sep=';',header=TRUE)
View(records)
# plot records

# Get the global landmasses
world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

# Plot the biodiversity records.
plot(world, col="Gray", border="Gray", axes=TRUE, main="Distribution records" , ylab="latitude", xlab="longitude")

points( records$Lon, records$Lat , pch=20, col="Black")

points( records[ , c("Lon","Lat") ] , pch=20, col="Black")

## -----------------------
# 02. remove NAs and duplicated records

# remove NA coordinates
records <- removeNA(records,"Lon","Lat")

# remove duplicate coordinates
records <- removeDuplicated(records,"Lon","Lat")

## -----------------------
# 03. identify and remove records on land

# based on a distance (km) to shore
records <- removeOverLandDist(records, "Lon", "Lat", landmassRaster = "Data/RasterLayers/BathymetryDepthMean.tif", dist = 9)

# identify and remove records on offshore regions [for intertidal species]
records <- removeOverOffshore(records, "Lon", "Lat", intertidalmask = "Data/RasterLayers/CoastLine.tif")

## -----------------------
# 04. confirm that all records belong to the know distribution of the species

# produce and plot a polygon defining the region of interest
world <- shapefile("Data/vectorShapefiles/globalLandmass/world.shp")

myExtent <- c(-15,35,30,50)
myRegion <- crop(world,extent(myExtent))

dev.new()
plot(myRegion,col="gray",border="gray")
points(records,col="black", pch=20)

# choose the region where the species occur
regionOfInterest <- drawPoly()

# clip the records of occurrence, give the proper Lon and Lat names
pointsInRegion <- whichOverPolygon(records,regionOfInterest,"Lon","Lat")

# clip the records of occurrence
records <- records[pointsInRegion, ]

plot(myRegion,col="gray",border="gray")
points(records[,c("Lon","Lat")],col="black", pch=20)

## -----------------------
# 05. plot final records

# plot records with plot function
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# plot records with ggplot
ggplot() +
  geom_polygon(data = myRegion, fill = "#B9B8B0", colour = "#707070", size = 0.2, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = Lon, y = Lat), color = "#9A3B04") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_fixed() +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Clean distribution records")

## -----------------------
# 06. export final records

# save data frame to external file
write.table(records,file="Data/myFile.csv",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
