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

# 01. get records from gbif with the function getOccurrencesGBIF()
# 02. get records from obis with the function getOccurrencesObis()
# 03. get additional external records
# 04. remove NA coordinates
# 05. remove duplicate coordinates with the function duplicated()
# 06. remove records outside the know distribution of the species
# 07. remove records outside the known vertical distribution
# 08. remove records over landmasses
# 09. plot records with the functions plot() and ggplot()
# 10. Save records with the function write.table()

## -----------------------
## -----------------------

# Set the working directory to where the data is located
setwd(".../A complete example")

# Load main functions
source("sourceFunctions.R")

# Read polygon defining global landmasses
world <- ne_countries(scale = 'medium')

## -----------------------
# 01. get records

# download the records from GBIF
recordsGBIF <- getOccurrencesGBIF("Paramuricea clavata")

# download the records from Obis
recordsObis <- getOccurrencesObis("Paramuricea clavata")

# open additional datasets with read.csv
recordsExternalFile <- read.csv("Data/Paramuricea_clavata.csv", sep=";")

## -----------------------
# 02. combine records into a unique dataset

# subset objects to get coordinates only
recordsGBIF <- recordsGBIF[,c("Lon","Lat")]
recordsObis <- recordsObis[,c("Lon","Lat")]

colnames(recordsExternalFile)
recordsExternalFile <- recordsExternalFile[,c("Lon","Lat")]

# test column names. If needed, change column names to allow rbind() function
colnames(recordsGBIF)
colnames(recordsObis)
colnames(recordsExternalFile)

# Change column names if needed
colnames(recordsExternalFile) <- c("Lon","Lat")

# merge datasets
records <- rbind(recordsGBIF,recordsObis,recordsExternalFile)

## -----------------------
# 03. plot records

# Plot the biodiversity records.

myExtent <- c(-20,50,20,60)
myRegion <- crop(world,extent(myExtent))
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

## -----------------------
# 04. remove NAs and duplicated records

# remove NA coordinates
records <- removeNA(records,"Lon","Lat")

# remove duplicate coordinates
records <- removeDuplicated(records,"Lon","Lat")

## -----------------------
# 05. identify and remove records on land

## based on a distance (km) to shore [alternative]
records <- removeOverLandDist(records, "Lon", "Lat", dist = 9)

## -----------------------
# 05.1. identify and remove records on offshore regions [for intertidal species]

# records <- removeOverOffshore(records, "Lon", "Lat", intertidalmask = "Data/CoastLine.tif")

## -----------------------
# 06. confirm that all records belong to the know distribution of the species

# choose the region where the species occur
regionOfInterest <- drawPoly()

# select the records within the drawn polygon
pointsInRegion <- whichOverPolygon(records,regionOfInterest)

# clip the records of occurrence
records <- records[pointsInRegion,]

plot(myRegion,col="gray",border="gray")
points(records,pch=20, col="Black")

# remove records outside the known vertical distribution (example at 80m depth)
bathymetry <- raster("Data/BathymetryDepthMean.tif")
plot(bathymetry)

depthUse <- extract(bathymetry,records)
head(depthUse)
hist(depthUse,breaks=50)
records <- records[ which(depthUse > -150) ,]

## -----------------------
# 07. plot final records

# plot records with plot function
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# plot records with ggplot
ggplot() +
  geom_polygon(data = myRegion, fill = "#B9B8B0", colour = "#707070", size = 0.2, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = Lon, y = Lat), color = "#000000") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_fixed() +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Clean distribution records")

## -----------------------
# 08. export final records

# save data frame to external file
write.table(records,file="Data/mycleanRecords.csv",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
