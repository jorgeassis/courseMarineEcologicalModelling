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

# Load main functions
source("sourceFunctions.R")

# Read polygon defining global landmasses
world <- ne_countries(scale = 'medium')

## -----------------------
# 01. get records from gbif with the function getOccurrencesGBIF()

# download the records from GBIF
recordsGBIF <- getOccurrencesGBIF("species name")

# download the records from Obis
recordsObis <- getOccurrencesObis("species name")

# open additional datasets with read.csv
recordsExternalFile <- read.csv("Data/dataBases/gbif.csv", sep=";")

colnames(recordsExternalFile)

# subset objects to get coordinates only
recordsGBIF <- recordsGBIF[,c("Lon","Lat")]
recordsObis <- recordsObis[,c("Lon","Lat")]
recordsExternalFile <- recordsExternalFile[,c("decimalLongitude","decimalLatitude")]

# test column names. If needed, change column names to allow rbind() function
colnames(recordsGBIF)
colnames(recordsObis)
colnames(recordsExternalFile)

colnames(recordsExternalFile) <- c("Lon","Lat")

# merge datasets
records <- rbind(recordsGBIF,recordsObis,recordsExternalFile)

# Plot the biodiversity records.
plot(world, col="Gray", border="Gray", axes=TRUE, main="Distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# remove NA coordinates
records <- removeNA(records,"Lon","Lat")

# remove duplicate coordinates
records <- removeDuplicated(records,"Lon","Lat")

# identify and remove records on land
records <- removeOverLand(records,"Lon","Lat")

# confirm that all records belong to the know distribution of the species
# plot a polygon defining the region of interest
plot(world)

# choose the region where the species occur
regionOfInterest <- drawPoly()

# select the records within the drawn polygon
pointsInRegion <- whichOverPolygon(records,regionOfInterest)

# clip the records of occurrence
records <- records[pointsInRegion,]

# remove records outside the known vertical distribution (example at 80m depth)
bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
plot(bathymetry)

depthUse <- extract(bathymetry,records[,c("Lon","Lat")])
head(depthUse)
hist(depthUse,breaks=50)
records <- records[ which(depthUse > -80) ,]

# plot records with plot funciton
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# plot records with ggplot
ggplot() + 
  geom_polygon(data = world, fill = "#B9B8B0", colour = "#707070", size = 0.2, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = Lon, y = Lat), color = "#9A3B04") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_fixed() +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Clean istribution records")

# save data frame to external file
write.table(records,file="",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------