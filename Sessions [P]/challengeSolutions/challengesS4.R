# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.1
# The main objective of this challenge is to collect and plot biodiversity records from GBIF - http://gbif.org - and iNaturalist - https://inaturalist.org/.

source("sourceFunctions.R")

dataWorld <- "Data/vectorShapefiles/globalLandmass/world.shp"
dataSpecies <- "Data/dataBases/observations-80347.csv"

world <- shapefile(dataWorld)
dataSpecies <- read.csv(dataSpecies)

colnames(dataSpecies)
View(dataSpecies)
plot(world , border="gray" , col="gray" )
points( dataSpecies[ , c("longitude","latitude") ] , col="red" , pch= 19)

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.2
# The main objective of this challenge is to collect and merge biodiversity data from different sources (inaturalist and GBIF) for a species of your interest.

source("sourceFunctions.R")

data <- "Data/vectorShapefiles/globalLandmass/world.shp"
data2 <- "Data/dataBases/inaturalist.csv"

world <- shapefile(dataWorld)
dataSpecies1 <- read.csv(data2,sep=",")
dataSpecies2 <- getOccurrencesGBIF("Laminaria ochroleuca")

colnames(dataSpecies1)
colnames(dataSpecies2)

# will give an error
dataAll <- rbind(dataSpecies1,dataSpecies2)

# column names need to match
dataSpecies1 <- dataSpecies1[,c("longitude","latitude")]
dataSpecies2 <- dataSpecies2[,c("Lon","Lat")]

colnames(dataSpecies1) <- c("Lon","Lat")

dataAll <- rbind(dataSpecies1,dataSpecies2)
plot(world)
points(dataAll[,c("Lon","Lat")])

ggplot() + geom_polygon(data = world, fill = "#B9B8B0", colour = "#707070", size = 0.2,
    aes(long, lat, group = group)) + geom_point(data = dataAll, aes(Lon, Lat), color = "#9A3B04") +
    xlab("Longitude") + ylab("Latitude") + ggtitle("Combined records")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 4.3 / 4.4
# The main objective of this challenge is to collect biodiversity data from GBIF and OBIS in R, clean records and produce a plot. For this challenge use the species Laminaria ochroleuca.

# Species: Laminaria ochroleuca
# Information: The species only occur from morocco to Brittany (NE Atlantic), in the Azores and the Western Mediterranean Sea at maximum depths of 80m

source("sourceFunctions.R")

world <- ne_countries(scale = 'medium')

# download the records for a species
recordsGBIF <- getOccurrencesGBIF("Laminaria ochroleuca")
recordsObis <- getOccurrencesObis("Laminaria ochroleuca")

# merge datasets
records <- rbind(recordsGBIF,recordsObis)

# remove NA coordinates
records <- removeNA(records,"Lon","Lat")

# remove duplicate coordinates
records <- removeDuplicated(records,"Lon","Lat")

# confirm that all records belong to the know distribution of the species
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# choose the region where the species occur
regionOfInterest <- drawPoly()

# select the records within the drawn polygon
pointsInRegion <- whichOverPolygon(records[, c("Lon", "Lat")], regionOfInterest)

# clip the records of occurrence
records <- records[pointsInRegion, c("Lon", "Lat")]

# reconfirm that all records bellong to the know distribution of the species
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# remove records outside the known vertical distribution (example at 80m depth)
bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
depthUse <- extract(bathymetry,records[,c("Lon","Lat")])
head(depthUse)
hist(depthUse,breaks=50)
records <- records[ which(depthUse > -80) ,]

# identify and remove records on land
records <- removeOverLand(records,"Lon","Lat")

# plot records V1
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,c("Lon","Lat")], pch=20, col="Black")

# plot records V2
ggplot() + geom_polygon(data = world, fill = "#B9B8B0", colour = "#707070", size = 0.2,
    aes(long, lat, group = group)) + geom_point(data = records, aes(Lon, Lat), color = "#9A3B04") +
    xlab("Longitude") + ylab("Latitude") + ggtitle("Clean distribution records")

# save data frame to external file
write.table(records,file="",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
