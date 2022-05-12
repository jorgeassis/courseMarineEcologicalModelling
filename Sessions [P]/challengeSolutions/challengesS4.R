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

# Use Recipe #1 
# 1. get records with getOccurrencesGBIF() and getOccurrencesObis()
# 2. remove NA coordinates
# 4. remove duplicate coordinates with duplicated()
# 5. identify records outside the know distibution (also depth)
# 6. remove records on land
# 8. plot records with plot() or ggplot()

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
