# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.1

source("sourceFunctions.R")

dataWorld <- "Data/vectorShapefiles/globalLandmass/world.shp"
dataSpecies <- "Data/dataBases/observations-80347.csv"

world <- shapefile(dataWorld)
dataSpecies <- read.csv(dataSpecies)

colnames(dataSpecies)
View(dataSpecies)
plot(world)
plot(  dataSpecies[ , c("longitude","latitude") ],add=TRUE) 

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 5.2

source("sourceFunctions.R")

data <- "Data/vectorShapefiles/globalLandmass/world.shp"
dataSpecies <- "Data/dataBases/observations-80347.csv"

world <- shapefile(dataWorld)
dataSpecies1 <- read.csv(dataSpecies)
dataSpecies2 <- getOccurrencesGBIF("Laminaria ochroleuca")

colnames(dataSpecies1)
colnames(dataSpecies2)

# Will give an error
dataAll <- rbind(dataSpecies1,dataSpecies2)

dataSpecies1 <- dataSpecies1[,c("longitude","latitude")]
dataSpecies2 <- dataSpecies2[,c("Lon","Lat")]

colnames(dataSpecies1) <- c("Lon","Lat")

dataAll <- rbind(dataSpecies1,dataSpecies2)
plot(world)
points(dataAll[,c("Lon","Lat")])

defineRegion(dataAll)
dataAll <- selectRecords(dataAll[,c("Lon","Lat")])
