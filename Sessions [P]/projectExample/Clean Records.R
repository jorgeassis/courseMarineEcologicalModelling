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

# 01. open records from file
# 02. remove NA coordinates
# 03. remove duplicate coordinates with the function duplicated()
# 04. remove records outside the know distribution of the species
# 05. plot records with the functions plot() and ggplot()
# 06. Save records with the function write.table()

## -----------------------
## -----------------------

# Set the working directory to where the data is located
setwd(".../A complete example")

# Load main functions
source("sourceFunctions.R")

# Read polygon defining global landmasses
world <- shapefile("Data/globalLandmass/world.shp")

## -----------------------
# 01. get records

# open records file from read.csv
records <- read.csv("Data/myRecords.csv", sep=";")

## -----------------------
# 03. plot records

# Plot the biodiversity records.

myExtent <- extent(-20,50,20,60)
myRegion <- crop(world,myExtent)
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Distribution records" , ylab="latitude", xlab="longitude")
points(records, pch=20, col="Black")

## -----------------------
# 04. remove NAs and duplicated records

head(records)

# remove NA coordinates
records <- removeNA(records,"Lon","Lat")

# remove duplicate coordinates
records <- removeDuplicated(records,"Lon","Lat")

## -----------------------
# 05. confirm that all records belong to the know distribution of the species

# choose the region where the species occur
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Distribution records" , ylab="latitude", xlab="longitude")
points(records, pch=20, col="Black")

regionOfInterest <- drawPoly()

# select the records within the drawn polygon
pointsInRegion <- whichOverPolygon(records,regionOfInterest)

# clip the records of occurrence
records <- records[pointsInRegion,]

plot(myRegion,col="gray",border="gray")
points(records,pch=20, col="Black")

## -----------------------
# 06. plot final records

# plot records with plot function
plot(myRegion, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records, pch=20, col="Black")

## -----------------------
# 08. export final records

# save data frame to external file
write.table(records,file="Data/mycleanRecords.csv",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------