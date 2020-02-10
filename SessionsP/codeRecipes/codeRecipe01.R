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
# 03. get additional records
# 04. populate NA coordinates 
# 05. remove NA coordinates 
# 06. remove duplicate coordinates with the function duplicated()
# 07. remove records outside the know distribution of the species
# 08. remove records outside the known vertical distribution
# 09. remove records over landmasses 
# 10. plot records with the functions plot() and ggplot()
# 11. Save records with the function write.table()

source("sourceFunctions.R")

world <- ne_countries(scale = 'medium')

# download the records for a species
recordsGBIF <- getOccurrencesGBIF("Laminaria ochroleuca")
recordsObis <- getOccurrencesObis("Laminaria ochroleuca")

# open additional datasets with read.csv
# if needed, change colunm names to allow rbind() function

# merge datasets
records <- rbind(recordsGBIF,recordsObis)

# get coordinates based on locality name
georeferencedLocalities <- getCoordinates( records[which( is.na(records$Lon) ), "Locality"] )
head(georeferencedLocalities)
nrow(georeferencedLocalities)

# populate NA coordinates
records[which( is.na(records$Lon) ), c("Lon","Lat")] <- georeferencedLocalities
sum( is.na(records$Lon) )

# remove NA coordinates
sum( is.na(records$Lon) )
records <- records[which( ! is.na(records$Lon) ), ]

# remove duplicate coordinates
sum( duplicated(records[,c("Lon","Lat")]) )
records <- records[which( ! duplicated( records[,c("Lon","Lat")] ) ), ]

# confirm that all records bellong to the know distribution of the species
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,3:4], pch=20, col="Black")

# identify final records and remove those outside the know distibution 
defineRegion(records)
records <- selectRecords(records)

# reconfirm that all records bellong to the know distribution of the species
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,3:4], pch=20, col="Black")

# remove records outside the known vertical distribution (example at 80m depth)
bathymetry <- raster("Data/rasterLayers/BathymetryDepthMean.tif")
depthUse <- extract(bathymetry,records[,c("Lon","Lat")])
head(depthUse)
hist(depthUse,breaks=50)
records <- records[ which(depthUse > -80) ,]

# identify and remove records on land
overLand <- whichOverLand(records)
length(overLand)
records <- records[-overLand,]

# plot records V1
plot(world, col="Gray", border="Gray", axes=TRUE, main="Clean distribution records" , ylab="latitude", xlab="longitude")
points(records[,3:4], pch=20, col="Black")

# plot records V2
ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = Lon, y = Lat), color = "red") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_fixed()

# plot records V3
ggplot() + theme(
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank()) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = Lon, y = Lat), color = "red") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_map("orthographic", orientation=c(30, 0, 0))

# save data frame to external file
write.table(records,file="",sep=";")

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------