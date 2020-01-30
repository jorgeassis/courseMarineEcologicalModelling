## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
##
## Recipes
##
## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
## Recipe 1
# The main objetive of this Recipe is to **collect** biodiversity data from GBIF in R, **clean records**, produce a **plot** and **save** records for latter use.

# 1. get records with gbif()
# 2. populate NA coordinates
# 3. remove NA coordinates
# 4. remove duplicate coordinates with duplicated()
# 5. remove records on land with over() a polygon with the shape of the world generated with ne_countries()
# 6. get depth for each record with extract() over bathymetry
# 7. remove records in regions deeper than -80 with over()
# 8. plot records with ggplot()
# 9. Save records with write.table()

source("sourceFunctions.R")

world <- ne_countries(scale = 'medium')

# download the records for a species
records <- gbif("Laminaria","ochroleuca")
sum( is.na(records$lon) )

# get coordinates based on locality name
georeferencedLocalities <- getCoordinates( records[which( is.na(records$lon) ), "locality"] )
head(georeferencedLocalities)
nrow(georeferencedLocalities)

# populate NA coordinates
records[which( is.na(records$lon) ), c("lon","lat")] <- georeferencedLocalities
sum( is.na(records$lon) )

# remove NA coordinates
sum( is.na(records$lon) )
records <- records[which( ! is.na(records$lon) ), ]

# remove duplicate coordinates
sum( duplicated(records[,c("lon","lat")]) )
records <- records[which( ! duplicated( records[,c("lon","lat")] ) ), ]

# identify and remove records on land
records <- SpatialPointsDataFrame(records[,c("lon","lat")], data=records)
crs(records) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

recordsOverLand <- which( ! is.na(over(records,world)[,1]) )
length(recordsOverLand)
records <- records[-recordsOverLand,]
records <- data.frame(records)

# identify records outside known vertical distribution
bathymetry <- raster("data/rasterLayers/BathymetryDepthMean.tif")
depthUse <- extract(bathymetry,records[,c("lon","lat")])
head(depthUse)
records <- records[ which(depthUse >= -80) ,]

# plot records
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = lon, y = lat), color = "red") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_fixed()

ggplot() + theme(
  axis.text.x=element_blank(),
  axis.text.y=element_blank(),
  axis.ticks=element_blank(),
  axis.title.x=element_blank(),
  axis.title.y=element_blank()) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  geom_point(data = records, aes(x = lon, y = lat), color = "red") +
  scale_y_continuous(breaks = seq(-90,90, by=20)) +
  scale_x_continuous(breaks = seq(-180,180,by=20)) +
  coord_map("orthographic", orientation=c(30, 0, 0))

# save data frame to external file
write.table(records,file="",sep=";")

```


## -----------------------------------------------------------------------------------------------
## -----------------------------------------------------------------------------------------------
