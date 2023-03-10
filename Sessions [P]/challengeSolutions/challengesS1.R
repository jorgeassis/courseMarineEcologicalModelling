# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.1
# The main objective of this challenge is to **install** a set of key R packages.

# install packages
install.packages("devtools", dependencies=TRUE)
install.packages("raster", dependencies=TRUE)
install.packages("sp", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("rgeos", dependencies=TRUE)
install.packages("rnaturalearth", dependencies=TRUE)
install.packages("rnaturalearthdata", dependencies=TRUE)
install.packages("rnaturalearthhires", dependencies=TRUE)
install.packages("rgdal", dependencies=TRUE)


# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.2
# The main objective of this challenge is to **import** a data table in R.

# set the working directory
setwd("/Volumes/Jellyfish/Dropbox/Tutoring/Classes & Courses/Modelling the distribution of biodiversity and climate change/Git/courseMarAfrica/")

# source a set of functions
source("sourceFunctions.R")

# ----------------------------------------------------
# ----------------------------------------------------
# Challenge 1.3
# The main objective of this challenge is to **import** a data table in R.

# set the working directory
setwd("/Volumes/Jellyfish/Dropbox/Tutoring/Classes & Courses/Modelling the distribution of biodiversity and climate change/Git/courseMarAfrica/")

# reading data frame from semicolon delimited files
myDF <- read.table("Data/dataBases/Paramuricea_clavata.csv", sep =";", header=T)

# view the data.frame in RStudio
View(myDF)
