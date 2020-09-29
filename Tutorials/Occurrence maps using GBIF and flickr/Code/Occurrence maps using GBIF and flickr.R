

# Coding Club Tutorial:
# Species occurrence maps based on GBIF and Flickr data
# Access: https://ourcodingclub.github.io/tutorials/seecc_1/index.html#Flickr 

# Visualising and creating heat maps of puffin occurrence records in GBIF (Global Biodiversity Information Facility) 
# and from online images on Flickr

# Presented by: Owen Middleton


# July 2020 paper in Trends in Ecology and Evolution:
# iEcology: Harnessing Large Online Resources to Generate Ecological Insights
# Access: https://www.sciencedirect.com/science/article/pii/S016953472030077X

# Abstract:
# iEcology is a new research approach that seeks to quantify patterns and processes in the natural world
# using data accumulated in digital sources collected for other purposes.

# iEcology studies have provided new insights into species occurrences, traits, phenology, functional
# roles, behavior, and abiotic environmental features.

# iEcology is expanding, and will be able to provide valuable support for ongoing research efforts, as
# comparatively low-cost research based on freely available data.

# We expect that iEcology will experience rapid development over coming years and become one of the major
# research approaches in ecology, enhanced by emerging technologies such as automated content analysis,
# apps, internet of things, ecoacoustics, web scraping, and open source hardware.



# Example of Flickr use in iEcology
# Increasing records of non-native reptile species in the UK:
# Author: Stephen Allain (PhD Student at the University of Kent)
# https://www.researchgate.net/profile/Steven_Allain/publication/334138130_Mining_Flickr_a_method_for_expanding_the_known_distribution_of_invasive_species/links/5d19fb5f92851cf4405a54d5/Mining-Flickr-a-method-for-expanding-the-known-distribution-of-invasive-species.pdf 


#————————————————————————————————————————————————————————————————————————————————————————————####
# Main tutorial ----
#————————————————————————————————————————————————————————————————————————————————————————————####

#————————————————————————————————————————————————————————————————————————————————————————————####
# 1. Install and upload packages ----
#————————————————————————————————————————————————————————————————————————————————————————————####
install.packages("pacman") # Installs (if not already done) and loads packages
library(pacman)

# Load other packages using the 'pacman' packages
pacman::p_load("rgbif", "ggplot2", "maps", "ggthemes", "sp", "rworldmap", "rgdal", "rgeos", "raster", "maptools")


#————————————————————————————————————————————————————————————————————————————————————————————####
# 2. GBIF data ---- 
#————————————————————————————————————————————————————————————————————————————————————————————####

# Get all records of puffins (Fratercula arctica) in UK from GBIF

# Get UK code ----
UK_code <- isocodes[grep("United Kingdom", isocodes$name), "code"]


# Load data ----
# BUT!!!
# This step takes some time...

# Art Attack Rules
# If something takes a long time to run, have the output data already saved to be called upon
occur <- occ_search(scientificName = "Fratercula arctica",
                    country = UK_code, hasCoordinate = TRUE, limit = 3000, year = '2006,2016', return = "data")
write.csv(occur, "./Tutorials/Occurrence maps using GBIF and flickr/Data/GBIF UK Puffin Records.csv")


# Load pre-downloaded GBIF data
occur <- read.csv("./Tutorials/Occurrence maps using GBIF and flickr/Data/GBIF UK Puffin Records.csv")

# Check structure (good practice)
str(occur)

# Create sp object ----
coordinates(occur) <- c("decimalLongitude", "decimalLatitude")             # go back to original dataframe and make it spatial
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")     # geographical, datum WGS84
proj4string(occur) <- crs.geo                                # assign the coordinate system
plot(occur, pch = 20, col = "steelblue")                     # plot the data

# Add uk borders
data(countriesLow)
plot(countriesLow, add = T)

# Re-project:
occur <- spTransform(occur, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

#————————————————————————————————————————————————————————————————————————————————————————————####
# 3. Flickr data ----
#————————————————————————————————————————————————————————————————————————————————————————————####

# You can access the code for downloading flickr data here: https://github.com/ourcodingclub/SEECC-workshop/blob/master/FlickrAPI.R

# Load data ----
flickr <- read.table("./Tutorials/Occurrence maps using GBIF and flickr/Data/flickr_puffins.txt", header = T, sep = "\t")
str(flickr)

# Plot Flickr occurrence records ----
geopics <- flickr[, c(4,5)]                           # subset the dataset to keep coordinates only
coordinates(geopics) <- c("longitude", "latitude")    # make it spatial
plot(geopics)                                         # plot it
# Notice there is one particular problematic coordinate


# Filter out no-UK records
which(flickr$latitude < 49.9) # Remove anything south of the most southern UK coordiate
flickr <- flickr[-which(flickr$latitude < 49.9),]


# Create sp object ----
coordinates(flickr) <- c("longitude", "latitude")             # go back to original dataframe and make it spatial
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")     # geographical, datum WGS84
proj4string(flickr) <- crs.geo                                # assign the coordinate system
plot(flickr, pch = 20, col = "steelblue")                     # plot the data

# Add uk borders
data(countriesLow)
plot(countriesLow, add = T)
# Okay, but there are lots of inland records which are unlikely to be puffin photos


# Tidying Flickr puffin records ----
# Remove inland records
UK <- getData("GADM", country = "GB", level = 0)

# Ensure they are in a projection that works with m as a unit:
UK_proj <- spTransform(UK, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
flickr_proj <- spTransform(flickr, CRS("+proj=utm +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

# Dissolve multiple polygons
UK_diss <- gUnaryUnion(UK_proj)

# Identify inland and marine records
flickr_terr <- flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL)) == FALSE),]
flickr_mar <- flickr_proj[which(is.na(over(flickr_proj, UK_diss, fn = NULL)) == TRUE),]

# Plot the inland and marine records
par(mfrow = c(1,2))
plot(flickr_terr)
plot(flickr_mar)

# Back to a spatial lines shapefile
UK_coast <- as(UK_diss, 'SpatialLines')

# Only keep records within 1km from the coat
dist <- gWithinDistance(flickr_terr, UK_coast, dist = 1000, byid = T)
dist.df <- as.data.frame(dist)
flickr_coast <- flickr_terr[which(dist.df == "TRUE"),]

# Plot new records
plot(flickr_coast)

# Add the marine records too
flickr_correct <- spRbind(flickr_mar, flickr_coast)
plot(UK_coast)
points(flickr_correct, pch = 20, col = "steelblue")


#————————————————————————————————————————————————————————————————————————————————————————————####
# 4. Density maps ----
#————————————————————————————————————————————————————————————————————————————————————————————####

# GBIF density maps ----
UK.Df <- fortify(UK_diss, region = "ID_0")
gbif.points <- fortify(cbind(occur@data, occur@coords))

gbif.density <- ggplot(data = gbif.points, aes(x = decimalLongitude, y = decimalLatitude)) +  # plot the flickr data
    geom_polygon(data = UK.Df, aes(x = long, y = lat, group = group),            # plot the UK
                 color = "black", fill = "gray82") + coord_fixed() +    # coord_fixed() ensures that one unit on the x-axis is the same length as one unit on the y-axis
    geom_point(color = "dodgerblue4",size = 2,shape = ".") +                   # graphical parameters for points
    stat_density2d(aes(x = decimalLongitude,                           # create the density layer based on where the points are
                       y = decimalLatitude,  fill = ..level.., alpha = ..level..),   # colour and transparency depend on density
                   geom = "polygon", colour = "grey95",size=0.3) +            # graphical parameters for the density layer
    scale_fill_gradient(low = "yellow", high = "red") +                 # set colour palette for density layer
    scale_alpha(range = c(.25, .5), guide = FALSE) +                    # set transparency for the density layer 
    facet_wrap(~ year) +                                                 # multipanel plot according to the variable "year" in the flickr dataset
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),  # don't display x and y axes labels, titles and tickmarks 
          axis.ticks.x = element_blank(),axis.title.y = element_blank(),   
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          text = element_text(size = 18),legend.position = c(.9, .15),       # size of text and position of the legend
          panel.grid.major = element_blank(),                            # eliminates grid lines from background
          panel.background = element_blank())                            # set white background

# Flickr density maps ----
UK.Df <- fortify(UK_diss, region = "ID_0")
flickr.points <- fortify(cbind(flickr_correct@data, flickr_correct@coords))

flickr.density <- ggplot(data = flickr.points, aes(x = longitude, y = latitude)) +  # plot the flickr data
    geom_polygon(data = UK.Df,aes(x = long, y = lat, group = group),            # plot the UK
                 color = "black", fill = "gray82") + coord_fixed() +    # coord_fixed() ensures that one unit on the x-axis is the same length as one unit on the y-axis
    geom_point(color = "dodgerblue4",size = 2,shape = ".")+                   # graphical parameters for points
    stat_density2d(aes(x = longitude,                           # create the density layer based on where the points are
                       y = latitude,  fill = ..level.., alpha = ..level..),   # colour and transparency depend on density
                   geom = "polygon", colour = "grey95",size=0.3) +            # graphical parameters for the density layer
    scale_fill_gradient(low = "yellow", high = "red") +                 # set colour palette for density layer
    scale_alpha(range = c(.25, .5), guide = FALSE) +                    # set transparency for the density layer 
    facet_wrap(~ year) +                                                 # multipanel plot according to the variable "year" in the flickr dataset
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),  # don't display x and y axes labels, titles and tickmarks 
          axis.ticks.x = element_blank(),axis.title.y = element_blank(),   
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          text = element_text(size = 18),legend.position = c(.9, .15),       # size of text and position of the legend
          panel.grid.major = element_blank(),                            # eliminates grid lines from background
          panel.background = element_blank())                            # set white background

# Save plots ----
ggsave("./Tutorials/Occurrence maps using GBIF and flickr/Results/FlickR Density Map.tiff",
       flickr.density, height = 6, width = 6, units = "in", dpi = 72)

ggsave("./Tutorials/Occurrence maps using GBIF and flickr/Results/GBIF Density Map.tiff",
       gbif.density, height = 6, width = 6, units = "in", dpi = 72)


#————————————————————————————————————————————————————————————————————————————————————————————####
# 5. Issues ----
#————————————————————————————————————————————————————————————————————————————————————————————####

# Problems with Flickr API access ----
 
# Could not get the API code to run.
# I wanted to apply this to another species but could not.

# Can anybody solve this?

#————————————————————————————————————————————————————————————————————————————————————————————####
# End of script ----
#————————————————————————————————————————————————————————————————————————————————————————————####