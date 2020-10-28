# Hypervolume script

# Load libraries
library(hypervolume)
library(tidyverse)
library(ggalt)
library(sf)

# The purpose of this script is to get familiar with the basic of the hypervolume package 
# for measuring species' morphological and ecological niche spaces

# We will use it to:
# 1. Explore differences in iris species morphological space
# 2. Explore species distribution modelling using the white oak and bioclim variables

# Exercise 1: Exploring differences in iris morphological space ----
# Load data that is already in R
data(iris)

# What is this dataset?
# Morphological traits measured from multiple individuals of three plant species
ggplot(data = iris,
       aes(x = Sepal.Length, y = Sepal.Width, fill = Species)) +
  geom_point(pch = 21, colour = "black", size = 3, alpha = 0.6) +
  scale_fill_manual(values = c("orange4", "blue4", "green4")) +
  labs(x = "Sepal length (mm)", y = "Sepal length (mm)") +
  theme_classic() +
  theme(panel.grid = element_blank())

# It's always a good thing to work with 'tidy' data, so convert from wide 
# to long:
iris.long <- gather(iris, "Trait", "Measure", Sepal.Length:Petal.Width)

# Plot density plot
ggplot(data = iris.long,
       aes(x = Measure, fill = Species)) +
  geom_density(alpha = 0.5) +
  facet_wrap(.~Trait) +
  theme_classic() +
  theme(panel.grid = element_blank())

# We want to know what the overlap is in the values of these measurements
# in a multi-dimensional space.

# There are multiple ways of performing hypervolume calculations and you
# need to think which one is most appropriate.

# Let's subset the data between species first:
setosa <- subset(iris, Species=="setosa")[,1:3]
virginica <- subset(iris, Species=="virginica")[,1:3]

# Calculate the hypervolumes
hv1 = hypervolume_gaussian(setosa)
hv2 = hypervolume_gaussian(virginica)

# Plot them
plot(hv1)
plot(hv2)

# Calculate the centroid distance
hypervolume_distance(hv1, hv2, type='centroid')
# Not so useful in itself, but useful when making multiple comparisons in relative terms.


# Create hypervolume list to plot on top of one another
data_split.test = split(iris[,1:3],
                        iris$Species)

# Perform the 'hypervolume' function on all of the objects in the list 
hvs_split.test = lapply(data_split.test, hypervolume)

# Sometimes certain packages need to combin things into their own kind of list for analyses:
hvs_joined.test = hypervolume_join(hvs_split.test)

# Plot the joined hypervolumes
plot(hvs_joined.test)
plot(hvs_joined.test,
     show.3d = TRUE)

# Calculate overlap for two hypervolumes
set1 <- hypervolume_set(hvs_joined.test@HVList[2]$versicolor,
                        hvs_joined.test@HVList[3]$virginica,
                        check.memory = FALSE)

# Plot:
# the two species morphological spaces
plot(set1[[1:2]])

# Intersection
plot(set1[[1:3]])

# Combined hypervolumes
plot(set1[[c(1:2, 4)]])

# Get volume of overlap and unique volume for each space.
get_volume(set1) 

# This could applied to any taxonomic group for any set of continuous variables.
# These methods have been used to measure intraspecific variation
# in functional traits and community functional diversity.


# Exercise 2: Species distribution modelling with the white oak

# The aim of SDMs is to combine environmental variables and occurrence records.

# Using the values of the environmental variables for each of the occurrence records, 
# you can predict where species are likely to occur given the environmental variables 
# of areas where they have not currently been recorded.

# Load dataset
data('quercus')
str(quercus)

# Subset species and columns
data_alba = subset(quercus,
                     Species=="Quercus alba")[,c("Longitude","Latitude")]

# Take a sample from the dataset 
data_alba = data_alba[sample(1:nrow(data_alba),500),]
plot(data_alba)
  
# Get worldclim data from internet (environmental variables)
require(maps)
require(raster)
climatelayers = getData('worldclim',
                        var='bio', res=10, path=tempdir())

# Lets take a look at these:
plot(climatelayers)

# We aren't interested in all of these:
climatelayers_ss = climatelayers[[c(1,12)]]

# z-transform climate layers to make axes comparable

# Getting Z-scores puts variables onto a similar scale for comparability.
# Expresses values as units of standard deviation away from the mean

# For-loops are handy for tidy code
for (i in 1:nlayers(climatelayers_ss)) {
   climatelayers_ss[[i]] <- (climatelayers_ss[[i]] - cellStats(climatelayers_ss[[i]], 'mean')) / cellStats(climatelayers_ss[[i]], 'sd')
   }

# Crop the extent 
climatelayers_ss = crop(climatelayers_ss,
                        extent(-150,-50,15,60))
plot(climatelayers_ss)
 
# extract transformed climate values for each occurrence record
climate_alba = raster::extract(climatelayers_ss,
                               data_alba[1:300,])
 
 # Compute the hypervolume
 hv_alba <- hypervolume_gaussian(climate_alba)
 
 # Now we have the hypervolume, you can project the values within the hypervolume onto the 
 # original raster variables.
 #raster_alba_projected_accurate <- hypervolume_project(hv_alba, rasters=climatelayers_ss)
 #writeRaster(x = raster_alba_projected_accurate,
#             filename = "./Projected alba.tif")
 raster_alba_projected_accurate <- raster("./Projected alba.tif")
 plot(raster_alba_projected_accurate,xlim=c(-150,-60),ylim=c(25,55))
 
 # We can also do this a faster way for ins and outs...
 raster_alba_projected_fast = hypervolume_project(hv_alba,
                                                  rasters=climatelayers_ss,
                                                  type='inclusion', fast.or.accurate='fast')
 plot(raster_alba_projected_fast,xlim=c(-150,-60),ylim=c(25,55))
 
 # We can plot this using ggplot
 gg.raster.data <- data.frame(x = raster::xyFromCell(raster_alba_projected_accurate, cell = 1:length(raster_alba_projected_accurate))[,1],
                              y = raster::xyFromCell(raster_alba_projected_accurate, cell = 1:length(raster_alba_projected_accurate))[,2],
                              value = getValues(raster_alba_projected_accurate))
 
 gg.raster.data[is.na(gg.raster.data$value),] <- 0
 gg.raster.data[gg.raster.data$value == 0,] <- NA
 
# Get the shape of North America
 north_america <- map_data("world", region = c("USA", "Canada"))
 
 # Plot the raster file
 ggplot() +
   geom_map(map = north_america, data = north_america,
            aes(long, lat, map_id = region), 
            color = "black", fill = "lightgrey", size = 0.3) +
   geom_tile(data = gg.raster.data, aes(x = x, y = y, fill = value)) +
   theme_classic() +
   scale_fill_viridis() +
   labs(x = "Longitude", y = "Latitude") +
   lims(x = c(-150, -50), y = c(20, 55)) +
   theme(panel.grid = element_blank(),
         legend.position = c(0.1,0.3))
 
 # Add species occurrence records to the plot
 ggplot() +
   geom_map(map = north_america, data = north_america,
            aes(long, lat, map_id = region), 
            color = "black", fill = "lightgrey", size = 0.3) +
   geom_tile(data = gg.raster.data, aes(x = x, y = y, fill = value)) +
   geom_point(data = data_alba, aes(x = Longitude, y = Latitude), pch = 21, fill = "red", alpha = 0.2, size = 3) +
   theme_classic() +
   scale_fill_viridis() +
   labs(x = "Longitude", y = "Latitude") +
   lims(x = c(-150, -50), y = c(20, 55)) +
   theme(panel.grid = element_blank(),
         legend.position = c(0.1,0.3))
 
  
# Hypervolume hole detection

# The benefit of using hypervolumes is that there could be holes in species'
# ecological niches

# Create an extreme example for demonstration 
data_annulus <- data.frame(matrix(data=runif(4000),ncol=2))
names(data_annulus) <- c("x","y")
data_annulus <- subset(data_annulus, sqrt((x-0.5)^2+(y-0.5)^2) > 0.4 & sqrt((x-0.5)^2+(y-0.5)^2) < 0.5)

# MAKE HYPERVOLUME (low reps for fast execution)
hv_annulus <- hypervolume_gaussian(data_annulus, kde.bandwidth=0.05,name='annulus',samples.per.point=1)

# GET CONVEX EXPECTATION
hv_convex <- expectation_convex(hypervolume_thin(hv_annulus, num.points=500), check.memory=FALSE,use.random=TRUE)
plot(hv_convex)

# DETECT HOLES (low npoints for fast execution)
features_annulus <- hypervolume_holes( hv.obs=hv_annulus,
                                       hv.exp=hv_convex, set.check.memory=FALSE)

# CLEAN UP RESULTS
features_segmented <- hypervolume_segment(features_annulus, check.memory=FALSE,distance.factor=2)
features_segmented_pruned <- hypervolume_prune(features_segmented, volume.min=0.02)

# PLOT RETAINED HOLE(S)
plot(hypervolume_join(hv_annulus, features_segmented_pruned))
















