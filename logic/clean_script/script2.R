library(sf)
library(tidyverse)
library("missForest")
library(visdat)
library(ggplot2)
library(cartography)
library(spdep)
library(RColorBrewer)

getwd()
setwd('~/spatial_econometrics-dev')
load("data/full_data.RData")

cor(full_data[, 4:14], full_data$emigrates)
cor(full_data[, 4:14], full_data$immigrates)

# Yes we see sign differences with the variables vulnerability, gdppercapita, deflactor and conflicts. This makes sense since there is an 
#inverse relationship between these variables. 


full_data %>% 
  ggplot( aes(x=emigrates)) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white')


my_colors <- brewer.pal(9, "Greens") 
my_colors <- colorRampPalette(my_colors)(50)

class_of_country <- cut(full_data$emigrates, 7)
my_colors <- my_colors[as.numeric(class_of_country)]
plot(st_geometry(full_data$geometry), col=my_colors)


class_of_country <- cut(full_data$immigrates, 7)
my_colors <- my_colors[as.numeric(class_of_country)]
plot(st_geometry(full_data$geometry), col=my_colors)


#Plot

ggplot() +
  geom_polygon(data = full_data, aes(fill = emigrates, x = lon, y = lat, group = iso3)) +
  theme_void() +
  coord_map()



sort(full_data$iso3)

unique(full_data$iso3)

#Weight matrix
plot(st_geometry(full_data$geometry))

bound_geo <- st_geometry(full_data$geometry)
bound_nb <- poly2nb(bound_geo)
bound_cen <- st_coordinates(st_centroid(bound_geo))
plot(bound_geo, lwd = 1)
par(new = TRUE)
plot(bound_nb, bound_cen, add = T, col = "dark green", lty = "dotted", lwd = 2)

bound_4nnb <- knn2nb(knearneigh(bound_cen, 4))

plot(bound_geo, lwd = 1)
par(new = TRUE)
plot(bound_4nnb, bound_cen, add = T, col = "dark green", lty = "solid", lwd = 1)
title("K = 4")
