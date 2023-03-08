library(sf)
library(tidyverse)
library("missForest")
library(naniar)

# files architecture
#
# data
#   - world-administrative-boundaries.geojson
#   - explanatory.RData
# doc
#   - .md file
# logic
#   - .R file

setwd(dir = "K:/Documents/spatial")

# 1.2 Contours data
# Import the data
bound_data <- read_sf("data/world-administrative-boundaries.geojson")

st_crs(bound_data)

# Plot geometry
plot(st_geometry(bound_data))

# Transform geometry to ESRI:54030 Robinson
transform_bound <- st_transform(bound_data, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")

# Plot new geometry

plot(st_geometry(bound_data), main="CRS ESRI:54030 Robinson")

# 1.3 Explanatory variables at country level
load("data/explanatory.Rdata")

nom_vars <- c("deflactor", "lifeexp", "dummyEarthquake", "population",
              "dummyStorm", "GDPpercapita_UN", "FD", "conflictpercapita", "politicalstability", 
              "landlocked", "vulnerability")

my_X <- my_X[,nom_vars]
# Potential issues of missing data
my_X[, nom_vars] <- missForest(my_X[, nom_vars])$ximp

# Visualize missing value
vis_miss(my_X)

# Count missing value
sum(is.na(my_X))


# Propose an additional explanatory variable : altitude/mountain country

# 1.4 Explanatory variables at country-pair level
load("data/pairs.Rdata")

# 2 Emigration and immigration rates by country
migration_data <- read.csv(file = "data/bilat_mig.csv", header = TRUE)


