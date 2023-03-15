library(sf)
library(tidyverse)
library("missForest")
library(visdat)
library(ggplot2)
library(cartography)
library(spdep)

# files architecture
#
# data
#   - world-administrative-boundaries.geojson
#   - explanatory.RData
# doc
#   - .md file
# logic
#   - .R file

setwd("~/spatial_econometrics-dev")

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
load("data/explanatory.RData")

nom_vars <- c("CountryCode", "deflactor", "lifeexp", "dummyEarthquake", "population",
              "dummyStorm", "GDPpercapita_UN", "FD", "conflictpercapita", "politicalstability", 
              "landlocked", "vulnerability")

my_X_clean <- my_X[,nom_vars]
# Potential issues of missing data
my_X_clean[, nom_vars[2:length(nom_vars)]] <- missForest(my_X_clean[, nom_vars[2:length(nom_vars)]])$ximp

# Visualize missing value
vis_miss(my_X_clean)

# Count missing value
sum(is.na(my_X_clean))


# Propose an additional explanatory variable : altitude/mountain country

# 1.4 Explanatory variables at country-pair level
load("data/pairs.Rdata")

# 2 Emigration and immigration rates by country
#Q1)

migration_data <- read.csv(file = "data/bilat_mig.csv", header = TRUE)

#data cleaning
migration_data_rem <- migration_data[, c("year0", "orig", "dest", "da_min_open")]
migration_data_rem <-migration_data_rem %>% filter(year0 == '2015')

#by origin = total in flow
by_orig <- aggregate(migration_data_rem$da_min_open, by = list(Origin = migration_data_rem$dest), FUN = sum)
by_orig <- rename(by_orig, "Total_in" = x, "CountryCode" = Origin)

#by destination = total out flow
by_dest <- aggregate(migration_data_rem$da_min_open, by = list(Destination = migration_data_rem$orig), FUN = sum)
by_dest <- rename(by_dest, "Total_out" = x, "CountryCode" = Destination)

total_flows <- merge(by_dest, by_orig, by= "CountryCode")

#Q2) 
unique(total_flows$CountryCode)
length(unique(clean_bound$iso3))
unique(my_X_clean$CountryCode)
setdiff(unique(clean_bound$iso3), unique(exp_data$CountryCode))


total_flows$CountryCode[total_flows$CountryCode %in% c("AIA", "FLK", "GIB", "MSR", "SHN", "VGB")] <- "GBR"
total_flows$CountryCode[total_flows$CountryCode %in% c("GLP", "GUF", "MTQ", "MYT", "REU", "SPM", "WLF")] <- "FRA"
total_flows$CountryCode[total_flows$CountryCode %in% c("COK", "NIU", "TKL")] <- "NZL"
total_flows$CountryCode[total_flows$CountryCode == "ESH"] <- "MAR"

total_flows <- total_flows %>% group_by(CountryCode) %>% 
  summarise(sum_in = sum(Total_in), sum_out = sum(Total_out), .groups = "drop") %>% 
  as.data.frame()

total_flows <- total_flows[!(total_flows$CountryCode == "NRU"),]
my_X_clean <- my_X_clean[!(my_X_clean$CountryCode %in% c("MAF", "XKX", "CHI", "CUW","SXM", "IMN")),]

exp_data <- merge(total_flows, my_X_clean, by = "CountryCode")

sapply(bound_data, class)

clean_bound <- bound_data %>% drop_na()
length(unique(clean_bound$iso3))


clean_bound$iso3[clean_bound$iso3 %in% c("AIA", "FLK", "GIB", "MSR", "SHN", "VGB", "IOT", "PCN")] <- "GBR"
clean_bound$iso3[clean_bound$iso3 %in% c("GLP", "GUF", "MTQ", "MYT", "REU", "SPM", "WLF")] <- "FRA"
clean_bound$iso3[clean_bound$iso3 %in% c("COK", "NIU", "TKL")] <- "NZL"
clean_bound$iso3[clean_bound$iso3 == "ESH"] <- "MAR"
clean_bound$iso3[clean_bound$iso3 == "TWN"] <- "CHN"
clean_bound$iso3[clean_bound$iso3 == "NFK"] <- "AUS"

clean_bound <- clean_bound[!(clean_bound$iso3 %in% c("VAT", "CCK","SJM", "NRU", "CXR")),]

length(unique(clean_bound$iso3))

exp_data <- rename(exp_data, "iso3" = CountryCode)

full_data <- merge(exp_data, clean_bound, by = "iso3")

length(unique(full_data$iso3))

full_data$emigrates <- full_data$sum_out/full_data$population
full_data$immigrates <- full_data$sum_in/full_data$population


rm(bound_data, by_dest, by_orig, clean_bound, exp_data, migration_data, migration_data_rem, my_X, my_X_clean, pairs, total_flows, transform_bound, nom_vars)

save.image(file = "data/full_data.RData")
