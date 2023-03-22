library(sf)
library(tidyverse)
library("missForest")
library(visdat)
library(ggplot2)
library(cartography)
library(spdep)
library(sfdep)
library(spatialreg)
library(stargazer)
library(gravity)
# files architecture
#
# data
#   - world-administrative-boundaries.geojson
#   - explanatory.RData
# doc
#   - .md file
# logic
#   - .R file

setwd(dir = "K:/Desktop/M2/S2/spatial_econometrics-main")

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

cor(full_data[, 4:14], full_data$emigrates)
cor(full_data[, 4:14], full_data$immigrates)
cor(full_data[, 4:14], full_data$lifeexp) #FD et GDPpercapita_UN
cor(full_data[, 4:14], full_data$dummystorm)



# Yes we see sign differences with the variables vulnerability, gdppercapita, deflactor and conflicts. This makes sense since there is an 
#inverse relationship between these variables. 

#Plot

ggplot() +
  geom_polygon(data = full_data, aes(fill = emigrates, x = as.numeric(substr(geo_point_2d, 10,20)), y = as.numeric(substr(geo_point_2d, 37,47)), group = iso3)) +
  theme_void() +
  coord_map()


full_data$geo_point_2d[1]
as.numeric(substr(full_data$geo_point_2d, 10,20))
as.numeric(substr(full_data$geo_point_2d, 37,47))

sort(full_data$iso3)

unique(full_data$iso3)

#Weight matrix
st_geometry(bound_data)
st_geometry(full_data)

bound_geo <- st_geometry(full_data$geometry)
bound_nb <- poly2nb(bound_geo)
bound_cen <- st_coordinates(st_centroid(bound_geo))
plot(bound_geo, lwd = 2)
plot(bound_nb, bound_cen, add = T, col = "dark green", lty = "dotted", lwd = 2)

bound_4nnb <- knn2nb(knearneigh(bound_cen, 4))

#old_par <- par(mfrow = c(1,2), oma = c(0, 0, 0, 0), mar = c(0, 0, 1, 0))
plot(bound_geo, lwd = 1)
par(new=TRUE)
plot(bound_4nnb, bound_cen,lwd=.2, col="blue", cex = .5)
title("K = 4")





# Moran scatter plot 

moran.plot(full_data$immigrates, nb2listw(bound_4nnb),X_name = "Immigrates")

# The middle east is very represented for countries with a high spatial autocorrelation 
# concerning immigrates flow followed by South Korea.


moran.plot(full_data$emigrates, nb2listw(bound_4nnb),X_name ="Emigrates")


# New Zealand and Great Britain appear to be the countries with a high 
# level of spatial autocorrelation concerning emigrates flow.


# Moran test

moran.test(full_data$immigrates, nb2listw(bound_4nnb)) # We reject the null hypothesis, 
#there is spatial autocorrelation at the level of 5%.


moran.test(full_data$emigrates, nb2listw(bound_4nnb)) # We also reject the null hypothesis, 
# there is spatial autocorrelation at the level of 5%.


# OLM model 

## First model

olm_1 <- lm(immigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
            GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
            vulnerability, data = full_data)
summary(olm_1)

# We see that a lot of variables are not significantly different from 0 at the level of 5%.
# The only variable which is is significant at this level is GDPpercapita_UN.

# Interpretation of the variable GDPpercapita_UN : if we increase the level of GDPpercapita_UN
# by one unit then the immigrates flow increase by 4.170e-07 unit.


## Second model

olm_2 <- lm(emigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
              GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
              vulnerability, data = full_data)
summary(olm_2)

# At the level of 5% only three variables are significantly different from 0 : deflactor, 
# lifeexp, FD and conflictpercapita_UN.


# Interpretation of the variable deflactor : if we increase the level of deflactor
# by one unit then the immigrates flow increase by 3.977e-0 unit.

# Interpretation of the variable FD : if we increase the level of FD
# by one unit then the immigrates flow increase by -3.731e-02 unit.

# Interpretation of the variable conflictpercapita : if we increase the level of conflictpercapita
# by one unit then the immigrates flow increase by 3.846e+0.

# We test the spatial autocorrelation in the residuals

lm.morantest(olm_1, nb2listw(bound_4nnb))

# There is the presence of spatial autocorrelation in the residuals.

#

mp <- moran.plot(residuals(olm_1), nb2listw(bound_4nnb), pch = 19)

index_class <- mp[, c("labels", "x", "wx")]

index_class$class <- ifelse((index_class$x > 0 & index_class$wx > 0), "HH",
                            ifelse((index_class$x < 0 & index_class$wx > 0), "LH",
                                   ifelse((index_class$x > 0 & index_class$wx < 0), "HL", "LL")))



# Location of the countries by HH, HL, LL, LH.

full_data$labels <- seq(1, 228, 1)
index_class$labels <- as.numeric(index_class$labels)
color_data <- merge(full_data, index_class, by = "labels")
color_data$class <- as.factor(color_data$class)
plot(st_geometry(color_data$geometry), lwd = 2, col = color_data$class)
legend("bottomleft", legend=c("HH", "HL", "LL", "LH"),
       fill=c("black", "red", "blue", "green"), cex=0.8)

# Testing strategy
# We have already test for spatial autocorrelation in OLM
# model so we test it against SLX model

# We see that only politicalstability is significant concerning the variable itself and the lag 
# but the SLX model seems more interesting given the previous Moran test and the presence of a lag 
# on politicalstability. We estimate a model with a lag on the dependent variable and the error term. 

# SLX model
slx_1 <- lm(immigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
              GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
              vulnerability, data = full_data)

slx_2 <- spatialreg::lmSLX(immigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
                 GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
                 vulnerability, data = full_data, 
               listw = nb2listw(bound_4nnb))
summary(slx_2)
# SEM model

full_data$pop_mod <- full_data$population / 1000000


sem <- spatialreg::errorsarlm(immigrates ~ deflactor + lifeexp + dummyEarthquake + pop_mod + dummyStorm +
                    GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
                    vulnerability, data = full_data, 
                  listw = nb2listw(bound_4nnb))
# Lag model
lagm <- spatialreg::lagsarlm(immigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
                   GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
                   vulnerability, data = full_data, 
                 listw = nb2listw(bound_4nnb))
# SDM model
durb <- spatialreg::lagsarlm(immigrates ~ deflactor + lifeexp + dummyEarthquake + population + dummyStorm +
                   GDPpercapita_UN + FD + conflictpercapita + politicalstability + landlocked +
                   vulnerability, data = full_data, 
                 listw = nb2listw(bound_4nnb), Durbin = T)





### PARTIE 3

## Daa preparation for the gravity model
new_data <- merge(migration_data, my_X, by.x = "orig", by.y = "CountryCode", suffixes = c("","_O"))
new_data <- merge(new_data, my_X, by.x = "dest", by.y = "CountryCode", suffixes = c("_O","_D"))

new_data_clean <- new_data[new_data$orig %in% full_data$iso3,]

new_data_clean <- new_data_clean[new_data_clean$dest %in% full_data$iso3,]

new_data_clean <- new_data_clean[, -c(3, 4, 5, 7, 8, 9)]

bound_data <- read_sf("data/world-administrative-boundaries.geojson") # besoin pour la boucle


N <- nrow(new_data_clean)
g <- numeric(N)
dist_mat <- st_distance(bound_data, bound_data, by_element = F)
dimnames(dist_mat) <- list(bound_data$iso3, bound_data$iso3)
new_data_clean$dist <- 0

for (k in 1:nrow(new_data_clean)) {
  new_data_clean[k, "dist"] <- dist_mat[new_data_clean[k, "dest"], new_data_clean[k, "orig"]]
}


save(new_data_clean, file = "new_data_clean.RData")
summary(new_data_clean)
## Gravity model estimation 

regressors_gm <- c("deflactor_O", "lifeexp_O", "GDPpercapita_UN_O", "population_O", 
                   "FD_O", "politicalstability_O", "landlocked_O", "dummyEarthquake_O", "dummyStorm_O",
                   "dummyFlood_O","Events_O", "Fatalities_O", "conflictpercapita_O", "vulnerability_O",
                   "deflactor_D", "lifeexp_D", "GDPpercapita_UN_D", "population_D", 
                   "FD_D", "politicalstability_D", "landlocked_D", "dummyEarthquake_D", "dummyStorm_D",
                   "dummyFlood_D", "Events_D", "Fatalities_D", "conflictpercapita_D", "vulnerability_D")
new_data_clean$dist <- as.numeric(new_data_clean$dist)

fit_gm <- ddm(
  dependent_variable = "da_min_open",
  distance = "dist",
  additional_regressors = regressors_gm,
  code_origin = "orig",
  code_destination = "dest",
  robust = FALSE,
  data = new_data_clean
)


summary(fit_gm)


fit_gm <- ppml(
  dependent_variable = "da_min_open",
  distance = "dist",
  additional_regressors = regressors_gm,
  data = new_data_clean
)

summary(fit_gm)

class(new_data_clean$dist)
