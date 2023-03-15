# Spatial Econometrics

## Files architecture

```
spatial-project
│   README.md
│
└───data
      └── world-administrative-boundaries.geojson
      └── full_data.RData
      └── explanatory.RData
      └── pairs.RData
      └── bilat_mig.csv
└───doc
      └── report.md
└───logic
      └── script1.R
      └── script2.R
      └── script3.R
```

The objective of the project is to analyse the migration flows and see how spatial models could be useful for this task.

It will be spread out into 3 parts, each contained in a associated R script :
 - Data description and processing (in script 1)
 - Unilateral migration analysis (by country) (in script 2)
 - Bilateram migration analysis (by pair of countries) (in script 3)


## Table of contents
1. [Data description (scrip1.R)](#data_description)
2. [Unilateral analysis (script2.R)](#uni_analysis)
2. [Bilateral analysis (script3.R)](#bi_analysis)


## 1. Data description <a name="data_description"></a>

In a first place, we need to select an estimate of the migrant flows (among the ones described in Abel and Cohen (2019)). We choose the **open demographic accounting system** since it **insert explanation**.

We import boundaries data, reflecting the countries' boundaries in the world found at https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/

    bound_data <- read_sf("data/world-administrative-boundaries.geojson")

We then transform the Coordinate Reference System with the **ESRI:54030**'s one.

    transform_bound <- st_transform(bound_data, "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ")

**insert plot of the world map**

Les données sont deux types :

- données propres à chaque pays (e.g. PIB, données économiques etc.)

- données "de paire", représentant certaines relations/données propres à ces deux pays (e.g. même langue, même monnaie etc.)

We consider several explanatory variables reflecting information about the countries and possibly useful for the migrant flows analysis. Among them, we have for instance *lifeexp*, *dummyEarthquake*, *population*, *dummyStorm*, *GDPpercapita_UN*, *political stability*

We propose a variable of type mountanous environement. More globally, a variable that represent the ease of living in the environment.

**need to propose data source/justification to justify and import this variable**


## 2. Unilateral analysis <a name="uni_analysis"></a>

```
nom_vars <- c("CountryCode", "deflactor", "lifeexp", "dummyEarthquake", "population",
              "dummyStorm", "GDPpercapita_UN", "FD", "conflictpercapita", "politicalstability", 
              "landlocked", "vulnerability")
my_X_clean <- my_X[,nom_vars]
```

```
migration_data <- read.csv(file = "data/bilat_mig.csv", header = TRUE)
migration_data_rem <- migration_data[, c("year0", "orig", "dest", "da_min_open")]
migration_data_rem <-migration_data_rem %>% filter(year0 == '2015')
```

```
by_orig <- aggregate(migration_data_rem$da_min_open, by = list(Origin = migration_data_rem$dest), FUN = sum)
by_orig <- rename(by_orig, "Total_in" = x, "CountryCode" = Origin)
```

```
by_dest <- aggregate(migration_data_rem$da_min_open, by = list(Destination = migration_data_rem$orig), FUN = sum)
by_dest <- rename(by_dest, "Total_out" = x, "CountryCode" = Destination)
```

```
total_flows <- merge(by_dest, by_orig, by= "CountryCode")
```

```
exp_data <- merge(total_flows, my_X_clean, by = "CountryCode")
```

```
full_data$emigrates <- full_data$sum_out/full_data$population
full_data$immigrates <- full_data$sum_in/full_data$population
```

```
save.image(file = "data/full_data.RData")
``` 






## 3. Bilateral analysis <a name="bi_analysis"></a>


