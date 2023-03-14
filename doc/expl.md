# Spatial Econometrics

The objective of the project is to analyse the migration flows and see how spatial models could be useful for this task.

It will be spread out into 3 parts :
 - Data description and processing
 - Unilateral migration analysis (by country)
 - Bilateram migration analysis (by pair of countries)


## Table of contents
1. [Data description](#data_description)
2. [Unilateral analysis](#uni_analysis)
2. [Bilateral analysis](#bi_analysis)


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

Q Propose at least one additional variable that could explain the emigration/immigration rate by country. If possible, give a link to the data source or a reference to an article that uses this information in the context of migration. Import this variable and merge it to the existing explanatory variables.

We propose a variable of type mountanous environement. More globally, a variable that represent the ease of living in the environment.

**-> need to propose data source/justification to justify and import this variable**

## 2. Unilateral analysis <a name="uni_analysis"></a>

## 3. Bilateral analysis <a name="bi_analysis"></a>




 

voir cartes fin de la page de Spatial Econometrics

### 1.1 Migration flow data
**choose one among the six estimates of the migrant flows described in Abel and Cohen (2019) and Berlemann et al. (2021), and to briefly describe the method chosen**

variable à expliquer sélectionnée : *open demographic accounting system*

*Given that not all countries collect data on immigrants’ place of birth, the number of countries of origin is generally larger than the number of countries of residence. Thus, when applying the Demographic Accounting approach, a decision must be made on how to deal with this type of asymmetry*

*Abel (2013) proposes an open demographic accounting system, which allows for moves to or from countries for which bilateral data is available in migrant stock tables*

    da_min_open

### 1.2 Contours data

**Import the data and transform the Coordinate Reference System using the following one**

https://mgimond.github.io/Spatial/coordinate-systems-in-r.html

    data_bound <- read_sf(dsn = "data/world-administrative-boundaries.shp")



# Q1.3 Propose an additional explanatory variable

-> mountains/altitude : see harsh environments for instance





## Unilateral Emigration/Immigration analysis

Modèle OLM

Tester autocorrélation dans les résidus avec test de Moran

Si oui, considérer modèle spatial

## Bilateral Emigration/Immigration analysis






