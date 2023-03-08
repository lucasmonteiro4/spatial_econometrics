# Spatial Econometrics

## 1 Data description

Les données sont deux types :

- données propres à chaque pays (e.g. PIB, données économiques etc.)

- données "de pair", représentant certaines relations/données propres à ces deux pays (e.g. même langue, même monnaie etc.) 

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









## Unilateral Emigration/Immigration analysis

Modèle OLM

Tester autocorrélation dans les résidus avec test de Moran

Si oui, considérer modèle spatial

## Bilateral Emigration/Immigration analysis