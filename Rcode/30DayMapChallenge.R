
  library(sf)
  library(tidyverse)
  
# Day 3: World map ----

## Download from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data
## Countries 2016_1:60 Million
  
  World_URL <- "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-60m.shp.zip"
  dir.create("Rdata")
  download.file(World_URL, destfile = "Rdata/World.zip")
  unzip(zipfile = "Rdata/World.zip", exdir   = "Rdata/World")
  unzip(zipfile = "Rdata/World/CNTR_RG_60M_2016_4326.shp.zip",
        exdir   = "Rdata/World_SHP")
  World <- read_sf("Rdata/World_SHP/CNTR_RG_60M_2016_4326.shp")
  unlink("Rdata", recursive = TRUE)
  
  ggplot() +
    geom_sf(data = World, aes(fill = CNTR_ID)) +
    labs(title = "Administrative boundaries at country level of the world",
         subtitle = "Scale 1:60 Million",
         caption = "© EuroGeographics for the administrative boundaries") +
    guides(fill = FALSE)
  
  dir.create("Rresults")
  ggsave("Rresults/Day3_World.png", width = 25, height = 15, units = "cm")
  
  
  
  