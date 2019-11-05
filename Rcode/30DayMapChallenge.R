
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
  
# Day 4: Hexagons ----
## Make hexagons over a region (e.g.Iberian Peninsula)
  
  SP <- World %>%
    filter(NAME_ENGL == "Spain" | NAME_ENGL == "Portugal") %>%
    st_cast("POLYGON") 
  
  IB <- SP %>%
    mutate(Area = units::set_units(st_area(SP), km^2)) %>%
    filter(Area > units::set_units(4000, km^2)) %>%
    st_union()
  
  Grids <- st_make_grid(IB, cellsize = .2, what = "polygons", square = FALSE) %>%
    st_sf() 
  
  ggplot() +
    geom_sf(data = Grids, col = "red") +
    geom_sf(data = IB, alpha = 1, colour = "black", fill = NA, size = 0.5) +
    labs(title = "Hexagon tessellation of the Iberian Peninsula") 
  
  ggsave("Rresults/Day4_Hexagons_IB.png", width = 15, height = 15, units = "cm")  
  
  
# Day 5: Raster ----
## Radiometric data (Geological Survey, Ireland)
## Download from https://www.gsi.ie/en-ie/programmes-and-projects/tellus/Pages/Data-and-Maps.aspx
  
  dir.create("Rdata")
  Radiometric_URL <- "https://secure.dccae.gov.ie/GSI_DOWNLOAD/Geophysics/Data/GSI_Tellus_A5_RAD_GRIDS_2019.zip"
  download.file(Radiometric_URL, destfile = "Rdata/Radiometrics.zip")
  unzip(zipfile = "Rdata/Radiometrics.zip", exdir   = "Rdata/Radiometrics")
  Uranium <- rgdal::readGDAL("Rdata/Radiometrics/A5_RAD_GRIDS_2019/GXF/A5_RAD_Uranium_equivalent.gxf")
  # Color palette
  Col_Ramp <- readxl::read_xls("Rdata/Radiometrics/A5_RAD_GRIDS_2019/Readme_Files_And_Instructions/QGIS_Clra_32_Geosoft_hex_Colour_Ramp.xls",
                               sheet = 1,
                               col_names = FALSE,
                               skip = 4) %>%
    select(3) %>%
    as_vector() %>%
    as.character()
  
  breaks <- quantile(Uranium@data$band1, probs = seq(0, 1, 1/38), na.rm = TRUE)
  ggplot() + 
    inlabru::gg(data = Uranium) + 
    labs(title = "Airborne geophysical Gamma-Ray data",
         subtitle = "GSI - Tellus 2019: A5 block (Limerick)") +
    scale_fill_gradientn(name = "eU [ppm]",
                         colors = Col_Ramp,
                         values = scales::rescale(breaks),
                         limits = c(min(breaks), max(breaks))) +
    theme(legend.key.size = unit(1.5, "cm"),
          legend.key.width = unit(0.5,"cm")) +
    coord_fixed(ratio = 1)
  
  ggsave("Rresults/Day5_Raster_Tellus.png", height = 15, units = "cm")
  unlink("Rdata", recursive = TRUE)
  