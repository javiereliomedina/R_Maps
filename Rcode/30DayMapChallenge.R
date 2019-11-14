
  
# Day 3: World map ----
## Download from https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data
## Countries 2016_1:60 Million
  
  library(sf)
  library(tidyverse)

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
  
  library(sf)
  library(tidyverse)
  
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
  
  
# Day 6: Blue ----
## European river catchments
  
  library(sf)
  library(tidyverse)
  
  River_URL <- "https://www.eea.europa.eu/data-and-maps/data/european-river-catchments-1/zipped-shapefile-vector-polygon/zipped-shapefile-vector-polygon/at_download/file.zip"
  dir.create("Rdata")
  download.file(River_URL, destfile = "Rdata/River.zip")
  unzip(zipfile = "Rdata/River.zip", exdir   = "Rdata/River")
  River <- read_sf("Rdata/River/ERC110108v2.shp")
  
  World_URL <- "https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/countries/download/ref-countries-2016-60m.shp.zip"
  dir.create("Rdata")
  download.file(World_URL, destfile = "Rdata/World.zip")
  unzip(zipfile = "Rdata/World.zip", exdir   = "Rdata/World")
  unzip(zipfile = "Rdata/World/CNTR_RG_60M_2016_4326.shp.zip",
        exdir   = "Rdata/World_SHP")
  World <- read_sf("Rdata/World_SHP/CNTR_RG_60M_2016_4326.shp") %>%
    st_transform(crs = st_crs(River))
  
  ggplot() +
    geom_sf(data = World, col = "#DEEBF7", fill = "#DEEBF7") +
    geom_sf(data = River, aes(fill = OCEAN_1)) +
    scale_fill_brewer(name = "Ocean", palette = "Blues") +
    coord_sf(xlim = c(2500000, 6500000), ylim = c(1500000, 5300000)) +
    labs(title = "European river catchments (ERC) classified by ocean",
         subtitle = "Scale 1:1 million",
         caption = "Source: European Environment Agency", color = "blue") +
    theme(axis.text = element_text(colour = "#6BAED6"),
          plot.title = element_text(colour = "#084594"),
          plot.subtitle = element_text(colour = "#2171B5"),
          plot.caption = element_text(colour = "#2171B5"),
          legend.title = element_text(color = "darkblue"),
          legend.text = element_text(color = "#084594"),
          panel.background = element_rect(fill = "#F7FBFF"),
          panel.grid.major = element_line(colour = "blue"))
  
  ggsave("Rresults/Day6_Blue.png", width = 8, height = 7)
  
  unlink("Rdata", recursive = TRUE)
  
# Day 13: Track ----
  
  library(sf)
  library(tidyverse)
  library(leaflet)
  
  dir.create("Rdata")
  Roads_250k_URL <- "http://data-osi.opendata.arcgis.com/datasets/1434c3b05da742cdb47e00040edc9dd5_24.zip"
  download.file(Roads_250k_URL, destfile = "Rdata/Roads_OSi_National_250k_Map_of_Ireland.zip")
  unzip(zipfile = "Rdata/Roads_OSi_National_250k_Map_of_Ireland.zip",
        exdir = "Rdata/Roads_OSi_National_250k_Map_of_Ireland")
  Roads_250k <- read_sf("Rdata/Roads_OSi_National_250k_Map_of_Ireland/Roads__OSi_National_250k_Map_of_Ireland.shp") %>%
    mutate(RTT = as.factor(RTT)) %>%
    mutate(RTT_Name = factor(recode(RTT,
                                    "-32768" = "Null",
                                    "984"    = "Local Route",
                                    "14"     = "Primary Route",
                                    "15"     = "Secondary Route",
                                    "16"     = "National Motorway"),
                             levels = c("Null",
                                        "Local Route",
                                        "Primary Route",
                                        "Secondary Route",
                                        "National Motorway")
    ) 
    )
  
  p_popup <- paste("<strong> OBJECTID: </strong>", Roads_250k$OBJECTID, "<br>",
                   "<strong> RTT: </strong>", Roads_250k$RTT_Name, "<br>",
                   "<strong> LEN: </strong>", round(Roads_250k$LEN, 2), "km")
  pal_fun <- colorFactor("YlOrRd", NULL, n = 4)
  Road_250k_map <- leaflet() %>%
    addPolylines(data = Roads_250k, 
                 stroke = TRUE,
                 weight = 2,
                 col = ~pal_fun(Roads_250k$RTT_Name),
                 fillOpacity = 0.5,
                 highlightOptions = highlightOptions(color = "blue",
                                                     weight = 3,
                                                     bringToFront = TRUE),
                 label = paste("OBJECTID:", Roads_250k$OBJECTID),
                 popup = p_popup) %>%
    addTiles() %>%
    addLegend(data = Roads_250k,
              pal = pal_fun,
              values = ~RTT_Name,
              opacity = 1,
              title = "Roads - OSi National 250k Map of Ireland")
  Road_250k_map
  
  # Save as HTML
  mapview::mapshot(Road_250k_map, url = "Rresults/Road_250k_map.html")
    
  # Save as png  
  mapview::mapshot(Road_250k_map, file = "Rresults/Day13_Road_250k_map.png")

  unlink("Rdata", recursive = TRUE)
  
# Day 14: Boundaries ----
## Browse source code at: https://github.com/riatelab/cartography 
## install.packages("cartography")  
  
  library(sf)
  library(cartography)
  
  # path to the geopackage file embedded in cartography
  path_to_gpkg <- system.file("gpkg/mtq.gpkg", package="cartography")
  # import to an sf object
  mtq <- st_read(dsn = path_to_gpkg, quiet = TRUE)
  # Compute the population density (inhab./km2) using sf::st_area()
  mtq$POPDENS <- as.numeric(1e6 * mtq$POP / st_area(mtq))
  # Get a SpatialLinesDataFrame of countries borders
  mtq.contig <- getBorders(mtq)
  
  # get the figure ratio
  sizes <- getFigDim(x = mtq, width = 1200, mar = c(0, 0, 1.2, 0))
  # save the maps in png format
  png(filename = "Rresults/Day14_Disc_Map.png",
      width = sizes[1],
      height = sizes[2],
      res = 200)
  
  # plot municipalities (only the backgroung color is plotted)
  plot(st_geometry(mtq), col = NA, border = NA, bg = "lightblue1", 
       xlim = c(690574, 745940))
  # Plot the population density with custom breaks
  choroLayer(x = mtq,
             var = "MED",
             breaks = c(min(mtq$MED), seq(13000, 21000, 2000), max(mtq$MED)),
             col = carto.pal("green.pal", 6),
             border = "white",
             lwd = 0.5, 
             legend.pos = "topright",
             legend.title.txt = "Median Income\n(euros)",
             add = TRUE)
  # Plot discontinuities
  discLayer(
    x = mtq.contig, 
    df = mtq, 
    var = "MED",
    type = "rel", 
    method = "geom", 
    nclass = 3,
    threshold = 0.4,
    sizemin = 0.7, 
    sizemax = 6, 
    col = "red4",
    legend.values.rnd = 1, 
    legend.title.txt = "Relative\nDiscontinuities", 
    legend.pos = "right",
    add = TRUE
  )
  # Layout
  layoutLayer(title = "Wealth Disparities in Martinique, 2015", 
              author =  paste0("cartography ", packageVersion("cartography")),
              sources = "Sources: Insee and IGN, 2018",
              frame = FALSE, scale = 5, tabtitle = TRUE,theme = "grey.pal")
  # north arrow
  north(pos = "topleft")
  
  dev.off()
  