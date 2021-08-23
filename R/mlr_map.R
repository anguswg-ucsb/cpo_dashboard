library(tidyverse)

# district shapefile path
shp_path = "water_districts_simple.geojson"

# load shapefiles as spatial polygon object
shp <- sf::read_sf(paste0(shp_path), quiet = TRUE) %>%
  filter(DISTRICT %in% distr_num) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

# Tidied MLR results by district
tidy_mlr <- readRDS("tidy_mlr.rds")

# Join MLR results w/ district shapefile
mlr_shp <- left_join(shp, tidy_mlr, by = c("DISTRICT" = "district"))

pal <- colorNumeric("YlOrRd", domain = mlr_shp$variance_rsq, n = 21)
# pal2 <- colorNumeric(c("darkred", "brown1", "yellow"), domain = centroids$af_total)
# Leaflet map
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
  addPolygons(
    data = mlr_shp,
    color = "black",
    fillOpacity = 0.8,
    fillColor = ~pal(variance_rsq),
    weight = 2,
    label = ~DISTRICT,
    labelOptions = labelOptions(
      noHide = F,
      # direction = 'center',
      # textOnly = F)
      style = list(
        "color" = "black",
        "font-weight" = "1000")
    )
  ) %>%
  addLegend(
    data = mlr_shp,
    "bottomright",
    pal = pal,
    values = ~variance_rsq,
    title = "Variance x R2",
    labFormat = labelFormat(digits = 10,),
    opacity = 1
  ) %>%
  addScaleBar("bottomleft") %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "feet",
    primaryAreaUnit = "sqmiles",
    activeColor = "red",
    completedColor = "green" ) %>%
  leafem::addMouseCoordinates()
# addLabelOnlyMarkers(
#      data = centers,
#      label = ~DISTRICT,
#      labelOptions = labelOptions(
#          noHide = TRUE,
#          direction = 'center',
#          textOnly = T,
#          style = list(
#                    "color" = "black",
#                    "font-weight" = "1000",
#                    "font-size" = "9px",
#                    "border" = "1.5px solid black",
#                    "background-color"="LightCyan",
#                    "border-color" = "rgba(0,0,0,0.9)"
#             )
#          )
#      ) %>%
# addLegend(
#   data = centroids,
#   "bottomright",
#   pal = pal,
#   values = ~af_total,
#   title = "Coefficient",
#   labFormat = labelFormat(digits = 10,),
#   opacity = 1
# )
