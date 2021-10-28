
pal_fact <- colorFactor(
  c("red3", "dodgerblue3"),
  # topo.colors(5),
  domain = shp$BASIN)
# district shapefile path
shp_path = "water_districts_simple.geojson"
path2 <- "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
# load shapefiles as spatial polygon object
shp_all <- sf::read_sf(paste0(shp_path), quiet = TRUE) %>%
  # filter(DISTRICT %in% distr_num) %>%
  filter(BASIN %in% c("Colorado", "South Platte")) %>%
  filter(!DISTRICT %in% distr_num) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

shp <- sf::read_sf(paste0(shp_path), quiet = TRUE) %>%
  filter(DISTRICT %in% distr_num) %>%
  # filter(BASIN %in% c("Colorado", "South Platte")) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

basin_outline <- sf::read_sf(paste0(path2), quiet = TRUE) %>%
  # filter(DISTRICT %in% distr_num) %>%
  filter(BASIN %in% c("Colorado", "South Platte")) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  group_by(BASIN) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_simplify(dTolerance = 50) %>%
  st_cast("MULTILINESTRING")
saveRDS(basin_outline, "basin_outline2.rds")
mapview::npts(basin_outline)
mapview::mapview(basin_outline)

co <- USAboundaries::us_states() %>%
  filter(name == "Colorado") %>%
  st_cast("MULTILINESTRING")
mapview::mapview(shp)

centr <- shp %>%
  # st_cast("MULTIPOLYGON") %>% () %>%
  st_as_sf() %>%
  st_union() %>%
  st_centroid()
mapview::mapview(centr) + shp
centr
leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
  addPolygons(
        data      = shp_all,
        fillColor = 'white', fillOpacity = 0.5,
        col       = "black", opacity     = 1,
        weight    = 2,       label       = ~DISTRICT,
        labelOptions =
          labelOptions(
            noHide = F,
            style = list( "color" = "black", "font-weight" = "1000"))
      ) %>%
  addPolygons(
    data = shp,
    fillColor  = ~pal_fact(BASIN), fillOpacity = 0.5,
    col        = "black",          opacity     = 1,
    weight     = 2,                label       = ~DISTRICT,
    labelOptions =
      labelOptions(
        noHide = F,
        style = list( "color" = "black", "font-weight" = "1000") )
  )  %>%
  addPolylines(
    data = rivers,
    col = "cyan", weight = 4,
    opacity = 1,label = ~river
  ) %>%
addPolylines(
    data = basin_outline[1,],
    # col = "darkred",
    col = "black",
    weight = 6,
    opacity = 1
) %>%
addPolylines(
    data = basin_outline[2,],
    # col = "darkblue",
     col = "black",
    weight = 6,
    opacity = 1
  ) %>%
addPolylines(
    data = co,
    col = "black",
    weight = 6, opacity = 1,
  )




# --------------------
# ----- MLR MAP ------
# --------------------

pal <- colorNumeric("YlOrRd", domain = shp$variance_rsq, n = 21)
# Leaflet map
leaflet() %>%
  # addProviderTiles(providers$Esri.DeLorme, group = "ESRI DeLorme") %>%
  # addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic") %>%
  # addLayersControl(options = layersControlOptions(collapsed = FALSE),baseGroups = c("Nat Geo Topographic", 'Topographic', "Imagery") # ) %>%
  addPolygons(
    data = shp,
    color = "black",
    opacity = 1,
    fillOpacity = 0.7,
    fillColor = ~pal(variance_rsq),
    weight = 2.5,
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
  addPolylines(
    data = co,
    # col = "green",
    col = "black",
    weight = 6,
    opacity = 1,
  ) %>%
  addLegend(
    data = shp,
    "bottomright",
    pal = pal,
    values = ~variance_rsq,
    title = "Climate sensitivity",
    labFormat = labelFormat(digits = 10,),
    opacity = 1
  ) %>%
  addScaleBar("bottomleft") %>%
  # addMeasure(  position = "bottomleft",   primaryLengthUnit = "feet",  primaryAreaUnit = "sqmiles", activeColor = "red",  completedColor = "green" ) %>%
  leafem::addMouseCoordinates() %>%
  leaflet::setView(lng = -105.6, lat = 39.7, zoom = 6)












