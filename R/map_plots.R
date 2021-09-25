
pal_fact <- colorFactor(
  c("red3", "dodgerblue3"),
  # topo.colors(5),
  domain = shp$BASIN)
co <- USAboundaries::us_states() %>%
  filter(name == "Colorado") %>%
  st_cast("MULTILINESTRING")
mapview::mapview(co)

centr <- shp %>%
  # st_cast("MULTIPOLYGON") %>% () %>%
  st_as_sf() %>%
  st_union() %>%
  st_centroid()
mapview::mapview(centr) + shp
centr
leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
  # addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
  # addProviderTiles(providers$Esri.DeLorme, group = "ESRI DeLorme") %>%
  # addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  # addLayersControl(
  #   options = layersControlOptions(collapsed = FALSE),
  #   baseGroups = c("Nat Geo Topographic","Nat Geo Topographic2", 'Topographic', "Imagery")
  # ) %>%
  addPolygons(
    data = shp,
    # fillColor = 'grey',
    fillColor = ~pal_fact(BASIN),
    fillOpacity = 0.5,
    col = "black",
    opacity = 1,
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
  addPolylines(
    data = rivers,
    col = "cyan",
    # col = "dodgerblue",
    weight = 4,
    opacity = 1,
    label = ~river
  ) %>%
addPolylines(
    data = basins[1,],
    # col = "red",
    col = "black",
    weight = 5,
    opacity = 1,
    label = ~basin_clean,
) %>%
addPolylines(
    data = basins[2,],
    # col = "green",
    col = "black",
    weight = 5,
    opacity = 1,
    label = ~basin_clean,
  ) %>%
addPolylines(
    data = co,
    # col = "green",
    col = "black",
    weight = 6,
    opacity = 1,
  ) %>%
  leafem::addMouseCoordinates()




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












