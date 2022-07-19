remove(list = ls())

library(tidyverse)

# # Water rights node points
# node_pts    <- readRDS("shortage_by_right_pts.rds")

# # annual shortages by right
ditch_data <- readRDS("shortages_by_right_rank_year.rds")
# ditch_data  <- readRDS("shortages_by_right_year.rds")
# 2302911

# admin number to appropriation date look up table
admin_dates <- readRDS("admin_dates.rds") %>%
  mutate(date = as.character(date))

# clean ditch dataset for structure information table
admins <- ditch_data %>%
  # group_by(admin) %>%
  group_by(admin, id) %>%
  summarize(
    node_id     = node_id,
    id          = id,
    name        = name,
    admin_rank  = admin_rank

  ) %>%
  group_by(admin, id) %>%
  # group_by(admin) %>%
  slice(n = 1) %>%
  mutate(
    admin_number  = round(as.numeric(admin), 0),
    admin         = as.numeric(admin)
  ) %>%
  dplyr::select(name, node_id, id, admin, admin_number, admin_rank) %>%
  ungroup() %>%
  left_join(
    admin_dates,
    by = "admin_number"
    ) %>%
  dplyr::select(name, node_id, id, admin, admin_rank, date)

length(unique(admins$id))
length(unique(ditch_data$id))

# save clean data for structure information table
saveRDS(admins, "node_id_table.rds")

system.time(
node_split_demand <- ditch_data %>%
  # filter(node_id == node()) %>%
  # filter(node_id == "0700699") %>%
  # filter(node_id == "0200809") %>%
  mutate(admin = as.numeric(admin)) %>%      # mutate(admin = round(as.numeric(admin), 0)) %>%
  group_by(node_id, admin) %>%
  mutate(x = year, y = round(demand, 1)) %>% # mutate(x = year, y = round(demand_dir, 1)) %>%
  dplyr::select(x, y, admin, id) %>%
  group_split() %>%
  as.list()
)


# lapply(node_split_demand, function(x) {
#
# }
# initialize highchart
right_demand_plot <- plot_right_demand(
  df_list = node_split_demand
)
# values 0 - 100%
vect = seq(0, 100, 10)

# binned colors of mean annual direct flow shortage as % of demand
binpal <- colorBin(
  "Spectral",
  domain  = vect,
  n       = 4,
  reverse = T,
  pretty  = T
)
bb <- shp %>%
  filter(DISTRICT == 43) %>%
  # filter(DISTRICT == node_district()) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(4326) %>%
  st_as_sf()

# Fly to bounds on Node map
bounds <- st_bbox(bb) %>%
  st_as_sfc() %>%
  # st_buffer(0.009) %>%
  st_bbox() %>%
  as.vector()

node_marker <- node_pts %>%
  # filter(district == node_district())
filter(district == 43)

node_type_label     <- c("Agricultural", "Municipal")
node_type_pal       <- colorFactor(
                                c("red", "dodgerblue"),
                                domain = node_type_label,
                                reverse = F
                              )

%>%
  addLegend(
    pal       = node_type_pal,
    position  = "topleft",
    values    = node_type_label,
    title     = "Node Type",
    group     = "Nodes",  layerId   = "node_id")
addRasterImage(
  oyster_harvest,
  project   = T,
  colors    = c("red", "yellow", "green"),
  opacity   = 0.3,
  group     = "Oyster harvest areas",
  layerId   = "Oyster harvest areas") %>%
  addLegend(
    pal       = oyster_harvest_pal,
    position  = "topleft",
    values    = oyster_harvest_labels,
    title     = "Oyster harvest areas",
    group     = "Oyster harvest areas",  layerId   = "Oyster harvest areas") %>%


# leafletProxy("districtMap") %>%
#   clearMarkers() %>%
#   clearShapes() %>%
#   addMarkers(data = pt) %>%
  library(leaflet)
leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(
    # data         = filter(shp, DISTRICT != node_district()),
    # data         = filter(shp, DISTRICT != 7),
    data         = shp,
    color        = "black",
    opacity      = 1,
    fillOpacity  = 0.7,
    fillColor    = ~binpal(mean_short_dir),
    # fillColor    = ~pal(var_sensitivity_norm),
    weight       = 2,
    label        = ~paste0("District: ", DISTRICT),
    layerId      = ~DISTRICT,
    labelOptions = labelOptions(
      noHide = F,
      # direction = 'center',
      # textOnly = F)
      style = list(
        "color"       = "black",
        "font-weight" = "1000")
    )
  ) %>%
addCircleMarkers(
  data        = node_marker,
  # radius      = ~((2 + size)*5),
  radius      = ~(size_sq),
  # radius      = ~(5 + size*6),
  color       = "black",
  fillColor   = node_marker$color,
  fillOpacity = 0.7,
  opacity     = 1,
  weight      = 3,
  stroke      = TRUE,
  group       = "Nodes",
  layerId     = ~node_id,
  label       = ~paste0("Node ID: ", node_id),
  labelOptions = labelOptions(
    noHide = F,
    style = list(
      "color" = "black",
      "font-weight" = "1000")
  )) %>%
  addLegend(
    pal       = node_type_pal,
    position  = "topleft",
    values    = node_type_label,
    title     = "Node Type",
    group     = "Nodes",  layerId   = "node_id")

  # flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])

reactable::reactable(
  admins,
  style    = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px", fontWeight = 600),
  columns  = list(
    name       = colDef(name = "Structure Name",
                        align = "center"
    ),
    node_id    = colDef(name = "WDID",
                        align = "center",
                        cell = function(value, index) {
                          node_url <- sprintf("https://dwr.state.co.us/Tools/Structures/%s", admins[index, "node_id"])
                          node_id  <- paste(admins[index, "node_id"])
                          tagList(
                            tags$a(class = "node_id", href = node_url, target = "_blank", node_id)
                            # span(class = "team-record", team_record)
                          )
                        }),
    id          = colDef(name = "ID", align = "center"),
    admin       = colDef(name = "Priority Admin No.", align = "center"),
    date        = colDef(name = "Appropriation Date", align = "center")),
  highlight = TRUE,
  outlined  = TRUE,
  bordered  = T,
  theme     = reactableTheme(
    borderColor = "#black",
    cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
    headerStyle = list(
      backgroundColor = "hsl(207, 16%, 80%)"
    ))
)


library(leaflet)
library(leaflegend)
data("quakes")
symbols <- makeSizeIcons(
  values = quakes$depth,
  shape = 'diamond',
  color = 'red',
  fillColor = 'red',
  opacity = .5,
  baseSize = 10
)



leaflet() %>%
  addTiles() %>%
  addMarkers(data = quakes,
             icon = symbols,
             lat = ~lat, lng = ~long) %>%
  addLegendSize(
    values = quakes$depth,
    color = 'red',
    fillColor = 'red',
    opacity = .5,
    title = 'Depth',
    shape = 'diamond',
    orientation = 'horizontal',
    breaks = 5)

symbols <- makeSizeIcons(
  values = log(node_marker$demand),
  shape = 'circle',
  color = 'red',
  fillColor = 'red',
  opacity = .5,
  baseSize = 1,
  breaks = 5
)

leaflet() %>%
  addPolygons(
    # data         = filter(shp, DISTRICT != node_district()),
    # data         = filter(shp, DISTRICT != 7),
    data         = shp,
    # data         = shp,
    color        = "black",
    opacity      = 1,
    fillOpacity  = 0.7,
    fillColor    = ~binpal(mean_short_dir),
    weight       = 2,
    label        = ~paste0("District: ", DISTRICT),
    layerId      = ~DISTRICT,
    labelOptions = labelOptions(
      noHide = F,
      # direction = 'center',
      # textOnly = F)
      style = list(
        "color"       = "black",
        "font-weight" = "1000")
    )
  ) %>%
  addCircleMarkers(
    data        = node_marker,
    radius      = ~(size_sq),
    color       = "black",
    fillColor   = node_marker$color,
    fillOpacity = 0.7,
    opacity     = 1,
    weight      = 3,
    stroke      = TRUE,
    layerId     = ~node_id,
    label       = ~paste0("Node ID: ", node_id),
    labelOptions = labelOptions(
      noHide = F,
      style = list(
        "color" = "black",
        "font-weight" = "1000")
    )) %>%
  addLegendSize(
    values = node_marker$demand,
    color = 'red',
    fillColor = 'red',
    opacity = .5,
    title = 'Depth',
    shape = 'circle',
    orientation = 'horizontal',
    breaks = 5)

