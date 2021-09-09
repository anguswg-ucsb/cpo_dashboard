library(sf)
node_marker <- node_pts %>%
  # filter(district == 6) %>%
  mutate(size = abs(avg_dem - mean(avg_dem))/ sd(avg_dem))
  # filter(district == district_id$district)
node
leaflet() %>%
  # addPolygons(
  #   data = filter(shp, DISTRICT == 6),
  #   fillColor = 'white',
  #   fillOpacity = 0.4,
  #   col = "black",
  #   weight = 2,
  #   label = ~DISTRICT,
  #   labelOptions = labelOptions(
  #     noHide = F,
  #     # direction = 'center',
  #     # textOnly = F)
  #     style = list(
  #       "color" = "black",
  #       "font-weight" = "1000")
  #   )
  # ) %>%
  addCircleMarkers(
    data = node_marker,
    radius = ~(3.5 + size*4),
    # radius = 7,
    color = "black",
    fillColor ="red",
    fillOpacity = 0.7,
    weight = 3,
    layerId = ~node_id,
    stroke = TRUE)
sum_avg_dem <- ditch_data %>%
       group_by(node_id, year) %>%
       summarize(avg_dem = sum(demand_all)) %>%
       ungroup() %>%
       group_by(node_id) %>%
       summarize(avg_dem = mean(avg_dem))

node_pts <- left_join(node_pts, sum_avg_dem, by = "node_id")
saveRDS(node_pts, "node_pts2.rds")
# node points
cdss <- sf::read_sf("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/water_rights/CDSS/CDSS_Structures/Structure.shp")

# extract lat long from points
cdss <- cdss %>%
  st_transform(5070) %>%
  mutate(
    lat = st_coordinates(geometry)[,2],
    lng = st_coordinates(geometry)[,1]
  ) %>%
  st_drop_geometry()

# shortages by node and water right
ditch_shortages <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/impacts/statemod_output/shortages_by_right/final/shortages_by_right_all.rds")
ditch_shortages <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/impacts/statemod_output/shortages_by_right/shortages_by_right_all2.rds") %>%
  filter(district != "38")
# fix special admin numbers, remove characters
ditch_shortages$node_id <- stringr::str_remove(ditch_shortages$node_id, "_D")
ditch_shortages$node_id <- stringr::str_remove(ditch_shortages$node_id, "_I")

# join node data w/ Lat, long coords
ditch_pts <- left_join(ditch_shortages, dplyr::select(cdss, WDID, lat, lng
                                                      # lat = LatDecDeg,
                                                      # lng = LongDecDeg
                                                      ),
                       by = c("node_id" = "WDID"))
ditch_data2 <- ditch_data %>%
  mutate(admin = as.numeric(admin))
node_admin_match <- ditch_data2 %>%
  dplyr::select(node_id, admin, year)
length(unique(ditch_data$node_id))
length(unique(ditch_data2$admin))

left_join(by_admin, dplyr::select(ditch_data2, year, admin, node_id), by = c("year", "admin"))

ditch_pts2 <- ditch_pts %>%
  dplyr::select(date, year, district, node_id, id, admin, decree_af, 18:24)
ditch_pts4 <- ditch_pts2 %>%
  group_by(year, district, node_id, admin) %>%
  summarize(across(c(supply_all:short_dir), sum))  %>%
  ungroup()

# remove leading 0 from district column
ditch_pts4$district <- stringr::str_remove(ditch_pts4$district, "^0+")

length(unique(ditch_pts3$node_id))
length(unique(ditch_pts$admin))
 ditch_pts3 <- left_join(ditch_pts2, dplyr::select(ditch_pts, admin, year, node_id), by = c("admin"))

saveRDS(ditch_pts4, "ditch_data2.rds")
# make point for each admin number
node_pts <- ditch_pts %>%
  group_by(node_id) %>%
  slice(n = 1) %>%
  na.omit() %>%
  ungroup() %>%
  # group_by(admin) %>%
  # mutate(
  #   lat = lat+(0.01*priority)
  # )
  st_as_sf(coords = c("lng", "lat"), crs = 5070)

mapview::mapview(node_pts)

tbl_coeff <- tbl_data %>%
  filter(Term != "(Intercept)") %>%
  mutate(
     Term = "Precip1",
    )
tbl_int <- tbl_data %>%
  filter(Term == "(Intercept)")

bind_rows(tbl_int, tbl_coeff)
# remove leading 0 from district column
node_pts$district <- stringr::str_remove(node_pts$district, "^0+")
node_pts <- node_pts %>%
  st_transform(4326) %>%
  rename(district = district) %>%
  mutate(district = as.numeric(district))
# saveRDS(node_pts, "node_pts.rds")
# write_sf(node_pts, "node_pts.shp")
tmp <- by_admin %>%
  filter(admin == 4151.00000)
# ---- Node timeseries ----
# filter data to node
ditch <- ditch_data %>%
  filter(node_id == "0700699") %>%
  group_by(year, admin) %>%
  summarize(across(c(19:24), sum)) %>%
  ungroup()

length(unique(by_admin$admin))
pal <- viridis::turbo(n = 28)
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)
pal <- topo.colors(n = 15)
grDevices::terrain.colors(

)
par(mar = rep(0, 4))
pie(rep(1, length(pal)), col = pal)
# split node by admin
node_split_short <- ditch %>%
  group_by(admin) %>%
  mutate(x = year, y = short_dir) %>%
  dplyr::select(x, y, admin) %>%
  group_split() %>%
  as.list()

node_split_demand <- ditch %>%
  group_by(admin) %>%
  mutate(x = year, y = demand) %>%
  dplyr::select(x, y, admin) %>%
  group_split() %>%
  as.list()

node_split_demand <- ditch_data %>%
  # filter(node_id == node) %>%
  filter(node_id == "0700699") %>%
  mutate(admin = round(as.numeric(admin), 0)) %>%
  group_by(admin) %>%
  mutate(x = year, y = demand_dir) %>%
  dplyr::select(x, y, admin) %>%
  group_split() %>%
  as.list()

# initialize highchart
admin_dem_hc <- highchart() %>%
  hc_plotOptions(
    line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 4),
    area = list(stacking = 'normal'),
    column = list(stacking = 'normal')
  ) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

# for loop to generate highcharter series for each admin number
for (i in 1:length(node_split_demand)) {
  admin_dem_hc <- admin_dem_hc %>%
    hc_add_series(node_split_demand[[i]], name = paste0(node_split_demand[[i]][1,3]), type = "line")
}
admin_dem_hc

# initialize highchart
admin_hc <- highchart() %>%
  hc_plotOptions(
    line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 4),
    column = list(stacking = 'normal')
    ) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

# for loop to generate highcharter series for each admin number
for (i in 1:length(node_split)) {
  admin_hc <- admin_hc %>%
    hc_add_series(node_split[[i]], name = paste0(node_split[[i]][1,3]), type = "line")
}
admin_hc

unique(ditch_pts$priority)
library(ggplot2)
ggplot() +
  geom_line(data = ditch, aes(x = year, y = short_dir, col = admin)

           )  + facet_wrap(~admin)

