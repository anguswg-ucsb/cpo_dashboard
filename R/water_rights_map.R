library(sf)

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
# fix special admin numbers, remove characters
ditch_shortages$node_id <- stringr::str_remove(ditch_shortages$node_id, "_D")
ditch_shortages$node_id <- stringr::str_remove(ditch_shortages$node_id, "_I")

# join node data w/ Lat, long coords
ditch_pts <- left_join(ditch_shortages, dplyr::select(cdss, WDID, lat, lng
                                                      # lat = LatDecDeg,
                                                      # lng = LongDecDeg
                                                      ),
                       by = c("node_id" = "WDID"))


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

# remove leading 0 from district column
node_pts$district <- stringr::str_remove(node_pts$district, "^0+")
node_pts <- node_pts %>%
  st_transform(4326) %>%
  rename(district = district) %>%
  mutate(district = as.numeric(district))
saveRDS(node_pts, "node_pts.rds")
# write_sf(node_pts, "node_pts.shp")

basemap(shp) %>%
  addCircleMarkers(data = node_pts,
                   radius = 2,
                   fillColor ="blue",
                   # color = "black",
                   stroke = TRUE)

tmp <- ditch %>%
  filter(node_id == "0100514")

ggplot() +
  geom_line(data = tmp, aes(x = date, y = short_dir)) +
  facet_wrap(~admin)

# date
start_date = "2022-01-01"
end_date = "2022-03-01"

# aoi
aoi <- AOI::aoi_get("Goleta")

# BCCA
bcca <- climateR::getBCCA( aoi, param = "prcp", startDate = start_date, endDate = end_date)





