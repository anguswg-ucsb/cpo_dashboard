utah <- USAboundaries::us_states() %>%
  filter(name == "Utah")

nlcd_path    = 'D:/NLCD/NLCD_2016_Land_Cover_L48_20190424.img'
lc           = raster(nlcd_path)
utah2         = st_transform(utah, st_crs(lc))

# crop to state
crop <- crop(lc, utah2)

# mask NLCD raster to shape
lc_mask <- mask(crop, utah2)


t = exactextractr::exact_extract(lc, utah2, function(value, cov_frac) {
  data.frame(value = value, cov_frac = cov_frac) %>%
    group_by(CLASS = as.character(substr(value,1,1))) %>%
    summarize(c = sum(cov_frac*(900/1e6))) %>%
    right_join(data.frame(CLASS = as.character(c('0', '1', '2', '3',
                                                 '4', '5', '7', '8', '9')),
                          tmp = rep(NA, 9)), by = 'CLASS') %>%
    pull(c)
})

# Data libraries
library(osmdata)   # OSM API
library(sf)
library(tidyverse)

# ---- CREATE BASIN OUTLINES ----
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"

distr_num <- c(
  1,  2,  4,  5,  6,  7,  8,  9,  23, 64, 80,
  36, 37, # 38,
  39, 45, 50, 51, 52, 53, 70, 72)

shp2 <- st_read(paste0(shp_path), quiet = T) %>%
  filter(DISTRICT %in% c(distr_num)) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

mapview::mapview(shp[c(1,2,3, 4),]) + shp2[c(1,2,3, 4),]

# union shape into basin outlines, clean up names for labels
basins <- shp2 %>%
  group_by(BASIN) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  st_cast("MULTILINESTRING") %>%
  mutate(
    basin = case_when(
      BASIN == "Colorado" ~ "colorado",
      BASIN == "South Platte" ~ "south_platte")
    ) %>%
  dplyr::select(basin, basin_clean = BASIN, geometry)
# clean up names for labels


sf::write_sf(basins, "basin_outline.shp")

# ---- GET WATERWAYS FROM OPEN STREET MAP ----
basin_poly <- shp2 %>%
  group_by(BASIN) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  mutate(
    basin = case_when(
      BASIN == "Colorado" ~ "colorado",
      BASIN == "South Platte" ~ "south_platte")
  ) %>%
  dplyr::select(basin, basin_clean = BASIN, geometry)
# get SP + CO river stems

basin_poly[1,]
# OSM waterway query
waterways = opq(basin_poly[1,]) %>%
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf()


waterways_sp = opq(basin_poly[2,]) %>%
  add_osm_feature(key = 'waterway') %>%
  osmdata_sf()

streams_sp = waterways_sp$osm_lines



co_river <- streams %>%
  filter(name == "Colorado River")
co_int <- st_filter(co_river, basin_poly) %>%
  dplyr::select(osm_id, name, geometry) %>%
  mutate(river = "Colorado River") %>%
  group_by(river) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

sp_river <- streams_sp %>%
  filter(str_detect(name, "South Platte"))

sp_int <- st_filter(sp_river, basin_poly) %>%
  dplyr::select(osm_id, name, geometry) %>%
  mutate(river = "South Platte River") %>%
  group_by(river) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

mapview::mapview(co_river) +sp_river +sp_int + co_int + basin_poly

# bind CO and SP rivers to same dataframe
basin_streams = bind_rows(co_int, sp_int)


sf::write_sf(basin_streams, "basin_rivers.shp")





mapview::mapview(basins[1,]) + basins[2,]
