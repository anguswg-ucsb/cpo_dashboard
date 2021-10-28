
# ---------------------------------------------------
# ------ by admin shortage join w/ climate var ------
# ---------------------------------------------------

point_data <- by_admin %>%
  group_by(year, district) %>%
  mutate(
    year                = as.factor(year),
    admin_number        = as.integer(round(admin, 0))
  ) %>%
  dplyr::filter(!year %in% c(1980, 2013)) %>%
  group_by(basin, district, year) %>%
  summarise(
    short               = sum(short, na.rm = T),
    short_dir           = sum(short_dir, na.rm = T),
    demand              = sum(demand, na.rm = T),
    supply              = sum(supply, na.rm = T),
    supply_dir          = sum(supply_dir, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.factor(year)
  )

## ------ Join model data w/ admin level supply/demand/short -------
point_data2 <- left_join(
  point_data,
  dplyr::select(short_year, year = wyear, district, 20:40),
  by = c("district", "year")
) %>%
  # filter(district == 6) %>%
  # filter(district == district_id$district) %>%
  dplyr::filter(!year %in% c(1980, 2013)) %>%
  # group_by(basin, district, water_right, year) %>%
  group_by(basin, district, year) %>%
  summarise(
    short               = sum(short, na.rm = T),
    short_dir           = sum(short_dir, na.rm = T),
    demand              = sum(demand, na.rm = T),
    supply              = sum(supply, na.rm = T),
    supply_dir          = sum(supply_dir, na.rm = T),
    af_total            = mean(af_total, na.rm = T),
    swe_max             = mean(swe_max, na.rm = T),
    prcp                = mean(prcp, na.rm = T),
    # prcp_norm           = mean(prcp_norm, na.rm = T),
    pdsi                = mean(pdsi_gridmet, na.rm = T),
    # pdsi_gridmet        = mean(pdsi_gridmet, na.rm = T),
    eddi1               = mean(eddi1, na.rm = T),
    eddi3               = mean(eddi3, na.rm = T),
    eddi6               = mean(eddi6, na.rm = T),
    eddi12              = mean(eddi12, na.rm = T),
    spi1                = mean(spi1, na.rm = T),
    spi3                = mean(spi3, na.rm = T),
    spi6                = mean(spi6, na.rm = T),
    spi9                = mean(spi9, na.rm = T),
    spi12               = mean(spi12, na.rm = T),
    tavg                = mean(tavg, na.rm = T),
    tmax                = mean(tmax, na.rm = T),
    tmin                = mean(tmin, na.rm = T),
    aet                 = mean(aet, na.rm = T),
    pet                 = mean(pet, na.rm = T),
    soilm               = mean(soilm, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.factor(year)
  ) %>%
  ungroup()

saveRDS(point_data2, "model_data.rds")

# ------- Timeseries plot data ----------

# ---- Water supply data summarized to the year ----
ws_data <- by_admin %>%
  group_by(year, district) %>%
  mutate(
    year          = as.factor(year),
    admin_number  = as.integer(round(admin, 0))
  ) %>%
  dplyr::filter(!year %in% c(1980, 2013)) %>%
  group_by(basin, district, year) %>%
  summarise(
    short               = sum(short, na.rm = T),
    short_dir           = sum(short_dir, na.rm = T),
    demand              = sum(demand, na.rm = T),
    supply              = sum(supply, na.rm = T),
    supply_dir          = sum(supply_dir, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.factor(year)
  )

ws_wide <- ws_data %>%
  # filter(district == 7)  %>%
  # filter(district == district_id$district)  %>%
  mutate(
    year        = as.numeric(as.character(year)),
    aug_supply2 = aug_supply + supply_dir,
    demand2     = demand - aug_supply2
  ) %>%
  rename(
    "Supply Augmented2"    = aug_supply2,
    "Supply Augmented"     = aug_supply,
    "Demand"               = demand,
    "Demand_diff"          = demand2,
    "Supply Direct flow"   = supply_dir) %>%
  mutate(across(where(is.numeric), round, 0))

saveRDS(ws_wide, "timeseries_data.rds")








