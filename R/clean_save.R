# Script for reading in, cleaning and resaving data for use in Shiny app
remove(list = ls())
library(tidyverse)

# model_data <- readRDS("model_data_all.rds")
model_data   <- readRDS("statemod_climate_year.rds")

model_data_clean <- model_data %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year < 2013)

# saveRDS(model_data_clean, "statemod_climate_year2.rds")
model_data_clean <- readRDS("statemod_climate_year2.rds")

# water supply MLR timeseries
water_ts <- model_data_clean %>%
  group_by(district) %>%
  filter(year >= 1980, year < 2013)  %>%
  mutate(
    year        = as.numeric(as.character(year)),
    aug_supply  = demand - (short + supply_dir)
  )  %>%
  mutate(
  # aug_supply2  = case_when(
  #   aug_supply < 0 ~ supply_dir + aug_supply + short
  # )
    supply_dir2  = case_when(
      aug_supply < 0  ~ supply_dir + aug_supply,
      TRUE ~ supply_dir
    )
  )  %>%
  dplyr::select(year, district, short, aug_supply, supply_dir2, demand) %>%
  # dplyr::select(year, district, short, aug_supply, supply_dir, demand) %>%
  dplyr::rename(
    "Supply Augmented"     = aug_supply,
    "Demand"               = demand,
    # "short" = "demand_diff",
    "Supply Direct flow"   = supply_dir2
  ) %>%
  mutate(across(where(is.numeric), round, 1)) %>%
  ungroup()

  # mutate(
  #   aug_supply = case_when(
  #     aug_supply < 0 ~ 0,
  #     aug_supply >= 0 ~ aug_supply
  #   )
  # ) %>%
  # filter(district == mlr_district(), year >= 1980, year < 2013)  %>%
  # mutate(
  #   year        = as.numeric(as.character(year)),
  #   aug_supply2 = aug_supply + supply_dir,
  #   demand2     = demand - aug_supply2
  # ) %>%
  # dplyr::select(year, demand:short_dir_pct_dem,aug_supply2, demand2) %>%
  # dplyr::select(year, district, short, aug_supply, supply_dir, demand) %>%
  # # mutate(
  # #   # total_bar   = aug_supply + supply_dir + short,
  # #   # diff        = demand - total_bar,
  # #   demand_diff = demand - (aug_supply + supply_dir)
  # # ) %>%
  # # dplyr::select(year, district, demand_diff, aug_supply, supply_dir, demand) %>%
  # dplyr::rename(
  #   # "Supply Augmented2"    = aug_supply2,
  #   "Supply Augmented"     = aug_supply,
  #   "Demand"               = demand,
  #   # "short" = "demand_diff",
  #   # "Demand_diff"          = demand2,
  #   "Supply Direct flow"   = supply_dir
  # ) %>%
  # mutate(across(where(is.numeric), round, 1)) %>%
  # ungroup()

# tmp <- water_ts %>%
  # filter(district == 42) %>%
  # mutate(
  #   aug_supply2 = case_when(
  #     aug_supply < 0 ~ 0,
  #     aug_supply >= 0 ~ aug_supply
  #   )
  #   # demand_diff = demand - (aug_supply + supply_dir),
  #   # total = short + aug_supply + supply_dir,
  #   # total2 = demand_diff + aug_supply + supply_dir,
  #   # diff  = demand - total,
  #   # diff2  = demand - total2
  #   )
mlr_fit2 <- mlr_fit %>%
  filter(district == 64)
tmp <- model_data %>%
  filter(district == 64)
# saveRDS(water_ts, "water_timeseries.rds")
# test <- readRDS("water_timeseries.rds")
# df <- water_ts %>%
#   filter(district == 42)
# highchart() %>%
#   hc_plotOptions(
#     column = list(stacking = 'normal'),
#     line   = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
#   hc_yAxis(
#     min      = 0,
#     title    = list(
#       text   = "Water volume (AF)",
#       style  = list(fontSize = 14, fontWeight = "bold", color = "black")),
#     labels   = list(
#       y      = 10,
#       style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
#   hc_xAxis(
#     # categories    = ws_wide$year,
#     categories    = df$year,
#     tickInterval  = 2,
#     labels   = list(
#       y      = 35,
#       style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
#   hc_add_series(
#     # data        = ws_wide,
#     data        = df,
#     name        = "Total shortage",
#     type        = 'column',
#     tooltip     = list(pointFormat = "Total shortage: {point.short} AF"),
#     fillOpacity = 0.3,
#     hcaes(
#       x  = year,
#       y  = short
#     )
#   ) %>%
#   hc_add_series(
#     # data        = ws_wide,
#     data        = df,
#     name        = "Augmented supply",
#     type        = 'column',
#     tooltip     = list(pointFormat = "Augmented supply: {point.Supply Augmented} AF"),
#     fillOpacity = 0.3,
#     hcaes(
#       x  = year,
#       y  = `Supply Augmented`
#     )
#   ) %>%
#   hc_add_series(
#     # data        = ws_wide,
#     data        = df,
#     name        = "Direct Flow Supply",
#     type        = 'column',
#     tooltip     = list(pointFormat = "Direct Flow Supply: {point.Supply Direct flow} AF"),
#     fillOpacity = 0.3,
#     hcaes(
#       x  = year,
#       y  = `Supply Direct flow`)) %>%
#   hc_add_series(
#     # data        = ws_wide,
#     data        = df,
#     name        = "Demand",
#     type        = 'line',
#     tooltip     = list(pointFormat = "Demand: {point.Demand} AF"),
#     hcaes(
#       x  = year,
#       y  = Demand
#     )
#   ) %>%
#   hc_add_theme(hc_theme_elementary()) %>%
#   hc_colors(c("#E18686",  "#70BCE2", "#2984B2", "black")) %>%
#   hc_chart(
#     plotBorderWidth = 0.5,
#     plotBorderColor = '#b4b4b4',
#     height          = NULL)




# ---- Cleaned names for plots ----

name_df <- data.frame(
  swe               = "District SWE maximum (mm)",
  swe_max           = "Basin SWE maximum (mm)",
  prcp              = "Precipitation (mm)",
  pdsi              = "PDSI",
  eddi1             = "EDDI 1 month",
  eddi3             = "EDDI 3 month",
  eddi6             = "EDDI 6 month",
  eddi9             = "EDDI 9 month",
  eddi12            = "EDDI 12 month",
  spi1              = "SPI 1 month",
  spi3              = "SPI 3 month",
  spi6              = "SPI 6 month",
  spi9              = "SPI 9 month",
  spi12             = "SPI 12 month",
  tavg              = "Average temperature (C)",
  tmax              = "Maximum temperature (C)",
  tmin              = "Minimum temperature (C)",
  aet               = "Actual evapotranspiration (mm)",
  pet               = "Potential Evapotranspiration (mm)",
  soilm             = "Soil moisture (mm)",
  short             = "Total shortage (AF)",
  short_pct_dem     = "Total shortage (% of demand)",
  short_dir         =  "Direct shortage (AF)",
  short_dir_pct_dem = "Direct shortage (% of demand)",
  demand            = "Demand (AF)",
  supply            = "Supply (AF)",
  supply_dir        = "Direct supply (AF)",
  aug_supply        = "Augmented Supply (AF)",
  aug_supply_pct_dem = "Augmented Supply (% of demand)"
  ) %>%
  pivot_longer(
    cols      = c(1:29),
    names_to  = "var",
    values_to = "clean_name"
  )

saveRDS(name_df, "clean_variable_names.rds")

name_df <- data.frame(
  prcp              = "Precipitation (mm)",
  pdsi              = "PDSI",
  spi1              = "SPI 1 month",
  spi3              = "SPI 3 month",
  spi6              = "SPI 6 month",
  spi9              = "SPI 9 month",
  spi12             = "SPI 12 month",
  tavg              = "Average temperature (C)",
  short             = "Total shortage (AF)",
  short_pct_dem     = "Total shortage (% of demand)",
  short_dir         =  "Direct shortage (AF)",
  short_dir_pct_dem = "Direct shortage (% of demand)",
  demand            = "Demand (AF)",
  supply            = "Supply (AF)",
  supply_dir        = "Direct supply (AF)",
  aug_supply        = "Augmented Supply (AF)",
  aug_supply_pct_dem = "Augmented Supply (% of demand)"
) %>%
  pivot_longer(
    cols      = c(1:29),
    names_to  = "var",
    values_to = "clean_name"
  )
# clean climate model names
clim_model_indicator_lst <- list(
  "Precipitation (mm)"                 = "prcp",
  "Average temperature (C)"            = "tavg",
  "Maximum temperature (C)"            = "tmax",
  "Minimum temperature (C)"            = "tmin",
  "Potential Evapotranspiration (mm)"  = "pet",
  "PDSI"                               = "pdsi",
  # "Potential Evapotranspiration (mm)"  = "et_harg",
  # "PDSI (Hargreaves)"                  = "pdsi_harg",
  "SPI 1 month"                        = "spi1",
  "SPI 3 month"                        = "spi3",
  "SPI 6 month"                        = "spi6",
  "SPI 9 month"                        = "spi9",
  "SPI 12 month"                       = "spi12"
)

# ---- Clean MLR fitted data ----
# fitted MLR data
mlr_fit      <- readRDS("mlr_fit.rds")

# round values to 2 decimals
mlr_fit <- mlr_fit %>%
  mutate(across(where(is.numeric), round, 2))

# save
saveRDS(mlr_fit, "mlr_fit.rds")


# district MLR predictors
mlr_predictors <- readRDS("mlr_predictors.rds")

# Tidied MLR results by district
mlr_metrics     <- readRDS("mlr_metrics.rds")

# data for water supply/demand/shortages timeseries
ws_ts           <- readRDS("water_timeseries.rds")

# district shapefile path
shp_path     <- "water_districts_simple.geojson"

# districts for analysis
distr_num    <- unique(mlr_metrics$district)

# load shapefiles as spatial polygon object
shp <- sf::read_sf(paste0(shp_path), quiet = TRUE) %>%
  filter(DISTRICT %in% distr_num) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON") %>%
  left_join(mlr_metrics, by = c("DISTRICT" = "district")) %>%
  dplyr::select(DISTRICT, BASIN, NAME, var_sensitivity:var_sensitivity_std, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.6) %>%
  geojsonsf::sf_geojson()

# save as GeoJSON
geojsonio::geojson_write(shp, file = "water_districts.geojson")





