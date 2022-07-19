library(tidyverse)

data_path <- "C:/Users/angus/OneDrive/Desktop/github/cpo_data_processing/data/"

clim_files <- paste0(data_path, "climate/", list.files(paste0(data_path, "climate/")))

# climate data
climate <- read_csv(clim_files[1])

statemod_files <-  paste0(data_path, "outputs/",list.files(paste0(data_path, "outputs/")))

# statemod data
statemod <- read_csv(statemod_files[9]) %>%
  mutate(district = as.numeric(district))

# join climate + statemod data
statemod_climate <- left_join(
  statemod,
  climate,
  by = c("date", "district")
)

# extra swe data
swe_data <- read_csv("C:/Users/angus/OneDrive/Desktop/statemod_climate_data_monthly.csv") %>%
  dplyr::select(date, district, swe_max, swe_cuml)

# basin_lst <- readRDS("match_basin_district.rds") %>%
#   mutate(DISTRICT = as.numeric(DISTRICT)) %>%
#   dplyr::select(district = DISTRICT, basin = BASIN)

# statemod_climate <- left_join(
#   statemod_climate,
#   basin_lst,
#   by = c("district")
# ) %>%
#   dplyr::relocate(date, basin, district)

statemod_climate <- left_join(
  statemod_climate,
  dplyr::select(swe_data, date, district, swe_max, swe_cuml),
  by = c("date", "district")
)

stm_clim_month <- statemod_climate %>%
        group_by(district, date) %>%
        summarize(
          demand     = sum(demand),
          supply     = sum(supply),
          supply_dir = sum(supply_dir),
          short      = sum(short),
          short_dir  = sum(short_dir),
          prcp       = mean(prcp),
          tmax       = mean(tmax),
          tmin       = mean(tmin),
          pdsi       = mean(pdsi),
          aet        = mean(aet),
          pet        = mean(pet),
          soilm      = mean(soilm),
          swe_max_nrcs = max(swe_max),
          swe_max     = max(swe),
          swe_cuml   = mean(swe_cuml),
          spi1       = mean(spi1),
          spi3       = mean(spi3),
          spi6       = mean(spi6),
          spi9       = mean(spi9),
          spi12      = mean(spi12),
          eddi1      = mean(eddi1),
          eddi3      = mean(eddi3),
          eddi6      = mean(eddi6),
          eddi9      = mean(eddi9),
          eddi12     = mean(eddi12)
        ) %>%
  mutate(
    short_pct_dem     = round((short/demand)*100, 3),
    short_dir_pct_dem = round((short_dir/demand)*100, 3)
  ) %>%
  mutate(
    short_pct_dem = case_when(
      is.nan(short_pct_dem) ~ 0,
      TRUE ~ short_pct_dem
    ),
    short_dir_pct_dem = case_when(
      is.nan(short_dir_pct_dem) ~ 0,
      TRUE ~ short_dir_pct_dem
    )
  )

stm_clim_year <- stm_clim_month %>%
  mutate(
    year = lfstat::water_year(date)
  ) %>%
  group_by(district, year) %>%
  summarize(
    demand     = sum(demand, na.rm = T),
    supply     = sum(supply, na.rm = T),
    supply_dir = sum(supply_dir, na.rm = T),
    short      = sum(short, na.rm = T),
    short_dir  = sum(short_dir, na.rm = T),
    prcp       = sum(prcp, na.rm = T),
    tmax       = mean(tmax, na.rm = T),
    tmin       = mean(tmin, na.rm = T),
    pdsi       = mean(pdsi, na.rm = T),
    aet        = mean(aet, na.rm = T),
    pet        = mean(pet, na.rm = T),
    soilm      = mean(soilm, na.rm = T),
    # swe_max_nrcs = max(swe_max),
    swe_max    = max(swe_max, na.rm = T),
    # swe_cuml   = mean(swe_cuml),
    spi1       = mean(spi1, na.rm = T),
    spi3       = mean(spi3, na.rm = T),
    spi6       = mean(spi6, na.rm = T),
    spi9       = mean(spi9, na.rm = T),
    spi12      = mean(spi12, na.rm = T),
    eddi1      = mean(eddi1, na.rm = T),
    eddi3      = mean(eddi3, na.rm = T),
    eddi6      = mean(eddi6, na.rm = T),
    eddi9      = mean(eddi9, na.rm = T),
    eddi12     = mean(eddi12, na.rm = T)
  ) %>%
  mutate(
    short_pct_dem     = round((short/demand)*100, 3),
    short_dir_pct_dem = round((short_dir/demand)*100, 3)
  ) %>%
  mutate(
    year              = as.numeric(as.character(year)),
    short_pct_dem     = case_when(
      is.nan(short_pct_dem) ~ 0,
      TRUE ~ short_pct_dem
    ),
    short_dir_pct_dem = case_when(
      is.nan(short_dir_pct_dem) ~ 0,
      TRUE ~ short_dir_pct_dem
    )
  ) %>%
  dplyr::relocate(district, year, demand, supply, supply_dir, short, short_dir, short_pct_dem, short_dir_pct_dem)
# model_data <- readRDS("model_data.rds")
model_data <- readRDS("model_data_all.rds")
# future_smples <- readRDS("maca_future_sample
# years over 1980, remove district 99
stm_clim_year2 <- stm_clim_year %>%
  filter(year >= 1980, district != 99)

ggplot() +
  geom_col(data = stm_clim_year2, aes(x = year, y = prcp)) +
  facet_wrap(~district)

districts <- unique(stm_clim_year2$district)

for (i in 1:length(districts)) {

  logger::log_info("MLR - district: {districts[i]}")
  stm_clim_month %>%
  dplyr::select(
    short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9,
    spi12, eddi1, eddi3, eddi6, eddi12, swe_max, soilm
    # pet,
  )

}


# Apply MLR model to each district and get variance * R2 for map colors
stat <- model_data %>%
  filter(district == dist()) %>%
  # filter(district == 2)  %>%
  dplyr::select(
    short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9,
    spi12, eddi1, eddi3, eddi6, eddi12, swe_max, soilm
    # pet,
  )

# subset and log transform data for MLR
log_trans <- district_data %>%
  ungroup() %>%
  mutate(
    short_dir_norm = log10(short_dir_norm)
  )

#     # replace Infinite w/ 0
is.na(log_trans) <- sapply(log_trans, is.infinite)
log_trans[is.na(log_trans)] <- 0

# MLR w/ VIF reduction + stepwise regression
mlr_vfit <- lm(short_dir_norm~., data = log_trans) %>%
  ols_step_forward_p()

  # summarize(
  #   demand     = sum(demand, na.rm = T),
  #   supply     = sum(supply, na.rm = T),
  #   supply_dir = sum(supply_dir, na.rm = T),
  #   short      = sum(short, na.rm = T),
  #   short_dir  = sum(short_dir, na.rm = T),
  #   prcp       = sum(prcp, na.rm = T),
  #   tmax       = mean(tmax, na.rm = T),
  #   tmin       = mean(tmin, na.rm = T),
  #   pdsi       = mean(pdsi, na.rm = T),
  #   aet        = mean(aet, na.rm = T),
  #   pet        = mean(pet, na.rm = T),
  #   soilm      = mean(soilm, na.rm = T),
  #   swe_max    = max(swe, na.rm = T),
  #   swe_sum    = sum(swe, na.rm = T),
  #   spi1       = mean(spi1, na.rm = T),
  #   spi3       = mean(spi3, na.rm = T),
  #   spi6       = mean(spi6, na.rm = T),
  #   spi9       = mean(spi9, na.rm = T),
  #   spi12      = mean(spi12, na.rm = T),
  #   eddi1      = mean(eddi1, na.rm = T),
  #   eddi3      = mean(eddi3, na.rm = T),
  #   eddi6      = mean(eddi6, na.rm = T),
  #   eddi9      = mean(eddi9, na.rm = T),
  #   eddi12     = mean(eddi12, na.rm = T)
  # )






