library(raster)
library(tidyverse)
library(lubridate)
library(climateR)
library(sf)
library(mapview)

source("utils/data_utils.R")
source("misc/find_google_drive.R")

# **************** INPUT PARAMS ************************
# ======================================================
# Major river basin
basin = "South Platte"

# district shapefile path
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
shp <- sf::st_read(shp_path, quiet = TRUE) %>%
  dplyr::filter(BASIN %in% !!basin) %>%
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

tmin <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/temp/tmin_daily_maca_rcp45_sp.rds")
tmax <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/temp/tmax_daily_maca_rcp45_sp.rds") %>%
  rename(tmax = prcp)

temp <- left_join(tmin, tmax, by = c("district", "date")) %>%
  rename(tmax = prcp)

temp <- temp %>%
  mutate(
    tmin    =   tmin - 273.15,
    tmax    =   tmax - 273.15,
    tavg    =   (tmin + tmax)/2
  )
# saveRDS(temp, "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/temp/temp_daily_maca_rcp45_sp.rds")
#

# ----- SOUTH PLATTE TEMP + PRCP ------
temp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/temp/temp_daily_maca_rcp45_sp.rds")
prcp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/prcp/prcp_daily_maca_rcp45_sp.rds")

indicators_maca <- temp %>%
  left_join(prcp, by = c("district", "date"))

# ----- COLORADO TEMP + PRCP -----

tmax <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/colorado/temp/tmax_maca_1950_2099_rcp45_co.rds")
tmin <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/colorado/temp/tmin_maca_1950_2099_rcp45_co.rds")

temp <- left_join(tmax, tmin, by = c('district', 'date')) %>%
  mutate(
    tmax    = tmax - 273.15,
    tmin    = tmin - 273.15,
    tavg    = (tmax+tmin)/2
  )

rm(tmax, tmin, temp1)

temp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/colorado/temp/temp_maca_1950_2099_rcp45_co.rds")
prcp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/colorado/prcp/prcp_maca_1950_2099_rcp45_co.rds")

indicators_maca <- prcp %>%
  left_join(temp, by = c("district", "date"))

basin <- "Colorado"

# split data into groups by district to loop over
data_list <- split(indicators_maca, f = indicators_maca$district)

# Water districts shape
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
shp <- sf::st_read(shp_path, quiet = TRUE) %>%
  dplyr::filter(BASIN %in% !!basin) %>%
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

indicator_lst <- list()

# rm(et, tmp, tmax_avg, temp, indicator_lst, shp, shp2, wichita, et_ts, bcca_lst, evap, lat_coord, pb, evap_hargreaves, evap_thornwaite, et_df, clim, temp)
# rm(clim_df, pdsi, pdsi_thorn, pdsi_harg)

# for loop calculates ET, PDSI
for (i in 1:length(data_list)){
  # first dataframe in list
  tmp <- data_list[[i]]

  # district number of list
  distr <- tmp$district[1]

  # get latitude of each district
  lat_coord <- shp %>%
    filter(DISTRICT == distr) %>%
    st_centroid() %>%
    st_coordinates(distr_lat) %>%
    data.frame() %>%
    dplyr::select(Y)
  # summarize(lat_avg = mean(Y))

  # totaling precip volume for each cell in a district on each day
  tmp <- tmp %>%
    mutate(
      month     =  lubridate::month(date),
      year      =  lubridate::year(date)
    ) %>%
    group_by(district, year, month) %>%
    summarise(
      prcp      =  sum(prcp),
      tmax      =  mean(tmax),
      tmin      =  mean(tmin),
      tavg      =  mean(tavg)
    )
  # mutate(tmax_avg = tmax_avg - 273.15)

  # add date column
  tmp$date <- zoo::as.yearmon(paste(tmp$year, tmp$month), "%Y %m")
  tmp$date <- zoo::as.Date(tmp$date)

  # tmp <- tmp %>%
  #   mutate(
  #     wyear     = lfstat::water_year(date, origin = 10, as.POSIX=TRUE)
  #     )

  clim <- tmp %>%
    mutate(
      YEAR      = as.integer(year),
      MONTH     = as.integer(month),
      PRCP      = prcp,
      TMAX      = tmax,
      TMIN      = tmin,
      TMED      = tavg
    ) %>%
    ungroup() %>%
    dplyr::select(YEAR, MONTH, PRCP, TMAX, TMIN, TMED)

  # Hargreaves ET formula
  evap_hargreaves <- SPEI::hargreaves(Tmin = clim$TMIN, Tmax = clim$TMAX, lat = lat_coord$Y, Pre = clim$PRCP)

  # Thornwaite ET formula
  evap_thornwaite <- SPEI::thornthwaite(Tave = clim$TMED, lat = lat_coord$Y)

  # clean data vector
  date_col <- seq.Date(from = as.Date("1950-01-01"), to = as.Date("2099-12-31"), by = "1 month")

  # arrange by date and add correct dates
  evap_thornwaite <- evap_thornwaite %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, et_thorn = value)

  evap_hargreaves <- evap_hargreaves %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, et_harg = value)

  # ET data
  evap <- left_join(evap_thornwaite, evap_hargreaves, by = c("district", "date"))

  # PDSI using Thornwaite ET
  pdsi_thorn <- scPDSI::pdsi(P = tmp$prcp, PE = evap$et_thorn)
  pdsi_thorn <- pdsi_thorn$X %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, pdsi_thorn = value)

  # PDSI using Hargreaves ET
  pdsi_harg <- scPDSI::pdsi(P = tmp$prcp, PE = evap$et_harg)
  pdsi_harg <- pdsi_harg$X %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, pdsi_harg = value)

  # PDSI dataframe
  pdsi <- left_join(pdsi_harg, pdsi_thorn, by = c("district", "date"))

  spi <- clim %>%
    dplyr::select(YEAR, MONTH, PRCP)

  # add a date column from YEAR and MONTH cols
  spi$date <- zoo::as.yearmon(paste(spi$YEAR, spi$MONTH), "%Y %m")

  # make YEAR & MONTH columns integers, required for SPI function
  spi$YEAR <- as.integer(spi$YEAR)
  spi$MONTH <- as.integer(spi$MONTH)

  # SPI dataframe a timeseries
  prcp_ts <- ts(spi, end=c(2099,12), frequency=12)

  # calculate SPI - 1 month time scale
  spi_1 <- SPEI::spi(prcp_ts[,3], 1)
  spi_1 <- tsbox::ts_data.frame(spi_1$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi1 = value)

  # calculate SPI - 1 month time scale
  spi_3 <- SPEI::spi(prcp_ts[,3], 3)
  spi_3 <- tsbox::ts_data.frame(spi_3$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi3 = value)

  # calculate SPI - 1 month time scale
  spi_6 <- SPEI::spi(prcp_ts[,3], 6)
  spi_6 <- tsbox::ts_data.frame(spi_6$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi6 = value)

  # calculate SPI - 1 month time scale
  spi_9 <- SPEI::spi(prcp_ts[,3], 9)
  spi_9 <- tsbox::ts_data.frame(spi_9$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi9 = value)

  # calculate SPI - 1 month time scale
  spi_12 <- SPEI::spi(prcp_ts[,3], 12)
  spi_12 <- tsbox::ts_data.frame(spi_12$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi12 = value)

  spi_all <- spi_1 %>%
    left_join(spi_3, by = c("district", "date")) %>%
    left_join(spi_6, by = c("district", "date")) %>%
    left_join(spi_9, by = c("district", "date")) %>%
    left_join(spi_12, by = c("district", "date"))

  clim_df <- tmp %>%
    left_join(evap, by = c("district", "date")) %>%
    left_join(pdsi, by = c("district", "date")) %>%
    left_join(spi_all, by = c("district", "date")) %>%
    ungroup() %>%
    dplyr::select(district, date, prcp:tavg, et_thorn:spi12)

  # add ET dataframe to list
  indicator_lst[[i]] <- clim_df
}

# Evapotranspiration dataframe
indicator_df_co <- bind_rows(indicator_lst)
indicators_all <- bind_rows(indicator_df_sp, indicator_df_co)
unique(indicators_all$district)
saveRDS(indicators_all, "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/monthly/drought_indicators_maca_rcp45.rds")

# ----- BCCA loop  -----
temp_co <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/colorado/temp/temp_daily_bcca_rcp45_co.rds")
prcp_co <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/colorado/prcp/prcp_daily_bcca_rcp45_co.rds")
temp_sp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/south_platte/temp/temp_daily_bcca_rcp45_sp.rds")
prcp_sp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/south_platte/prcp/prcp_daily_bcca_rcp45_sp.rds")

temp <- bind_rows(temp_co, temp_sp)
prcp <- bind_rows(prcp_co, prcp_sp)

indicators_bcca <- prcp %>%
  left_join(temp, by = c("district", "date"))

# split data into groups by district to loop over
data_list <- split(indicators_bcca, f = indicators_bcca$district)

# Water districts shape
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
shp <- sf::st_read(shp_path, quiet = TRUE) %>%
  dplyr::filter(DISTRICT %in% indicators_bcca$district) %>%
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

indicator_lst <- list()

# rm(et, tmp, tmax_avg, temp, indicator_lst, shp, shp2, wichita, et_ts, bcca_lst, evap, lat_coord, pb, evap_hargreaves, evap_thornwaite, et_df, clim, temp)
# rm(clim_df, pdsi, pdsi_thorn, pdsi_harg)

# for loop calculates ET, PDSI
for (i in 1:length(data_list)){
  # first dataframe in list
  tmp <- data_list[[i]]

  # district number of list
  distr <- tmp$district[1]

  # get latitude of each district
  lat_coord <- shp %>%
    filter(DISTRICT == distr) %>%
    st_centroid() %>%
    st_coordinates(distr_lat) %>%
    data.frame() %>%
    dplyr::select(Y)
  # summarize(lat_avg = mean(Y))

  # totaling precip volume for each cell in a district on each day
  tmp <- tmp %>%
    mutate(
      month     =  lubridate::month(date),
      year      =  lubridate::year(date)
    ) %>%
    group_by(district, year, month) %>%
    summarise(
      prcp      =  sum(prcp),
      tmax      =  mean(tmax),
      tmin      =  mean(tmin),
      tavg      =  mean(tavg)
    )
  # mutate(tmax_avg = tmax_avg - 273.15)

  # add date column
  tmp$date <- zoo::as.yearmon(paste(tmp$year, tmp$month), "%Y %m")
  tmp$date <- zoo::as.Date(tmp$date)

  # tmp <- tmp %>%
  #   mutate(
  #     wyear     = lfstat::water_year(date, origin = 10, as.POSIX=TRUE)
  #     )

  clim <- tmp %>%
    mutate(
      YEAR      = as.integer(year),
      MONTH     = as.integer(month),
      PRCP      = prcp,
      TMAX      = tmax,
      TMIN      = tmin,
      TMED      = tavg
    ) %>%
    ungroup() %>%
    dplyr::select(YEAR, MONTH, PRCP, TMAX, TMIN, TMED)

  # Hargreaves ET formula
  evap_hargreaves <- SPEI::hargreaves(Tmin = clim$TMIN, Tmax = clim$TMAX, lat = lat_coord$Y, Pre = clim$PRCP)

  # Thornwaite ET formula
  evap_thornwaite <- SPEI::thornthwaite(Tave = clim$TMED, lat = lat_coord$Y)

  # clean data vector
  date_col <- seq.Date(from = as.Date("1950-01-01"), to = as.Date("2099-12-31"), by = "1 month")

  # arrange by date and add correct dates
  evap_thornwaite <- evap_thornwaite %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, et_thorn = value)

  evap_hargreaves <- evap_hargreaves %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, et_harg = value)

  # ET data
  evap <- left_join(evap_thornwaite, evap_hargreaves, by = c("district", "date"))

  # PDSI using Thornwaite ET
  pdsi_thorn <- scPDSI::pdsi(P = tmp$prcp, PE = evap$et_thorn)
  pdsi_thorn <- pdsi_thorn$X %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, pdsi_thorn = value)

  # PDSI using Hargreaves ET
  pdsi_harg <- scPDSI::pdsi(P = tmp$prcp, PE = evap$et_harg)
  pdsi_harg <- pdsi_harg$X %>%
    tsibble::as_tsibble() %>%
    arrange(index) %>%
    mutate(
      date      = date_col,
      district  = distr
    ) %>%
    data.frame() %>%
    dplyr::select(district, date, pdsi_harg = value)

  # PDSI dataframe
  pdsi <- left_join(pdsi_harg, pdsi_thorn, by = c("district", "date"))

  spi <- clim %>%
    dplyr::select(YEAR, MONTH, PRCP)

  # add a date column from YEAR and MONTH cols
  spi$date <- zoo::as.yearmon(paste(spi$YEAR, spi$MONTH), "%Y %m")

  # make YEAR & MONTH columns integers, required for SPI function
  spi$YEAR <- as.integer(spi$YEAR)
  spi$MONTH <- as.integer(spi$MONTH)

  # SPI dataframe a timeseries
  prcp_ts <- ts(spi, end=c(2099,12), frequency=12)

  # calculate SPI - 1 month time scale
  spi_1 <- SPEI::spi(prcp_ts[,3], 1)
  spi_1 <- tsbox::ts_data.frame(spi_1$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi1 = value)

  # calculate SPI - 1 month time scale
  spi_3 <- SPEI::spi(prcp_ts[,3], 3)
  spi_3 <- tsbox::ts_data.frame(spi_3$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi3 = value)

  # calculate SPI - 1 month time scale
  spi_6 <- SPEI::spi(prcp_ts[,3], 6)
  spi_6 <- tsbox::ts_data.frame(spi_6$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi6 = value)

  # calculate SPI - 1 month time scale
  spi_9 <- SPEI::spi(prcp_ts[,3], 9)
  spi_9 <- tsbox::ts_data.frame(spi_9$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi9 = value)

  # calculate SPI - 1 month time scale
  spi_12 <- SPEI::spi(prcp_ts[,3], 12)
  spi_12 <- tsbox::ts_data.frame(spi_12$fitted) %>%
    mutate(district = distr) %>%
    dplyr::select(district, date = time, spi12 = value)

  spi_all <- spi_1 %>%
    left_join(spi_3, by = c("district", "date")) %>%
    left_join(spi_6, by = c("district", "date")) %>%
    left_join(spi_9, by = c("district", "date")) %>%
    left_join(spi_12, by = c("district", "date"))

  clim_df <- tmp %>%
    left_join(evap, by = c("district", "date")) %>%
    left_join(pdsi, by = c("district", "date")) %>%
    left_join(spi_all, by = c("district", "date")) %>%
    ungroup() %>%
    dplyr::select(district, date, prcp:tavg, et_thorn:spi12)

  # add ET dataframe to list
  indicator_lst[[i]] <- clim_df
}

indicators_df <- bind_rows(indicator_lst)
saveRDS(indicators_df, "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/monthly/drought_indicators_bcca_rcp45.rds")


# REMOVE 0 FROM BEGINNING OF DISTRICT COLUMN IN ADMIN_YEAR DATA
by_admin2$district <- as.factor(str_replace(by_admin2$district, "^0+" ,""))

# get unique basins to join w/ admin data
basins <- dplyr::select(mod_df, district, basin) %>%
  unique()

# JOIN DISTRICTS W/ BASIN NAMES
by_admin2 <- left_join(by_admin2, basins, by = "district")














