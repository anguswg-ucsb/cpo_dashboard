
library(SPEI)
library(tidyverse)
library(sf)

temp <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/temp_month_all.rds") %>%
  filter(district == 6)

tmax_maca <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/temp/tmax_daily_maca_rcp45_sp.rds")

# split data into groups by district to loop over
data_list <- split(tmax_maca, f = tmax_maca$district)

# Water districts shape
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
shp <- sf::st_read(shp_path, quiet = TRUE) %>%
  # dplyr::filter(BASIN %in% !!basin) %>%
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

et_lst <- list()
rm(et, tmp, tmax_avg)

for (i in 1:length(data_list)){
  # first dataframe in list
  tmp <- data_list[[i]]

  # district number of list
  distr <- tmp$district[1]

  # get latitude of each district
  lat_coord <- shp %>%
    filter(DISTRICT == distr) %>%
    st_coordinates(distr_lat) %>%
    data.frame() %>%
    dplyr::select(Y) %>%
    summarize(lat_avg = mean(Y))

  # totaling precip volume for each cell in a district on each day
  tmax_avg <- tmp %>%
    mutate(
      month = lubridate::month(date),
      year = lubridate::year(date)
    ) %>%
    group_by(district, year, month) %>%
    summarise(
      tmax_avg  = mean(prcp)
    ) %>%
    mutate(tmax_avg = tmax_avg - 273.15)

  # add date column
  tmax_avg$date <- zoo::as.yearmon(paste(tmax_avg$year, tmax_avg$month), "%Y %m")
  tmax_avg$date <- zoo::as.Date(tmax_avg$date)

  tmax_avg <- tmax_avg %>%
    mutate(
      YEAR = as.integer(year),
      MONTH = as.integer(month),
      TMED = tmax_avg
    ) %>%
    ungroup() %>%
    dplyr::select(YEAR, MONTH, TMED)

  et <- thornthwaite(Tave = tmax_avg$TMED, lat_coord$lat_avg) %>%
    tsbox::ts_data.frame() %>%
    mutate(district = distr)

  et_lst[[i]] <- et
}
et_lst[5]
# first dataframe in list
tmp <- data_list[[1]]

# district number of list
distr <- tmp$district[1]

# get latitude of each district
lat_coord <- shp %>%
  filter(DISTRICT == distr) %>%
  st_coordinates(distr_lat) %>%
  data.frame() %>%
  dplyr::select(Y) %>%
  summarize(lat_avg = mean(Y))

# totaling precip volume for each cell in a district on each day
tmax_avg <- tmp %>%
  mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) %>%
  group_by(district, date) %>%
  summarize(
      tmax_avg = mean(prcp)
  ) %>%
  ungroup() %>%
  # filter(district == 6) %>%
  mutate(
    month = lubridate::month(date),
    year = lubridate::year(date)
  ) %>%
  group_by(district, year, month) %>%
  summarise(
    tmax_avg  = mean(tmax_avg)
    # prcp_log = log(prcp_mean_month) # log transformation produces a normal distribution, haven't done anything more with this
  ) %>%
  mutate(tmax_avg = tmax_avg - 273.15)

# add date column
tmax_avg$date <- zoo::as.yearmon(paste(tmax_avg$year, tmax_avg$month), "%Y %m")
tmax_avg$date <- zoo::as.Date(tmax_avg$date)

tmax_avg <- tmax_avg %>%
  mutate(
    YEAR = as.integer(year),
    MONTH = as.integer(month),
    TMED = tmax_avg
  ) %>%
  ungroup() %>%
  dplyr::select(YEAR, MONTH, TMED)

et <- thornthwaite(Tave = tmax_avg$TMED, lat_coord$lat_avg) %>%
  tsbox::ts_data.frame()

et_lst[[i]] <- et
}
  # data.frame() %>%
  # mutate(district = distr)


# save each tibble in data list to an RDS in file system
lapply(X = seq_len(length(data_list)), FUN = function(x)
  saveRDS(data_list[[x]], file = paste0("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/spi/co/prcp_district_", names(data_list)[x], "_co.rds")
  ))
precip_files <- list.files("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/spi/co", pattern = "prcp_district")

temp <- temp %>%
  mutate(
    YEAR = as.integer(year),
    MONTH = as.integer(month),
    TMED = tavg_mean
  ) %>%
  ungroup() %>%
  dplyr::select(YEAR, MONTH, TMED)
class(wichita)
data(wichita)
attach(wichita)
names(wichita)

lat <- data.frame(id = 1:397, lat = 40)
thornthwaite(Tave = temp$TMED, 37.6475)
SPEI::thornthwaite(Tave = temp, lat = 40)
mean_files <- list.files("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/spi/co/", pattern = "prcp_district")

t <- mean_files[5]
stringr::str_sub(t, 6, -1)

r <- readRDS(paste0("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/spi/", t))

for (i in mean_files) {
  prcp <- readRDS(paste0("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/all/non_spatial/spi/", i))

  # Prep monthly mean precip data for Standardized Precip Index using SPEI package
  spi <- prcp %>%
    rename(
      YEAR = year,
      MONTH = month,
      PRCP = prcp
    ) %>%
    ungroup() %>%
    dplyr::select(YEAR, MONTH, PRCP)

  # add a date column from YEAR and MONTH cols
  spi$date <- zoo::as.yearmon(paste(spi$YEAR, spi$MONTH), "%Y %m")

  # make YEAR & MONTH columns integers, required for SPI function
  spi$YEAR <- as.integer(spi$YEAR)
  spi$MONTH <- as.integer(spi$MONTH)

  # SPI dataframe a timeseries
  prcp_ts <- ts(spi, end=c(2013,1), frequency=12)

  # calculate SPI - 1 month time scale
  spi_1 <- SPEI::spi(prcp_ts[,3], 1)
  spi_1 <- tsbox::ts_data.frame(spi_1$fitted) %>%
    mutate(district = prcp$district)

  # calculate SPI - 3 month time scale
  spi_3 <- SPEI::spi(prcp_ts[,3], 3)
  spi_3 <- tsbox::ts_data.frame(spi_3$fitted) %>%
    mutate(district = prcp$district)

  # calculate SPI - 6 month time scale
  spi_6 <- SPEI::spi(prcp_ts[,3], 6)
  spi_6 <- tsbox::ts_data.frame(spi_6$fitted) %>%
    mutate(district = prcp$district)

  # calculate SPI - 9 month time scale
  spi_9 <- SPEI::spi(prcp_ts[,3], 9)
  spi_9 <- tsbox::ts_data.frame(spi_9$fitted) %>%
    mutate(district = prcp$district)

  # calculate SPI - 1 month time scale
  spi_12 <- SPEI::spi(prcp_ts[,3], 12)
  spi_12 <- tsbox::ts_data.frame(spi_12$fitted) %>%
    mutate(district = prcp$district)


