library(raster)
library(tidyverse)
library(lubridate)
library(climateR)
library(sf)
library(mapview)
library(foreach)
library(doParallel)
library(elevatr)

tidy_bcca_raster <- function(raster) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L, end = 11))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}
get_maca <- function(district, param, start_date, end_date = NULL,
                     shp_path) {
  # load shapefiles as spatial polygon object
  shp <- sf::st_read(shp_path, quiet = TRUE) %>%
    dplyr::filter(DISTRICT %in% !!district) %>%
    # dplyr::filter(BASIN %in% !!basin) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")
  # dplyr::filter(DISTRICT %in% c(64))

  maca <- climateR::getMACA(
    shp,
    param     = param,
    startDate = start_date,
    endDate   = end_date,
    scenario = "rcp45"
    # param     = "tmax",
    # startDate = "2091-01-01",
    # endDate   = "2091-01-06"
    # timeRes   = "monthly"
  )


  maca <- raster::stack(maca)

  # mask stacks to districts
  maca_mask <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(maca, shp[x, ])
    }
  )


  stack_list     <- lapply(X = maca_mask, FUN = raster::stack)       # stack list of masked rasterstacks
  district_names <- paste0(shp$DISTRICT)                        # district number
  stack_list     <- setNames(stack_list, nm = district_names)

  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_maca <- lapply(X = stack_list, FUN = tidy_bcca_raster)

  tidy_maca <- lapply(
    X = names(tidy_maca),
    FUN = function(x) {
      dplyr::mutate(
        tidy_maca[[x]],
        district = x
      )
    }
  ) %>%
    bind_rows()
}
# add elevation data to climate lat/lon data
add_elev <- function(df, subdate) {
  clim <- df %>%
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) %>%
    dplyr::filter(date == subdate)

  clim2 <- clim %>%
    dplyr::select(x = lon, y = lat)

  pt_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  sp::coordinates(clim2) <- ~x+y
  sp::gridded(clim2) <- TRUE

  clim_sp <- sp::SpatialPoints(
    sp::coordinates(clim2),
    proj4string = sp::CRS(pt_prj)
  )

  elev <- elevatr::get_elev_point(
    locations = clim_sp,
    prj = pt_prj,
    src = "aws"
  ) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(
      lon = st_coordinates(.)[,1],
      lat = st_coordinates(.)[,2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-elev_units)

  clim_join <- dplyr::left_join(df, elev, by = c("lon", "lat"))
}

# **************** INPUT PARAMS ************************
# ======================================================


# district shapefile path

distr2 <- c(28, 29, 30, 31, 32, 33, 34, 40, 41, 42, 43, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 68, 69, 71, 73, 77, 78)

# district shapefile path
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"
shp <- sf::st_read(shp_path, quiet = TRUE) %>%
  filter(DISTRICT %in% distr2) %>%
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON")

districts <- unique(shp$DISTRICT)

# rm(start, end, prcp, maca, maca_lst, pb, stack_list, tidy_maca, maca_mask, district_names)
maca_lst <- list()
# years <- 1950:2099
years <- 1950:2099
# library(progress)
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(years),
                       # total = 2,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)
climateR::param_meta$maca
for (i in years){
  pb$tick()
  # start <- as.Date(paste0(1950, "-01-01"))
  # end <- as.Date(paste0(1950, "-02-01"))
  start <- as.Date(paste0(i, "-01-01"))
  end <- as.Date(paste0(i, "-12-31"))
  doParallel::stopImplicitCluster()
  maca <- get_maca(district   = districts,
                   param      = "prcp",
                   start_date = start,
                   end_date   = end,
                   shp_path   = shp_path)

  maca <- add_elev(maca, subdate = start)

  prcp <- maca %>%
    filter(elevation <= 2800) %>%
    group_by(district, date) %>%
    summarise(prcp = mean(value))

  maca_lst[[i]] <- prcp
  maca_lst <- maca_lst %>% discard(is.null)
}
install.packages("raster")
maca_prcp <- bind_rows(maca_lst)
saveRDS(maca_prcp, "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/gm_sj_ym_wm/maca_prcp_gm_sj_ym_wm_1950_2035.rds")
tmp <- maca_prcp %>%
  mutate(year = lubridate::year(date)) %>% filter(year >= 2035)
unique(tmp$district)
unique(maca_prcp$district)










