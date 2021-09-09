library(raster)
library(tidyverse)
library(lubridate)
library(climateR)
library(sf)
library(mapview)
library(elevatr)

source("C:/Users/angus/OneDrive/Desktop/github/drought_data_processing/utils/data_utils.R")
source("misc/find_google_drive.R")


# -------- Inputs --------
distr_num <- c(1,  2,  4,  5,  6,  7,  8,  9,  23, 64, 80, 36, 37,  38, 39, 45, 50, 51, 52, 53, 70, 72)

# Major river basin
basin = c("Colorado", "South Platte")
getTerraClim()
# specify the time domain of the data query
start_date = "1970-10-01"
end_date = "1970-12-01"
# end_date = "2013-09-01"
time = seq(ymd(start_date), ymd(end_date), by = '1 day')
short <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/model_data/final/shortages_by_right_all.rds")
# district shapefile path
shp_path = "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/Water_Districts/Water_Districts.shp"

# load shapefiles as spatial polygon object
shp <- st_read(paste0(shp_path), quiet = T) %>%
  filter(DISTRICT %in% distr_num) %>%
  st_transform(4326) %>%
  st_cast("MULTIPOLYGON")

# ---------------- load LOCA data from climateR -----------------------
terra_lst <- list()
years <- 1970:2013
# READ IN JOINED DITCH SHORTAGES BY RIGHT DATA
ditch_short <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/model_data/final/shortages_by_right_all.rds")

rm(ditch_short, short)

library(progress)

pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(years),
                       # total = 2,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)
climateR::param_meta$loca
params <- c("tmin", "tmax")

for (i in years){
  pb$tick()
  start <- as.Date(paste0(i, "-01-01"))
  end <- as.Date(paste0(i, "-12-31"))
  doParallel::stopImplicitCluster()
  terra <- get_terra(districts = distr_num,
                   param = "tmin",
                   start_date = start,
                   end_date = end,
                   shp_path = shp_path)

  # terra <- add_elev(terra, subdate = start)

  terra <- terra %>%
    # filter(elevation <= 2800) %>%
    group_by(district, date) %>%
    summarise(tmin = mean(value))

  terra_lst[[i]] <- terra
  terra_lst <- terra_lst %>% discard(is.null)
}
    climateR::param_meta$terraclim
tmin <- bind_rows(terra_lst)
saveRDS(tmin, "C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/terraclim/final/tmin_monthly_terraclim.rds")

# ---- DITCH SHORTAGES BY RIGHT DATA ----
ditch_short <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/model_data/final/shortages_by_right_all.rds")  %>%
  filter(district != 38) %>%
  mutate(
    demand = demand + 0.1,
    supply = supply + 0.1,
    admin = as.numeric(admin)
  ) %>%
  dplyr::select(date, year, month, district, node_id,
                demand, supply, short, supply_dir, short_dir,
                priority, admin, decree, decree_af, name, id)

# --------- ADMIN YEAR V2 BY INDIVIDUAL ADMIN NUMBER -------------

# Shortages for each admin number in each district for each timestamp
by_admin3 <- ditch_short %>%
  group_by(date, district, node_id) %>%
  # filter(date >= "1980-01-01") %>%
  mutate(
    demand_tot = sum(demand, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(date, district, admin) %>%
  summarise(
    # admin              = mean(admin, na.rm = T),
    demand             = sum(demand, na.rm = T),
    demand_tot         = sum(demand_tot, na.rm = T),
    supply             = sum(supply, na.rm = T),
    supply_dir         = sum(supply_dir, na.rm = T),
    short              = sum(short, na.rm = T),
    short_dir          = sum(short_dir, na.rm = T)
  ) %>%
  ungroup()

by_admin2 <- by_admin3 %>%
  # filter(district != "36") %>%
  mutate(
    year = lubridate::year(date)
  ) %>%
  group_by(year, district, admin) %>%
  summarise(
    short             = round(sum(short, na.rm = T), 3),
    short_dir         = round(sum(short_dir, na.rm = T), 3),
    demand            = round(sum(demand, na.rm = T), 3),
    demand_tot        = round(sum(demand_tot, na.rm = T), 3),
    supply            = round(sum(supply, na.rm = T), 3),
    supply_dir        = round(sum(supply_dir, na.rm = T), 3),
  ) %>%
  ungroup() %>%
  mutate(
    admin             = as.numeric(admin),
    short_norm        = round(100*(short/demand), 3),
    short_dir_norm    = round(100*(short_dir/demand), 3),
    aug_supply        = round((supply - supply_dir), 3),
    aug_supply_norm   = round(100*(aug_supply/supply), 3)
  )
tmp <- by_admin2 %>% filter(year >= 1980, district != "38")
# REMOVE 0 FROM BEGINNING OF DISTRICT COLUMN IN ADMIN_YEAR DATA
by_admin2$district <- as.factor(str_replace(by_admin2$district, "^0+" ,""))

# get unique basins to join w/ admin data
basins <- dplyr::select(mod_df, district, basin) %>%
  unique()

# JOIN DISTRICTS W/ BASIN NAMES
by_admin2 <- left_join(by_admin2, basins, by = "district")
get_terra <- function(districts, param, start_date, end_date = NULL,
                      shp_path) {

  shp <- sf::st_read(shp_path, quiet = TRUE) %>%
    dplyr::filter(DISTRICT %in% !!districts) %>%
    # dplyr::filter(BASIN %in% !!basin) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")

  terra <- climateR::getTerraClim(
    AOI       = sf::st_transform(shp, 4326),
    param     = param,
    startDate = start_date,
    endDate   = end_date
  ) %>%
    stack()


  terra <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(terra, shp[x, ])
    }
  )


  stack_list     <- lapply(X = terra, FUN = raster::stack)       # stack list of masked rasterstacks
  district_names <- paste0(shp$DISTRICT)                        # district number
  stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks

  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_terra <- lapply(X = stack_list, FUN = tidy_terra_raster)

  tidy_terra <- lapply(
    X = names(tidy_terra),
    FUN = function(x) {
      dplyr::mutate(
        tidy_terra[[x]],
        district = x
      )
    }
  ) %>%
    bind_rows()
}

tidy_terra_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        paste0(stringr::str_sub(colnames(.)[-(1:2)], start = 2L), ".1")
      )) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}
