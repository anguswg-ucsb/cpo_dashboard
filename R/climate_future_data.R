# Apply a Multiple linear Regression model to annual district level StateMod data

# Number of districts = 50
# Each district has 32 usable years of data on record
# individual MLR models are built for each district (50 districts, 50 models)
# Dependent variable  = Annual Direct Flow Shortage as a percent of total Demand (short_dir_pct_dem)
# Modeling steps:
# 1. Log transform short_dir_pct_dem

# 2. MLR model: short_dir_pct_dem = all climate variables

# 3. Apply stepwise regression:
# Stepwise regression: forward - Build regression model from set candidate predictor variables by
# entering predictors based on p values, in a stepwise manner until  no variable left to enter

# Stepwise regression: Both direction - Build regression model from set of candidate predictor variables by
# entering & removing predictors based on p values, in a stepwise manner until no variable left to enter or remove

# 4. Variable Inflation Factor to remove predictors

# 5.
remove(list = ls())
library(tidyverse)
library(olsrr)
library(car)
library(logger)
library(leaflet)
library(RColorBrewer)
library(viridis)
library(sf)

# data utils functions
source("data_utils.R")

shp_path <- "data/spatial/water_districts.shp"

# statemod_year <- readRDS("data/final/statemod_climate_year.rds") %>%
#   filter(year > 1980, year < 2013) %>%
#   dplyr::select(
#     year, district, short_dir_pct_dem, prcp, tavg, pdsi, spi1, spi3, spi6, spi9,
#     spi12, eddi1, eddi3, eddi6, eddi9, eddi12, swe, swe_max, soilm)

model_data      <- readRDS("statemod_climate_year2.rds") %>%
  dplyr::select(
    year, district,
    demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem,
    # prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12
    prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12
    )

climate_models     <- readRDS("climate_future_year.rds") %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year <= 2099) %>%
  dplyr::select(district, year, prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12)

samples_future     <- readRDS("climate_future_samples.rds")

samples_historic   <- readRDS("climate_historic_samples.rds")

present_climate    <- readRDS("climate_present_year.rds") %>%
  dplyr::select(district, year, prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12)
# dplyr::select(-swe_max)


model_join <- model_data %>%
  dplyr::select(
    year, district,
    # prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12
    prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12
  ) %>%
  mutate(
    district = as.character(district)
  ) %>%
  dplyr::relocate(district, year)

future_join <- climate_models %>%
  filter(year >= 2020, district %in% unique(model_join$district))

present_join <- present_climate %>%
  filter(year >=  2013, year <=  2019, district %in% unique(model_join$district))

 # unique(model_join$district)  %in% unique(present_join$district)

climate_all <- bind_rows(model_join, present_join, future_join)

# save annual data from 1970 - 2099
saveRDS(climate_all, "future_climate_timeseries.rds")

# stm_month <- stm %>%
#   filter(district == "72") %>%
#   mutate(variance = var(short_dir_norm))

statemod_district <- model_data %>%
  group_by(district) %>%
  group_split()

# statemod_district <- statemod_district[1:2]

district_mlr_lst    <- list()
district_table_lst  <- list()
district_vif_lst    <- list()
impute_mean <- function(x) replace(
  x,
  is.na(x),
  mean(x, na.rm = TRUE)
  )


# statemod_district2 <- statemod_district[1:3]
# i = 42
for (i in 1:length(statemod_district)) {

  # annual district data
  df    <- bind_rows(statemod_district[[i]])

  # district text
  distr <- df$district[1]

  logger::log_info("Running MLR --- district {distr}")

  # dependent variable names to iterate through
  dependent_vars <- df %>%
    # dplyr::select(demand, supply, short, short_dir_pct_dem) %>%
    dplyr::select(demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem) %>%
    names()

  # independent_vars <- df %>%
  #   # dplyr::select(prcp, tavg, pdsi) %>%
  #   dplyr::select(prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12) %>%
  #   names()

  mlr_lst         <- list()
  table_lst       <- list()
  vif_lst         <- list()


  for (k in 1:length(dependent_vars)) {

    logger::log_info("MLR for {dependent_vars[k]} --- district {distr}")

    # metric_lst <- list()
    # fit_lst    <- list()

    # select columns for model
    mlr_df <- df %>%
      ungroup() %>%
      dplyr::select(dependent_vars[k], prcp:spi12)
      # mutate(
      #   short_dir_pct_dem  =case_when(
      #     short_dir_pct_dem == 0 ~ mean(short_dir_pct_dem),
      #       TRUE ~ short_dir_pct_dem
      #     ),
      #   short_dir_pct_dem = log(short_dir_pct_dem)
      # )

    mlr_df[,1] <- sqrt(mlr_df[,1])

    # mlr_df[,1] <- log(mlr_df[,1])
    #
    # # replace Infinite w/ 0
    # is.na(mlr_df) <- sapply(mlr_df, is.infinite)
    # mlr_df[is.na(mlr_df)] <- 0


    mlr_model <- lm(
      paste0(dependent_vars[k], "~."),
      data = mlr_df
    ) %>%
      olsrr::ols_step_forward_p()

    if(length(mlr_model$predictors) > 1) {

      # Calculate VIF
      vf <- car::vif(mlr_model$model)

      # VIF - remove variables w/ VIF > 5
      vf_df <- data.frame(vif = vf) %>%
        rownames_to_column() %>%
        arrange(vif) %>%
        slice(n = 1:3)
      # filter(vif <= 5)

      log_info("selecting {vf_df$rowname}")

      # step wise regression
      mlr_step <- lm(
        as.formula(paste(dependent_vars[k], paste(vf_df$rowname, collapse=" + "), sep=" ~ ")),
        data = mlr_df
        )


      # Future MACA data for individual district
      climate_proj <- climate_models %>%
        filter(district == distr, year >=  2020) %>%
        mutate(across(where(is.numeric), round, 3))
      # setNames(c("year", "climate_var"))

      # predict future shorts past 2021
      pred_df <- data.frame(
        year   = 2020:2099,
        fitted = predict(mlr_step, climate_proj)
      ) %>%
        mutate(
          prediction = fitted**2
          # prediction          = exp(fitted)
        ) %>%
        mutate(across(where(is.numeric), round, 2))

      # 2013-2020 Observed data for individual district
      current_data <- present_climate %>%
        filter(district == distr, year >=  2013, year <=  2019) %>%
        dplyr::select(-year, -district) %>%
        mutate(across(where(is.numeric), round, 3))

      # predict current shorts 2013-2020, apply model to observed data from 2013 - 2020
      pred_current_df <- data.frame(
        year   = 2013:2019,
        fitted = predict(mlr_step, current_data)
      ) %>%
        mutate(
          prediction = fitted**2
          # prediction          = exp(fitted)
        ) %>%
        mutate(across(where(is.numeric), round, 2))

      # historic impacts
      impact_historic <- df %>%
        # dplyr::select(year, futureDepVar()) %>%
        dplyr::select(year, dependent_vars[k]) %>%
        mutate(
          year    = as.numeric(as.character(year)),
          source  = "historic"
        ) %>%
        setNames(c("year", "impact", "source"))

      # predicted current impacts
      impact_current <- pred_current_df %>%
        # filter(year > 2020) %>%
        dplyr::select(year, prediction) %>%
        mutate(source = "current") %>%
        setNames(c("year", "impact", "source"))

      # predicted future impacts
      impact_projected <- pred_df %>%
        filter(year >= 2020) %>%
        dplyr::select(year, prediction) %>%
        mutate(source = "projected") %>%
        setNames(c("year", "impact", "source"))

      # join historic impacts data w/ predicted impacts
      impact_ts <- bind_rows(impact_historic, impact_current, impact_projected)

      impact_ts <- impact_ts %>%
        mutate(
          district = distr,
          var      = dependent_vars[k]
               ) %>%
        dplyr::relocate(district, var)

      # ---- Select predictors from data ----
      # hist_predictors <- df %>%
      #   dplyr::select(year, vf_df$rowname) %>%
      #   # dplyr::select(year, lm_run$predictors[1:2]) %>%
      #   mutate(across(where(is.numeric), round, 2)) %>%
      #   mutate(year = as.numeric(as.character(year)))
      #
      # current_predictors <- present_climate  %>%
      #   filter(district == distr, year >= 2013,  year <=  2019) %>%
      #   # filter(district == 64, year >= 2013,  year <=  2019) %>%
      #   dplyr::select(year, vf_df$rowname) %>%
      #   mutate(across(where(is.numeric), round, 2)) %>%
      #   mutate(year = as.numeric(as.character(year)))
      #
      # future_predictors <- climate_models %>%
      #   filter(district == distr, year >= 2020) %>%
      #   # filter(district == 64, year >= 2020) %>%
      #   dplyr::select(year, vf_df$rowname) %>%
      #   mutate(across(where(is.numeric), round, 2))
      #
      # predictor_ts <- bind_rows(hist_predictors, current_predictors, future_predictors)
      #
      # predictor_ts <- predictor_ts %>%
      #   mutate(district = distr) %>%
      #   dplyr::relocate(district)

      # Metrics glance at model
      metrics  <- mlr_step %>%
        glance() %>%
        dplyr::select(r_squared = r.squared, p_value = p.value) %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        pivot_longer(cols = everything(), names_to = "term", values_to = "estimate")

      # tidy model
      tidied   <- mlr_step %>%
        tidy() %>%
        dplyr::select(term, estimate) %>%
        mutate(across(where(is.numeric), round, 3))
      # pivot_wider(names_from = "term", values_from = "estimate")

      metrics_table <- bind_rows(tidied, metrics) %>%
        mutate(
          clean_name = case_when(
            term == "(Intercept)" ~ "Intercept",
            term == "r_squared" ~ "R2",
            term == "p_value" ~ "p-value",
            term == "prcp" ~ "Precipitation",
            term == "pdsi" ~ "PDSI",
            term == "spi1" ~ "SPI 1 month",
            term == "spi3" ~ "SPI 3 month",
            term == "spi6" ~ "SPI 6 month",
            term == "spi9" ~ "SPI 9 month",
            term == "spi12" ~ "SPI 12 month",
            term == "tavg" ~ "Average Temperature",
            term == "tmax" ~ "Maximum Temperature",
            term == "tmin" ~ "Minimum Temperature",
            term == "pet" ~ "PET"
          ),
          district = distr,
          dep_var      = dependent_vars[k]
        ) %>%
        dplyr::relocate(district, dep_var)

      # metrics_table <- bind_cols(tidied, metrics)
      #
      # # recode names
      # clean_names <- names(rename_all(metrics_table, recode,
      #                                 "(Intercept)"     = "Intercept",
      #                                 r_squared         = "R2",
      #                                 p_value           = "p-value",
      #                                 prcp              = "Precipitation",
      #                                 pdsi              = "PDSI",
      #                                 spi1              = "SPI 1 month",
      #                                 spi3              = "SPI 3 month",
      #                                 spi6              = "SPI 6 month",
      #                                 spi9              = "SPI 9 month",
      #                                 spi12             = "SPI 12 month",
      #                                 tavg              = "Average Temperature",
      #                                 pet               = "PET"
      #                                 )
      #
      #
      # )
      #
      # metrics_table <-  metrics_table %>%
      #   setNames(clean_names) %>%
      #   mutate(
      #     district = distr,
      #     var      = dependent_vars[k]
      #   ) %>%
      #   dplyr::relocate(district, var)


      # vif dataframe
      vf_df          <- data.frame(vif = vf) %>%
        rownames_to_column() %>%
        arrange(vif) %>%
        mutate(
          district = distr,
          var      = dependent_vars[k]
               ) %>%
        setNames(c("variable", "vif", "district", "dep_var")) %>%
        dplyr::relocate(district, dep_var) %>%
        as_tibble()

      mlr_lst[[k]]         <- impact_ts
      vif_lst[[k]]         <- vf_df
      table_lst[[k]]       <- metrics_table

     } else {

        log_info("Stepwise regression yeilds single predictor variable - {mlr_model$predictors}")

        # step wise regression
        mlr_step <- lm(
          as.formula(paste(dependent_vars[k], paste(mlr_model$predictors, collapse=" + "), sep=" ~ ")),
          data = mlr_df
          )

        # Future MACA data for individual district
        climate_proj <- climate_models %>%
          filter(district == distr, year >=  2020) %>%
          mutate(across(where(is.numeric), round, 3))
        # setNames(c("year", "climate_var"))

        # predict future shorts past 2021
        pred_df <- data.frame(
          year   = 2020:2099,
          fitted = predict(mlr_step, climate_proj)
        ) %>%
          mutate(
            prediction          = fitted**2
            # prediction          = exp(fitted)
          ) %>%
          mutate(across(where(is.numeric), round, 2))

        #         # 2013-2020 Observed data for individual district
        current_data <- present_climate %>%
          filter(district == distr, year >=  2013, year <=  2019) %>%
          dplyr::select(-year, -district) %>%
          mutate(across(where(is.numeric), round, 3))

        # predict current shorts 2013-2020, apply model to observed data from 2013 - 2020
        pred_current_df <- data.frame(
          year   = 2013:2019,
          fitted = predict(mlr_step, current_data)
        ) %>%
          mutate(
            prediction          = fitted**2
            # prediction          = exp(fitted)
          ) %>%
          mutate(across(where(is.numeric), round, 2))

        # historic impacts
        impact_historic <- df %>%
          # dplyr::select(year, futureDepVar()) %>%
          dplyr::select(year, dependent_vars[k]) %>%
          mutate(
            year    = as.numeric(as.character(year)),
            source  = "historic"
          ) %>%
          setNames(c("year", "impact", "source"))

        # predicted current impacts
        impact_current <- pred_current_df %>%
          # filter(year > 2020) %>%
          dplyr::select(year, prediction) %>%
          mutate(source = "current") %>%
          setNames(c("year", "impact", "source"))

        # predicted future impacts
        impact_projected <- pred_df %>%
          filter(year >= 2020) %>%
          dplyr::select(year, prediction) %>%
          mutate(source = "projected") %>%
          setNames(c("year", "impact", "source"))

        # join historic impacts data w/ predicted impacts
        impact_ts <- bind_rows(impact_historic, impact_current, impact_projected)

        impact_ts <- impact_ts %>%
          mutate(
            district = distr,
            var      = dependent_vars[k]
          ) %>%
          dplyr::relocate(district, var)

        # Metrics glance at model
        metrics  <- mlr_step %>%
          glance() %>%
          dplyr::select(r_squared = r.squared, p_value = p.value) %>%
          mutate(across(where(is.numeric), round, 3)) %>%
          pivot_longer(cols = everything(), names_to = "term", values_to = "estimate")

        # tidy model
        tidied   <- mlr_step %>%
          tidy() %>%
          dplyr::select(term, estimate) %>%
          mutate(across(where(is.numeric), round, 3))
          # pivot_wider(names_from = "term", values_from = "estimate")

        metrics_table <- bind_rows(tidied, metrics) %>%
          mutate(
            clean_name = case_when(
              term == "(Intercept)" ~ "Intercept",
              term == "r_squared" ~ "R2",
              term == "p_value" ~ "p-value",
              term == "prcp" ~ "Precipitation",
              term == "pdsi" ~ "PDSI",
              term == "spi1" ~ "SPI 1 month",
              term == "spi3" ~ "SPI 3 month",
              term == "spi6" ~ "SPI 6 month",
              term == "spi9" ~ "SPI 9 month",
              term == "spi12" ~ "SPI 12 month",
              term == "tavg" ~ "Average Temperature",
              term == "tmax" ~ "Maximum Temperature",
              term == "tmin" ~ "Minimum Temperature",
              term == "pet" ~ "PET"
              ),
            district = distr,
            dep_var      = dependent_vars[k]
            ) %>%
          dplyr::relocate(district, dep_var)

        # metrics_long %>%
        #   dplyr::select(-term) %>%
        #   pivot_wider(names_from = "clean_name", values_from = "estimate")


        # metrics_table <- bind_cols(tidied, metrics)

        # recode names
        # clean_names <- names(rename_all(metrics_table, recode,
        #                                 "(Intercept)"     = "Intercept",
        #                                 r_squared         = "R2",
        #                                 p_value           = "p-value",
        #                                 prcp              = "Precipitation",
        #                                 pdsi              = "PDSI",
        #                                 spi1              = "SPI 1 month",
        #                                 spi3              = "SPI 3 month",
        #                                 spi6              = "SPI 6 month",
        #                                 spi9              = "SPI 9 month",
        #                                 spi12             = "SPI 12 month",
        #                                 tavg              = "Average Temperature",
        #                                 pet               = "PET"
        # )
        #
        #
        # )
        #
        # metrics_table <-  metrics_table %>%
        #   setNames(clean_names) %>%
        #   mutate(
        #     district = distr,
        #     var      = dependent_vars[k]
        #   ) %>%
        #   dplyr::relocate(district, var)


        # vif dataframe
        vf_df          <- data.frame(
          district     = distr,
          dep_var      = dependent_vars[k],
          variable     = mlr_model$predictors,
          vif          = NA_real_
        )  %>%
          as_tibble()

        mlr_lst[[k]]         <- impact_ts
        vif_lst[[k]]         <- vf_df
        table_lst[[k]]       <- metrics_table
      }
  }

  district_mlr_lst[[i]]   <- bind_rows(mlr_lst)
  district_table_lst[[i]] <- bind_rows(table_lst)
  district_vif_lst[[i]]   <- bind_rows(vif_lst)

  # rm(r2, mlr_vfit, log_trans, lm_step, year_variance, year_sd, df, vf, vf_df)
}

# fitted MLR futre
mlr_future_fit   <- bind_rows(district_mlr_lst)

# replace % of demand value over 100 with 100
mlr_future_fit <- mlr_future_fit %>%
  mutate(
    impact = case_when(
      impact > 100 & var %in% c("short_pct_dem", "short_dir_pct_dem") ~ 100,
      TRUE   ~ impact
    )
  )

# ggplot() +
#   geom_col(data = tmp, aes(x = year, y = impact2)) +
#   facet_wrap(~district) +
#   scale_x_continuous(breaks = seq(1970, 2100, 10))

table_data       <- bind_rows(district_table_lst)
predictor_data   <- bind_rows(district_vif_lst)

# save LM results
saveRDS(mlr_future_fit, "mlr_future_fit.rds")
saveRDS(table_data, "mlr_future_metrics.rds")
saveRDS(predictor_data, "mlr_future_predictors.rds")


# Univarieta linear regression for future climate
# model_data      <- readRDS("statemod_climate_year2.rds") %>%
# dplyr::select(
#   year, district,
#   demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem,
#   prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi9, eddi12, swe, swe_max, soilm)

# clean indicator names
indicator_lst <- data.frame(
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
  soilm             = "Soil moisture (mm)") %>%
  pivot_longer(cols = everything())

# clean impacts names
impacts_lst <- list(
  "Total shortage (AF)"                 = "short",
  "Total shortage (% of demand)"        = "short_pct_dem",
  "Direct shortage (AF)"                = "short_dir",
  "Direct shortage (% of demand)"       = "short_dir_pct_dem",
  "Demand (AF)"                         = "demand",
  "Supply (AF)"                         = "supply",
  "Direct supply (AF)"                  = "supply_dir",
  "Augmented Supply (AF)"               = "aug_supply",
  "Augmented Supply (% of demand)"      = "aug_supply_pct_dem"
  # "Natural flows (AF)"                  = "af_total"
) %>%
  bind_rows() %>%
  pivot_longer(cols = everything())


model_data      <- readRDS("statemod_climate_year2.rds") %>%
  dplyr::select(
    year, district,
    demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem,
    # prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12
    prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12
  )

climate_models     <- readRDS("climate_future_year.rds") %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year <= 2099) %>%
  dplyr::select(district, year, prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12)

samples_future     <- readRDS("climate_future_samples.rds")

samples_historic   <- readRDS("climate_historic_samples.rds")

present_climate    <- readRDS("climate_present_year.rds") %>%
  dplyr::select(district, year, prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12)

# dplyr::select(-swe_max)
statemod_district <- model_data %>%
  group_by(district) %>%
  group_split()


# statemod_district <- statemod_district[1:2]


district_results_lst    <- list()


# i = 5
# k = 8
# z = 1

for (i in 1:length(statemod_district)) {

  # single districts annual data
  df <- statemod_district[[i]]

  # # district text
  distr <- df$district[1]
  #
  # Log message
  logger::log_info("Generating models --- District: {distr}")

  # dependent variable names to iterate through
  dependent_vars <- df %>%
    # dplyr::select(demand, supply, short, short_dir_pct_dem) %>%
    dplyr::select(demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem) %>%
    names()

  independent_vars <- df %>%
    # dplyr::select(prcp, tavg, pdsi) %>%
    dplyr::select(prcp, tavg, tmax, tmin, pdsi, pet, spi1, spi3, spi6, spi9, spi12) %>%
    names()

  results_lst          <- list()

  for (k in 1:length(dependent_vars)) {

    fit_lst    <- list()

    for(z in 1:length(independent_vars)) {
      # Log message
      logger::log_info("Linear regression model for {dependent_vars[k]} vs. {independent_vars[z]}  --- District: {distr}")

      lm_data  <- df %>%
        dplyr::select(dependent_vars[k], independent_vars[z])

      lm_data[,1] <- sqrt(lm_data[,1])
      # lm_data[,1] <- log(lm_data[,1])

      # replace Infinite w/ 0
      # is.na(lm_data)          <- sapply(lm_data, is.infinite)
      # lm_data[is.na(lm_data)] <- 0

      lm_model <- lm(
        paste0(dependent_vars[k], "~", independent_vars[z]),
        data = lm_data
      )

      future_data <- climate_models %>%
        filter(district == distr) %>%
        dplyr::select(district, year, independent_vars[z])
      # filter(district == future_district(), year > 1950)



      # pred_future <- predict(
      #   lm_model,
      #   dplyr::select(future_data, prcp)
      # )

      # pred_future <- predict(lm_fit, dplyr::select(future_data,  futureClimVar()))
      pred_df     <- data.frame(
        year    = 1969:2099,
        fitted   =  predict(
          lm_model,
                        dplyr::select(future_data, independent_vars[z])
                        )
        # fitted = pred_future
      ) %>%
        left_join(
          future_data,
          # dplyr::select(future_data, -district),
          by = "year"
        ) %>%
        ungroup() %>%
        mutate(
          prediction          = fitted**2
          # prediction          = exp(fitted)
        ) %>%
        dplyr::relocate(district, year) %>%
        setNames(c("district", "year", "fitted", "independent_future", "prediction")) %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        mutate(
          dep_var = dependent_vars[k],
          ind_var = independent_vars[z]
               )

      fit_lst[[z]]     <- pred_df

      rm(intercept, coeff, model_metrics, tidied, metrics, indicator_label, impact_label, results)
    }

    results_lst[[k]] <-  bind_rows(fit_lst)

  }
  district_results_lst[[i]] <-  bind_rows(results_lst)
}

# Future fitted LM models
results_df <- bind_rows(district_results_lst)

# replace % of demand value over 100 with 100
results_df <- results_df %>%
  mutate(
    prediction2 = case_when(
      prediction > 100 & dep_var %in% c("short_pct_dem", "short_dir_pct_dem") ~ 100,
      TRUE   ~ prediction
    )
  )

# summary_df <- bind_rows(district_summary_lst)

# save LM results
saveRDS(results_df, "lm_future_fit.rds")
# saveRDS(summary_df, "lm_metrics.rds")
