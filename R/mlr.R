library(tidyverse)
library(olsrr)
library(car)
library(logger)
# Apply a Multiple linear Regression model to annual district level StateMod data

# Number of districts = 50
# Each district has 32 usable years of data on record
# individual MLR models are built for each district (50 districts, 50 models)
# Dependent variable  = Annual Direct Flow Shortage as a percent of total Demand (short_dir_pct_dem)
# Modeling steps:
# 1. square root transform short_dir_pct_dem

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

statemod_year  <- readRDS("statemod_climate_year.rds") %>%
  filter(year > 1980, year < 2013) %>%
  dplyr::select(
    year, district, short_dir_pct_dem, prcp, tavg, pdsi, spi1, spi3, spi6, spi9,
    spi12, eddi1, eddi3, eddi6, eddi9, eddi12, swe, swe_max, soilm
    )
# dplyr::select(-swe_max)


# stm_month <- stm %>%
#   filter(district == "72") %>%
#   mutate(variance = var(short_dir_norm))

statemod_district <- statemod_year %>%
  group_by(district) %>%
  group_split()

# tmp <- stm %>% filter(district == "9")
mlr_lst        <- list()
sensitivity_lst <- list()
vif_lst         <- list()

# statemod_district2 <- statemod_district[1:3]
# i = 5
for (i in 1:length(statemod_district)) {

  # annual district data
  df    <- bind_rows(statemod_district[[i]])

  # district text
  distr <- df$district[1]

  # calculate variance and standard deviation
  annual_short <- df %>%
    ungroup() %>%
    mutate(
      mean     = mean(short_dir_pct_dem),
      variance = var(short_dir_pct_dem),
      sd       = sd(short_dir_pct_dem)
    ) %>%
    dplyr::select(mean, variance, sd)

  # mean
  year_mean       <- annual_short$mean[1]

  # variance
  year_variance   <- annual_short$variance[1]

  # standard deviation
  year_sd         <- annual_short$sd[1]

  logger::log_info("Running MLR - district {distr}")

  # select columns for model
  df <- df %>%
    ungroup() %>%
    dplyr::select(short_dir_pct_dem, prcp:eddi12)

  # # subset and log transform data for MLR
  # log_trans <- df %>%
  #   ungroup() %>%
  #   mutate(
  #     short_dir_pct_dem = log10(short_dir_pct_dem)
  #   )

  if(sum(df$short_dir_pct_dem) <= 0) {

    log_info("District {distr} has no shortage data on record to use in model")

    # climate sensitivity dataframe
    sensitivity_df <- data.frame(
      district         = distr,
      var_sensitivity  =  NA_real_,
      sd_sensitivity   =  NA_real_,
      r2               =  NA_real_,
      mean             = year_mean,
      variance         = year_variance,
      sd               = year_sd
    )

    # vif dataframe
    vf_df          <- data.frame(
      district = distr,
      variable = NA,
      vif      = NA_real_
    )


    vif_lst[[i]]         <- vf_df
    sensitivity_lst[[i]] <- sensitivity_df

  } else {

  # subset and log transform data for MLR
  mlr_df <- df %>%
    ungroup() %>%
    mutate(
      short_dir_pct_dem = sqrt(short_dir_pct_dem)
      # short_dir_pct_dem = log(short_dir_pct_dem)
    )

    # MLR w/ VIF reduction + stepwise regression
    mlr_vfit <- lm(short_dir_pct_dem~., data = mlr_df) %>%
      # ols_step_both_p()
      ols_step_forward_p()

    if(length(mlr_vfit$predictors) > 1) {

      # Calculate VIF
      vf <- car::vif(mlr_vfit$model)

      # VIF - remove variables w/ VIF > 5
      vf_df <- data.frame(vif = vf) %>%
        rownames_to_column() %>%
        arrange(vif) %>%
        slice(n = 1:3)
      # filter(vif <= 5)

      log_info("selecting {vf_df$rowname}")

      # step wise regression
      lm_step <- lm(
        as.formula(paste("short_dir_pct_dem", paste(vf_df$rowname, collapse=" + "), sep=" ~ ")),
        data = mlr_df)

      # fitted   <- lm_step$fitted.values # fitted values
      observed <- lm_step$model %>%
        dplyr::select(short_dir_pct_dem) # Observed values

      # observed vs. fitted dataframe
      pred_df <- data.frame(
          fitted    = lm_step$fitted.values,
          observed  = observed$short_dir_pct_dem
          ) %>%
        mutate(
          # fitted    = exp(fitted),
          # observed  = exp(observed),
          fitted    = fitted**2,
          observed  = observed**2,
          district  = distr,
          variable  = "short_dir_pct_dem"
        ) %>%
        setNames(c("Fitted", "Direct flow shortage normalized", "district", "variable")) %>%
        dplyr::relocate(district, variable) %>%
        as_tibble()

      # R²
      r2 <-  summary(lm_step)$r.squared

      # climate sensitivity dataframe
      sensitivity_df <- data.frame(
        district         = distr,
        var_sensitivity  = r2*year_variance,
        sd_sensitivity   = r2*year_sd,
        r2               = r2,
        mean             = year_mean,
        variance         = year_variance,
        sd               = year_sd
      )

      # vif dataframe
      vf_df          <- data.frame(vif = vf) %>%
        rownames_to_column() %>%
        arrange(vif) %>%
        mutate(district = distr) %>%
        setNames(c("variable", "vif", "district")) %>%
        dplyr::relocate(district) %>%
        as_tibble()

      mlr_lst[[i]]         <- pred_df
      vif_lst[[i]]         <- vf_df
      sensitivity_lst[[i]] <- sensitivity_df

    } else {

      log_info("Stepwise regression yeilds single predictor variable - {mlr_vfit$predictors}")

      # step wise regression
      lm_step <- lm(
        as.formula(paste("short_dir_pct_dem", paste(mlr_vfit$predictors, collapse=" + "), sep=" ~ ")),
        data = mlr_df)

      # fitted   <- lm_step$fitted.values # fitted values

      observed <- lm_step$model %>%
        dplyr::select(short_dir_pct_dem) # Observed values

      # observed vs. fitted dataframe
      pred_df <- data.frame(
          fitted      = lm_step$fitted.values,
          observed    = observed$short_dir_pct_dem
          ) %>%
        mutate(
          # fitted    = exp(fitted),
          # observed  = exp(observed),
          fitted    = fitted**2,
          observed  = observed**2,
          district  = distr,
          variable  = "short_dir_pct_dem"
        ) %>%
        setNames(c("Fitted", "Direct flow shortage normalized", "district", "variable")) %>%
        dplyr::relocate(district, variable) %>%
        as_tibble()

      # R²
      r2 <- mlr_vfit$metrics$r2

      # climate sensitivity dataframe
      sensitivity_df <- data.frame(
        district         = distr,
        var_sensitivity  = r2*year_variance,
        sd_sensitivity   = r2*year_sd,
        r2               = r2,
        mean             = year_mean,
        variance         = year_variance,
        sd               = year_sd
      )

      vf_df          <- data.frame(
        district = distr,
        variable = mlr_vfit$predictors,
        vif      = NA_real_
      )  %>%
        as_tibble()

      mlr_lst[[i]]        <- pred_df
      vif_lst[[i]]         <- vf_df
      sensitivity_lst[[i]] <- sensitivity_df
    }
  }
  rm(r2, mlr_vfit, mlr_df, lm_step, year_variance, year_sd, df, vf, vf_df)
}

# Fitted MLR data
mlr_df     <- mlr_lst %>%
  bind_rows()  %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  ungroup()

# save fitted MLR data
saveRDS(mlr_df, "mlr_fit.rds")

# MLR results
metrics_df <- sensitivity_lst %>%
  bind_rows()  %>%
  na.omit() %>%
  mutate(
    var_sensitivity_log  = log10(var_sensitivity),
    sd_sensitivtiy_log   = log10(sd_sensitivity),
    var_sensitivity_norm = normalize(var_sensitivity),
    sd_sensitivity_norm  = normalize(sd_sensitivity),
    var_sensitivity_std  = abs(var_sensitivity - mean(var_sensitivity))/sd(var_sensitivity)
  ) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  ungroup()

# save MLR metrics
saveRDS(metrics_df, "mlr_metrics.rds")
# write.csv(metrics_df, "data/models/mlr/mlr_metrics.csv", row.names = F)

# variables after stepwise regresiion, before VIF removal
vif_df <- bind_rows(vif_lst)

# # Predictors selected after VIF
# predictors <- vif_df %>%
#   group_by(district) %>%
#   slice(1:3) %>%
#   ungroup()

# save MLR predictors
saveRDS(vif_df, "mlr_predictors.rds")

# ************************************************

# write.csv(vif_df, "data/models/mlr/mlr_predictors.csv", row.names = F)
data_path <- "C:/Users/angus/OneDrive/Desktop/github/cpo_data_processing/data/"

# extra swe data
stm <- read_csv("C:/Users/angus/OneDrive/Desktop/statemod_climate_data_monthly.csv") %>%
  dplyr::select(-1)
  # dplyr::select(date, district, swe_max, swe_cuml)

stm_year <- stm %>%
  mutate(year = lfstat::water_year(date)) %>%
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
      swe_max    = max(swe_max, na.rm = T),
      spi1       = mean(spi1, na.rm = T),
      spi3       = mean(spi3, na.rm = T),
      spi6       = mean(spi6, na.rm = T),
      spi9       = mean(spi9, na.rm = T),
      spi12      = mean(spi12, na.rm = T),
      eddi1      = mean(eddi1, na.rm = T),
      eddi3      = mean(eddi3, na.rm = T),
      eddi6      = mean(eddi6, na.rm = T),
      # eddi9      = mean(eddi9, na.rm = T),
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
    filter(year < 2013) %>%
    ungroup() %>%
    dplyr::relocate(district, year, demand, supply, supply_dir, short, short_dir, short_pct_dem, short_dir_pct_dem)

stm_month <- stm %>%
  filter(district == "72") %>%
  mutate(variance = var(short_dir_norm))

stm_district <- stm_year %>%
  group_by(district) %>%
  group_split()
tmp <- stm %>% filter(district == "9")

sensitivity_lst <- list()
vif_lst         <- list()
stm_district2 <- stm_district[1:6]
# i = 2
for (i in 1:length(stm_district2)) {

  # annual district data
  df    <- bind_rows(stm_district2[[i]])

  # district text
  distr <- df$district[1]

  # calculate variance and standard deviation
  annual_short <- df %>%
    ungroup() %>%
    mutate(
      variance = var(short_dir_pct_dem),
      sd       = sd(short_dir_pct_dem)
      ) %>%
    dplyr::select(variance, sd)

  # variance
  year_variance <- annual_short$variance[1]

  # standard deviation
  year_sd       <- annual_short$sd[1]

  logger::log_info("Running MLR - district {distr}")

  # select columns for model
  df <- df %>%
    ungroup() %>%
    dplyr::select(short_dir_pct_dem, prcp:eddi12)

  # # subset and log transform data for MLR
  # log_trans <- df %>%
  #   ungroup() %>%
  #   mutate(
  #     short_dir_pct_dem = log10(short_dir_pct_dem)
  #   )

  if(sum(df$short_dir_pct_dem) <= 0) {

        log_info("District {distr} has no shortage data on record to use in model")

        # climate sensitivity dataframe
        sensitivity_df <- data.frame(
          district         = distr,
          var_sensitivity  =  NA_real_,
          sd_sensitivity   =  NA_real_,
          r2               =  NA_real_,
          variance         = year_variance,
          sd               = year_sd
        )

        # vif dataframe
        vf_df          <- data.frame(
                                    district = distr,
                                    variable = NA,
                                    vif      = NA_real_
                                    )


        vif_lst[[i]]         <- vf_df
        sensitivity_lst[[i]] <- sensitivity_df

  } else {

        # subset and log transform data for MLR
        log_trans <- df %>%
          ungroup() %>%
          mutate(
            short_dir_pct_dem = log10(short_dir_pct_dem)
          )

        # replace Infinite w/ 0
        is.na(log_trans) <- sapply(log_trans, is.infinite)
        log_trans[is.na(log_trans)] <- 0

        # MLR w/ VIF reduction + stepwise regression
        mlr_vfit <- lm(short_dir_pct_dem~., data = log_trans) %>%
          ols_step_both_p()
          # ols_step_forward_p()


      if(length(mlr_vfit$predictors) > 1) {

            # Calculate VIF
            vf <- car::vif(mlr_vfit$model)

            # VIF - remove variables w/ VIF > 5
            vf_df <- data.frame(vif = vf) %>%
              rownames_to_column() %>%
              arrange(vif) %>%
              filter(vif <= 5)
              # slice(n = 1:3)

            # step wise regression
            lm_step <- lm(
              as.formula(paste("short_dir_pct_dem", paste(vf_df$rowname, collapse=" + "), sep=" ~ ")),
              data = log_trans)

            # R²
            r2 <-  summary(lm_step)$r.squared

            # climate sensitivity dataframe
            sensitivity_df <- data.frame(
                                        district         = distr,
                                        var_sensitivity  = r2*year_variance,
                                        sd_sensitivity   = r2*year_sd,
                                        r2               = r2,
                                        variance         = year_variance,
                                        sd               = year_sd
                                      )

            # vif dataframe
            vf_df          <- data.frame(vif = vf) %>%
              rownames_to_column() %>%
              arrange(vif) %>%
              mutate(district = distr) %>%
              setNames(c("variable", "vif", "district")) %>%
              dplyr::relocate(district)

            vif_lst[[i]]         <- vf_df
            sensitivity_lst[[i]] <- sensitivity_df

      } else {

            log_info("Stepwise regression yeilds single predictor variable - {mlr_vfit$predictors}")

            # R²
            r2 <- mlr_vfit$metrics$r2

            # climate sensitivity dataframe
            sensitivity_df <- data.frame(
                                        district         = distr,
                                        var_sensitivity  = r2*year_variance,
                                        sd_sensitivity   = r2*year_sd,
                                        r2               = r2,
                                        variance         = year_variance,
                                        sd               = year_sd
                                      )

            vf_df          <- data.frame(
                                        district = distr,
                                        variable = mlr_vfit$predictors,
                                        vif      = NA_real_
                                      )
            vif_lst[[i]]         <- vf_df
            sensitivity_lst[[i]] <- sensitivity_df
      }
  }
  rm(r2, mlr_vfit, log_trans, lm_step, year_variance, year_sd, df, vf, vf_df)
}

mlr_df <- bind_rows(sensitivity_lst)
vif_df <- bind_rows(vif_lst)

















