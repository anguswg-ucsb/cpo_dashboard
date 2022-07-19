# Script for processing and saving model data for univeriate regression
# Extract fitted model & metrics for each district
remove(list = ls())
library(tidyverse)
library(logger)

model_data      <- readRDS("statemod_climate_year.rds") %>%
  filter(year < 2013) %>%
  dplyr::select(
    year, district,
    demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem,
    prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi9, eddi12, swe, swe_max, soilm)

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

# dplyr::select(demand, supply, supply_dir, aug_supply, short, short_dir, aug_supply_pct_dem, short_pct_dem, short_dir_pct_dem) %>%
statemod_district <- model_data %>%
  group_by(district) %>%
  group_split()

# statemod_district <- statemod_district[3:6]

# tmp <- stm %>% filter(district == "9")
# fit_lst              <- list()
# metric_lst           <- list()
# dependent_lst        <- list()
# district_lst         <- list()
district_results_lst    <- list()
district_summary_lst    <- list()

# statemod_district2 <- statemod_district[1:3]
# i = 5
# k = 8
# z = 12
for (i in 1:length(statemod_district)) {

    # single districts annual data
    df <- statemod_district[[i]]
    #
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
      dplyr::select(prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi9, eddi12, swe, swe_max, soilm) %>%
      names()

    results_lst          <- list()
    summary_lst          <- list()

  for (k in 1:length(dependent_vars)) {

    metric_lst <- list()
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


      # fitted data
      results  <- lm_model %>%
        augment() %>%
        dplyr::select(dependent_vars[k], independent_vars[z], Fitted = .fitted) %>%
        setNames(c("Dependent", "Independent", "Fitted")) %>%
        mutate(
          district = distr,
          dep_var  = dependent_vars[k],
          ind_var  = independent_vars[z]
        ) %>%
        dplyr::relocate(district, dep_var, ind_var) %>%
        mutate(
          Dependent          = Dependent**2,               # Extract fitted values and back transform Logs
          Fitted             = Fitted**2
          # Dependent          = exp(Dependent),               # Extract fitted values and back transform Logs
          # Fitted             = exp(Fitted)
        ) %>%
        mutate(across(where(is.numeric), round, 2))

      # Metrics glance at model
      metrics  <- lm_model %>%
        glance() %>%
        dplyr::select(r_squared = r.squared, p_value = p.value)

      # tidy model
      tidied   <- lm_model %>%
        tidy() %>%
        dplyr::select(term, coefficient = estimate)
        # mutate(
        #   # coefficient = exp(coefficient)
        #   coefficient = case_when(
        #     # term == "(Intercept)" ~ exp(coefficient),     # back transform log intercept
        #     # TRUE                  ~ coefficient
        #     )
        #   )

      model_metrics <- bind_cols(tidied, metrics) %>%
        mutate(
          coefficient   = round(coefficient, 3),
          r_squared     = round(r_squared,3),
          p_value       = round(p_value, 6)
          ) %>%
        rename(
          # District      = district,
          Term          = term,
          Coefficient   = coefficient,
          "R2"          = r_squared,
          "p-value"     = p_value
        )

      # Clean indicators data labels
      indicator_label <- indicator_lst %>%
        filter(name == paste0(independent_vars[z]))
      indicator_label <- indicator_label$value

      # Clean impacts data labels
      impact_label <- impacts_lst %>%
        filter(value == paste0(dependent_vars[k]))

      impact_label <- impact_label$name

      coeff <- model_metrics %>%
        filter(Term != "(Intercept)") %>%
        mutate(
          Term = indicator_label
        )

      intercept  <- model_metrics %>%
        filter(Term == "(Intercept)") %>%
        mutate(
          Term = impact_label
          )
      # clean_model  <- bind_rows(coeff, intercept)
      clean_model  <- bind_rows(intercept, coeff) %>%
        mutate(
          district = distr,
          dep_var  = dependent_vars[k],
          ind_var  = independent_vars[z]
        ) %>%
        dplyr::relocate(district, dep_var, ind_var)

      fit_lst[[z]]     <- results
      metric_lst[[z]]  <- clean_model
      rm(intercept, coeff, model_metrics, tidied, metrics, indicator_label, impact_label, results)
    }

    results_lst[[k]] <-  bind_rows(fit_lst)
    summary_lst[[k]] <-  bind_rows(metric_lst)

  }
  district_results_lst[[i]] <-  bind_rows(results_lst)
  district_summary_lst[[i]] <-  bind_rows(summary_lst)
}

results_df <- bind_rows(district_results_lst)
summary_df <- bind_rows(district_summary_lst)

# tmp <- results_df %>%
#   filter(
#     district %in% c(1, 64, 6, 7, 8),
#     ind_var %in% c("tavg", "pdsi", "prcp", "eddi1", "spi1", "swe", "swe_max"),
#     dep_var %in% c("short_pct_dem", "short_dir_pct_dem", "aug_supply_pct_dem")
#     ) %>%
#   # filter(ind_var %in% c("pdsi")) %>%
#   group_by(ind_var) %>%
#   mutate(id = 1:n(), district = as.character(district)) %>%
#   ungroup()
#
# ggplot() +
#   geom_point(data = tmp, aes(x = Dependent, y = Independent, color = district), size = 2) +
#   scale_x_continuous(limits = c(0, 100)) +
#   facet_grid(ind_var~dep_var, scales = "free")

# save LM results
saveRDS(results_df, "lm_fit.rds")
saveRDS(summary_df, "lm_metrics.rds")

# district_lm <- results_df %>%
#   # filter(district == lm_district(), ind_var == climVar(), dep_var == depVar()) %>%
#   filter(district == 7, ind_var == "prcp", dep_var == "short_pct_dem") %>%
#   dplyr::select(-district, -ind_var, -dep_var)
#
# district_lm_log <- lm_fit %>%
#   # filter(district == lm_district(), ind_var == climVar(), dep_var == depVar()) %>%
#   filter(district == 7, ind_var == "prcp", dep_var == "short_pct_dem") %>%
#   dplyr::select(-district, -ind_var, -dep_var)
#
# # get clean names for plot
# indicator_label <- clean_names %>%
#   filter(var == "prcp")
#   # filter(var == climVar())
# # filter(var %in% district_lm$ind_var)
# indicator_label <- indicator_label$clean_name
#
# # get clean names for plot
# impact_label    <- clean_names %>%
#   filter(var == "short_dir_pct_dem")
#   # filter(var == depVar())
# # filter(var %in% district_lm$dep_var)
# impact_label <- impact_label$clean_name
#
# # Regression line and climate density plot
#  sqrt_plot <- plot_lm_density(
#   df        = district_lm,
#   dep_label = impact_label,
#   ind_label = indicator_label
# )
#  sqrt_plot
# log_plot <- plot_lm_density(
#    df        = district_lm_log,
#    dep_label = impact_label,
#    ind_label = indicator_label
#  )
# log_plot
# lm_density_plot

