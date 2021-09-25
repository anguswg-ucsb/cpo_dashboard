# ---- Summarize data for MLR ----
mlr_data <- by_admin %>%
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

## ------ Join model data w/ admin level supply/demand/short -------
mlr_district <- left_join(
  mlr_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  filter(district == 2) %>%
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
    pdsi                = mean(pdsi, na.rm = T),
    spi1                = mean(spi1, na.rm = T),
    spi3                = mean(spi3, na.rm = T),
    spi6                = mean(spi6, na.rm = T),
    spi9                = mean(spi9, na.rm = T),
    spi12               = mean(spi12, na.rm = T),
    tavg                = mean(tavg, na.rm = T),
    tmax                = mean(tmax, na.rm = T),
    tmin                = mean(tmin, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.factor(year)
  ) %>%
  ungroup()


# # ---- log transform data ----
mod_df <- mlr_district %>%
  mutate(
    short               = log10(short),
    short_dir           = log10(short_dir),
    short_norm          = log10(short_norm),
    short_dir_norm      = log10(short_dir_norm),
    aug_supply          = log10(aug_supply),
    aug_supply_norm     = log10(aug_supply_norm),
    supply              = log10(supply),
    supply_dir          = log10(supply_dir),
    demand              = log10(demand),
    af_total            = log10(af_total)
  )
# ---- Prep data for model ----
mod_df <- mod_df %>%
  dplyr::select(short_dir, 11:20)
  # dplyr::select(district, short_dir_norm, 11:18)
  # dplyr::select(futureDepVar(), 11:20)

#     # replace Infinite w/ 0
is.na(mod_df) <- sapply(mod_df, is.infinite)
mod_df[is.na(mod_df)] <- 0

# mod_df <- mod_df %>%
#   filter(is.infinite(futureDepVar()) == FALSE)
# filter(is.infinite(short_dir_norm) == FALSE)
# mutate(year = as.numeric(as.character(year)))


# multivariate stepwise regression
lm_run <- lm(
  short_dir~., data = mod_df) %>%
  # as.formula(
  #   paste0(futureDepVar()," ~ .")),
  # data = mod_df) %>%
  # rm_collinearity(vif_thresh = 3.5) %>%
  ols_step_forward_p()

lm_step <- lm_run$model

# as.formula(
#   paste0(futureDepVar()," ~ .")),
# data = .)
# lm_fit <- lm_run$fit[[1]]

climate_proj <- climate_models %>%
  ungroup() %>%
  # filter(district == district_id$district, year > 2020) %>%
  filter( district == 2, year >2020) %>%
  dplyr::select(-dataset, -year, -district, -pet,-pet_harg, -pdsi_harg) %>%
  mutate(across(where(is.numeric), round, 3))
# setNames(c("year", "climate_var"))

# predict future shorts past 2021
pred_future <- predict(lm_step, climate_proj)
# pred_future <- predict(lm_fit, dplyr::select(climate_proj, aet))
pred_df <- data.frame(year = 2021:2099, fitted = pred_future)
# pred_df <- left_join(pred_df, dplyr::select(future_data, -district), by = "year")

pred_df <- pred_df %>%
  ungroup() %>%
  mutate(
    prediction          = 10^fitted
  ) %>%
  mutate(across(where(is.numeric), round, 2))

current_data <- present_indicators %>%
  ungroup() %>%
  # filter(district == district_id$district) %>%
  filter(district == 2) %>%
  dplyr::select(-year, -district) %>%
  mutate(across(where(is.numeric), round, 3))

# predict current shorts 2013-2020
pred_current <- predict(lm_step, current_data)
# pred_future <- predict(lm_fit, dplyr::select(climate_proj, aet))
pred_current_df <- data.frame(year = 2013:2020, fitted = pred_current)
# pred_df <- left_join(pred_df, dplyr::select(future_data, -district), by = "year")
pred_current_df <- pred_current_df %>%
  ungroup() %>%
  mutate(
    prediction          = 10^fitted
  ) %>%
  mutate(across(where(is.numeric), round, 2))

# indicator_label <- names(clim_model_indicator_lst[grep(pattern = paste0("^", futureClimVar(), "$"), clim_model_indicator_lst)])
impact_label <- names(impacts_lst[grep(pattern = paste0("^", futureDepVar(), "$"), impacts_lst)])
# indicator_label <- names(clim_model_indicator_lst[grep(pattern = paste0("aet"), clim_model_indicator_lst)])
impact_label <- names(impacts_lst[grep(pattern = paste0("short_dir"), impacts_lst)])

# historic impacts
impact_historic <- mlr_district %>%
  ungroup() %>%
  # dplyr::select(year, futureDepVar()) %>%
  dplyr::select(year, short_dir) %>%
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
  filter(year > 2020) %>%
  dplyr::select(year, prediction) %>%
  mutate(source = "projected") %>%
  setNames(c("year", "impact", "source"))

# join historic impacts data w/ predicted impacts
impact_ts <- bind_rows(impact_historic, impact_current, impact_projected)

# ---- Select predictors from data ----
hist_predictors <- mlr_district %>%
  ungroup() %>%
  dplyr::select(year, lm_run$predictors[1:2]) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(year = as.numeric(as.character(year)))

current_predictors <- present_indicators  %>%
  ungroup() %>%
  filter(district == 2) %>%
  # filter(district == district_id$district) %>%
  dplyr::select(year, lm_run$predictors[1:2]) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  mutate(year = as.numeric(as.character(year)))

future_predictors <- climate_models %>%
  ungroup() %>%
  filter(district == 2, year > 2020) %>%
  # filter(district == district_id$district, year >2020) %>%
  dplyr::select(year, lm_run$predictors[1:2]) %>%
  # dplyr::select(-dataset, -district, -tmin, -tmax, -pet,-pet_harg, -pdsi_harg) %>%
  mutate(across(where(is.numeric), round, 3))

predictor_ts <- bind_rows(hist_predictors, current_predictors, future_predictors)
# Fix names for axis, labels, titles

# ---- Predictors highchart ----
highchart() %>%
  hc_plotOptions(
    # column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 4)) %>%
  # hc_title(text = "Forecasted Water Shortages") %>%
  hc_yAxis(
    max = 200000,
    tickInterval = 50000,
    title = list(text = "Direct Flow Shortage (acre feet)",
                 margin = 60,
                 style = list(fontSize = 32, fontWeight = "bold", color = "black")),
    labels = list(y = 25, style = list(fontSize =  32, color = "black")), min = 0) %>%
  hc_xAxis(
    tickInterval = 10,
    title = list(
      text = "Year",
      margin = 60,
      style = list(fontSize = 32, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize =  32, color = "black")),
    plotBands = list(
      list(from =1981, to =2020, color = "rgba(0, 100, 0, 0.1)",
           label = list(text = "Historical record", y = 28, style = list( fontSize = 32, color = "black", fontWeight = 'bold' ))))
  ) %>%
  # hc_add_series(
  #   data = predictor_ts,
  #   name = pred_names[2],
  #   type = 'line',
  #   hcaes(x = year,  y = !!lm_run$predictors[1]),
  #   yAxis = 0,
  #   fillOpacity = 0.1) %>%
  hc_add_series(
    data = dplyr::arrange(impact_ts, year),
    type = 'column', name = impact_label[1],
    # hcaes(x = Independent, y = Dependent),
    hcaes(x = year, y =  impact),
     fillOpacity = 0.5) %>%
  hc_xAxis(categories = impact_ts$year) %>%
  hc_legend(enabled = F) %>%
  hc_colors(c("black")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
pred_hc
