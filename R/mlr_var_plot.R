library(tidyverse)
source("data_utils.R")

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
# %>%
#   dplyr::select(1:8, 30)
# left_join(dplyr::select(breaks, district, admin_number, water_right), by = c("district", "admin_number"))

## ------ Join model data w/ admin level supply/demand/short -------
mlr_district <- left_join(
  mlr_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  filter(district == 6) %>%
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
    prcp_norm           = mean(prcp_norm, na.rm = T),
    pdsi                = mean(pdsi, na.rm = T),
    pdsi_gridmet        = mean(pdsi_gridmet, na.rm = T),
    eddi1               = mean(eddi1, na.rm = T),
    eddi3               = mean(eddi3, na.rm = T),
    eddi6               = mean(eddi6, na.rm = T),
    eddi12              = mean(eddi12, na.rm = T),
    spi1                = mean(spi1, na.rm = T),
    spi3                = mean(spi3, na.rm = T),
    spi6                = mean(spi6, na.rm = T),
    spi9                = mean(spi9, na.rm = T),
    spi12               = mean(spi12, na.rm = T),
    tavg                = mean(tavg, na.rm = T),
    aet                 = mean(aet, na.rm = T),
    pet                 = mean(pet, na.rm = T),
    soilm               = mean(soilm, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.factor(year)

  ) %>%
  ungroup()

mod_df <- mlr_district %>%
  dplyr::select(short_dir_norm, 10:11, 13:27)

lm_vfit <- lm(short_dir_norm~., data = mod_df) %>%
              rm_collinearity(vif_thresh = 3.5) %>%
              ols_step_forward_p()

summary(lm_vfit$model)
lm_vfit$predictors[1:2]

ws_predictors <- mlr_district %>%
    ungroup() %>%
    dplyr::select(year, lm_vfit$predictors[1:2])
# saveRDS(lm_vfit, "lm_vfit.rds")
# saveRDS(ws_predictors, "predictors_df.rds")

ws_pred_long <- ws_predictors %>%
  pivot_longer(c(-year)) %>%
  mutate(year = as.numeric(as.character(year)))

ggplot() +
  geom_line(data = ws_pred_long, aes(x = year, y = value, color = name), size = 1) +
  facet_wrap(~name, scale = "free")
  # scale_y_continuous(
  #   "mpg (US)",
  #   sec.axis = sec_axis(~ . * 5, name = "mpg (UK)")
  # )
pred1 <- lm_vfit$predictor[1]
lm_vfit$predictors

highchart() %>%
    hc_plotOptions(column = list(stacking = 'normal')) %>%
    hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
    hc_yAxis_multiples(
      list(title = list(text = lm_vfit$predictors[1]), top = "0%", height = "50%",
           min = 0, max = max(ws_predictors$swe_max)),
      list(title = list(text = lm_vfit$predictors[2]),
           min = 0, max =max(ws_predictors$aet), top = "50%", height = "50%", opposite = TRUE)
      ) %>%
    hc_add_series(
      data = ws_predictors, name = lm_vfit$predictors[1],
      type = 'line',
      hcaes(x = year,
            y = lm_vfit$predictors[1]),
      yAxis = 0,
      fillOpacity = 0.1) %>%
    hc_add_series(
      data = ws_predictors, name = lm_vfit$predictors[2],
      type = 'line', hcaes(x = year, y = aet),
      yAxis = 1, fillOpacity = 0.1) %>%
    hc_xAxis(categories = ws_predictors$year) %>%
    hc_colors(c("darkblue",  "darkred")) %>%
    hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
highchart() %>%
      hc_plotOptions(column = list(stacking = 'normal')) %>%
      hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
      hc_add_series(
        data = ws_predictors,
        # name = lm_vfit$predictors[2],
        type = "line",
        hcaes(x = year,
              y =  dplyr::select(ws_predictors, 2)),
              # y =  lm_vfit$predictors[2]),
        # yAxis = 0,
        fillOpacity = 0.1
      )
install.packages("dplyr")
highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
  hc_add_series(
    data = ws_predictors,
    name = lm_vfit$predictors[2],
    type = "line",
    hcaes_string(x = "year",
                 y =  lm_vfit$predictors[2]),
    yAxis = 0,
    fillOpacity = 0.1
  )
class(ws_predictors$year)
fitted <- lm_vfit$model$fitted.values
observed <- lm_vfit$model$model %>%
  dplyr::select(short_dir_norm)
pred_df <- data.frame(fitted = fitted, observed = observed) %>%
  setNames(c("Fitted", "Direct flow shortage normalized"))


highchart() %>%
  hc_yAxis(title = list(text = "Observed"), min = 0) %>%
  hc_xAxis(title = list(text = "Simulated"), min = 0) %>%
  hc_add_series(
    data = pred_df, name = "Prediction",
    type = 'point', hcaes(x = Fitted, y =  `Direct flow shortage normalized`), yAxis = 0, fillOpacity = 0.1) %>%
  hc_add_series(
    data = pred_df, name = "One to one line",
    type = 'spline', hcaes(x = `Direct flow shortage normalized`, y =  `Direct flow shortage normalized`), yAxis = 0, fillOpacity = 0.1)













