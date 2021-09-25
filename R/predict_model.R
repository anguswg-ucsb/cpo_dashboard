## ------ Join model data w/ admin level supply/demand/short -------
mlr_district <- left_join(
  mlr_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  # filter(district == 64) %>%
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

  ) %>%
  mutate(
    # short_norm          = 100*(round(short/demand, 3)),
    # short_dir_norm      = 100*(round(short_dir/demand, 3)),
    short_norm          = (round(short/demand, 3)),
    short_dir_norm      = (round(short_dir/demand, 3)),
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
  dplyr::select(year, district, short_dir_norm, 11:18)
  # dplyr::select(district, short_dir_norm_norm, 11:18)
  # dplyr::select(futureDepVar(), 11:18)
mod_df <- mod_df %>%
  filter(is.infinite(short_dir_norm) == FALSE) %>%
  mutate(dataset = "historical") %>%
  mutate(year = as.numeric(as.character(year)))

#     # replace Infinite w/ 0
# is.na(mod_df) <- sapply(mod_df, is.infinite)
# mod_df[is.na(mod_df)] <- 0

# lm_run <- mod_df %>%
#   # dplyr::select(district, futureDepVar(), 11:18) %>%
#       group_by(district) %>%
#       nest(-district) %>%
#       mutate(
#         fit       = map(data,
#           # data, ~ols_step_forward_p(lm(as.formula(
#           #                  paste0(futureDepVar()," ~ .")),
#           #                data = .)))) %>%
#
#                          ~ols_step_forward_p(lm(short_dir_norm_norm~., data = .)))
#       )
#
#     lm_step <- lm_run$fit[[16]]
#
# fit_vals <- 10^lm_step$model$fitted.values
#
# fit_df <- data.frame(year = 1981:2012, fit_vals = fit_vals)

maca <- climate_models %>%
  group_by(district) %>%
  # filter(district %in% distr_num, dataset == "MACA", year > 1980, year < 2013) %>%
  filter(district %in% distr_num, dataset == "MACA") %>%
  dplyr::select(year, district, prcp, tavg, pdsi, spi1:spi12, dataset) %>%
  ungroup()



all_data <- mod_df %>%
  bind_rows(maca) %>%
  group_by(district) %>%
  nest()

clim_predict_step <- all_data  %>%
  mutate(
      model_data      = data %>% map(., ~dplyr::select(filter(., dataset == "historical"), -year, -dataset)),
      validate_data   = data %>% map(., ~dplyr::select((filter(., dataset == "MACA")), -short_dir_norm)),
      # model           = model_data %>% map(., ~lm(short_dir_norm ~., data = .)),
      # step_model      = lapply(model, '[[',6),
      model           = model_data %>% map(., ~ols_step_forward_p(lm(short_dir_norm ~ ., data = .))),
      step_model      = lapply(model, '[[',6),
      prediction      = map2(.x = step_model, .y = validate_data, ~predict(object = .x, newdata = .y))
  )

step_predictions <- clim_predict_step %>%
  dplyr::select(district, model_data, validate_data, prediction) %>%
  unnest(c(4)) %>%
  dplyr::select(district, prediction) %>%
  group_by(district) %>%
  mutate(
    year             = as.numeric(1951:2099),
    prediction       = 10^prediction,
    dataset          = "mlr_step_wise_prediction"
    ) %>%
  ungroup() %>%
  dplyr::select(year, district, prediction, dataset)

actual_data <- mod_df %>%
  mutate(short_dir_norm = 10^short_dir_norm)

 eval <- left_join(dplyr::select(step_predictions, -dataset), dplyr::select(actual_data, -dataset), by = c("district", "year")) %>%
  na.omit()
  # mutate(short_dir_norm = 10^short_dir_norm)

 eval2 <- eval %>%
   # filter(district %in% c(6)) %>%
   group_by(district) %>%
   dplyr::select(district, year, short_dir_norm, prediction) %>%
   mutate(
     prediction  = round(as.numeric(prediction),3),
     diff        = short_dir_norm - prediction,
     mean_diff   = mean(diff),
     mape        = mean(abs(abs(short_dir_norm - prediction) / abs(short_dir_norm)))
   ) %>%
   mutate(
     prediction = 100*prediction,
     short_dir_norm = 100*short_dir_norm
   )

 ggplot() +
   geom_col(data = step_predictions, aes(x = year, y = prediction)) +
   facet_wrap(~district)
 rmse <- yardstick::rmse(eval2, short_dir_norm, prediction)
 ggplot() +
   geom_col(data = rmse, aes(x = district, y = .estimate))
eval_step <- bind_rows(step_predictions, dplyr::select(actual_data, year, district, short_dir_norm, dataset))

eval2_step <- eval_step %>%
  pivot_wider(id_cols = c(year, district, short_dir_norm, dataset),
                               names_from = "dataset",
                              values_from = "short_dir_norm") %>%
  na.omit() %>%
  group_by(district)

eval_step_no_hist <- eval_step %>%
  filter(dataset != "historical") %>%
  na.omit()

eval_all <- bind_rows(eval_no_step, eval_step_no_hist)

ggplot() +
  geom_line(data = eval_all, aes(x = year, y = short_dir_norm, color = dataset), size = 1.1) +
  facet_wrap(~district) +
  labs(
    title = "MLR model predictions w/ and w/o stepwise variable selection\nHistorical direct shortages % vs. Predicted direct shortages % with MLR model  (using step wise regression)",
    subtitle = "Historical obs. vs predictions generated from MACA dataset (1981 - 2012)"

  ) +
  # viridis::scale_color_viridis(discrete = T, option = "H", direction = -1)
  theme_bw()
rmse_no_step <- yardstick::rmse(eval2_no_step, historical, mlr_no_step_wise_prediction)
rmse_step <- yardstick::rmse(eval2_step, historical, mlr_step_wise_prediction)
rmse_no_step <- rmse_no_step %>%
  mutate(model = "mlr_no_stepwise")
rmse_step <- rmse_step %>%
  mutate(model = "mlr_stepwise_regression")

rmse_all <- bind_rows(rmse_no_step, rmse_step)


ggplot() +
  geom_line(data = eval_step, aes(x = year, y = short_dir_norm, color = dataset),
             size = 1.25) +
  facet_wrap(~district) +
  labs(
    title = "MLR model using stepwise regression to select variables\nHistorical direct shortages % vs. Predicted direct shortages % with MLR model  (using step wise regression)",
    subtitle = "Historical obs. vs predictions generated from MACA dataset (1981 - 2012)"

  )

ggplot() +
  geom_col(data = rmse_all, aes(x = district, y = .estimate, fill = model), width =0.6 ,
           # position = "dodge")
           # width = 2,
           position = position_dodge2(width = 0.1)) +
  labs(
    title = "RMSE of models cross validated using MACA dataset",
    subtitle = "Historical obs. vs predictions generated from MACA dataset (1981 - 2012)",
    y = "Root mean squared error",
    x = "District"
  )

eval <- left_join(predictions, mod_df, by = c("district", "year")) %>%
  na.omit() %>%
  mutate(short_dir_norm = 10^short_dir_norm)

eval2 <- eval_step %>%
  # filter(district %in% c(6)) %>%
  group_by(district) %>%
  dplyr::select(district, year, short_dir_norm, prediction) %>%
  mutate(
    prediction  = as.numeric(prediction),
    diff        = short_dir_norm - prediction,
    mean_diff   = mean(diff),
    mape        = mean(abs((short_dir_norm - prediction) / short_dir_norm))
  )

rmse <- yardstick::rmse(eval2, short_dir_norm, prediction)


ggplot() +
  geom_line(data = eval2, aes(x = year, y = prediction), size = 1.25, col = "blue") +
  geom_line(data = eval2, aes(x = year, y = short_dir_norm), size = 1.25, col = "red") +
  facet_wrap(~district)

# ) %>%
#   unnest(c(6))
hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
               scatter = list(marker = list(symbol = "circle"))) %>%
  hc_xAxis(
    title = list(text = "Year", style = list(fontWeight = "bold",fontSize = '1.2em'
    )),
    labels = list(style = list(fontSize =  '1.2em')),
    plotBands = list(
      list(from =1981, to =2012, color = "rgba(0, 100, 0, 0.1)",
           label = list(text = "Historical record", style = list( color = "black", fontWeight = 'bold' ))))
  ) %>%
  hc_add_series(
    data = dplyr::arrange(impact_ts, year),
    type = 'column', name = impact_label,
    # hcaes(x = Independent, y = Dependent),
    hcaes(x = year, y =  impact),
    yAxis = 0, fillOpacity = 0.5) %>%
  #   yAxis = 0,fillOpacity = 0.5) %>%
  hc_colors(c("#1EB1CD", "#C05555")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 4)) %>%
  hc_title(text = "Best climate predictors of shortages in district") %>%
  hc_yAxis(title = list(text = "Water volume (acre feet)"), min = 0) %>%
  hc_yAxis_multiples(
    list(title = list(
      text = pred_names[2],
      style = list(fontWeight = "bold",  fontSize = '1.2em')
    ),
    labels = list(style = list(fontSize =  '1.2em')),
    top = "0%",
    height = "33%"
    ),
    list(title = list(
      text   = pred_names[3],
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
      labels = list(style = list(fontSize =  '1.2em')),
      top      = "33%",
      height = "33%",
      opposite = TRUE)
    ,
    list(title = list(
      text   = impact_label,
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
      labels = list(style = list(fontSize =  '1.2em')),
      top      = "66%",
      height = "33%",
      opposite = F)
  ) %>%
  hc_xAxis(labels = list(style = list(fontSize =  '1.2em'))
  ) %>%
  hc_add_series(
    data = predictor_ts,
    name = pred_names[2],
    type = 'line',
    hcaes(x = year,  y = !!lm_run$predictors[1]),
    yAxis = 0,
    fillOpacity = 0.1) %>%
  hc_add_series(
    data = predictor_ts,
    name = pred_names[3],
    type = 'line', hcaes(x = year, y =  !!lm_run$predictors[2]),
    yAxis = 1, fillOpacity = 0.1) %>%
  hc_add_series(
    data = dplyr::arrange(impact_ts, year),
    type = 'column', name = impact_label,
    # hcaes(x = Independent, y = Dependent),
    hcaes(x = year, y =  impact),
    yAxis = 2, fillOpacity = 0.5) %>%
  hc_xAxis(categories = predictor_ts$year) %>%
  hc_colors(c("darkblue",  "darkred")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)





