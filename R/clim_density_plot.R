density(by_district$prcp)
density_vals <- density(mod_vals$Independent)
density_yaxis <- density_vals$y

# sampled data for District
tmp_samples <- climate_samples %>%
  filter(district == 6)
future_samples <- climate_samples %>%
  dplyr::select(district, id, contains("future"))

colnames(future_samples)[3:11] <-  sub("_.*", "", colnames(future_samples)[3:11])

hist_samples <- climate_samples %>%
  dplyr::select(district, id, contains("hist"))

colnames(hist_samples)[3:11] <-  sub("_.*", "", colnames(hist_samples)[3:11])

saveRDS(future_samples, "maca_future_samples.rds")
# Future density plot
density_future <- density(tmp_samples$prcp_future)
future_yaxis <- density_future$y

# Historic density plot
density_hist <- density(tmp_samples$prcp_hist)
hist_yaxis <- density_hist$y

# change column names to match model
clim_var <- climate_models %>%
  filter(district == 6, dataset == "MACA") %>%
  dplyr::select(year, district, prcp)

tmp_hist <- tmp_samples %>%
  dplyr::select(district, id, prcp_hist)

pred_future <- predict(lm_fit, dplyr::select(clim_var, prcp))
# pred_future <- predict(lm_fit, dplyr::select(future_data,  futureClimVar()))
pred_df <- data.frame(year = 1951:2099, fitted = pred_future)

pred_df <- left_join(pred_df, dplyr::select(future_data, -district), by = "year")

pred_df <- pred_df %>%
  ungroup() %>%
  mutate(
    prediction          = 10^fitted
  ) %>%
  setNames(c("year", "fitted", "independent_future", "prediction")) %>%
  mutate(across(where(is.numeric), round, 3))


ggplot() +
  geom_line(data = clim_var , aes(x = year, y = prcp, col = dataset), size = 1.5) +
  facet_wrap(~dataset)

highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle"))) %>%
  hc_yAxis_multiples(
    list(title = list(text = "", style = list(fontWeight = "bold",  fontSize = '1.2em')),
         labels = list(style = list(fontSize =  '1.2em')),
         min = 0, max = pmax(max(future_yaxis), max(hist_yaxis)), opposite = TRUE),
    list(title = list(text = impact_label,
                      style = list(fontWeight = "bold", fontSize = '1.2em'),
                      labels = list(style = list(fontSize =  '1.2em')),
                      min = 0, max = max(pred_df$prediction)))) %>%
  hc_xAxis(
    title = list(
      text = indicator_label,
      style = list(
        fontWeight = "bold",
        fontSize = '1.2em')),
    labels = list(style = list(fontSize =  '1.2em'))) %>%
  hc_add_series(
    data = density(tmp_samples$prcp_hist),
    type = 'area',
    # name = "Historic climate variable distribution",
    name = paste0(indicator_label, " historic distribution"),
    yAxis = 0, fillOpacity = 0.7) %>%
  hc_add_series(
    data = density(tmp_samples$prcp_future),
    type = 'area',
    # name = "Projected climate variable distribution",
    name = paste0(indicator_label, " projected distribution"),
    yAxis = 0, fillOpacity = 0.7) %>%
  hc_add_series(
    data = dplyr::arrange(pred_df, year),
    type = 'line', name = impact_label,
    hcaes(x = independent_future, y =  prediction),
    yAxis = 1, fillOpacity = 0.7) %>%
  # hc_colors(c("#C05555", "#55C0C0", "black")) %>% # "#5D6D7E"
  # hc_colors(c("#6DC878", "#C86DBD", "black")) %>% # "#5D6D7E"
  hc_colors(c("#50A15D", "#71DC83", "black")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)









