remove(list = ls())
library(tidyverse)
model_data      <- readRDS("statemod_climate_year2.rds")

distr_lst       <- unique(model_data$district)

samples_future     <- readRDS("climate_future_samples.rds") %>%
  filter(district %in% distr_lst)

samples_historic   <- readRDS("climate_historic_samples.rds") %>%
  filter(district %in% distr_lst)

output_future_lst   <- list()
output_historic_lst <- list()

for (i in 1:length(distr_lst)) {

  future_df <- samples_future %>%
    filter(district == distr_lst[i])

  historic_df <- samples_historic %>%
    filter(district == distr_lst[i])

  distr <- future_df$district[1]

  logger::log_info("Climate samples - District {distr}")

  ind_vars <- future_df %>%
    dplyr::select(-district, -type) %>%
    names()

  density_future_lst   <- list()
  density_historic_lst <- list()

  for (k in 1:length(ind_vars)) {
    logger::log_info("{ind_vars[k]} density - District {distr}")

    # density of future climate data
    sample_density_future  <- future_df %>%
      dplyr::select(-district, -type) %>%
      pivot_longer(cols = everything()) %>%
      filter(name == ind_vars[k])

    # X & Y density values
    future_x <- density(sample_density_future$value)$x
    future_y <- density(sample_density_future$value)$y

    # X Y in tidy dataframe
    density_future <- data.frame(
      # district = distr,
      ind_var  = ind_vars[k],
      x        = future_x,
      y        = future_y
    ) %>%
      setNames(c('ind_var', paste0(ind_vars[k], "_x"),  paste0(ind_vars[k], "_y"))) %>%
      as_tibble() %>%
      dplyr::select(-ind_var) %>%
      mutate(across(where(is.numeric), round, 5))


    # density of historic climate data
    sample_density_historic  <- historic_df %>%
      dplyr::select(-district, -type) %>%
      pivot_longer(cols = everything()) %>%
      filter(name == ind_vars[k])

    # X & Y density values
    historic_x <- density(sample_density_historic$value)$x
    historic_y <- density(sample_density_historic$value)$y

    # X Y in tidy dataframe
    density_historic <- data.frame(
      # district = distr,
      ind_var  = ind_vars[k],
      x        = historic_x,
      y        = historic_y
    ) %>%
      setNames(c('ind_var', paste0(ind_vars[k], "_x"),  paste0(ind_vars[k], "_y"))) %>%
      as_tibble() %>%
      dplyr::select(-ind_var) %>%
      mutate(across(where(is.numeric), round, 5))

    density_future_lst[[k]]   <- density_future
    density_historic_lst[[k]] <- density_historic
  }

  density_future_clean <-  density_future_lst %>%
    bind_cols() %>%
    mutate(district = distr) %>%
    dplyr::relocate(district)

  density_historic_clean <-  density_historic_lst %>%
    bind_cols() %>%
    mutate(district = distr) %>%
    dplyr::relocate(district)

  output_future_lst[[i]]   <- density_future_clean
  output_historic_lst[[i]] <- density_historic_clean


  rm(historic_y, historic_x, density_historic, density_future, sample_density_future,sample_density_historic)
}

density_future   <- bind_rows(output_future_lst)

density_historic <- bind_rows(output_historic_lst)

# density <- bind_rows(density_future, density_historic)

# save density values
# saveRDS(density_future, "climate_future_density.rds")
# saveRDS(density_historic, "climate_historic_density.rds")
# saveRDS(density, "climate_density.rds")
tmp <- density_future %>%
  filter(district == 1) %>%
  mutate(

  )


tmp %>%
    pivot_wider(
      id_cols     = c(district, ind_var),
      names_from  = "ind_var",
      names_glue  = "{ind_var}_{.value}",
      values_from = c(x, y)
      # values_fill = NA,
      # values_from = c(dvolume, inflow, outflow)
    )

plot(tmp$y~tmp$x)
sample_density <- density(tmp$prcp)
sample_density2 <- density(tmp$tmax)
xx <- sample_density$x
yy <- sample_density$y
df <- data.frame(xx, yy)
plot(sample_density)
plot(df$yy~df$xx)
sample_lst <- c(sample_density, sample_density2) %>% bind_rows()

matrix(sample_density)
sample_density$y
class(sample_density)
tmp_samples <- climate_samples %>%
  filter(district == 6) %>%
  dplyr::select(district, id, contains("tavg"))

# Future density plot
density_future <- density(tmp_samples[[4]])
future_yaxis <- density_future$y

# Historic density plot
density_hist <- density(tmp_samples[[3]])
hist_yaxis <- density_hist$y

indicator_label <- names(clim_model_indicator_lst[grep(pattern = paste0("pdsi"), clim_model_indicator_lst)])

impact_label <- names(impacts_lst[grep(pattern = paste0("short_dir_norm"), impacts_lst)])

highchart() %>%
  # hc_plotOptions(
  #   line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
  #   scatter = list(marker = list(symbol = "circle"))
  #   ) %>%
  # hc_yAxis(
  #   # min = 0,
  #   # max = 0.3,
  #   tickInterval = 0.1,
  #   title = list(
  #     text = "", style = list(fontWeight = "bold",   fontSize = '1.2em')),
  #   labels = list(
  #     y = 20,
  #     style = list(fontWeight = "bold",fontSize =  30, color = "black")),
  #
  #   min = 0, max = pmax(max(future_yaxis), max(hist_yaxis)),
  #   opposite = F
  #   ) %>%
  # hc_xAxis(
  #   tickInterval = 1,
  #   min = 5.5,
  #   max = 13.5,
  #   title = list(
  #     # text = indicator_label,
  #     text = "Average Temperature (C)",
  #     style = list(fontWeight = "bold",fontSize = 30, color = "black")),
  #   labels = list(
  #     y = 45,
  #     style = list(fontWeight = "bold",fontSize = 30, color = "black"))
  #   ) %>%
  hc_add_series(

    # data = density(tmp_samples[[3]]),
    # data = df,
    data = sample_density,
    type = 'area',
    # hcaes(x = xx, y = yy),
    name = paste0("Historic distribution"),
    yAxis = 0, fillOpacity = 0.7) %>%
  hc_add_series(
    data = density(tmp_samples[[4]]),
    type = 'area',
    name = paste0("Projected distribution"),
    yAxis = 0, fillOpacity = 0.7) %>%
  # hc_colors(c("#C05555", "#55C0C0", "black")) %>% # "#5D6D7E"
  # hc_colors(c("#55C0C0", "#C05555", "black")) %>% # "#5D6D7E"
  # hc_colors(c("#6DC878", "#C86DBD", "black")) %>% # "#5D6D7E"
  hc_colors(c("#679890", "#98676F", "black")) %>% # "#5D6D7E"
  hc_legend(enabled = F) %>%
  # hc_colors(c("#50A15D", "#71DC83", "black")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
















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









