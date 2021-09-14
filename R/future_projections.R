
co_indicators <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/colorado/monthly/drought_indicators_maca_rcp45_co.rds")
sp_indicators <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/south_platte/monthly/drought_indicators_maca_rcp45_sp.rds")


# --- ADJUST TIMESERIES ----
highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle"))) %>%
  hc_yAxis_multiples(
    list(title = list(
      text = indicator_label,
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
      labels = list(style = list(fontSize =  '1.2em')),
      top      = "0%",
      height = "50%",
      opposite = TRUE),
    list(title = list(
      text  = impact_label,
      style = list(fontWeight = "bold",  fontSize = '1.2em')
    ),
    labels = list(style = list(fontSize =  '1.2em')),
    top = "50%",
    height = "50%"
    )
  ) %>%
  hc_xAxis(
    title = list(text = "Year", style = list(fontWeight = "bold",fontSize = '1.2em'
    )),
    labels = list(style = list(fontSize =  '1.2em')),
    plotBands = list(
      list(from =2013, to =2099, color = "rgba(0, 100, 0, 0.1)",
           label = list(text = "Historical record", style = list( color = "black", fontWeight = 'bold' )
                        )))
  ) %>%
  hc_add_series(
    data = dplyr::arrange(indicator_ts, year),
    type = 'line', name = indicator_label,
    hcaes(x = year, y =  prcp),
    # hcaes(x = year, y =  !!futureClimVar()),
    yAxis = 0,fillOpacity = 0.5) %>%
  hc_add_series(
    data = dplyr::arrange(impact_ts, year),
    type = 'column', name = impact_label,
    # hcaes(x = Independent, y = Dependent),
    hcaes(x = year, y =  impact),
    yAxis = 1, fillOpacity = 0.5)

#  historic climate variable data
indicator_historic <- by_district %>%
  ungroup() %>%
  dplyr::select(year, prcp) %>%
  mutate(
    year   = as.numeric(as.character(year)),
         source = "historic"
    )
# modeled climate variable
indicator_projected <- climate_proj %>%
  filter(year >= 2013) %>%
  mutate(source = "projected")

# join historic climate variable data w/ modeled data
indicator_ts <- bind_rows(indicator_historic, indicator_projected)

# historic impacts
impact_historic <- by_district %>%
  ungroup() %>%
  dplyr::select(year, short_dir_norm) %>%
  mutate(
    year   = as.numeric(as.character(year)),
    source = "historic"
  ) %>%
  setNames(c("year", "impact", "source"))

# predicted impacts
impact_projected <- pred_df %>%
  filter(year >= 2013) %>%
  dplyr::select(year, prediction) %>%
  mutate(source = "projected") %>%
  setNames(c("year", "impact", "source"))

# join historic impacts data w/ predicted impacts
impact_ts <- bind_rows(impact_historic, impact_projected)


clim <- bind_rows(co_indicators, sp_indicators)
bcca <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/bcca/monthly/drought_indicators_bcca_rcp45.rds") %>%
  na.omit()
maca <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/maca/monthly/drought_indicators_maca_rcp45.rds")%>%
  na.omit()
loca <- readRDS("C:/Users/angus/OneDrive/Desktop/lynker/CPO/data/future_climate/loca/monthly/drought_indicators_loca_rcp45.rds")%>%
  na.omit()
saveRDS(bcca, "bcca_data.rds" )

# Nest LM models for each district and predictor
# # log transform data
trans_log <- by_district %>%
  mutate(
    short               = log10(short),
    short_dir           = log10(short_dir),
    short_norm          = log10(short_norm),
    short_dir_norm      = log10(short_dir_norm),
    aug_supply          = log10(aug_supply),
    aug_supply_norm     = log10(aug_supply_norm),
    supply              = log10(supply),
    supply_dir          = log10(supply_dir),
    demand              = log10(demand)
  )

# replace Infinite w/ 0
is.na(trans_log) <- sapply(trans_log, is.infinite)
trans_log[is.na(trans_log)] <- 0

# by_right_scale <- group_by(by_right_scale, district, water_right)
data_new <- group_by(trans_log, district)

# Independent variables
var_list <- colnames(data_new)[c(11, 13, 19:24, 26)]

# Independent variables pairings grid
pairings <- expand.grid(
  # lhs = c(paste0(depVar5())),
  # lhs = c("short", "short_dir", "short_norm", "short_dir_norm"),
  lhs = c("value"),
  # lhs = c(paste0(as.list(outcome_lst_admin$outcome_variable)[2])),
  rhs = var_list
  # rhs = c('af_total', 'swe_max')
)

# make formula from each variable pairing
pairings[["formula"]] <- lapply(
  X   = paste(pairings[["lhs"]], "~", pairings[["rhs"]]),
  FUN = as.formula
)

df_long <- data_new %>%
  pivot_longer(cols = c("short", "short_dir", "short_norm", "short_dir_norm"), names_to = "dependent", values_to = "value")
# ---- Linear regression on dependent var vs. independent var for both junior & senior water rights in each district ----

tidy_coeff <- df_long %>%
  dplyr::select(district, dependent, value, prcp, pet, pdsi, tavg, spi1, spi3, spi6, spi9, spi12) %>%
  nest(-district, -dependent) %>%
  mutate(
    fit = map(data,  ~lapply(
      X    = pairings[["formula"]],
      FUN  = lm,
      data = .)
    )
    # tidied = map(fit, tidy)
  ) %>%
  unnest(c(4)) %>%
  mutate(
    tidied = map(fit, tidy),
    glance = map(fit, glance)
  ) %>%
  unnest(c(5)) %>%
  filter(term != "(Intercept)") %>%
  dplyr::select(1:5)

saveRDS(tidy_coeff, "lm_nested.rds")

lm_filter <- tidy_coeff %>%
  filter(district == 6, dependent == "short_dir_norm", term == "prcp")

lm_fit2 <- lm_filter$fit[[1]]


pred_future2 <- predict(lm_fit2, dplyr::select(future_clim, prcp))
pred_df2 <- data.frame(year = 1951:2099, short_dir_norm = pred_future2)

mod_vals2 <- pred_df2 %>%
  ungroup() %>%
  mutate(
    log_trans          = 10^short_dir_norm
  )
ggplot() +
  geom_line(data = mod_vals, aes(x = year, y = log_trans), color = "red", size =1.5) +
  geom_line(data = pred_df2, aes(x = year, y = short_dir_norm), color = "blue", size =1.5)
  geom_line(data = distr, aes(x = year, y = short_dir_norm), color = "blue", size =1.5)


# replace Infinite w/ 0
is.na(trans_log) <- sapply(trans_log, is.infinite)
trans_log[is.na(trans_log)] <- 0

trans_log <- trans_log %>%
  dplyr::select(depVar5(), indVar5())

# NESTED LM RUN
lm_run <- trans_log %>%
  group_by(water_right) %>%
  nest(-water_right) %>%
  mutate(
    fit      = map(data,
                   # lm(short~prcp, data = .)),
                   ~lm(as.formula(
                     paste(depVar5()," ~ ", indVar5())
                   ),
                   data = .)),
    results  = map(fit, augment)
  ) %>%
  unnest(c(4)) %>%
  dplyr::select(water_right, depVar5(), indVar5(), .fitted) %>%
  setNames(c("water_right", "dependent", "independent", "fitted"))
unique(maca$district)

distr <- by_district %>%
  filter(district == 6) %>%
  mutate(year    =  as.numeric(as.character(year)))

future_loca <- loca %>%
  mutate(
    # year = lfstat::water_year(date, origin = "usgs", as.POSIX = FALSE)
    year = lubridate::year(date)
  ) %>%
  group_by(year, district) %>%
  summarise(
    prcp          =  sum(prcp),
    tavg          =  mean(tavg),
    tmax          =  mean(tmax),
    tmin          =  mean(tmin),
    et_thorn      =  mean(et_thorn),
    et_harg       =  mean(et_harg),
    pdsi_thorn    =  mean(pdsi_thorn),
    pdsi_harg     =  mean(pdsi_harg),
    spi1          =  mean(spi1),
    spi3          =  mean(spi3),
    spi6          =  mean(spi6),
    spi9          =  mean(spi9),
    spi12         =  mean(spi12)
    # year    =  as.numeric(as.character(year))
  ) %>%
  ungroup() %>%
  filter(year>1950) %>%
  mutate(dataset = "LOCA")
future_all <- bind_rows(future_bcca, future_loca, future_maca)
saveRDS(future_all, "climate_models.rds")

density_vals <- density(mod_vals$independent)
density_yaxis <- density_vals$y

tmp <- climate_models %>%
  group_by(dataset, district) %>%
  filter(district %in% c(1, 64, 6, 7))

ggplot() +
  geom_line(data = tmp, aes(x = year, y = et_harg, col = dataset), size = 1.2) +
  facet_wrap(~district)

highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle"))) %>%
  hc_yAxis_multiples(
    list(title = list(text = "", style = list(fontWeight = "bold",  fontSize = '1.2em')),
         labels = list(style = list(fontSize =  '1.2em')),
         min = 0, max = pmax(max(density_yaxis_future), max(density_yaxis_hist)), opposite = TRUE),
    list(title = list(text = impact_label,
                      style = list(fontWeight = "bold", fontSize = '1.2em'),
                      labels = list(style = list(fontSize =  '1.2em')),
                      min = 0, max = max(pred_df$prediction)))) %>%
  hc_xAxis(
    title = list(
      text = indicator_label,
      style = list(
        fontWeight = "bold",
        fontSize = '1.2em'
      )),
    labels = list(style = list(fontSize =  '1.2em'))
  ) %>%
  hc_add_series(
    data = density(lm_run2$independent_historic),
    type = 'area',
    name = "Historic climate variable distribution",
    # name = paste0(indicator_label, " distribution"),
    yAxis = 0,
    fillOpacity = 0.7) %>%
  hc_add_series(
    data = density(pred_df$independent_future),
    type = 'area',
    name = "Projected climate variable distribution",
    # name = paste0(indicator_label, " distribution"),
    yAxis = 0,
    fillOpacity = 0.7) %>%
  hc_add_series(
    data = dplyr::arrange(pred_df, independent_future),
    # data = dplyr::arrange(mod_vals, `Precipitation`),
    type = 'line',
    name = "Prediction",
    # hcaes(x = Independent, y = Dependent),
    # hcaes(x = `Precipitation`, y =  `Normalized direct shortage`),
    hcaes(x = independent_future, y =  prediction),
    yAxis = 1,
    fillOpacity = 0.5) %>%
  hc_colors(c("#91BEEA", "#EE8E8C", "black")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
highchart() %>%
  hc_add_series(
    data = c(29.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4)
  ) %>%
  hc_xAxis(
    tickInterval = 0.5,
    gridLineWidth = 1
  ) %>%
  hc_annotations(
    list(
      labels =
        list(
          list(
            point = list(x = 3, y = 129.2, xAxis = 0, yAxis = 0),
            text = "x: {x}<br/>y: {y}"
          ),
          list(
            point = list(x = 9, y = 194.1, xAxis = 0, yAxis = 0),
            text = "x: {x}<br/>y: {y}"
          ),
          list(
            point = list(x = 5, y = 100, xAxis = 0),
            text = "x: {x}<br/>y: {point.plotY} px"
          ),
          list(
            point = list(x = 0, y = 0),
            text = "x: {point.plotX} px<br/>y: {point.plotY} px"
          )
        )
    )
  )

n <- 10
reps <- 10000

# perform random sampling
samples <- replicate(reps, rnorm(by_district$prcp)) # 10 x 10000 sample matrix

# compute sample means
sample.avgs <- colMeans(samples)
hist(sample.avgs,
     ylim = c(0, 10),
     col = "steelblue" ,
     freq = F,
     breaks = 20)
tmp <- pred_df %>% filter(year >1980, year<2013)

highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle"))) %>%
  hc_yAxis_multiples(
    list(title = list(
      text  = impact_label,
      style = list(fontWeight = "bold",  fontSize = '1.2em')
    ),
    labels = list(style = list(fontSize =  '1.2em')),
    top = "0%",
    height = "50%"
    ),
    list(title = list(
      text = indicator_label,
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
      labels = list(style = list(fontSize =  '1.2em')),
      top      = "50%",
      height = "50%",
      opposite = TRUE)
  ) %>%
  hc_xAxis(
    title = list(text = indicator_label, style = list(fontWeight = "bold",fontSize = '1.2em'
    )),
    labels = list(style = list(fontSize =  '1.2em'))
  ) %>%
  hc_add_series(
    data = dplyr::arrange(pred_df, year),

    # data = dplyr::arrange(mod_vals, `Precipitation`),
    type = 'line',
    name = impact_label,
    # hcaes(x = Independent, y = Dependent),
    # hcaes(x = `Precipitation`, y =  `Normalized direct shortage`),
    hcaes(x = year, y =  prediction),
    yAxis = 0,
    fillOpacity = 0.5) %>%
  hc_add_series(
    data = dplyr::arrange(climate_proj, year),
    type = 'line',
    name = indicator_label,
    # hcaes(x = year, y =  prcp),
    hcaes(x = year, y =  !!climVar()),
    yAxis = 1,
    fillOpacity = 0.5) %>%
  hc_colors(c("#91BEEA", "#EE8E8C", "#34495E")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

# ggplot(data=distr, aes(lm$residuals)) +
#   geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
#   theme(panel.background = element_rect(fill = "white"),
#         axis.line.x=element_line(),
#         axis.line.y=element_line()) +
#   ggtitle("Histogram for Model Residuals")
pred_future <- predict(lm, dplyr::select(future_clim, prcp))

pred_df <- data.frame(year = 1951:2099, short_dir_norm = pred_future)
ggplot() +
  geom_line(data = pred_df, aes(x = year, y = short_dir_norm), color = "red", size =1.5) +
  geom_line(data = distr, aes(x = year, y = short_dir_norm), color = "blue", size =1.5)

ggplot() +
  geom_line(data = future_clim, aes(x = year, y = prcp), color = "red", size =1.5) +
  geom_line(data = distr, aes(x = year, y = prcp), color = "blue", size =1.5)

