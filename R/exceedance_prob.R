# ----------------------------------------------
# ---- Projected shortages exceedance prob  ----
# ----------------------------------------------
pred_df <- pred_df %>%
  mutate(pred2 = case_when(
      prediction > 100 ~ 100,
      prediction <= 100 ~ prediction
        )
      )

ep_prediction <- pred_df %>%
  dplyr::select(year, pred2)
  # filter(district == district_id$district) %>%
  # filter(district == 1) %>%
  # mutate(
  #   sn    = 100*(short/demand),
  #   sdn   = 100*(short_dir/demand)
  # ) %>%
  # mutate(across(everything(), .fns = ~replace_na(.,0)))

ep_prediction <- ep_prediction %>%
  # dplyr::select(date, sn, sdn) %>%
  mutate(
    rank   = rank(pred2, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

# get total number of values/occurance (timestamp)
ep_prediction <- ep_prediction %>%
  mutate(
    total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )

# ----------------------------------------------
# ----- Historic shortages exceedance prob  ----
# ----------------------------------------------
mod_df2 <- mod_df %>%
  mutate(
    short_dir_norm2 = 10^short_dir_norm,
    year            = 1981:2012
  )

ep_historic <- mod_df2 %>%
  dplyr::select(year, short_dir_norm2)

ep_historic <- ep_historic %>%
  mutate(
    rank   = rank(short_dir_norm2, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

# get total number of values/occurance (timestamp)
ep_historic <- ep_historic %>%
  mutate(
    total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )
# -------------------------------------------------------------
# ---- Plot Exceedance prob of future projected shortages -----
# -------------------------------------------------------------
highchart() %>%
  hc_plotOptions( line = list(marker = list(enabled = FALSE), lineWidth = 6)) %>%
  hc_title(text = "Exceedance Probability of Direct shortage as a % of demand", style = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
  hc_yAxis(
    max = 100,
    min = 0,
    title = list(text = "Shortage % of demand", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
  hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
  hc_xAxis(
    max = 100,
    min = 0,
    title = list(text = "Exceedance Probability (%)", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
  hc_add_series(
    data = ep_historic,
    name = "Historic record",
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm2),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = F) %>%
  hc_add_series(
    data = ep_prediction,
    name = "Future projection",
    type = 'line',
    hcaes(x = ep_pct,  y = pred2),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = F) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  # hc_add_theme(hc_theme_smpl()) %>%
  hc_colors(c("black", "red")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

# ------------------------------
# ---- Plot all future data ----
# ------------------------------

highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
  hc_title(text = "Forecasted Water Shortages", style = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
  hc_yAxis(title = list(text = "Water volume (acre feet)"), min = 0) %>%
  hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
  hc_yAxis_multiples(create_yaxis(naxis = 4)) %>%
  # hc_yAxis_multiples(
  #   list(title = list(
  #     text   = name_df$clean_name[1],
  #     style = list(fontSize = 16, fontWeight = "bold", color = "black")
  #   ),
  #   labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
  #   top = "0%",
  #   height = "33%"
  #   ),
  #   list(title = list(
  #     text   = name_df$clean_name[2],
  #     style  = list(fontSize = 16, fontWeight = "bold", color = "black")),
  #     labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
  #     top      = "33%",
  #     height = "33%",
  #     opposite = TRUE)
  #   ,
  #   list(title = list(
  #     text   = impact_label,
  #     style  = list(fontSize = 16, fontWeight = "bold", color = "black")),
  #     labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
  #     top      = "66%",
  #     height = "33%",
  #     opposite = F)
  # ) %>%
  hc_xAxis(
    categories = predictor_ts$year,
    tickInterval = 10,
    title = list(text = "Year", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    plotBands = list(
      list(from =1981, to =2020, color = "rgba(0, 100, 0, 0.1)",
           label = list(text = "Historical record", style = list(fontSize = 16, color = "black", fontWeight = "bold" ))))) %>%
  hc_add_series(
    data = predictor_ts,
    name =name_df$clean_name[1],
    type = 'line',
    hcaes(x = year,  y = !!lm_run$predictors[1]),
    yAxis = 0,
    fillOpacity = 0.1) %>%
  hc_add_series(
    data = predictor_ts,
    name =name_df$clean_name[2],
    type = 'line', hcaes(x = year, y =  !!lm_run$predictors[2]),
    yAxis = 1, fillOpacity = 0.1) %>%
  hc_add_series(
    data = dplyr::arrange(impact_ts, year),
    type = 'column', name = impact_label,
    # hcaes(x = Independent, y = Dependent),
    hcaes(x = year, y =  impact),
    yAxis = 2, fillOpacity = 0.5) %>%
  hc_xAxis(categories = predictor_ts$year) %>%
  hc_colors(c("darkblue",  "darkred", "black")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)


short_month <- readRDS("model_data_month_v10.rds") %>%
  mutate(date = paste0(wyear, "-", month, "-01")) %>%
  dplyr::select(wyear, month, date, district, demand, short, short_norm, short_dir,  short_dir_norm)

ep_month <- short_month %>%
  filter(district == 6) %>%
  mutate(
    sn  = 100*(short/demand),
    sdn = 100*(short_dir/demand)
    ) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0)))

ep_month <- ep_month %>%
  dplyr::select(date, sn, sdn) %>%
  mutate(
    rank   = rank(sdn, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

# get total number of values/occurance (timestamp)
ep_month <- ep_month %>%
  mutate(
    total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )

ggplot() +
  geom_line(data = ep_month, aes(x = ep_pct, y = sdn))
ggplot() +
  geom_line(data = ep_month, aes(x = date, y = short_dir_norm))
dplyr::select(year, short_norm, short_dir_norm) %>%

district_data <- model_data %>%
  filter(district == 6)

ep_shortage <- district_data %>%
  dplyr::select(year, short_norm, short_dir_norm) %>%
  mutate(
      rank   = rank(short_dir_norm, na.last = "keep", ties.method = "first")
    ) %>%
  arrange(rank)
ep_shortage

# get total number of values/occurance (timestamp)
ep_shortage <- ep_shortage %>%
  mutate(
    total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )

gg2 <- ggplot() +
  geom_line(data = ep_shortage, aes(x = ep_pct, y = short_dir_norm)) +
  labs(title = "Yearly EP")
gg1 <- ggplot() +
  geom_line(data = ep_month, aes(x = ep_pct, y = sdn)) +
  labs(title = "Monthly EP")
# library(patchwork)
gg1 + gg2


highchart() %>%
  hc_plotOptions( line = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
  hc_title(text = "Forecasted Water Shortages", style = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
  hc_yAxis(
    max = 100,
    min = 0,
    title = list(text = "Water volume (acre feet)"), min = 0) %>%
  hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
  hc_xAxis(
    max = 100,
    min = 0,
    title = list(text = "Year", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
  hc_add_series(
    data = ep_shortage,
    # name =name_df$clean_name[1],
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1)

# admins = c(4535.00000, 8736.00000, 35731.00000)
# filter to single node ID and admin
node <- water_users %>%
  mutate(admin = as.numeric(admin)) %>%
  filter(
    node_id == "0600513",
    admin   == 8736
  )
# dplyr::select(date, demand:short_dir) %>%
# arrange(-short_dir)

# calc shortage normalized values
node_normal <- node %>%
  group_by(date) %>%
  summarise(
    short_dir       = sum(short_dir, na.rm = T),
    demand          = sum(demand, na.rm = T) + 0.00001,
    short_dir_norm  = round(100*short_dir/demand, 3)
  )

# rank and arrange values from greatest to least
node_tmp <- node_normal %>%
  mutate(
    rnk = rank(node_normal$short_dir_norm,
               na.last = "keep",
               ties.method = "average")
  ) %>%
  arrange(rnk)

# get total number of values/occurance (timestamp)
node_tmp <- node_tmp %>%
  mutate(
    total_values    = nrow(node_tmp),
    ep              = (-rnk/(total_values + 1)) + 1
  )

ggplot() +
  geom_line(data = node_tmp, aes(x = ep, y = short_dir_norm))
