# ---- Water supply data summarized to the year ----
ws_data <- by_admin %>%
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

# ---- WIDE district demand, aug supply, supply data ----
ws_wide <- ws_data %>%
  filter(district == 6)  %>%
  # filter(district == district_id$district)  %>%
  mutate(
    year                  = as.numeric(as.character(year)),
    aug_supply2           = aug_supply + supply_dir,
    demand2               = demand - aug_supply2,
    direct_flow_shortage  = aug_supply + short
  ) %>%
  rename(
    "Supply Augmented2"    = aug_supply2,
    "Supply Augmented"     = aug_supply,
    "Demand"               = demand,
    "Demand_diff"          = demand2,
    "Supply Direct flow"   = supply_dir,
    # "Direct Flow Shortage" = short_dir,
    "Direct Flow Shortage" = direct_flow_shortage) %>%
  mutate(across(where(is.numeric), round, 0))
# ---- HC Dem/sup bar + shortage line ----
stk_hc <-
  highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 10, groupPadding = 0.6)) %>%
  hc_yAxis(
        min = 0,
        tickInterval = 10000,
        title = list(
          text = "Water volume (AF)",
          # text = "Water volume (AF)",
          style = list(fontSize = 46, fontWeight = "bold", color = "black")),
        labels = list(
          y = 20,
          style = list(fontSize = 34, color = "black", fontWeight = "bold"))
    ) %>%
  hc_legend(itemStyle = list(fontSize = 20, color = "black")) %>%
  hc_add_series(
        data = ws_wide, name = "Direct Flow Shortage",
        type = 'column', hcaes(x = year, y = `Direct Flow Shortage`),
        tooltip = list(pointFormat = "Direct Flow Shortage: {point.Direct Flow Shortage} AF"),
        # yAxis = 1,
        fillOpacity = 0.1) %>%
  hc_add_series(
    data = ws_wide, name = "Direct Flow Supply",
    type = 'column',  hcaes(x = year, y = `Supply Direct flow`),
    tooltip = list(pointFormat = "Direct Flow Supply: {point.Supply Direct flow} AF"),
    # yAxis = 1,
    fillOpacity = 0.1) %>%
  hc_add_series(
    data = ws_wide, name = "Demand",
    type = 'line', hcaes(x = year, y = Demand),
    tooltip = list(pointFormat = "Demand: {point.Demand} AF")
    # yAxis = 1
  )  %>%
  hc_xAxis(
    categories = ws_wide$year,
    tickInterval = 2,
           labels = list(
    align = "center",
    y = 30,
    padding = 6,
    style = list(fontSize = 34, color = "black", fontWeight = "bold"))
    ) %>%
  hc_colors(c("#C00000", "#00B0F0", "black")) %>%
  hc_legend(enabled = F) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL)
stk_hc

highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 6)) %>%
  hc_yAxis(
    # tickInterval = 10,
    title = list(
      text = "Water volume (acre feet)",
      style = list(fontSize = 24, fontWeight = "bold", color = "black")),
    min = 0,
    labels = list(
      # format = "{value} %",
      y = 10,
      style = list(fontSize = 22, color = "black", fontWeight = "bold"))
  ) %>%
  hc_xAxis(
    categories = ws_wide$year,
    tickInterval = 2,
    # title = list(
    #   # text = "Water volume (acre feet)",
    #   style = list(fontSize = 24, fontWeight = "bold", color = "black")),
    # min = 0,
    labels = list(
      # format = "{value} %",
      y = 30,
      style = list(fontSize = 22, color = "black", fontWeight = "bold"))
  ) %>%

  hc_add_series(
    data = ws_wide, name = "Total shortage",
    type = 'column', hcaes(x = year, y = short),
    tooltip = list(pointFormat = "Total shortage: {point.short} AF"),
    # yAxis = 1,
    fillOpacity = 0.3) %>%
  hc_add_series(
    data = ws_wide, name = "Augmented supply",
    type = 'column', hcaes(x = year, y = `Supply Augmented`),
    tooltip = list(pointFormat = "Augmented supply: {point.Supply Augmented} AF"),
    # yAxis = 1,
    fillOpacity = 0.3) %>%
  hc_add_series(
    data = ws_wide, name = "Direct Flow Supply",
    type = 'column',  hcaes(x = year, y = `Supply Direct flow`),
    tooltip = list(pointFormat = "Direct Flow Supply: {point.Supply Direct flow} AF"),
    # yAxis = 1,
    fillOpacity = 0.3) %>%
  # hc_add_series(data = ws_wide, name = "Total shortage",type = 'line', hcaes(x = year, y = short), yAxis = 0)  %>%
  # hc_add_series(data = ws_wide, name = "Direct shortage",type = 'line', hcaes(x = year, y = short_dir), yAxis = 0)  %>%
  hc_add_series(
    data = ws_wide, name = "Demand",
    type = 'line', hcaes(x = year, y = Demand),
    tooltip = list(pointFormat = "Demand: {point.Demand} AF")
    # yAxis = 1
  )  %>%
  hc_colors(c("#E18686",  "#70BCE2", "#2984B2", "black")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
