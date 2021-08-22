library(tidyverse)

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
ws_district <- ws_data %>%
  # filter(district == 6)  %>%
  filter(district == district_id$district)  %>%
  mutate(
    year          = as.numeric(as.character(year)),
    aug_supply2   = aug_supply + supply_dir,
    demand2       = demand - aug_supply2
  ) %>%
  rename(
    "Supply Augmented"    = aug_supply,
    "Demand"              = demand2,
    "Supply Direct flow"  = supply_dir) %>%
  pivot_longer(cols = c("Demand", "Supply Augmented", "Supply Direct flow"))

# %>%
#   dplyr::select(1:8, 30)
  # left_join(dplyr::select(breaks, district, admin_number, water_right), by = c("district", "admin_number"))

## ------ Join model data w/ admin level supply/demand/short -------
district_data <- left_join(
  by_admin2,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  # filter(district == 6) %>%
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
  dplyr::select(1:8, 30)

distr <- district_data %>%
  filter(district == 6)  %>%
  mutate(
    year        = as.numeric(as.character(year)),
    aug_supply2 = aug_supply + supply_dir,
    demand2     = demand - aug_supply2,
    demand_val     = demand
         ) %>%
  rename(
    "Supply Augmented" = aug_supply2,
    "Demand" = demand2,
    "Supply Direct flow" = supply_dir) %>%
  pivot_longer(cols = c("Demand", aug_supply, "Supply Direct flow"))

# % of demand values for bar chart
distr_pct <- distr %>%
  mutate(value_pct = value/demand_val)


distr_wide <- district_data %>%
  filter(district == 6)  %>%
  mutate(
    year        = as.numeric(as.character(year)),
    aug_supply2 = aug_supply + supply_dir,
    demand2     = demand - aug_supply2
  ) %>%
  rename(
    "Supply Augmented" = aug_supply2,
    "Demand" = demand,
    "Supply Direct flow" = supply_dir)

ws_plot(ws_district)
tmp_short <- distr %>%
  dplyr::select(year, short_dir) %>%
  unique()


ggplot()  +
  geom_col(data = tmp_short,
           aes(x = year, y = short_dir),
           fill = "darkred", alpha = 0.6, col = "black"
           ) +
  geom_line(data = distr,
            aes(x = year, y =value, color = name),
             size = 1.25)



scale_x_continuous(breaks = scales::pretty_breaks(n = 32))
unique(distr$short_dir)
stk_gg <- ggplot() +
  geom_col(data = distr,
            aes(x = year, y =value, fill = name),
           color = "black",
           position = "stack"
            # position = position_dodge2(width = 0)
           # stat = "identity"
  ) +
  geom_area(data = distr,
            aes(x = year, y =value, fill = name), col = "darkgrey", size = 0.1,
            position = "stack"
            ) +
  geom_line(data = distr,
            aes(x = year, y = short_dir),
            col = "black",
            size = 1.25
  ) +
  labs(
    y = "Water volume (M/gal)",
    x = "Year",
    title = "Water supply and demand",
    fill  = " "
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16))+
  hrbrthemes::theme_ipsum() +
  theme(
    axis.title.x  = element_text(size = 14,  face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold"),
    plot.title    = element_text(size = 16, face = "bold", hjust = 0.5)
        )


stk_gg
plotly::ggplotly(stk_gg)


highchart() %>%
  # hc_plotOptions(series = list(stacking = 'normal')) %>%
  hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
  hc_add_series(
    data = tmp_short, name = "Shortage",
    type = 'column', hcaes(x = year, y = short_dir))  %>%
  hc_add_series(
    data = distr_wide, name = "Demand",
    type = 'line', hcaes(x = year, y = Demand)) %>%
  hc_add_series(
    data = distr_wide, name = "Augmented supply",
    type = 'line', hcaes(x = year, y = `Supply Augmented`)) %>%
  hc_add_series(
    data = distr_wide, name = "Direct Flow Supply",
    type = 'line',  hcaes(x = year, y = `Supply Direct flow`)) %>%
  hc_xAxis(categories = distr$year) %>%
  # hc_colors(c("darkred", "lightgreen", "#1aadce", "yellow")) %>%
  hc_colors(c("darkred", "darkblue", "forestgreen", "dodgerblue")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  # hc_yAxis_multiples(
  #   list(min = 0, max = 35000, opposite = TRUE),
  #   list(min = 0, max = 80000)
  # ) %>%
 # hc_add_series(
 #  data = tmp_short, name = "Shortage",
 #  type = 'column', hcaes(x = year, y = short_dir), yAxis = 1)  %>%
 #  hc_add_series(
 #    data = distr_wide, name = "Demand",
 #    type = 'line', hcaes(x = year, y = Demand), yAxis = 1) %>%
 #  hc_add_series(
 #    data = distr_wide, name = "Augmented supply",
 #    type = 'line', hcaes(x = year, y = `Supply Augmented`), yAxis = 1) %>%
 #  hc_add_series(
 #    data = distr_wide, name = "Supply Direct Flow",
 #    type = 'line',  hcaes(x = year, y = `Supply Direct flow`), yAxis = 1) %>%

  # hc_add_series(data = distr, name = "Direct flow shortages",
  #               type = 'spline', hcaes(x = year, y = short_dir)) %>%
  # hc_xAxis(categories = distr$year) %>%
  # # hc_colors(c("darkred", "lightgreen", "#1aadce", "yellow")) %>%
  # hc_colors(c("darkred", "dodgerblue", "lightblue", "darkblue")) %>%
  # hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)













