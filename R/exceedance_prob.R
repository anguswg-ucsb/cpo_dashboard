# Script for creating exceedance probabilities data & plots
remove(list = ls())

# Data manipulation
library(tidyr)
library(dplyr)

# Plotting packages
library(highcharter)

node_ep      <- readRDS("node_ep.rds") %>%
district_ep  <- readRDS("district_ep.rds")
basin_ep     <- readRDS("basin_ep.rds")

# unique(node_err$node_id)
node_split_demand <- ditch_data %>%
  filter(node_id == "7200938") %>%
  # filter(node_id == "0200809") %>%
  mutate(admin = as.numeric(admin)) %>%      # mutate(admin = round(as.numeric(admin), 0)) %>%
  group_by(id) %>%
  mutate(x = year, y = round(demand, 1)) %>% # mutate(x = year, y = round(demand_dir, 1)) %>%
  dplyr::select(x, y, admin, id) %>%
  group_split() %>%
  as.list() %>%
  rev()


group_split() %>%
  as.list()

test_node_ep <- node_ep %>%
  filter(node_id == "3800545")

test_district_ep <- district_ep %>%
  filter(district == 38)

test_basin_ep <- basin_ep %>%
  filter(basin == "colorado", district == 38)

plot_ep(
  node_ep     = test_node_ep,
  district_ep = test_district_ep,
  basin_ep    = test_basin_ep,
  ep          = "short_pct_dem"
  # ep          = "short_dir_pct_dem"
)

# round values and save
node_ep      <- readRDS("node_ep.rds") %>%
  mutate(across(where(is.numeric), round, 2))

district_ep  <- readRDS("district_ep.rds") %>%
  mutate(across(where(is.numeric), round, 2))

basin_ep     <- readRDS("basin_ep.rds") %>%
  mutate(across(where(is.numeric), round, 2))

saveRDS(node_ep, "node_ep.rds")
saveRDS(district_ep, "district_ep.rds")
saveRDS(basin_ep, "basin_ep.rds")



# ********************************************
# ---- Exceedance probabilities map color ----
# ********************************************
# number of years above threshold Direct flow shortage % of demand

threshold <- district_ep %>%
  group_by(district) %>%
  mutate(
    mean   = mean(short_dir_pct_dem, na.rm = T),
    median = median(short_dir_pct_dem, na.rm = T)
  ) %>%
  mutate(
    occurance       = case_when(
      short_dir_pct_dem > mean  ~ 1,
      short_dir_pct_dem <= mean ~ 0
      ),
    total_years     = n(),
    total_occurance = sum(occurance)
  ) %>%
  summarize(
    mean_short_dir  = round(mean(mean, na.rm = T), 2),
    threshold_pct   = round(100*(mean(total_occurance)/mean(total_years)), 2)
    ) %>%
  ungroup()
# hist(threshold$threshold_pct, breaks = 10)
# hist(threshold$mean, breaks = 10)
# Test out threshold colors on leaflet map

# district shapefile path
shp_path        <- "water_districts.geojson"

# load shapefiles as spatial polygon object
shp <- sf::read_sf(paste0(shp_path), quiet = TRUE) %>%
  left_join(
    threshold,
    by = c("DISTRICT" = "district")
    ) %>%
  geojsonsf::sf_geojson()

# save as GeoJSON
geojsonio::geojson_write(shp, file = "water_districts.geojson")

save

# pal <- colorNumeric(
# viridisLite::magma(n = 10, direction = -1),
# # viridisLite::turbo(n = 30, direction = -1),
#               domain  = shp$threshold_pct,
#               reverse = F,
#               n       = 30
#               )

# RColorBrewer::display.brewer.all()
# pal <- colorNumeric("Spectral", domain = shp$threshold_pct, reverse = T, n = 30)
# pal <- colorNumeric(
#   "RdYlGn",
#   # viridisLite::viridis(n = 30, direction = 1),
#                     domain =shp$threshold_pct_normal,
#                     # domain = shp$threshold_pct,
#                     reverse = T, n = 30)


qpal <- colorQuantile("Spectral",reverse = T,  vect, n = 5)
# pal2 <- colorNumeric(
#   "RdYlGn",
#   # viridisLite::viridis(n = 30, direction = 1),
#   domain =vect,
#   # domain = shp$threshold_pct,
#   reverse = T, n = 30
#   )

vect = seq(0, 100, 10)
binpal <- colorBin("Spectral",
                   domain =vect,
                   # domain = shp$threshold_pct,
                   4,  reverse = T,
                   pretty = T)
# Leaflet map
leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic") %>%
  addPolygons(
    data = shp,
    color = "black",
    opacity = 1,
    fillOpacity = 0.7,
    fillColor = ~binpal(mean_short_dir),
    weight = 2,
    label = ~paste0( mean_short_dir, " \nThreshold %:  ", threshold_pct),
    layerId = ~DISTRICT,
    labelOptions = labelOptions(
      noHide = F,
      # direction = 'center',
      # textOnly = F)
      style = list(
        "color" = "black",
        "font-weight" = "1000")
    )
  ) %>%
  addLegend(
    data = shp,
    "bottomright",
    pal = binpal,
    values = ~vect,
    title = "Direct Flow Shortage",
    labFormat = labelFormat(
      digits = 10,
      suffix = "% of demand"),
    opacity = 1
  ) %>%
  addScaleBar("bottomleft") %>%
  # leafem::addMouseCoordinates() %>%
  leaflet::setView(lng = -105.6, lat = 39.7, zoom = 6)



  # pivot_longer(cols = c(-district))

plt <- ggplot() +
  geom_col(data = threshold, aes(x = name, y = value)) +
  facet_wrap(~district)
plotly::ggplotly(plt)



# ************************************


node_ep <- ditch_data %>%
  filter(node_id == node()) %>%
  # filter(node_id == "0100514") %>%
  # filter(node_id == "0700699") %>%
  group_by(node_id, year) %>%
  summarise(
    demand     = sum(demand_all, na.rm = T),
    short_dir  = sum(short_all, na.rm = T),
  ) %>%
  mutate(
    short_dir_pct_dem = 100*(short_dir/demand)
  ) %>%
  ungroup() %>%
  mutate(across(everything(), .fns = ~replace_na(.,0)))

node_ep <- node_ep %>%
  dplyr::select(year, short_dir_norm) %>%
  mutate(
    rank   = rank(short_dir_norm, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

# get total number of values/occurance (timestamp)
node_ep <- node_ep %>%
  mutate(
    total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )
# District exceedance prob.
district_ep <- model_data %>%
  group_by(district) %>%
  dplyr::select(year, aug_supply_norm, basin) %>%
  mutate(
    rank   = rank(aug_supply_norm, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

count_vals <- count(district_ep, "district") %>%
  dplyr::select(district, total_values = n)

district_ep <- left_join(district_ep, count_vals, by = c("district"))


# get total number of values/occurance (timestamp)
district_ep <- district_ep %>%
  mutate(
    # total_values    = ddply(., .(district), nrow),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )  %>%
  mutate(
    basin = case_when(
      basin == "south_platte" ~ "South Platte",
      basin == "colorado"     ~ "Colorado",
      basin == "gunnison"     ~ "Gunnison",
      basin == "san_juan"     ~ "San Juan",
      basin == "yampa"        ~ "Yampa",
      basin == "white"        ~ "White"
    ))

ggplot() +
  geom_line(data = district_ep, aes(x = ep_pct, y = short_dir_norm,  group = district)) +
  geom_line(data = basin_all, aes(x = ep_pct, y = short_dir_norm, group = basin, colour = basin), size = 2.5) +
  # stat_summary(fun = mean, na.rm = TRUE, group = 3, color = 'black', geom ='line')
  facet_wrap(~basin) +
  theme_bw()

basin_all <- model_data %>%
  mutate(basin = case_when(
    district == "43" ~ "white",
    district != "43"     ~ basin
  )) %>%
  group_by(year, basin) %>%
  summarize(
    demand     = sum(demand, na.rm = T),
    short      = sum(short, na.rm = T),
    short_dir  = sum(short_dir, na.rm = T),
    aug_supply = short_dir - short
  ) %>%
  mutate(
    short_norm      = 100*(short/demand),
    short_dir_norm  = 100*(short_dir/demand),
    aug_supply_norm = 100*(aug_supply/demand)
  ) %>%
  ungroup() %>%
  group_by(basin) %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
  dplyr::select(year, aug_supply_norm, basin) %>%
  mutate(
    rank   = rank(aug_supply_norm, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

count_vals_basin <- count(basin_all, "basin") %>%
  dplyr::select(basin, total_values = n)

basin_all <- left_join(basin_all, count_vals_basin, by = c("basin"))


basin_all <- basin_all  %>%
  mutate(
    # total_values    = nrow(.),
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  ) %>%
  mutate(
    basin = case_when(
      basin == "south_platte" ~ "South Platte",
      basin == "colorado"     ~ "Colorado",
      basin == "gunnison"     ~ "Gunnison",
      basin == "san_juan"     ~ "San Juan",
      basin == "yampa"        ~ "Yampa",
      basin == "white"        ~ "White"
      ))

# col_pal <- c("#30123BFF", "#3E9BFEFF", "#46F884FF", "#E1DD37FF", "#F05B12FF", "#7A0403FF")
# col_pal <- c("#30123BFF", "#3E9BFEFF", "#46F884FF", "#E1DD37FF", "#F05B12FF", "#7A0403FF")
col_pal <- c("dodgerblue", "forestgreen", "darkorange", "red3", "deeppink1", "black")
# col_pal <- c("red3", "red3", "red3", "red3", "red3", "red3")

ggplot() +
  geom_line(data = district_ep, aes(x = ep_pct, y = aug_supply_norm,  group = district)) +
  geom_line(data = basin_all, aes(x = ep_pct, y = aug_supply_norm, group = basin), colour = "red", size = 2.5) +
  # geom_line(data = basin_all2, aes(x = ep_pct, y = short_dir_norm, group = basin, colour = basin), size = 2.5) +
  # scale_color_manual(values = "red") +
  # stat_summary(fun = mean, na.rm = TRUE, group = 3, color = 'black', geom ='line')
  facet_wrap(~basin) +
  labs(
    colour = "Basin",
    y = "Augment supply as percent of demand",
    x = "Exceedance probability (%)"
  ) +
  ylim(c(0, 100)) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, vjust = .5, face = "bold"),
    strip.text.y = element_text(size = 12, color = "black", face = "bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold"),
    axis.text =  element_text(size = 12, face = "bold")
  )
# Average demand annual - by basin
avg_demand <- model_data %>%
  mutate(basin = case_when(
    district == "43" ~ "white",
    district != "43"     ~ basin
  )) %>%
  group_by(basin, year) %>%
  summarize(demand = mean(demand, na.rm = T)) %>%
  mutate(
    basin = case_when(
      basin == "south_platte" ~ "South Platte",
      basin == "colorado"     ~ "Colorado",
      basin == "gunnison"     ~ "Gunnison",
      basin == "san_juan"     ~ "San Juan",
      basin == "yampa"        ~ "Yampa",
      basin == "white"        ~ "White"
    ))

ggplot() +
  geom_col(data = avg_demand, aes(x = year, y = demand,  group = basin), fill = "dodgerblue") +
  facet_wrap(~basin) +
  labs(
    # colour = "Basin",
    y = "Average annual demand (AF)",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16, vjust = .5, face = "bold"),
    strip.text.y = element_text(size = 12, color = "black", face = "bold"),
    strip.text.x = element_text(size = 12, color = "black", face = "bold"),
    axis.text =  element_text(size = 12, face = "bold")) +
  scale_x_discrete(breaks = seq(1981, 2012, by = 5)) +
  scale_y_continuous(labels = scales::comma)
ggplot() +
  geom_line(data = basin_all, aes(x = ep_pct, y = short_dir_norm, col = basin)) +
  facet_wrap(~basin) +
  theme_bw()

 unique(model_data$district) %>% sort()

col_pal <- c("dodgerblue", "forestgreen", "darkorange", "red", "deeppink", "black")
highchart() %>%
  hc_plotOptions( line = list(marker = list(enabled = FALSE), lineWidth = 7)) %>%
  # hc_title(text = "Exceedance Probability of Direct shortage as a % of demand", style = list(fontSize = 28, fontWeight = "bold", color = "black")) %>%
  hc_yAxis(
    lineColor = 'black',
    lineWidth = 3,
    max = 100,
    min = 0,
    title = list(text = "Augment supply as percent of demand", margin = 20, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold"))) %>%
  hc_legend( itemStyle = list(fontSize = 26, color = "black", fontWeight = "bold")) %>%
  hc_xAxis(
    lineColor = 'black',
    lineWidth = 3,
    max = 100,
    min = 0,
    title = list(text = "Exceedance probability (%)", margin = 25, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold"))) %>%
  # hc_add_series(
  #   data = node_ep,
  #   name = "Node EP",
  #   # name =name_df$clean_name[1],
  #   type = 'line',
  #   hcaes(x = ep_pct,  y = short_dir_norm),
  #   yAxis = 0,
  #   fillOpacity = 0.1,
  #   showInLegend = T) %>%
  # hc_add_series(
  #   data = district_ep,
  #   name = "District EP",
  #   type = 'line',
  #   hcaes(x = ep_pct,  y = short_dir_norm),
  #   yAxis = 0,
  #   fillOpacity = 0.1,
  #   showInLegend = T) %>%
  hc_add_series(
    data = filter(basin_all, basin == "Colorado"),
    name = "Colorado",
    type = 'line',
    hcaes(x = ep_pct,  y = aug_supply_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = filter(basin_all, basin == "Gunnison"),
    name = "Gunnison",
    type = 'line',
    hcaes(x = ep_pct,  y = aug_supply_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = filter(basin_all, basin == "San Juan"),
    name = "San Juan",
    type = 'line',
    hcaes(x = ep_pct,  y = aug_supply_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = filter(basin_all, basin == "South Platte"),
    name = "South Platte",
    type = 'line',
    hcaes(x = ep_pct,  y = aug_supply_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = filter(basin_all, basin == "White"),
    name = "White",
    type = 'line',
    hcaes(x = ep_pct,  y = aug_supply_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
      data = filter(basin_all, basin == "Yampa"),
      name = "Yampa",
      type = 'line',
      hcaes(x = ep_pct,  y = aug_supply_norm),
      yAxis = 0,
      fillOpacity = 0.1,
      showInLegend = T) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(col_pal) %>%
    # hc_colors(c("#30123BFF", "#3E9BFEFF", "#46F884FF", "#E1DD37FF", "#F05B12FF", "#7A0403FF")) %>%
  # hc_colors(c("darkred", "darkblue", "darkgreen", "darkorange", "purple")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

  highchart() %>%
  hc_plotOptions( line = list(marker = list(enabled = FALSE), lineWidth = 7)) %>%
  # hc_title(text = "Exceedance Probability of Direct shortage as a % of demand", style = list(fontSize = 28, fontWeight = "bold", color = "black")) %>%
  hc_yAxis(
    lineColor = 'black',
    lineWidth = 3,
    max = 100,
    min = 0,
    title = list(text = "Shortage as percent of demand", margin = 20, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 28, color = "black", fontWeight = "bold"))) %>%
  hc_legend( itemStyle = list(fontSize = 28, color = "black", fontWeight = "bold")) %>%
  hc_xAxis(
    lineColor = 'black',
    lineWidth = 3,
    max = 100,
    min = 0,
    title = list(text = "Exceedance probability (%)", margin = 25, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
    labels = list(style = list(fontSize = 28, color = "black", fontWeight = "bold"))) %>%
hc_add_series(
  data = basin_ep1,
  name = "South Platte",
  type = 'line',
  hcaes(x = ep_pct,  y = short_dir_norm),
  yAxis = 0,
  fillOpacity = 0.1,
  showInLegend = T) %>%
  hc_add_series(
    data = basin_ep2,
    name = "Colorado",
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  # hc_add_theme(hc_theme_smpl()) %>%
  hc_colors(c("darkred", "darkblue", "darkgreen", "pink")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
ep_plot
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
sp <- shp %>%
  filter(BASIN == "South Platte") %>%
  st_drop_geometry()

all_districts <- model_data %>%
  filter(district %in% sp$DISTRICT) %>%
  dplyr::select(year, short_dir_norm)

ep_historic <- all_districts %>%
  mutate(
    rank   = rank(short_dir_norm, na.last = "keep", ties.method = "first")
  ) %>%
  arrange(rank)

# get total number of values/occurance (timestamp)
ep_historic <- ep_historic %>%
  mutate(
    # total_values    = nrow(.),
    total_values    = 352,
    ep              = ((-rank/(total_values + 1)) + 1),
    ep_pct          = 100*ep
  )

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
    data = node_ep,
    name = "Node EP",
    # name =name_df$clean_name[1],
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = district_ep,
    name = "District EP",
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = basin_ep,
    name = "Basin EP",
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_series(
    data = ep_historic,
    name = " All districts EP",
    type = 'line',
    hcaes(x = ep_pct,  y = short_dir_norm),
    yAxis = 0,
    fillOpacity = 0.1,
    showInLegend = T) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  # hc_add_theme(hc_theme_smpl()) %>%
  hc_colors(c("darkred", "darkblue", "darkgreen", "pink")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

ggplot() +
  geom_line(data = ep_historic, aes(x = ep_pct, y = short_dir_norm), size = 1.5) +
  ylim(0, 100) +
  # facet_wrap(~district) +
  # geom_text(data = filter(ep_historic, short_dir_norm == max(short_dir_norm)),
  #           aes(label = district,  x = ep_pct, y = short_dir_norm), hjust = -1) +
  labs(
    title = "Exceedance probability all districts",
    x     = "Exceedance probability (%)",
    y     = "Direct shortage normalized (% of demand)"
  ) +
  theme_bw()
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
