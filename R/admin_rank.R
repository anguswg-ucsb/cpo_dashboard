# Script for admin rank plots
remove(list = ls())

# Data manipulation
library(tidyr)
library(dplyr)
library(ggplot2)

# Plotting packages
library(highcharter)

ditch_data  <- readRDS("shortages_by_right_rank_year.rds")

# single node ID
node  <- ditch_data %>%
  filter(node_id == "0700699") %>%
  mutate(demand_rank = round(demand_rank, 2))

highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE), lineWidth = 7)) %>%
  hc_title(text = "Demand rank") %>%
  hc_add_series(
    data = node,
    type = 'line',
    name = unique(node$id),
    hcaes(
      x     = year,
      group = id,
      y     = demand_rank
    ),
    fillOpacity = 0.5
  ) %>%
  hc_colors(RColorBrewer::brewer.pal(n = length(unique(node$id)),  "Paired")) %>%
  hc_xAxis(
    categories = node$year,
    labels     = list(style = list(fontSize =  '1.1em'))) %>%
  hc_yAxis(
    title  = list(
      text   = "Demand rank",
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
    labels = list(style = list(fontSize =  '1.1em'))) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_chart(
    plotBorderWidth  = 0.5,
    plotBorderColor  = '#b4b4b4',
    height           = NULL)

hc_plotOptions(
  # series = list(label = list(enabled = FALSE)),
  line   = list(
    marker   = list(enabled = FALSE, symbol = "circle"),
    label    = list(enabled = FALSE), lineWidth = 4),
  area   = list(
    stacking = 'normal',
    marker   = list(enabled = FALSE),
    label    = list(enabled = FALSE)),
  column = list(
    stacking = 'normal',
    label    = list(enabled = FALSE)),
  bar    = list(
    stacking = 'normal',
    label    = list(enabled = FALSE))
) %>%
  hc_title(text = "Demand") %>%
  # hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Spectral")) %>%
  hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Paired")) %>%
  # hc_colors(rev(RColorBrewer::brewer.pal(n = length(df_list),  "Paired"))) %>%
  # hc_colors(viridisLite::viridis(n = length(df_list), direction = 1)) %>%
  # hc_colors(viridisLite::cividis(n = length(df_list), direction = -1)) %>%
  hc_xAxis(
    categories = df_list[[1]][1],
    labels     = list(style = list(fontSize =  '1.1em'))) %>%
  hc_yAxis(
    title  = list(
      text   = "Demand (AF)",
      style  = list(fontWeight = "bold",  fontSize = '1.2em')),
    labels = list(style = list(fontSize =  '1.1em'))) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_chart(
    plotBorderWidth  = 0.5,
    plotBorderColor  = '#b4b4b4',
    height           = NULL)
# node for plotting admin rank
node_summary <- node %>%
  group_by(id) %>%
  summarise(
    admin_rank = mean(admin_rank)
  ) %>%
  arrange(admin_rank) %>%
  mutate(color = "red")

# district for plotting admin rank
district_summary <- ditch_data %>%
  filter(district == "7") %>%
  group_by(id) %>%
  summarise(
    admin_rank = mean(admin_rank)
  ) %>%
  arrange(admin_rank) %>%
  # filter(!id %in% node$id) %>%
  mutate(color = "green")


admin_ranks <- bind_rows(node_summary, district_summary) %>%
  arrange(admin_rank) %>%
  group_by(color)

library(tidyverse)

dfr <- data.frame(sample=c("A","B","C","D"),part=c(2,3,4,6),cat=c(5,7,3,3))
dfr1 <- dfr %>% pivot_longer(cols = -sample, names_to = "metric")
dfr1

dfr1 %>%
  hchart(.,"scatter",hcaes(x=metric,y=value,group=factor(sample))) %>%
  hc_xAxis(type="category",title=list(text="Metrics"),crosshair=TRUE) %>%
  hc_yAxis(type="linear",title=list(text="Counts"),crosshair=TRUE) %>%
  hc_chart(zoomType="xy",inverted=T) %>%
  hc_tooltip(useHTML=TRUE,formatter = JS(
    "function(args){
      var this_point_index = this.series.data.indexOf(this.point);
      var this_series_index = this.series.index;
      var that_series_index = this.series.index == 0 ? 1 : 0;
      var that_series = args.chart.series[that_series_index];
      var that_point = that_series.data[this_point_index];
      return ('<b>Sample: </b>'+this.point.sample+'</br>');
    }"
  ))

highchart() %>%
  hc_plotOptions( line = list(marker = list(enabled = FALSE), lineWidth = 7)) %>%
  hc_xAxis(categories = admin_ranks$id) %>%
  hc_add_series(
    data = admin_ranks,
    # data = dplyr::arrange(mod_vals, !!climVar()),
    type = 'column',
    name = "single node",
    # hcaes(x = `Precipitation`, y = Fitted),
    hcaes(
      x = id,
      group = color,
      y = admin_rank
      ),
    fillOpacity = 0.5
  ) %>%
  hc_xAxis(categories = admin_ranks$id)
  hc_xAxis(
    categories   = admin_ranks$admin_rank,
    title        = list(
      text  = "Admin rank",
      style = list(fontSize = 16, color = "black", fontWeight = "bold")),
    labels       = list(
      style = list(fontSize = 16, color = "black", fontWeight = "bold"))
  )
  hc_add_series(
    data = dplyr::arrange(node_summary, admin_rank),
    # data = dplyr::arrange(mod_vals, !!climVar()),
    type = 'column',
    name = "single node",
    # hcaes(x = `Precipitation`, y = Fitted),
    hcaes(x = id, y = admin_rank),
    fill = "red",
    # yAxis = 1,
    fillOpacity = 0.5
  ) %>%
  hc_add_series(
    data = dplyr::arrange(district_summary, admin_rank),
    # data = dplyr::arrange(mod_vals, !!climVar()),
    type = 'column',
    name = "district nodes",
    # hcaes(x = `Precipitation`, y = Fitted),
    hcaes(x = id, y = admin_rank),
    # yAxis = 1,
    fillOpacity = 0.5
  )  %>%
  # hc_colors(c("#91BEEA", "#34495E", "black")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  # hc_title(text = "Exceedance Probability of Direct shortage as a % of demand", style = list(fontSize = 28, fontWeight = "bold", color = "black")) %>%
#   hc_yAxis(
#     lineColor = 'black',
#     lineWidth = 3,
#     max = 100,
#     min = 0,
#     title = list(text = "Augment supply as percent of demand", margin = 20, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
#     labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold"))) %>%
#   hc_legend( itemStyle = list(fontSize = 26, color = "black", fontWeight = "bold")) %>%
#   hc_xAxis(
#     lineColor = 'black',
#     lineWidth = 3,
#     max = 100,
#     min = 0,
#     title = list(text = "Exceedance probability (%)", margin = 25, style = list(fontSize = 28, color = "black", fontWeight = "bold")),
#     labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold")))
#
#
# ggplot() +
#   geom_col(data = district_summary, aes(x = reorder(id, -admin_rank), y = admin_rank)) +
  gghighlight::gghighlight(id %in% node$id) +
  theme(
    axis.text.x = element_blank()
  )

ggplot() +
  geom_line(data = node, aes(x = year, y = demand_rank, col = id))
  # geom_col(data = node, aes(x = year, y = demand_rank, fill = id), position =  position_dodge2())
  # facet_wrap(~id)















