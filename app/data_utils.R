# --- Shiny utils ---
basemap <- function(shp, pts = NULL) {

    leaflet() %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic2") %>%
          addPolygons(
              data = shp,
              fillColor = 'white',
              # fillColor = 'grey',
              # fillColor = ~pal_fact(BASIN),
              fillOpacity = 0.7,
              col = "black",
              opacity = 1,
              weight = 2.5,
              label = ~paste0("District  ", DISTRICT),
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
          addScaleBar("bottomleft") %>%
          # leafem::addMouseCoordinates() %>%
          leaflet::setView(lng = -105.6, lat = 39.7, zoom = 6)

}

# Leaflet map with MLR metrics as district colors
mlr_map <- function(shp, pts = NULL) {

  # Variance * R2 colors (normalized)
  pal <- colorNumeric(viridisLite::magma(n = 30, direction = -1), domain = shp$var_sensitivity_norm, reverse = F, n = 30)


      # Leaflet map
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic") %>%
        addPolygons(
          data = shp,
          color = "black",
          opacity = 1,
          fillOpacity = 0.7,
          fillColor = ~pal(var_sensitivity_norm),
          weight = 2,
          label = ~paste0("District  ", DISTRICT),
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
          pal = pal,
          values = ~var_sensitivity_norm,
          title = "Climate sensitivity",
          labFormat = labelFormat(digits = 10,),
          opacity = 1
        ) %>%
        addScaleBar("bottomleft") %>%
        # leafem::addMouseCoordinates() %>%
        leaflet::setView(lng = -105.6, lat = 39.7, zoom = 6)

}

# Map mean annual direct flow shortage as % of demand
ep_map <- function(shp, pts = NULL) {

  # values 0 - 100%
  vect = seq(0, 100, 10)

  # binned colors of mean annual direct flow shortage as % of demand
  binpal <- colorBin(
                    "Spectral",
                    domain  = vect,
                    n       = 4,
                    reverse = T,
                    pretty  = T
                    )

  # Node type legend
  node_type_label     <- c("Agricultural", "Municipal")
  node_type_pal       <- colorFactor(
                                    c("red", "dodgerblue"),
                                    domain = node_type_label,
                                    reverse = F
                                  )

  # Leaflet map
  leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat Geo Topographic") %>%
    addPolygons(
      data        = shp,
      color       = "black",
      opacity     = 1,
      fillOpacity = 0.7,
      fillColor   = ~binpal(mean_short_dir),
      weight      = 2,
      label       = ~paste0("District  ", DISTRICT),
      layerId     = ~DISTRICT,
      labelOptions = labelOptions(
        noHide = F,
        style  = list(
          "color" = "black",
          "font-weight" = "1000"))
    ) %>%
    addLegend(
      data    = shp,
      "bottomright",
      pal     = binpal,
      values  = ~vect,
      opacity = 1,
      title   = "Direct Flow Shortage",
      labFormat = labelFormat(
        digits = 10,
        suffix = " % of demand")
    )  %>%
    addScaleBar("bottomleft") %>%
    addLegend(
      pal       = node_type_pal,
      position  = "bottomleft",
      values    = node_type_label,
      # title     = "Node Type",
      group     = "Nodes",
      layerId   = "node_id") %>%
    leaflet::setView(lng = -105.6, lat = 39.7, zoom = 6)
}

# coeff_map <- function(shp, pts)
# custom highcharter theme
custom_theme <- hc_theme_merge(
  hc_theme_elementary(),
  hc_theme(
    chart = list(
      style = list(
        # fontFamily = "Montserrat"
        fontFamily = "Helvetica"
        # fontFamily = "Arial"
      )
    ),
    subtitle = list(
      style = list(
        # fontFamily = "Montserrat"
        fontFamily = "Helvetica"
        # fontFamily = "Arial"
      )
    ),
    legend = list(
      itemStyle = list(
        fontFamily = "Helvetica"
      )),
    title = list(
      style = list(
        fontFamily = "Helvetica"
      )
    )
  )
)

plot_fitted <- function(fitted_data) {

  hc_plot <-
    highchart() %>%
      hc_plotOptions(
        line    = list(marker = list(enabled = FALSE), lineWidth = 5),
        scatter = list(marker = list(symbol = "circle", radius = 3))
      )%>%
      hc_title(
        text   = "Model Performance",
        style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
      hc_yAxis(
        tickInterval = 20,
        min          = 0,
        max          = 100,
        title        = list(
          text   = "Statemod (Observed)", # margin = 60,
          style  = list(fontSize = 14, fontWeight = "bold", color = "black")),
        labels       = list(
          format = "{value} %",
          y      = 10,
          style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
      hc_xAxis(
        tickInterval = 20,
        min          = 0,
        max          = 100,
        title        = list(
          text   = "Climate Regression Simulated", # margin = 60,
          style  = list(fontSize = 14, fontWeight = "bold", color = "black")),
        labels       = list(
          format = "{value} %",
          y      = 35,
          style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
      # hc_annotations( list( labels = list(list(point = list(x = 22, y = 22, xAxis = 0, yAxis = 0), text = "x: {rsquared}")))) %>%
      hc_add_series(
        data         = fitted_data,
        name         = "Prediction",
        type         = 'point',
        yAxis        = 0,
        fillOpacity  = 0.5,
        hcaes(
          x  = Fitted,
          y  =  `Direct flow shortage normalized`)) %>%
      hc_add_series(
        data         = fitted_data,
        name         = "One to one line",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.5,
        hcaes(
          x  = `Direct flow shortage normalized`,
          y  =  `Direct flow shortage normalized`)
        ) %>%
    hc_add_theme(custom_theme) %>%
    # hc_add_theme(hc_theme_elementary()) %>%
    hc_colors(c("#70BCE2", "black")) %>%
    hc_chart(
      plotBorderWidth = 0.5,
      plotBorderColor = '#b4b4b4',
      height          = NULL)

      return(hc_plot)
}

plot_water_ts <- function(df) {
  water_ts_plot <-
      highchart() %>%
            hc_plotOptions(
              column = list(stacking = 'normal'),
              line   = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
            hc_title(
              text   = "Timeseries of district-level demand, direct-flow supply, and shortages",
              style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
            hc_yAxis(
              min      = 0,
              title    = list(
                          text   = "Water volume (AF)",
                          style  = list(fontSize = 14, fontWeight = "bold", color = "black")),
              labels   = list(
                          y      = 10,
                          style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
            hc_xAxis(
              # categories    = ws_wide$year,
              categories    = df$year,
              tickInterval  = 2,
              labels   = list(
                          y      = 35,
                          style  = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
            hc_add_series(
              # data        = ws_wide,
              data        = df,
              name        = "Total shortage",
              type        = 'column',
              tooltip     = list(pointFormat = "Total shortage: {point.short} AF"),
              fillOpacity = 0.3,
              hcaes(
                x  = year,
                y  = short
                )
              ) %>%
            hc_add_series(
              # data        = ws_wide,
              data        = df,
              name        = "Augmented supply",
              type        = 'column',
              tooltip     = list(pointFormat = "Augmented supply: {point.Supply Augmented} AF"),
              fillOpacity = 0.3,
              hcaes(
                x  = year,
                y  = `Supply Augmented`
                )
              ) %>%
            hc_add_series(
              # data        = ws_wide,
              data        = df,
              name        = "Direct Flow Supply",
              type        = 'column',
              tooltip     = list(pointFormat = "Direct Flow Supply: {point.Supply Direct flow} AF"),
              fillOpacity = 0.3,
              hcaes(
                x  = year,
                y  = `Supply Direct flow`)) %>%
            hc_add_series(
              # data        = ws_wide,
              data        = df,
              name        = "Demand",
              type        = 'line',
              tooltip     = list(pointFormat = "Demand: {point.Demand} AF"),
              hcaes(
                x  = year,
                y  = Demand
                )
              ) %>%
            hc_add_theme(custom_theme) %>%
            # hc_add_theme(hc_theme_elementary()) %>%
            hc_colors(c("#E18686",  "#70BCE2", "#2984B2", "black")) %>%
            hc_chart(
              plotBorderWidth = 0.5,
              plotBorderColor = '#b4b4b4',
              height          = NULL)
  return(water_ts_plot)
}

plot_mlr_predictors <- function(predictors, names, number_vars = 2) {

  # names <- plot_labels
  # predictors <- predictors_data
  if(number_vars == 2) {

      predictors_ts_plot <-
          highchart() %>%
              hc_plotOptions(
                column = list(stacking = 'normal'),
                line = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
              hc_title(
                text   = "Best climate predictors of shortages",
                style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
              hc_yAxis_multiples(
                list(title     = list(
                  # text      = name_df$clean_name[1],
                  # text      =  filter(name_df, var == names(ws_predictors)[2])$clean_name,
                  text      =  filter(names, var == names(predictors)[2])$clean_name,
                  style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
                  labels    = list(
                    style      = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  top    = "0%",
                  height = "50%"),
                list(title     = list(
                  # text      = name_df$clean_name[2],
                  # text      =  filter(name_df, var == names(ws_predictors)[3])$clean_name,
                  text      =  filter(names, var == names(predictors)[3])$clean_name,
                  style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
                  labels   = list(
                    style     = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  top       = "50%",
                  height    = "50%",
                  opposite  = TRUE,
                  y         = 35)) %>%
              hc_xAxis(
                tickInterval  = 2,
                y             = 35,
                categories    = predictors$year,
                labels        = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
              hc_add_series(
                data        = predictors,
                # name = name_df$clean_name[1],
                name        = filter(names, var == names(predictors)[2])$clean_name,
                type        = 'line',
                yAxis       = 0,
                fillOpacity = 0.1,
                hcaes(
                  x = year,
                  y = !!names(predictors)[2])
                ) %>%
              hc_add_series(
                data        = predictors,
                name        = filter(names, var == names(predictors)[3])$clean_name,
                type        = 'line',
                yAxis       = 1,
                fillOpacity = 0.1,
                hcaes(
                  x = year,
                  y = !!names(predictors)[3])
                  ) %>%
              hc_add_theme(custom_theme) %>%
              # hc_add_theme(hc_theme_elementary()) %>%
              hc_colors(c("darkblue",  "darkred")) %>%
              hc_chart(
                plotBorderWidth = 0.5,
                plotBorderColor = '#b4b4b4',
                height          = NULL)
      return(predictors_ts_plot)
  } else {
    predictors_ts_plot <-
          highchart() %>%
              hc_plotOptions(
                column = list(stacking = 'normal'),
                line = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
              hc_title(
                text   = "Best climate predictors of shortages in district",
                style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
              hc_yAxis(title     = list(
                text      =  filter(names, var == names(predictors)[2])$clean_name,  # text   =  filter(name_df, var == names(ws_predictors)[2])$clean_name,
                style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
                labels    = list(
                  style      = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
              hc_xAxis(
                tickInterval  = 2,
                y             = 35,
                categories    = predictors$year,  # categories    = ws_predictors$year,
                labels        = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
              hc_add_series(
                data        = predictors,  # data = ws_predictors,
                name        = filter(names, var == names(predictors)[2])$clean_name, # name = filter(name_df, var == names(ws_predictors)[2])$clean_name,
                type        = 'line',
                yAxis       = 0,
                fillOpacity = 0.1,
                hcaes(
                  x = year,
                  # y = !!names(ws_predictors)[2])
                  y = !!names(predictors)[2])
              ) %>%
              hc_colors(c("darkblue")) %>%
              hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

    return(predictors_ts_plot)
  }
}
# plot_mlr_predictors(predictors = ws_predictors, names = name_df, number_vars = 1)


plot_lm_density <- function(df, dep_label, ind_label) {

  # Values for Y axis of density plot
  density_yaxis  <- density(df$Independent)$y

  lm_density_plot <-
    highchart() %>%
          hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                         scatter = list(marker = list(symbol = "circle", radius = 5))) %>%
          hc_title(
            text   = "Linear Regression Model",
            style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
          hc_yAxis_multiples(
            list(
              title  = list(
                        text  = "",
                        style = list(fontSize = 16, fontWeight = "bold", color = "black")),
              labels = list(
                        style = list(fontSize = 16, color = "black", fontWeight = "bold")),
              min    = 0,
              y      = 40,
              max    = max(density_yaxis),
              opposite = TRUE),
            list(
              title  = list(
                        text  = dep_label,
                        style = list(fontSize = 16, fontWeight = "bold", color = "black")),
              labels = list(
                        style = list(fontSize = 16, color = "black", fontWeight = "bold")),
              min    = 0,
              y      = 40,
              max    = max(df$Dependent))) %>%
          hc_xAxis(
            title = list(
              text = ind_label,
              style = list(fontSize = 16, fontWeight = "bold", color = "black")),
            labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))
          ) %>%
          hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
          hc_add_series(
            data = density(df$Independent),
            # data = density(mod_vals$!!climVar()),
            type = 'area',
            name = "Climate variable distribution",
            # name = paste0(ind_label, " distribution"),
            yAxis = 0,
            fillOpacity = 0.8) %>%
          hc_add_series(
            data = dplyr::arrange(df, Independent),
            # data = dplyr::arrange(mod_vals, `Precipitation`),
            type = 'scatter',
            name = "Observed",
            # hcaes(x = Independent, y = Dependent),
            # hcaes(x = `Precipitation`, y =  `Normalized direct shortage`),
            hcaes(x = Independent, y =  Dependent),
            yAxis = 1,
            fillOpacity = 0.5) %>%
          hc_add_series(
            data = dplyr::arrange(df, Independent),
            # data = dplyr::arrange(mod_vals, !!climVar()),
            type = 'line',
            name = "Fitted",
            # hcaes(x = `Precipitation`, y = Fitted),
            hcaes(x = Independent, y = Fitted),
            yAxis = 1,
            fillOpacity = 0.5
          )  %>%
          hc_add_theme(custom_theme) %>%
          # hc_add_theme(hc_theme_elementary()) %>%
          hc_colors(c("#91BEEA", "#34495E", "black")) %>% # "#5D6D7E"
          hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

  return(lm_density_plot)
}

# formatabble table/colors
customGreen0 <-  "#DeF7E9"
customGreen  <-  "#71CA97"
customRed    <-  "#ff7f7f"

improvement_formatter <-
  formatter("span",
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))
            )
  )

plot_right_short <- function(df_list) {
# df_list <- node_split_short
  right_short_plot <-
        highchart() %>%
              hc_plotOptions(
                # series = list(label = list(enabled = FALSE)),
                line   = list(marker    = list(
                                  enabled = FALSE, symbol = "circle"),
                              label     = list(enabled = FALSE),
                              lineWidth = 4),
                area   = list(stacking  = 'normal',
                              marker    = list(enabled = FALSE),
                              label     = list(enabled = FALSE)),
                column = list(stacking  = 'normal',
                              label     = list(enabled = FALSE)),
                bar    = list(stacking  = 'normal',
                              label     = list(enabled = FALSE))
              ) %>%
              hc_title(
                text  = "Direct flow shortage",
                style = list(
                  fontSize = 20, fontWeight = "bold", color = "black")
                ) %>%
              # hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Spectral")) %>%
              hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Paired")) %>%
              # hc_colors(viridisLite::viridis(n = length(df_list), direction = 1)) %>%
              # hc_colors(viridisLite::cividis(n = length(df_list), direction = -1)) %>%
              hc_xAxis(
                categories = df_list[[1]][1],
                tickInterval  = 2,
                labels   = list(
                  y      = 35,
                  style  = list(fontSize = 16, color = "black", fontWeight = "bold"))
                # labels     = list(style = list(fontSize =  '1.1em')),
                ) %>%
              hc_yAxis_multiples(
                list(
                  title     = list(
                    text      =  "Direct shortage (AF)",
                    style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
                    labels    = list(
                    style      = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  top    = "0%",
                  height = "100%"),
                list(
                  title     = list(
                    text      =  "Direct shortage (% of demand)",
                    style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
                  labels   = list(
                    style     = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  top          = "0%",
                  height       = "100%",
                  min          = 0,
                  max          = 100,
                  tickInterval = 20,
                  opposite     = TRUE,
                  y            = 35)) %>%
              # hc_yAxis(title = list(
              #   text =  "Direct flow shortage (AF)",
              #   style = list(fontWeight = "bold",  fontSize = '1.2em')),
              #   labels = list(style = list(fontSize =  '1.1em'))) %>%
              hc_add_theme(custom_theme) %>%
              # hc_add_theme(hc_theme_elementary()) %>%
              hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  return(right_short_plot)
}

plot_right_demand <- function(df_list) {

  right_demand_plot <-
    highchart() %>%
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
          hc_title(
            text  = "Demand",
            style = list(
              fontSize = 20, fontWeight = "bold", color = "black")
                   ) %>%
          # hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Spectral")) %>%
          hc_colors(RColorBrewer::brewer.pal(n = length(df_list),  "Paired")) %>%
          # hc_colors(rev(RColorBrewer::brewer.pal(n = length(df_list),  "Paired"))) %>%
          # hc_colors(viridisLite::viridis(n = length(df_list), direction = 1)) %>%
          # hc_colors(viridisLite::cividis(n = length(df_list), direction = -1)) %>%
          hc_xAxis(
            categories = df_list[[1]][1],
            tickInterval  = 2,
            labels   = list(
              y      = 35,
              style  = list(fontSize = 16, color = "black", fontWeight = "bold"))
            # labels     = list(style = list(fontSize =  '1.1em')),
          ) %>%
          hc_yAxis(
            title     = list(
              text      =  "Demand (AF)",
              style     = list(fontSize = 14, color = "black", fontWeight = "bold")),
            labels    = list(
              style      = list(fontSize = 16, color = "black", fontWeight = "bold"))
            ) %>%
          hc_add_theme(custom_theme) %>%
          # hc_add_theme(hc_theme_elementary()) %>%
          hc_chart(
            plotBorderWidth  = 0.5,
            plotBorderColor  = '#b4b4b4',
            height           = NULL)
  return(right_demand_plot)
}

# exceedance probability plotting function
plot_ep <- function(
  node_ep,
  district_ep,
  basin_ep,
  ep = "short_dir_pct_dem"
) {

  if(ep == "short_dir_pct_dem") {

    exceedance_prob_plot <-
      highchart() %>%
      hc_plotOptions(
        line  = list(marker = list(enabled = FALSE), lineWidth = 6)
      ) %>%
      hc_title(
        text  = "Exceedance Probability of Direct shortage (% of demand)",
        style = list(
          fontSize = 20, fontWeight = "bold", color = "black")
      ) %>%
      hc_yAxis(
        max    = 100,
        min    = 0,
        tickInterval  = 20,
        title  = list(
          text  = "Direct Shortage % of demand",
          style = list(fontSize = 16, color = "black", fontWeight = "bold")),
        labels = list(
          style = list(fontSize = 16, color = "black", fontWeight = "bold"))
      ) %>%
      hc_legend(
        itemStyle = list(
          fontSize = 16, color = "black", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        max    = 100,
        min    = 0,
        tickInterval  = 20,
        title  = list(text = "Exceedance Probability (%)", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
        labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
      hc_add_series(
        data         = node_ep,
        hcaes(
          x = ep_pct_short_dir,
          y = short_dir_pct_dem),
        name         = "Node EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_series(
        data         = district_ep,
        hcaes(
          x = ep_pct_short_dir,
          y = short_dir_pct_dem),
        name         = "District EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_series(
        data         = basin_ep,
        hcaes(
          x = ep_pct_short_dir,
          y = short_dir_pct_dem),
        name         = "Basin EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_theme(custom_theme) %>%
      # hc_add_theme(hc_theme_elementary()) %>%
      hc_colors(c("dodgerblue", "darkred", "darkgreen")) %>%
      hc_chart(
        plotBorderWidth = 0.5,
        plotBorderColor = '#b4b4b4',
        height          = NULL
      )

    return(exceedance_prob_plot)

  } else if (ep == "short_pct_dem") {
    exceedance_prob_plot <-
      highchart() %>%
      hc_plotOptions(
        line  = list(marker = list(enabled = FALSE), lineWidth = 6)
      ) %>%
      hc_title(
        text  = "Exceedance Probability of Total shortage as a % of demand",
        style = list(
          fontSize = 20, fontWeight = "bold", color = "black")
      ) %>%
      hc_yAxis(
        max    = 100,
        min    = 0,
        tickInterval  = 20,
        title  = list(
          text  = "Total Shortage % of demand",
          style = list(fontSize = 16, color = "black", fontWeight = "bold")),
        labels = list(
          style = list(fontSize = 16, color = "black", fontWeight = "bold"))
      ) %>%
      hc_legend(
        itemStyle = list(
          fontSize = 16, color = "black", fontWeight = "bold")
      ) %>%
      hc_xAxis(
        max    = 100,
        min    = 0,
        tickInterval  = 20,
        title  = list(text = "Exceedance Probability (%)", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
        labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold"))) %>%
      hc_add_series(
        data         = node_ep,
        hcaes(
          x = ep_pct_short,
          y = short_pct_dem),
        name         = "Node EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_series(
        data         = district_ep,
        hcaes(
          x = ep_pct_short,
          y = short_pct_dem),
        name         = "District EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_series(
        data         = basin_ep,
        hcaes(
          x = ep_pct_short,
          y = short_pct_dem),
        name         = "Basin EP",
        type         = 'line',
        yAxis        = 0,
        fillOpacity  = 0.1,
        showInLegend = T) %>%
      hc_add_theme(custom_theme) %>%
      # hc_add_theme(hc_theme_elementary()) %>%
      hc_colors(c("dodgerblue", "darkred", "darkgreen")) %>%
      hc_chart(
        plotBorderWidth = 0.5,
        plotBorderColor = '#b4b4b4',
        height          = NULL
      )

    return(exceedance_prob_plot)
  }
}
plot_demand_rank <- function(node) {
  demand_rank_plot <-
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
      hc_add_theme(custom_theme) %>%
      # hc_add_theme(hc_theme_elementary()) %>%
      hc_chart(
        plotBorderWidth  = 0.5,
        plotBorderColor  = '#b4b4b4',
        height           = NULL)
  return(demand_rank_plot)

}
plot_climate_forecast <- function(
                                prediction_data,
                                historic_data,
                                future_data,
                                dep_label,
                                ind_label
                                ) {

  # prediction_data = pred_df
  # historic_data   = historic_sample
  # future_data     = future_sample
  # dep_label       = impact_label
  # ind_label       = indicator_label

  # Future density plot
  density_future <- future_data$y
  # density_future <- density(future_data[[2]])$y

  # Historic density plot
  density_hist   <- historic_data$y
  # density_hist <- density(historic_data[[2]])$y

  climate_forecasts_density_plot <-
    highchart() %>%
        hc_plotOptions(
          line    = list(
                      marker    = list(enabled = FALSE, symbol = "circle"),
                      lineWidth = 5
                      ),
          scatter = list(
                      marker    = list(symbol = "circle")
                      )
          ) %>%
        hc_title(
          text   = "Frequency Distribution",
          style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
        hc_yAxis_multiples(
          list(
            title    = list(
                        text  = "",
                        style = list(fontSize = 16, fontWeight = "bold", color = "black")
                        ),
            labels   = list(
                        style = list(fontSize = 16, color = "black", fontWeight = "bold")
                        ),
            min      = 0,
            max      = pmax(max(density_future), max(density_hist)),
            y        = 40,
            opposite = TRUE),
          list(
            title    = list(
                        text  = dep_label,
                        style = list(fontSize = 16, fontWeight = "bold", color = "black")
                        ),
            labels   = list(
                        style = list(fontSize = 16, color = "black", fontWeight = "bold")
                        ),
            min      = 0,
            max      = max(prediction_data$prediction),
            y        = 40)
          ) %>%
        hc_xAxis(
          title = list(
                    text  = ind_label,
                    style = list(fontSize = 16, fontWeight = "bold", color = "black")
                    ),
          labels = list(
                    style = list(fontSize = 16, color = "black", fontWeight = "bold")
                    )
          ) %>%
        hc_legend(
          itemStyle   = list(fontSize = 16, color = "black", fontWeight = "bold")
          ) %>%
        hc_add_series(
          data = historic_data,          # data        = density(historic_data[[2]]),
          hcaes(
            x = x,
            y = y
          ),
          type        = 'area',
          name        = paste0("Historic distribution"),
          yAxis       = 0,
          fillOpacity = 0.5) %>%
        hc_add_series(
          data = future_data,       # data        = density(future_data[[2]]),
          hcaes(
            x = x,
            y = y
          ),
          type        = 'area',
          name        = paste0("Projected distribution"),
          yAxis       = 0,
          fillOpacity = 0.4) %>%
        hc_add_series(
          data        = dplyr::arrange(prediction_data, independent_future),
          type        = 'line',
          name        = dep_label,
          hcaes(
            x = independent_future,
            y =  prediction
            ),
          yAxis       = 1,
          fillOpacity = 0.7) %>%
        hc_add_theme(custom_theme) %>%
        # hc_add_theme(hc_theme_elementary()) %>%
        hc_colors(c("#679890", "#98676F", "black")) %>% # "#5D6D7E"
        hc_chart(
          plotBorderWidth = 0.5,
          plotBorderColor = '#b4b4b4',
          height          = NULL
          )

  return(climate_forecasts_density_plot)
}

plot_future_prediction <- function(
                                predictors,
                                impacts,
                                clean_text,
                                dep_label,
                                ind_label,
                                number_vars = 2
                                ) {

  # predictors <- predictor_ts
  # impacts     <- impact_ts
  # clean_text <- name_data
  # dep_label <- impact_label
  # ind_label <- indicator_label
  # vars <- length(clean_text$var)

  if(number_vars == 2) {

      future_prediction_plot <-
            highchart() %>%
                hc_plotOptions(
                  column = list(stacking = 'normal'),
                  line   = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
                hc_title(
                  text   = "Predicted Water Shortages",
                  style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
                hc_yAxis(
                  title  = list(text = "Water volume (acre feet)"),
                  min    = 0) %>%
                hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
                hc_yAxis_multiples(
                  list(
                    title = list(
                      text  = clean_text$clean_name[1],
                      style = list(fontSize = 16, fontWeight = "bold", color = "black")),
                  labels  = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  top     = "0%",
                  height  = "33%"
                  ),
                  list(
                    title = list(
                      text   = clean_text$clean_name[2],
                      style  = list(fontSize = 16, fontWeight = "bold", color = "black")),
                    labels = list(
                       style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                    top    = "33%",
                    height = "33%",
                    opposite = TRUE)
                  ,
                  list(title = list(
                    text   = dep_label,
                    style  = list(fontSize = 16, fontWeight = "bold", color = "black")),
                    labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                    top      = "66%",
                    height = "33%",
                    opposite = F)
                ) %>%
                hc_xAxis(
                  categories   = predictors$year,
                  tickInterval = 10,
                  title        = list(
                    text  = "Year",
                    style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  labels       = list(
                    style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                  plotBands    = list(
                    list(
                      from  = 1970,
                      to    = 2020,
                      color = "rgba(0, 100, 0, 0.1)",
                      label = list(
                        text  = "Historical record",
                        style = list(fontSize = 16, color = "black", fontWeight = "bold" )))
                    )
                  ) %>%
                hc_add_series(
                  data        = predictors,
                  name        = clean_text$clean_name[1],
                  type        = 'line',
                  hcaes(x = year,  y = !!clean_text$var[1]),
                  # hcaes(x = year,  y = !!lm_run$predictors[1]),
                  yAxis       = 0,
                  fillOpacity = 0.1) %>%
                hc_add_series(
                  data        = predictors,
                  name        = clean_text$clean_name[2],
                  type        = 'line',
                  hcaes(x = year,  y = !!clean_text$var[2]),
                  # hcaes(x = year, y =  !!lm_run$predictors[2]),
                  yAxis       = 1,
                  fillOpacity = 0.1) %>%
                hc_add_series(
                  data        = dplyr::arrange(impacts, year),
                  type        = 'column',
                  name        = dep_label,
                  # hcaes(x = Independent, y = Dependent),
                  hcaes(x = year, y =  impact),
                  yAxis       = 2,
                  fillOpacity = 0.5) %>%
                hc_xAxis(categories = predictors$year) %>%
                hc_add_theme(custom_theme) %>%
                # hc_add_theme(hc_theme_elementary()) %>%
                hc_colors(c("darkblue",  "darkred", "black")) %>%
                hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
      return(future_prediction_plot)
  } else {
        future_prediction_plot <-
          highchart() %>%
            hc_plotOptions(
              column = list(stacking = 'normal'),
              line   = list(marker = list(enabled = FALSE), lineWidth = 5)) %>%
            hc_title(
              text   = "Predicted Water Shortages",
              style   = list(fontSize = 20, fontWeight = "bold", color = "black")) %>%
            hc_yAxis(
              title  = list(text = "Water volume (acre feet)"),
              min    = 0) %>%
            hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
            hc_yAxis_multiples(
              list(
                title = list(
                  # text   = name_df$clean_name[1],
                  text  = clean_text$clean_name[1],
                  style = list(fontSize = 16, fontWeight = "bold", color = "black")),
                labels  = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                top     = "0%",
                height  = "50%"
              ),
              list(title = list(
                text   = dep_label,
                style  = list(fontSize = 16, fontWeight = "bold", color = "black")),
                labels = list(style = list(fontSize = 16, color = "black", fontWeight = "bold")),
                top      = "50%",
                height = "50%",
                opposite = F)
            ) %>%
            hc_xAxis(
              categories   = predictors$year,
              tickInterval = 10,
              title        = list(
                text  = "Year",
                style = list(fontSize = 16, color = "black", fontWeight = "bold")),
              labels       = list(
                style = list(fontSize = 16, color = "black", fontWeight = "bold")),
              plotBands    = list(
                list(
                  from  = 1970,
                  to    = 2020,
                  color = "rgba(0, 100, 0, 0.1)",
                  label = list(
                    text  = "Historical record",
                    style = list(fontSize = 16, color = "black", fontWeight = "bold" )))
              )
            ) %>%
            hc_add_series(
              data        = predictors,
              name        = clean_text$clean_name[1],
              type        = 'line',
              hcaes(x = year,  y = !!clean_text$var[1]),
              # hcaes(x = year,  y = !!lm_run$predictors[1]),
              yAxis       = 0,
              fillOpacity = 0.1) %>%
            hc_add_series(
              data        = dplyr::arrange(impacts, year),
              type        = 'column',
              name        = dep_label,
              # hcaes(x = Independent, y = Dependent),
              hcaes(x = year, y =  impact),
              yAxis       = 1,
              fillOpacity = 0.5) %>%
            hc_xAxis(categories = predictors$year) %>%
            hc_add_theme(custom_theme) %>%
            # hc_add_theme(hc_theme_elementary()) %>%
            hc_colors(c("darkblue", "black")) %>%
            hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
        return(future_prediction_plot)
  }
}

# Makes table of Node IDs
make_node_table <- function(node_data) {

  # node_data <- node_table_data %>%
  #   filter(node_id == "7200938")

  node_table <-
    reactable::reactable(
      node_data,
      style    = list(fontFamily = "Work Sans, sans-serif", fontSize = "12px", fontWeight = 600),
      columns  = list(
        name       = colDef(
                name  = "Structure Name",
                align = "center"),
        node_id    = colDef(
                name  = "WDID",
                align = "center",
                cell  = function(value, index) {
                                  node_url <- sprintf("https://dwr.state.co.us/Tools/Structures/%s", node_data[index, "node_id"])
                                  node_id  <- paste(node_data[index, "node_id"])
                                  tagList(
                                    tags$a(class = "node_id", href = node_url, target = "_blank", node_id)
                                  )
                                }
          ),
        id         = colDef(
                name  = "ID",
                align = "center"),
        admin      = colDef(
                name  = "Priority Admin No.",
                align = "center"),
        admin_rank = colDef(
                name  = "Admin No. rank",
                align = "center"),
        date       = colDef(
                name  = "Appropriation Date",
                align = "center")
        ),
    highlight = TRUE,
    outlined  = TRUE,
    bordered  = T,
    theme     = reactableTheme(
      borderColor = "#black",
      cellStyle   = list(
        display         = "flex",
        flexDirection   = "column",
        justifyContent  = "center"
        ),
      headerStyle = list(
        backgroundColor = "hsl(207, 16%, 80%)"
        )
      )
    )
  # %>% add_subtitle("Structure Information",align = "center",  font_size = 16, margin = 3)

  return(node_table)
}

# Make table to display MLR results
make_mlr_table <- function(table_data) {

  rsq_colors <- data.frame("R squared" = c(0, 1))

  # Clean reactable table
  mlr_table <- reactable(
    table_data,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "14px", fontWeight = 600),
    defaultColDef = colDef(
      align = "center",
      style = pos_neg_colors("red", "green", bold = TRUE)),
    columns = list(
      "R2" = colDef(
        style = color_scales(rsq_colors, span = TRUE, colors = c("#FFFFcc", "#4c9900"),
                             opacity = .7, bold_text = T,
                             text_color = "black",
                             brighten_text_color = "black"),
      ),
      "Intercept" = colDef(
        style = pos_neg_colors("black", "black", bold = TRUE)
      )),
    # style = pos_neg_colors("red", "green", bold = TRUE))
    # ),
    highlight = TRUE,
    # outlined = T,
    # bordered = T,
    # compact = T,
    theme = reactableTheme(
      borderColor = "black",
      # cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
      headerStyle = list(
        backgroundColor = "hsl(207, 16%, 80%)"),
    )
  )
  return(mlr_table)
}


zoom_to_district = function(map, df, district){
          shp = filter(df, comid == district) # Build a buffered bounding box to center the map on:
          bounds = shp %>% # make bounding box
                st_bbox() %>% # Make spatial
                st_as_sfc() %>% # Buffer to .1 degree
                st_buffer(.1) %>% # make new bounding box
                st_bbox() %>% # extract coordinates as vector
                as.vector() # Clear all current shapes
            clearShapes(map) %>% # Add the county shape making the outline color red and the fill an opaque white
            addPolygons(
                data = shp,
                color = "red",
                fillColor  = "grey",
                fillOpacity = .3) %>%
            flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) # Fly the leaflet map to the buffered boundary
}

# Make water supply/demand/augmented supply ggplot, choice of area or bar
ws_plot <- function(data, type = c("area", "bar"), xbreaks = 16) {
  if(type == "area") {
    ggplot() +
      geom_area(
        data = data,
        aes(x = Year, y =value, fill = name),
        col = "darkgrey",
        size = 0.1,
        position = "stack"
      ) +
      geom_line(
        data = data,
        aes(x = Year, y = short_dir),
        col = "black",
        size = 1
      ) +
      labs(
        y = "Water volume (M/gal)",
        x = "Year",
        title = "Water supply and demand",
        fill  = " "
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = xbreaks))+
      hrbrthemes::theme_ipsum() +
      theme(
        axis.title.x  = element_text(size = 14,  face = "bold"),
        axis.title.y  = element_text(size = 14, face = "bold"),
        plot.title    = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  } else {
    ggplot() +
      geom_col(
        data = data,
        aes(x = Year, y =value, fill = name),
        color = "black",
        size = 0.1,
        position = "stack"
        # position = position_dodge2(width = 0)
        # stat = "identity"
      ) +
      geom_line(
        data = data,
        aes(x = Year, y = short_dir),
        col = "black",
        size = 1
      ) +
      labs(
        y = "Water volume (M/gal)",
        x = "Year",
        title = "Water supply and demand",
        fill  = " "
      ) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      scale_x_continuous(breaks = scales::pretty_breaks(n = xbreaks))+
      hrbrthemes::theme_ipsum() +
      theme(
        axis.title.x  = element_text(size = 14,  face = "bold"),
        axis.title.y  = element_text(size = 14, face = "bold"),
        plot.title    = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  }
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Robust scalar normalization
robust_scalar<- function(x){
  (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
}

# Min-Max Normalization
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

# Mean Normalization
mean_norm_minmax <- function(x){
  (x- mean(x)) /(max(x)-min(x))
}
