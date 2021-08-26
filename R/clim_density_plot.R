density(by_district$prcp)
density_vals <- density(mod_vals$Independent)
density_yaxis <- density_vals$y

highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle") )) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Distribution"), min = 0, max = max(density_yaxis), opposite = TRUE),
    list(title = list(text = "Dependent variable"), min = 0, max = max(mod_vals$Dependent))
  ) %>%
  hc_add_series(
    data = density(mod_vals$Independent),
    type = 'area',
    name = "Climate variable distribution",
    yAxis = 0,
    fillOpacity = 0.5) %>%
  # hc_yAxis(title = list(text = "Dependent variable")) %>%
  # hc_xAxis(title = list(text = "Fitted values")) %>%
  hc_add_series(
    data = dplyr::arrange(mod_vals, Independent),
    type = 'scatter',
    name = "Observed",
    hcaes(x = Independent, y = Dependent),
    yAxis = 1,
    fillOpacity = 0.5) %>%
  hc_add_series(
    data = dplyr::arrange(mod_vals, Independent),
    type = 'line',
    name = "Fitted",
    hcaes(x = Independent, y = Fitted),
    yAxis = 1,
    fillOpacity = 0.5
  )  %>%
  hc_colors(c("#91BEEA", "#34495E", "black")) %>% # "#5D6D7E"
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
max(density(den$y))
density_vals <- density(mod_vals$Independent)
density_yaxis <- density_vals$y
max(density_yaxis)
max(mod_vals$Dependent)
tmp <- by_district
names(tmp) <- names(rename_all(by_district, recode,
                               short             = "Total shortage",
                               short_dir         = "Direct shortage",
                               short_norm        = "Normalized total shortage",
                               short_dir_norm    = "Normalized direct shortage",
                               af_total          = "Natural flows",
                               swe_max           = "SWE maximum",
                               prcp              = "Precipitation",
                               pdsi              = "PDSI",
                               pdsi_gridmet      = "PDSI (gridMET)",
                               eddi1             = "EDDI 1 month",
                               eddi3             = "EDDI 3 month",
                               eddi6             = "EDDI 6 month",
                               eddi12            = "EDDI 12 month",
                               spi1              = "SPI 1 month",
                               spi3              = "SPI 3 month",
                               spi6              = "SPI 6 month",
                               spi9              = "SPI 9 month",
                               spi12             = "SPI 12 month",
                               tavg              = "Average temperature (C)",
                               aet               = "Actual evapotranspiration",
                               pet               = "Potential Evapotranspiration",
                               soilm             = "Soil moisture")
)

indicator_lst <- list("Precipitation" = "prcp",
  "SWE maximum" = "swe_max",
  "Average temperature" = "tavg",
  "Actual Evapotranspiration" = "aet",
  "Potential Evapotranspiration" = "pet",
  "Soil moisture" = "soilm",
  "PDSI" = "pdsi",
  "PDSI (gridMET)" = "pdsi_gridmet",
  "EDDI 1 month" = "eddi1",
  "EDDI 3 month" = "eddi3",
  "EDDI 6 month" = "eddi6",
  "EDDI 12 month" = "eddi12",
  "SPI 1 month" = "spi1",
  "SPI 3 month" = "spi3",
  "SPI 6 month" = "spi6",
  "SPI 9 month" = "spi9",
  "SPI 12 month" = "spi12")










