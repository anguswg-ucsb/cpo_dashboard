# ---- Data wrangling by admin in Shiny ----
by_admin2 <- by_admin %>%
  group_by(year, district) %>%
  # filter(basin == "south_platte") %>%
  # filter(basin == district_id$basin) %>%
  mutate(
    # water_right = case_when(
    #   # admin <= 7000  ~ "senior",
    #   # admin > 7000  ~ "junior"
    #   admin <= adminNum()  ~ "senior",
    #   admin > adminNum()  ~ "junior"
    # ),
    year         = as.factor(year),
    admin_number = as.integer(round(admin, 0))
  ) %>%
  na.omit()
# ---- Join model data w/ admin level supply/demand/short ----
by_right2 <- left_join(
  by_admin2,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  # filter(district == 6) %>%
  dplyr::filter(!year %in% c(1980, 2013)) %>%
  group_by( district, year) %>%
  summarise(
    short               = sum(short, na.rm = T),
    short_dir           = sum(short_dir, na.rm = T),
    demand              = sum(demand, na.rm = T),
    # supply              = sum(supply, na.rm = T),
    # supply_dir          = sum(supply_dir, na.rm = T),
    # af_total            = mean(af_total, na.rm = T),
    swe_max             = mean(swe_max, na.rm = T),
    prcp                = mean(prcp, na.rm = T),
    # prcp_norm           = mean(prcp_norm, na.rm = T),
    # prcp_pct_norm       = mean(prcp_pct_norm, na.rm = T),
    pdsi                = mean(pdsi_gridmet, na.rm = T),
    # pdsi_gridmet        = mean(pdsi_gridmet, na.rm = T),
    # eddi1               = mean(eddi1, na.rm = T),
    # eddi3               = mean(eddi3, na.rm = T),
    # eddi6               = mean(eddi6, na.rm = T),
    eddi12              = mean(eddi12, na.rm = T),
    # spi1                = mean(spi1, na.rm = T),
    # spi3                = mean(spi3, na.rm = T),
    # spi6                = mean(spi6, na.rm = T),
    # spi9                = mean(spi9, na.rm = T),
    spi12               = mean(spi12, na.rm = T),
    tavg                = mean(tavg, na.rm = T),
    # tmax                = mean(tmax, na.rm = T),
    # tmin                = mean(tmin, na.rm = T),
    # aet                 = mean(aet, na.rm = T),
    pet                 = mean(pet, na.rm = T),
    soilm               = mean(soilm, na.rm = T)
  ) %>%
  mutate(
    short_norm         = 100*(round(short/demand, 3)),
    short_dir_norm     = 100*(round(short_dir/demand, 3)),
    # aug_supply         = round((supply - supply_dir), 3),
    # aug_supply_norm    = round(100*(aug_supply/supply), 3),
    year               = as.factor(year)
  )

# ---- scale & apply LM ----
by_distr2 <- by_right2 %>%
  ungroup()

# ---- Scale independent variables
data_apply <- apply(by_distr2[,c(6:13)], 2, scale)

data_new <- by_distr2                                                         # Replicate original data
data_new[ , colnames(data_new) %in% colnames(data_apply)] <- data_apply  # Replace specific columns
data_new <- dplyr::select(data_new, -prcp_norm)

# by_right_scale <- group_by(by_right_scale, district, water_right)
data_new <- group_by(data_new, district)

# Independent variables
var_list <- colnames(data_new)[c(6:13)]

# Independent variables pairings grid
pairings <- expand.grid(
  # lhs = c(paste0(depVar5())),
  lhs = c('short_dir_norm'),
  # lhs = c(paste0(as.list(impacts_lst$)[2])),
  rhs = var_list
  # rhs = c('af_total', 'swe_max')
)

# make formula from each variable pairing
pairings[["formula"]] <- lapply(
  X   = paste(pairings[["lhs"]], "~", pairings[["rhs"]]),
  FUN = as.formula
)

# ---- Linear regression on dependent var vs. independent var for both junior & senior water rights in each district ----
tidy_coeff <- data_new %>%
  dplyr::select(district, short_dir_norm, 6:13) %>%
  nest(-district) %>%
  mutate(
    fit = map(data,  ~lapply(
      X    = pairings[["formula"]],
      FUN  = lm,
      data = .)
    )
    # tidied = map(fit, tidy)
  ) %>%
  unnest(c(3)) %>%
  mutate(
    tidied = map(fit, tidy),
    glance = map(fit, glance)
  ) %>%
  unnest(c(5)) %>%
  # dplyr::select(district, water_right, r.squared) %>%
  dplyr::select(1:6, p_val2 = p.value) %>%
  unnest(c(4)) %>%
  dplyr::select(district, term, estimate, r.squared, p.value) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(
    # district   = paste0(district)
    district  = as.numeric(district)
  ) %>%
  ungroup() %>%
  arrange(-district) %>%
  mutate(
    district    = as.character(district),
    estimate    = round(estimate, 4),
    r.squared   = round(r.squared, 4),
    p.value     = round(p.value, 5)
  )


  tidy_coeff2 <- tidy_coeff %>%
    filter(term %in% c("prcp", "tavg", "eddi12", "soilm", "pdsi", "spi12", "swe_max", "pet")) %>%
      mutate(term = case_when(

       term ==  "prcp"    ~ "Precipitation" ,
       term ==  "pdsi"    ~ "PDSI",
       term ==  "pet"     ~ "PET" ,
       term ==  "soilm"   ~ "Soil Moisture",
       term ==  "tavg"    ~ "Temp. Avg" ,
       term ==  "pdsi"    ~ "PDSI",
       term ==  "swe_max" ~ "SWE" ,
       term ==  "spi12"   ~ "SPI",
       term ==  "eddi12"  ~ "EDDI"
       # term ==   "spi1" ~"SPI 1 month",

      ))

    # pivot_wider(id_cols = c(term:estimate), names_from = "term", values_from = "estimate")
    #       tidy_coeff2$Precipitation
              # "Precipitation" = prcp,
              # "PDSI" = pdsi,
              # "Temp. Avg" = tavg,
              # "Temp. Max" = tmax,
              # "Temp. Min" = tmin,
              # "SPI 1 month" = spi1,
              # "SPI 3 month" = spi3,
              # "SPI 6 month" = spi6,
              # "SPI 9 month" = spi9,
              # "SPI 12 month" = spi12
              # )

# -----------------------------
# ---- Heatmap Highcharter ----
# -----------------------------

highchart() %>%
  hc_add_series(
    data = tidy_coeff2,
    name = "Coefficient heatmap",
    type = 'heatmap', hcaes(x = term, y = district, value = estimate, group = district),
    # yAxis = 1,
    fillOpacity = 0.5) %>%
    hc_xAxis(
      title = list(
          text = "Climate Variables", margin = 60,
          style = list(fontSize = 32, fontWeight = "bold", color = "black")),
      labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold")),
      categories = tidy_coeff2$term,
      opposite = T
    ) %>%
    hc_yAxis(
      title = list(
          text = "Disitrct", margin = 80,
          style = list(fontSize = 32, fontWeight = "bold", color = "black")),
        labels = list(style = list(fontSize = 26, color = "black", fontWeight = "bold")),
      categories = as.list(unique(tidy_coeff$district)),
      tickInterval = 1
    ) %>%
  hc_colorAxis(
    # align = "center",
    # width = 400,
    min = -20, max = 20,
    labels = list(style = list(fontSize = 17, color = "black", fontWeight = "bold")),
    # stops = color_stops(5, c("red3", "white", "dodgerblue4"))
    stops = color_stops(5, c("#a80000", "#DB7A7B", "#FFFEFE", "#6078a8", "#003060"))
    # stops = color_stops(5, c("#BB0103", "#DB7A7B", "#FFFEFE", "#8F88D2", "#1207A3"))
    ) %>%
  hc_legend(
    # width = 325,
    # align = "right",
    # verticalAlign = "middle",
    x =  85,
    title = list(text = "Scaled Coefficients",
                 style = list(fontSize = 18, fontWeight = "bold", color = "black"))
    )
  # hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL)
    # ) %>%
  # hc_xAxis(
  #   categories          = ws_wide$year,
  #   gridLineWidth  = 1,
  #   tickInterval  = 2,
  #   labels = list(
  #     align = "center",
  #     y = 35,
  #     padding = 6,
  #     style = list(fontSize = 36, color = "black", fontWeight = "bold")
  #   )) %>%
  # hc_colors(c("black", "red")) %>%
  # hc_colors(c("black", "#C00000")) %>%
  # hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL)
# ---- Coefficient Heatmap ----
ggplot() +
  geom_tile(data = tidy_coeff,
            aes(x = district, y = term, fill= estimate)
  ) +
  # gghighlight::gghighlight(p.value <= 0.05) +
  labs(
    # title = "Single variable linear regression coefficients",
    title = "Coefficients",
    y = "Variables",
    x = "Districts",
    fill = "Coefficient"
  ) +
  scale_fill_gradient2(low = "red3", mid = "white", high = "dodgerblue4") +
  theme_bw() +
  theme(
    plot.title        = element_text(face = "bold", size = 14, hjust = 0.5, vjust = .5),
    axis.text         = element_text(size = 8),
    strip.text.x      = element_text(size = 12, color = "black", face = "bold"),
    strip.background  = element_rect(color="black", fill="azure3", size=1.5, linetype="solid"),
    axis.title        = element_text(face = "bold",  hjust = 0.5, vjust = .5)
  )
