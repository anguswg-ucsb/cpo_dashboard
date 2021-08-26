# ```{r context = "server"}
# logRights <- eventReactive(input$submitButton6, {
#   click <- input$districtMap6_click %>%
#     data.frame() %>%
#     dplyr::select(lat,lng)
#
#   pt <- sf::st_as_sf(
#     click,
#     coords = c("lng", "lat"),
#     crs = 4326
#   )
#
#   # point intersection w/ polygons
#   pt_intersect <-  st_filter(shp, pt)
#
#   # ensure  app will not crash if a somewhere other than a shape is clicked and returns no results from point-shape intersection
#   if(nrow(pt_intersect) == 0) {
#     NULL
#   } else {
#     # } else if(outcome5() == "short_log") {
#     district_id <- pt_intersect %>%
#       rename(district = DISTRICT) %>%
#       mutate(district = as.numeric(district))
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
# ws_wide <- ws_data %>%
#   # filter(district == 7)  %>%
#   filter(district == district_id$district)  %>%
#   mutate(
#     year        = as.numeric(as.character(year)),
#     aug_supply2 = aug_supply + supply_dir,
#     demand2     = demand - aug_supply2
#   ) %>%
#   rename(
#     "Supply Augmented2" = aug_supply2,
#     "Supply Augmented" = aug_supply,
#     "Demand" = demand,
#     "Demand_diff" = demand2,
#     "Supply Direct flow" = supply_dir) %>%
#   mutate(across(where(is.numeric), round, 0))
    # ---- Data wrangling by admin in Shiny ----
    # by_admin2 <- by_admin %>%
    #   group_by(year, district) %>%
    #   # filter(district == 6) %>%
    #   # filter(district == district_id$district) %>%
    #   mutate(
    #     water_right = case_when(
    #       admin <= 7000  ~ "senior",
    #       admin > 7000  ~ "junior"
    #       # admin <= adminNum()  ~ "senior",
    #       # admin > adminNum()  ~ "junior"
    #     ),
    #     year         = as.factor(year),
    #     admin_number = as.integer(round(admin, 0))
    #   )
    # Join model data w/ admin level supply/demand/short
by_district <- left_join(
      ws_data,
      dplyr::select(short_year, year = wyear, district, 20:41),
      by = c("district", "year")
    ) %>%
      # filter(district == 6) %>%
      dplyr::filter(!year %in% c(1980, 2013)) %>%
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
      )
names(trans_log) <- janitor::make_clean_names(names(trans_log), "title")
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

    trans_log <- trans_log %>%
      dplyr::select(short, prcp)
      # dplyr::select(depVar5(), indVar5())

    # NESTED LM RUN
    lm_run <- trans_log %>%
      group_by(water_right) %>%
      nest(-water_right) %>%
      mutate(
        fit      = map(data,
                       # ~lm(short~prcp, data = .)),
                       ~lm(as.formula(
                         paste(depVar5()," ~ ", indVar5())
                       ),
                       data = .)),
        results  = map(fit, augment)
      ) %>%
      unnest(c(4)) %>%
      # dplyr::select(water_right, short, prcp, .fitted) %>%
      dplyr::select(water_right, depVar5(), indVar5(), .fitted) %>%
      setNames(c("water_right", "dependent", "independent", "fitted"))
    # ---- Linear regression run, nested dataframe ----
    lm_run <- trans_log %>%
      group_by(district) %>%
      nest(-district) %>%
      mutate(
        fit       = map(data,
                        ~lm(short_dir_norm~prcp, data = .)),
        # ~lm(as.formula(
        #   paste(depVar()," ~ ", climVar())
        # ),
        # data = .)),
        results   = map(fit, augment),
        tidied    = map(fit, tidy),
        metrics   = map(fit, glance)
      )
    mod_data <- lm_run %>%
      unnest(c(4)) %>%
      dplyr::select(district, short_dir_norm, prcp, .fitted) %>%
      # dplyr::select(district, depVar(), climVar(), .fitted) %>%
      setNames(c("district", "dependent", "independent", "fitted"))

    tbl_data <- lm_run %>%
      unnest(c(metrics)) %>%
      dplyr::select(district, tidied, r_squared = r.squared, p_value = p.value) %>%
      unnest(c(tidied))  %>%
      ungroup() %>%
      dplyr::select(term, coefficient = estimate, r_squared, p_value) %>%
      mutate(
        coefficient = round(coefficient, 3),
        r_squared = round(r_squared,3),
        p_value = round(p_value, 5)
             ) %>%
      # mutate(across(where(is.numeric), round, 3)) %>%
      # mutate(across(where(is.numeric), as.character)) %>%
      rename(
        # District      = district,
        Term          = term,
        Coefficient   = coefficient,
        "R Squared"   = r_squared,
        "p value"     = p_value
      )
    library(formattable)
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <-
      formatter("span",
                style = x ~ style(
                  font.weight = "bold",
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))

    formattable(tbl_data,
                align = c("l",rep("r", NCOL(tbl_data) - 1)),
                list(`Term` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                     `Coefficient` = improvement_formatter,
                     area(col = 3:4) ~ color_tile("#EAECEE", "#EAECEE")))
      # pivot_longer(cols = c(Coefficient:`p value`)
                   # names_to = "tt", values_to = "metric"
                   )
      # pivot_wider(id_cols = "Term")
      # filter(Term != "(Intercept)" & name == "0")
    library(sjPlot)
    lm_model <- lm_run$fit
    lm_model[[1]]
    sjPlot::tab_model(lm_model[[1]])
    library(gtsummary)

    library(formattable)
    customGreen0 = "#DeF7E9"
    customGreen = "#71CA97"
    customRed = "#ff7f7f"
    improvement_formatter <-
      formatter("span",
                style = x ~ style(
                  font.weight = "bold",
                  color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))

    formattable(tbl_data,
                align = c("l",rep("r", NCOL(tbl_data) - 1)),
                list(`Term` = formatter("span", style = ~ style(color = "black", font.weight = "bold")),
                     `Coefficient` = improvement_formatter,
                     area(col = 3:4) ~ color_tile("#EAECEE", "#EAECEE")))
                #      # `Coefficicent` = color_bar("#FA614B"),
                #      `R Squared` = formatter("span",
                #                                style = x ~ style(color = ifelse(x < 0.5, "red", "green"))
                #                              )
                #      )
                # )
    formattable(prevalence, align = c("l",rep("r", NCOL(prevalence) - 1)), list(
      `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
      area(col = 2:4) ~ color_tile("#DeF7E9", "#71CA97")))
    gtsummary::tbl_regression(lm_model[[1]])
gtsummary::tbl_regression(lm_model[[1]]) %>%
  gtsummary::add_glance_table(
    # label = list(sigma ~ "\U03C3"),
    include = c(r.squared, adj.r.squared)
  ) %>%
  gtsummary::modify_header(
    update = list(
      estimate ~ "**coeff**"
    )
  ) %>%
  gtsummary::bold_labels() %>%
  gtsummary::bold_levels()

    library(kableExtra)
    kableExtra::kable(tbl_data) %>%
      kableExtra::kable_styling(
        font_size = 12,
        bootstrap_options = c("striped", "hover", "condensed")
      )


    # Extract fitted values and back transform Logs
    mod_vals <- lm_run %>%
      ungroup() %>%
      mutate(
        dependent          = 10^lm_run$dependent,
        fitted             = 10^lm_run$fitted
      ) %>%
      mutate(across(where(is.numeric), round, 2))

    names(mod_vals) <- janitor::make_clean_names(names(mod_vals), "title")
    highchart() %>%
      hc_add_series(
        data = dplyr::arrange(mod_vals, Independent),
        type = 'point',
        name = "Observed",
        hcaes(x = Independent, y = Dependent),
        fillOpacity = 0.1) %>%
      hc_add_series(
        data = dplyr::arrange(mod_vals, Independent),
        type = 'spline',
        name = "Fitted",
        hcaes(x = Independent, y = Fitted)
        # fillOpacity = 0.1
      )
gg_log <- ggplot()+
      geom_point(
        data = mod_vals,
        aes(
          x = independent,
          y = transform_dependent),
      ) +
      geom_smooth(
        data = mod_vals,
        aes(
          x = independent,
          y = transform_fitted),
        size = 1,
        col = "red",
        span = 3,
      ) +
  theme_bw()
 # hrbrthemes::theme_ipsum()
plotly::ggplotly(gg_log)


highchart() %>%
  hc_add_series(
    data = dplyr::arrange(mod_vals, Independent),
    type = 'point',
    name = "Observed",
    hcaes(x = Independent, y = Dependent),
    fillOpacity = 0.1) %>%
  hc_add_series(
    data = dplyr::arrange(mod_vals, Independent),
    type = 'spline',
    name = "Fitted",
    hcaes(x = Independent, y = Fitted)
    # fillOpacity = 0.1
    )



     # summarise(
    #   short               = sum(short, na.rm = T),
    #   short_dir           = sum(short_dir, na.rm = T),
    #   demand              = sum(demand, na.rm = T),
    #   supply              = sum(supply, na.rm = T),
    #   supply_dir          = sum(supply_dir, na.rm = T),
    #   af_total            = mean(af_total, na.rm = T),
    #   swe_max             = mean(swe_max, na.rm = T),
    #   prcp                = mean(prcp, na.rm = T),
    #   prcp_norm           = mean(prcp_norm, na.rm = T),
    #   pdsi                = mean(pdsi, na.rm = T),
    #   pdsi_gridmet        = mean(pdsi_gridmet, na.rm = T),
    #   eddi1               = mean(eddi1, na.rm = T),
    #   eddi3               = mean(eddi3, na.rm = T),
    #   eddi6               = mean(eddi6, na.rm = T),
    #   eddi12              = mean(eddi12, na.rm = T),
    #   spi1                = mean(spi1, na.rm = T),
    #   spi3                = mean(spi3, na.rm = T),
    #   spi6                = mean(spi6, na.rm = T),
    #   spi9                = mean(spi9, na.rm = T),
    #   spi12               = mean(spi12, na.rm = T),
    #   tavg                = mean(tavg, na.rm = T),
    #   aet                 = mean(aet, na.rm = T),
    #   pet                 = mean(pet, na.rm = T),
    #   soilm               = mean(soilm, na.rm = T)
    # ) %>%
