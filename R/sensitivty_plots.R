# ---- Summarize data for MLR ----
mlr_data <- by_admin %>%
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

## ------ Join model data w/ admin level supply/demand/short -------
mlr_district <- left_join(
  mlr_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  filter(district == 6) %>%
  # filter(district == district_id$district) %>%
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
    # pdsi                = mean(pdsi, na.rm = T),
    pdsi                = mean(pdsi_gridmet, na.rm = T),
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
    tmax                = mean(tmax, na.rm = T),
    tmin                = mean(tmin, na.rm = T),
    aet                 = mean(aet, na.rm = T),
    pet                 = mean(pet, na.rm = T),
    soilm               = mean(soilm, na.rm = T)
  ) %>%
  mutate(
    short_norm          = 100*(round(short/demand, 3)),
    short_dir_norm      = 100*(round(short_dir/demand, 3)),
    aug_supply          = round((supply - supply_dir), 3),
    aug_supply_norm     = round(100*(aug_supply/supply), 3),
    year                = as.numeric(as.character(year))
  ) %>%
  ungroup()



# ---- Prep data for model ----
mod_df <- mlr_district %>%
  # dplyr::select(short, short_norm, short_dir, short_dir_norm, prcp,tavg, tmax, tmin, pdsi, spi1, spi3, spi6, spi9, spi12)
  dplyr::select(short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi12, swe_max, soilm)

# ---- log transform data ----
log_trans <- mod_df %>%
  mutate(
    # short               = log10(short),
    # short_norm          = log10(short_norm),
    # short_dir           = log10(short_dir),
    short_dir_norm      = log10(short_dir_norm)
  )

#     # replace Infinite w/ 0
is.na(log_trans) <- sapply(log_trans, is.infinite)
log_trans[is.na(log_trans)] <- 0

log_trans <- log_trans %>%
  dplyr::select(short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi12, swe_max, soilm)

# ---- MLR Model + stepwise regression ----
lm_vfit <- lm(short_dir_norm~., data = log_trans)%>%
  ols_step_forward_p()
# calc VIF
vf <- car::vif(lm_vfit$model)

vf_df <- data.frame(vif = vf) %>%
  rownames_to_column() %>%
  filter(vif < 5)


lm_step <- lm(
  as.formula(paste("short_dir_norm", paste(vf_df$rowname, collapse=" + "), sep=" ~ ")),
  data = log_trans)

summary(lm_vfit)
# predict shortages from MLR model
mlr <- lm_vfit$model
summary(mlr)
lm_vfit$ca
# data from selected variables
climate_predictors <- mod_df %>%
  # dplyr::select(prcp, tavg, tmax, tmin, pdsi, spi1, spi3, spi6, spi9, spi12)
  dplyr::select(names(lm_step$model)[-1])

pred <- predict(lm_step, climate_predictors)
pred_df <- data.frame(
    year = 1981:2012,
    prediction = pred
  ) %>%
  mutate(prediction = 10^prediction)

pred_df$historical <- mod_df$short_dir_norm


ggplot() +
  geom_line(data = pred_df, aes(x = year, y = historical), size = 2, col = "red") +
  geom_line(data = pred_df, aes(x = year, y = prediction), size = 2, col = "darkgreen")



highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 8, groupPadding = 0.6)) %>%
  hc_yAxis(
    min = 0,
    max = 100,
    tickInterval = 20,
    title = list(
      text = "Direct Flow Shortage (% of demand)", margin = 60,
      style = list(fontSize = 32, fontWeight = "bold", color = "black")),
    labels = list(format = "{value} %",
      y = 15,
      style = list(fontSize = 32, color = "black", fontWeight = "bold"))
  ) %>%
  hc_legend(enabled = F) %>%
  # hc_legend(itemStyle = list(fontSize = 26, color = "black")) %>%
  hc_add_series(
    data = pred_df, name = "Observed Shortages",
    type = 'line', hcaes(x = year, y = historical),
    # yAxis = 1,
    fillOpacity = 0.5) %>%
  hc_add_series(
    data = pred_df, name = "Modelled Shortages",
    type = 'line', hcaes(x = year, y = prediction),
    # yAxis = 1,
    fillOpacity = 0.5) %>%
  hc_xAxis(
    categories      = pred_df$year,
    gridLineWidth  = 1,
    tickInterval  = 5,
    labels = list(
      align = "center",
      y = 35,
      padding = 6,
      style = list(fontSize = 36, color = "black", fontWeight = "bold")
      )) %>%
  # hc_colors(c("black", "red")) %>%
  hc_colors(c("black", "#C00000")) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL)

# ---------------------------------------
# ----- MODELED vs. OBSERVED PLOTS ------
# ---------------------------------------
highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE)))%>%
  hc_yAxis(
    tickInterval = 10,
    title = list(
      text = "Statemod (Observed)",
      #margin = 60,
      style = list(fontSize = 24, fontWeight = "bold", color = "black")),
    labels = list(
      format = "{value} %",
      y = 10,
      style = list(fontSize = 22, color = "black", fontWeight = "bold"))
  ) %>%
  hc_xAxis(
    tickInterval = 10,
    title = list(
      text = "Climate regression simulated",
      # margin = 60,
      style = list(fontSize = 24, fontWeight = "bold", color = "black")),
    labels = list(
      format = "{value} %",
      y = 35,
      style = list(fontSize = 22, color = "black", fontWeight = "bold"))
  ) %>%
  hc_add_series(
    data = pred_df, name = "Prediction",
    type = 'point', hcaes(x = Fitted, y =  `Direct flow shortage normalized`),
    yAxis = 0, fillOpacity = 0.1) %>%
  hc_add_series(
    data = pred_df, name = "One to one line",
    type = 'line',
    hcaes(x = `Direct flow shortage normalized`, y =  `Direct flow shortage normalized`),
    yAxis = 0, fillOpacity = 0.1)

# ---- Water supply data summarized to the year ----
point_data <- by_admin %>%
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

## ------ Join model data w/ admin level supply/demand/short -------
point_data2 <- left_join(
  point_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  filter(district == 6) %>%
  # filter(district == 2) %>%
  # filter(district == district_id$district) %>%
  dplyr::filter(!year %in% c(1980, 2013)) %>%
  # group_by(basin, district, water_right, year) %>%
  group_by(basin, district, year) %>%
  summarise(
    short               = sum(short, na.rm = T),
    short_dir           = sum(short_dir, na.rm = T),
    demand              = sum(demand, na.rm = T),
    supply              = sum(supply, na.rm = T),
    supply_dir          = sum(supply_dir, na.rm = T),
    # af_total            = mean(af_total, na.rm = T),
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
    tmax                = mean(tmax, na.rm = T),
    tmin                = mean(tmin, na.rm = T),
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
  ungroup()
# ---- log transform data ----
log_trans <- point_data2 %>%
  mutate(
    short               = log10(short),
    short_norm          = log10(short_norm),
    short_dir           = log10(short_dir),
    short_dir_norm      = log10(short_dir_norm)
  )

#     # replace Infinite w/ 0
is.na(log_trans) <- sapply(log_trans, is.infinite)
log_trans[is.na(log_trans)] <- 0

# # SUBSET data for MLR
# log_trans <- log_trans %>%
#   dplyr::select(short_dir_norm, prcp, tavg, tmax, tmin, pdsi, spi1, spi3, spi6, spi9, spi12)
log_trans <- log_trans %>%
  dplyr::select(short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi12, pet, swe_max, soilm)

# dplyr::select(short_dir_norm, 10:11, 13:27)

# MLR w/ VIF reduction + stepwise regression
mlr_vfit <- lm(short_dir_norm~prcp, data = log_trans)
  # rm_collinearity() %>%
  # ols_step_forward_p()

# fitted values
fitted <- lm_step$fitted.values
fitted <- mlr_vfit$fitted.values
# Observed values
observed <- mlr_vfit$model
  # dplyr::select(short_dir_norm) # Observed values

observed$short_dir_norm
summary(mlr_vfit$model)
# rsquared <- mlr_vfit$metrics[1] %>%
#   round(3) %>%
#   as.character()
#

# observed vs. fitted dataframe
mod_perform <- data.frame(fitted = fitted, observed = observed$short_dir_norm, prcp = observed$prcp) %>%
  mutate(
    fitted    = 10^fitted,
    observed  = 10^observed
    # rsq = rsq
    )


highchart() %>%
  hc_plotOptions(
    line  = list(marker = list(enabled = FALSE), lineWidth = 8),
    scatter = list(marker = list(radius = 7))
    ) %>%
  hc_yAxis(
    tickInterval = 10,
    title = list(
      text = "Observed", margin = 60,
      style = list(fontSize = 32, fontWeight = "bold", color = "black")),
    labels = list(
      # format = "{value} %",
      y = 35,
      x = -20,
      style = list(fontSize = 32, color = "black", fontWeight = "bold"))
    ) %>%
  hc_xAxis(
    gridLineWidth  = 1,
    tickInterval  = 10,
    title = list(
      text = "Predictions",
      margin = 30,
      style = list(fontSize = 32, fontWeight = "bold", color = "black")),
    labels = list(
      align = "center",
      # format = "{value} %",
      y = 50,
      x = 35,
      padding = 1,
      style = list(fontSize = 36, color = "black", fontWeight = "bold")
    )) %>%
  hc_legend(enabled = F) %>%
  # hc_colors(c("black", "red")) %>%
  hc_colors(c("black", "#C00000")) %>%
  hc_chart(plotBorderWidth = 1, plotBorderColor = '#b4b4b4', height = NULL) %>%
  # hc_annotations(
  #   list(
  #     labels =
  #       list(list(
  #           point = list(x = 20, y = 80, xAxis = 0, yAxis = 0),
  #           text = paste0("x = ", rsq)
  #           ))
  #         )) %>%
  hc_add_series(
    data = mod_perform, name = "Prediction",
    type = 'point', hcaes(x = fitted, y =  observed), opacity = 0.9,
    yAxis = 0, fillOpacity = 0.1) %>%
  hc_add_series(
    data = arrange(mod_perform, prcp), name = "One to one line",
    type = 'line',
    hcaes(x = observed, y =  observed),
    opacity = 0.7,
    yAxis = 0, fillOpacity = 0.1)
library(snotelr)
library(shiny)
library(bs4Dash)

shiny::shinyApp(
  ui = bs4DashPage(
    enable_preloader = TRUE,
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(),
    controlbar = bs4DashControlbar(),
    footer = bs4DashFooter(),
    title = "test",
    body = bs4DashBody(
      bs4TooltipUI(
        actionButton("goButton", "Hover to see the tooltip"),
        title = "My tooltip",
        placement = "top"
      )
    )
  ),
  server = function(input, output) {}
)

snotelr::snotel_explorer()
library(raster)
r <- raster(ncols=10, nrows=10)
values(r) <- runif(ncell(r))
plot(r)
rasterVis::levelplot(r)
# 3x3 mean filter
r3 <- focal(r, w=matrix(1/9,nrow=3,ncol=3))
rasterVis::levelplot(r3)
# 5x5 mean filter
r5 <- focal(r, w=matrix(1/25,nrow=5,ncol=5))

# Gaussian filter
gf <- focalWeight(r, 2, "Gauss")
rg <- focal(r, w=gf)

# The max value for the lower-rigth corner of a 3x3 matrix around a focal cell
f = matrix(c(0,0,0,0,1,1,0,1,1), nrow=3)
f
rm <- focal(r, w=f, fun=max)
rasterVis::levelplot(rm)



#build blank raster modeled after large raster
r <- raster(xmn=1035792, xmx= 1116792, ymn=825303.6, ymx=937803.6, resolution = 12.5,crs = "+init=epsg:3174")
r <- setValues(r, 0)
rasterVis::levelplot(r)

#create lake polygon
x <- c(1199999, 1080000, 1093067, 1090190, 1087977, 1070419, 1180419)
y <- c(957803.6,937803.6, 894366.9, 872153.9, 853703.0, 825353.6, 805353.6)
poly.lake <- SpatialPolygons(list(Polygons(list(Polygon(data.frame(x,y))), ID = 1)))
poly_sf <- poly.lake %>% st_as_sf()
ggplot() +
  geom_sf(data = poly_sf)
# make values NA where lake polygon does not intersect raster
r <- mask(r, poly.lake)

#run distance function
r.dist <-distance(r)

















