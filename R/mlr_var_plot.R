library(tidyverse)
source("data_utils.R")
# ------ Variance inflation factor -----
district_data <- model_data %>%
  filter(district == 2)  %>%
  dplyr::select(
    short_dir_norm, prcp, tavg, pdsi, spi1, spi3, spi6, spi9,
    spi12, eddi1, eddi3, eddi6, eddi12, pet, swe_max, soilm
  )

# subset and log transform data for MLR
log_trans <- district_data %>%
  ungroup() %>%
  mutate(
    short_dir_norm = log10(short_dir_norm)
  )

#     # replace Infinite w/ 0
is.na(log_trans) <- sapply(log_trans, is.infinite)
log_trans[is.na(log_trans)] <- 0

# MLR w/ VIF reduction + stepwise regression
mlr_vfit <- lm(short_dir_norm~., data = log_trans)
forw <- lm(short_dir_norm~., data = log_trans) %>%
  ols_step_forward_p()
  # ols_step_both_p()
back <- lm(short_dir_norm~., data = log_trans) %>%
 ols_step_backward_p()

both <- lm(short_dir_norm~., data = log_trans) %>%
  # ols_step_forward_p()
  ols_step_both_p()
forw$model
back$model
both$model
vf_forw <- vif(forw$model)
vf_back <- vif(back$model)
vf_both <- vif(both$model)
vf_forw
vf_back
vf_both

summary(forw$model)
summary(forw$model)
summary(forw$model)
summary(mlr_vfit)
summary(mlr_step$model)
vf <- car::vif(mlr_vfit)
vf2 <- car::vif(mlr_step$model)
vf2
vf
vf_df <- data.frame(vif = vf) %>%
  rownames_to_column() %>%
  filter(vif < 5)
vf_df
vf_df2 <- data.frame(vif = vf2) %>%
  rownames_to_column() %>%
  filter(vif < 5)
vf_df2
form <- as.formula(paste("short_dir_norm", paste(vf_df$rowname, collapse=" + "), sep=" ~ "))

lm(form, data = log_trans) %>%
  ols_step_forward_p()
paste(vf_df$rowname, sep = " + ")
vf %>% filter()
# rm_collinearity() %>%
  # ols_step_forward_p()
# ------- REname predictors df -----
x = data.frame(q=1,w=2,e=3)
x
oldnames = c("q","e")
newnames = c("A","B")
data.table::setnames(x, x$q, newnames[1])
x %>% rename_with(~newnames, oldnames)
ws_predictors %>% mutate(
  clean_name = recode()
)
library(data.table)
DT <- data.table(a = 1, b = 2, d = 3)
DT
old <- c("a", "b", "c", "d")
new <- c("A", "B", "C", "D")
old <- c(
  "swe_max", "prcp", "pdsi","eddi1",
  "eddi3", "eddi6", "eddi12", "spi1", "spi3", "spi6", "spi9",
  "spi12", "tavg","tmax", "tmin",
  "pet", "soilm"
)
nm1 <- unlist(ws_predictors)
nm2 <-  nm1[nm1 %in% names(new)]
data %>%
  rename(!!! nm2) %>%
  select(names(nm2)) %>%
  head

new <-c(
    "SWE maximum (in)", "Precipitation (mm)","PDSI", "EDDI 1 month",
    "EDDI 3 month", "EDDI 6 month", "EDDI 12 month", "SPI 1 month", "SPI 3 month", "SPI 6 month", "SPI 9 month",
    "SPI 12 month","Average temperature (C)", "Maximum temperature (C)", "Minimum temperature (C)",
    "Potential Evapotranspiration (mm)", "Soil moisture (mm)"
) %>% data.frame()
name_df <- data.frame(swe_max           = "SWE maximum (in)",
prcp              = "Precipitation (mm)",
pdsi              = "PDSI",
# pdsi_gridmet      = "PDSI (gridMET)",
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
tmax              = "Maximum temperature (C)",
tmin              = "Minimum temperature (C)",
aet               = "Actual evapotranspiration (mm)",
# pet               = "Potential Evapotranspiration (mm)",
soilm             = "Soil moisture (mm)") %>%
  pivot_longer(cols = c(1:17))
ws_wide <- model_data %>%
  filter(district == 6)  %>%
  # filter(district == district_id$district)  %>%
  mutate(
    year        = as.numeric(as.character(year)),
    aug_supply2 = aug_supply + supply_dir,
    demand2     = demand - aug_supply2
  ) %>%
  dplyr::select(3:8, 28:33) %>%
  dplyr::rename(
    "Supply Augmented2"    = aug_supply2,
    "Supply Augmented"     = aug_supply,
    "Demand"               = demand,
    "Demand_diff"          = demand2,
    "Supply Direct flow"   = supply_dir) %>%
  mutate(across(where(is.numeric), round, 0))
stk_hc <-
  highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 4)) %>%
  hc_yAxis(
    # tickInterval  = int_diff,
    title = list(
      text = "Water volume (acre feet)",
      style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    min = 0,
    labels = list(
      y = 10,
      style = list(fontSize = 16, color = "black", fontWeight = "bold"))
  ) %>%
  hc_xAxis(
    categories    = ws_wide$year,
    tickInterval  = 2,
    # title = list(
    #   text = "Water volume (acre feet)",
    #   style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    labels = list(
      y           = 35,
      style       = list(fontSize = 16, color = "black", fontWeight = "bold"))
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
  hc_add_series(
    data = ws_wide, name = "Demand",
    type = 'line', hcaes(x = year, y = Demand),
    tooltip = list(pointFormat = "Demand: {point.Demand} AF")
    # yAxis = 1
  )  %>%
  hc_colors(c("#E18686",  "#70BCE2", "#2984B2", "black")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

shinyLP::runExample()
FunctionIntervalM <- function(a,b) {
  seq(from=min(ws_wide$Demand), to = max(ws_wide$Demand), by = (max(ws_wide$Demand)-min(ws_wide$Demand))/10)
}
int <- FunctionIntervalM(a = ws_wide$Demand, 8)
int[1]
int_diff <- int[3]-int[2] %>% truncate()
int_diff
stk_hc <-
  highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal'),
                 line = list(marker = list(enabled = FALSE), lineWidth = 4)) %>%
  hc_yAxis(
    # tickInterval  = int_diff,
    title = list(
      text = "Water volume (acre feet)",
      style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    min = 0,
    labels = list(
      y = 10,
      style = list(fontSize = 16, color = "black", fontWeight = "bold"))
  ) %>%
  hc_xAxis(
    categories    = ws_wide$year,
    tickInterval  = 2,
    # title = list(
    #   text = "Water volume (acre feet)",
    #   style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    labels = list(
      y           = 35,
      style       = list(fontSize = 16, color = "black", fontWeight = "bold"))
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
  hc_add_series(
    data = ws_wide, name = "Demand",
    type = 'line', hcaes(x = year, y = Demand),
    tooltip = list(pointFormat = "Demand: {point.Demand} AF")
    # yAxis = 1
  )  %>%
  hc_colors(c("#E18686",  "#70BCE2", "#2984B2", "black")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

stk_hc
name_df
setnames(ws_predictors, old, new, skip_absent = TRUE)
# ------ MLR equations -----
# VIP map calc
# ---- Linear regression run, nested dataframe ----
mod_df <- point_data %>%
  ungroup() %>%
  dplyr::select(district, short_dir_norm,
                prcp, pdsi, swe_max, pet, tavg,
                spi1, spi3, spi6, spi9, spi12, eddi1, eddi3, eddi6, eddi12)
ind <- c("prcp", "pdsi", "swe_max", "pet", "tavg",
         "spi1", "spi3", "spi6", "spi9", "spi12", "eddi1", "eddi3", "eddi6", "eddi12")
lm_run <- mod_df %>%
  group_by(district) %>%
  nest(-district) %>%
  mutate(
    fit       = map(data,
                    ~lm(short_dir_norm~., data = .) %>% ols_step_forward_p()),
                    # ~lm(as.formula(
                    #   paste(depVar()," ~ ", climVar())
                    # ),
                    # data = .)),
    # results   = map(fit, augment),
    # # step      = map(fit, ols_step_forward_p),
    # tidied    = map(fit, tidy),
    # metrics   = map(fit, glance)
  )
class(lm_run$fit[[1]])
class(tmp$fit[1])
tmp <- lm_run %>%
  flatten()
lm_lst <- tmp[c(43:63)]
lm1 <- lm_lst[[1]]
class(lm1)
lm1$model
ols_step_forward_p(lm1$model)
?ols_step_forward_p
  # mutate(x = x %>% map(function(y) tibble(x = y))) %>%
  # unnest(c("fit"))
  mutate(
    # step      = map(fit, unlist)
    step      = map(fit, ols_step_forward_p(fit[[1]]))
  )

  mlr_lst <- list()
for (i in distr_num) {
  df <- mod_df %>%
    filter(district == i) %>%
    dplyr::select(-district)
  lm_fit <- lm(short_dir_norm~., data = df)
  vf <- lm_fit %>% rm_collinearity(df = df)
  step   <- ols_step_forward_p(vf)
  mlr_lst[[i]] <- step
}

  mlr_df <- mlr_lst %>% purrr::discard(is.null)
  names(mlr_lst) <- c(1:80)
  mlr_df[1]
  spam = do.call("rbind", lapply(mlr_df, "[[", 1)) %>%
    data.frame() %>%
    rownames_to_column() %>%
    setNames(c("district", "pred1", "pred2", "pred3", "pred4", "pred5","pred6"))

  tmp_long <- pivot_longer(spam, cols = c("pred1", "pred2", "pred3", "pred4", "pred5","pred6")) %>%
    dplyr::select(-name) %>%
    distinct()
  saveRDS(tmp_long, "mlr_variables.rds")
  tmp_long %>%  group_by(district) %>% unique()
  unique(spam[,1])
  mlr_df <- mlr_lst %>% purrr::discard(is.null)
  mlr_df[1]
mlr_df <- flatten(mlr_df)
tmp <- pivot_wider(spam, names_from = "district", values_from = c("pred1", "pred2", "pred3", "pred4", "pred5","pred6"))

coeffs <- mlr_df[grep("predictors", names(mlr_df))]
coeffs_df <- unlist(coeffs)
df2 <-  data.frame(coeffs_df)
by_distr <- group_by(mod_df, district)

           # models <- plyr::dlply(by_distr, "district", function(df)
           #        vi(
           #          (ols_step_forward_p(
           #            rm_collinearity(
           #              lm(as.formula(paste(outcome2()," ~ ",paste(independent2, collapse="+"))), data = df),
           #              df = df)
           #            ))$model
           #          )
           #        )
library(vip)
models <- do(mod_df, "district", function(mod_df)
  vi(ols_step_forward_p(
      # rm_collinearity(
        lm(as.formula(paste("short_dir_norm"," ~ ",paste(ind, collapse="+"))), data = mod_df),
        # df = df, vif_thresh = 4)
    )$model
))

           models <- plyr::dlply(by_distr, "district", function(mod_df)
                  vi(
                    ols_step_forward_p(
                      # rm_collinearity(
                        lm(as.formula(paste("short_dir_norm ~ ",paste(ind, collapse="+"))), data = mod_df)
                        # df = mod_df, vif_thresh = 4)
                        # lm(as.formula(paste(outcome2()," ~ ",paste(independent2, collapse="+"))), data = df),
                        # df = df, vif_thresh = 4)
                    )$model
                    )
                  )
           install.packages("vip")
      tmp <- models$`1`

      flat <- flatten(models)
      coeffs <- flat[grep("coefficient", names(flat))]
      coeffs[[2]]
      tidy_vip <- lapply(
                X = names(models),
                FUN = function(x) {
                    dplyr::mutate(
                      models[[x]],
                      district = x
                    )
                  }
                ) %>%
              bind_rows() %>%
              group_by(district) %>%
              filter(Importance == max(Importance))
# ----- MLR variables -----
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
# %>%
#   dplyr::select(1:8, 30)
# left_join(dplyr::select(breaks, district, admin_number, water_right), by = c("district", "admin_number"))

## ------ Join model data w/ admin level supply/demand/short -------
mlr_district <- left_join(
  mlr_data,
  dplyr::select(short_year, year = wyear, district, 20:41),
  by = c("district", "year")
) %>%
  filter(district == 6) %>%
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
  ungroup()



mod_df <- mlr_district %>%
  dplyr::select(short_dir_norm, 10:11, 13:27)

lm_vfit <- lm(short_dir_norm~., data = mod_df) %>%
              rm_collinearity(vif_thresh = 3.5) %>%
              ols_step_forward_p()
lm_vfit$indvar
pred_names <- lm_vfit$predictors[1:2]
as.name(pred_names[2])
as.name()
ws_predictors <- mlr_district %>%
    ungroup() %>%
    dplyr::select(year, lm_vfit$predictors[1:2])
to_any_case(names(ws_predictors), case = "title")

pred_names <- names(rename_all(ws_predictors, recode,
              swe_max                            = "SWE maximum",
              prcp                               = "Precipitation",
              pdsi                               = "PDSI",
              pdsi_gridmet                       = "PDSI (gridMET)",
              eddi1                              = "EDDI 1 month",
              eddi3                              = "EDDI 3 month",
              eddi6                              = "EDDI 6 month",
              eddi12                             = "EDDI 12 month",
              spi1                               = "SPI 1 month",
              spi3                               = "SPI 3 month",
              spi6                               = "SPI 6 month",
              spi9                               = "SPI 9 month",
              spi12                              = "SPI 12 month",
              tavg                               = "Average temperature (C)",
              aet                                = "Actual evapotranspiration",
              pet                                = "Potential Evapotranspiration",
              soilm                              = "Soil moisture"
          ) )
#   "SWE maximum"                = swe_max,
#   "Precipitation"                 = prcp,
#   "PDSI"                          = pdsi,
#   "PDSI (gridMET)"                = pdsi_gridmet,
#   "EDDI 1 month"                  = eddi1,
#   "EDDI 3 month"                  = eddi3,
#   "EDDI 6 month"                  = eddi6,
#   "EDDI 12 month"                 = eddi12,
#   "SPI 1 month"                   = spi1,
#   "SPI 3 month"                   = spi3,
#   "SPI 6 month"                   = spi6,
#   "SPI 9 month"                   = spi9,
#   "SPI 12 month"                  = spi12,
#   "Average temperature (C)"       = tavg,
#   "Actual evapotranspiration"     = aet,
#   "Potential Evapotranspiration"  = pet,
#   "Soil moisture"                 = soilm
# )
  names(ws_predictors)[2]
  library(snakecase)

highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
  hc_yAxis_multiples(
    list(title = list(
      # text = lm_vfit$predictors[1]),
      text = pred_names[2]),
         top = "0%", height = "50%"
         # min = 0,
         # max = max(ws_predictors$swe_max)
         ),
    list(title = list(
      text = pred_names[3]),
      # text = lm_vfit$predictors[2]),
         # min = 0,
         # max =max(ws_predictors$aet),
         top = "50%", height = "50%", opposite = TRUE)
    ) %>%
  hc_add_series(
    data = ws_predictors,
    name = pred_names[2],
    type = 'line',
    hcaes(x = year,  y = !!lm_vfit$predictors[1]),
    yAxis = 0,
    fillOpacity = 0.1) %>%
  hc_add_series(
    data = ws_predictors,
    name = pred_names[3],
    type = 'line', hcaes(x = year, y = !!lm_vfit$predictors[2]),
    yAxis = 1, fillOpacity = 0.1) %>%
  hc_xAxis(categories = ws_predictors$year) %>%
  hc_colors(c("darkblue",  "darkred")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

ws_pred_long <- ws_predictors %>%
  pivot_longer(c(-year)) %>%
  mutate(year = as.numeric(as.character(year)))

ggplot() +
  geom_line(data = ws_pred_long, aes(x = year, y = value, color = name), size = 1) +
  facet_wrap(~name, scale = "free")
  # scale_y_continuous(
  #   "mpg (US)",
  #   sec.axis = sec_axis(~ . * 5, name = "mpg (UK)")
  # )


highchart() %>%
    hc_plotOptions(column = list(stacking = 'normal')) %>%
    hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
    hc_yAxis_multiples(
      list(title = list(text = lm_vfit$predictors[1]), top = "0%", height = "50%",
           min = 0, max = max(ws_predictors$swe_max)),
      list(title = list(text = lm_vfit$predictors[2]),
           min = 0, max =max(ws_predictors$aet), top = "50%", height = "50%", opposite = TRUE)
      ) %>%
    hc_add_series(
      data = ws_predictors, name = lm_vfit$predictors[1],
      type = 'line',
      hcaes(x = year,  y = !!lm_vfit$predictors[1]),
      yAxis = 0,
      fillOpacity = 0.1) %>%
    hc_add_series(
      data = ws_predictors, name = lm_vfit$predictors[2],
      type = 'line', hcaes(x = year, y = !!lm_vfit$predictors[2]),
      yAxis = 1, fillOpacity = 0.1) %>%
    hc_xAxis(categories = ws_predictors$year) %>%
    hc_colors(c("darkblue",  "darkred")) %>%
    hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

highchart() %>%
      hc_plotOptions(column = list(stacking = 'normal')) %>%
      hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
      hc_add_series(
        data = ws_predictors,
        # name = lm_vfit$predictors[2],
        type = "line",
        hcaes(x = year,
              y =  dplyr::select(ws_predictors, 2)),
              # y =  lm_vfit$predictors[2]),
        # yAxis = 0,
        fillOpacity = 0.1
      )

highchart() %>%
  hc_plotOptions(column = list(stacking = 'normal')) %>%
  hc_yAxis(title = list(text = "Water volume (units)"), min = 0) %>%
  hc_add_series(
    data = ws_predictors,
    name = lm_vfit$predictors[2],
    type = "line",
    hcaes_string(x = "year",
                 y =  "swe_max"),
    yAxis = 0,
    fillOpacity = 0.1
  )
class(ws_predictors$year)
fitted <- lm_vfit$model$fitted.values
observed <- lm_vfit$model$model %>%
  dplyr::select(short_dir_norm)
pred_df <- data.frame(fitted = fitted, observed = observed) %>%
  setNames(c("Fitted", "Direct flow shortage normalized"))


highchart() %>%
  hc_yAxis(title = list(text = "Observed"), min = 0) %>%
  hc_xAxis(title = list(text = "Simulated"), min = 0) %>%
  hc_add_series(
    data = pred_df, name = "Prediction",
    type = 'point', hcaes(x = Fitted, y =  `Direct flow shortage normalized`), yAxis = 0, fillOpacity = 0.1) %>%
  hc_add_series(
    data = pred_df, name = "One to one line",
    type = 'spline', hcaes(x = `Direct flow shortage normalized`, y =  `Direct flow shortage normalized`), yAxis = 0, fillOpacity = 0.1)



highchart() %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 5),
                 scatter = list(marker = list(symbol = "circle"))) %>%
  hc_yAxis_multiples(
    # list(title = list(text = paste0(!!climVar(), " distribution")), min = 0, max = max(density_yaxis), opposite = TRUE),
    # list(title = list(text = paste0(!!depVar())), min = 0, max = max(mod_vals$!!depVar()))
    list(title = list(text = "Distribution",
                      style = list(fontWeight = "bold",  fontSize = '1.4em')),
         labels = list(style = list(fontSize =  '1.3em')),
         min = 0, max = max(density_yaxis), opposite = TRUE),
    list(title = list(text = "Dependent variable",
                      style = list(fontWeight = "bold", fontSize = '1.4em'),
         labels = list(style = list(fontSize =  '1.3em')),
         min = 0, max = max(mod_vals$Dependent))
         ))%>%
  hc_xAxis(
  # title = list(
  #     style = list(
  #         fontWeight = "bold",   # Bold
  #         fontSize = '1.4em'    # 1.4 x tthe size of the default text
  #         # color = "#7cb5ec"      # Hex code for the default blue
  #     ))
  labels = list(style = list(fontSize =  '1.3em'))
  ) %>%
  hc_add_series(
    data = density(mod_vals$Independent),
    # data = density(mod_vals$!!climVar()),
    type = 'area',
    # name = paste0(!!climVar(), " distribution"),
    name = "Climate variable distribution",
    yAxis = 0,
    fillOpacity = 0.5) %>%
  # hc_yAxis(title = list(text = "Dependent variable")) %>%
  # hc_xAxis(title = list(text = "Fitted values")) %>%
  hc_add_series(
    data = dplyr::arrange(mod_vals, Independent),
    # data = dplyr::arrange(mod_vals, `Precipitation`),
    type = 'scatter',
    name = "Observed",
    # hcaes(x = Independent, y = Dependent),
    # hcaes(x = `Precipitation`, y =  `Normalized direct shortage`),
    hcaes(x = Independent, y =  Dependent),
    yAxis = 1,
    fillOpacity = 0.5)
lm_hc









