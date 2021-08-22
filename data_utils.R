# --- Shiny utils ---
basemap <- function(shp, pts = NULL) {
    # pal = colorNumeric("inferno", reverse= TRUE, domain = today$size, n = 50)
    # pal2 <- colorNumeric("inferno", reverse = TRUE, domain = today$cases, n = 50)

    leaflet() %>%
          addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
          addPolygons(
              data = shp,
              fillColor = 'grey',
              fillOpacity = 0.3,
              col = "black",
              weight = 2,
              label = ~DISTRICT,
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
            addMeasure(
              position = "bottomleft",
              primaryLengthUnit = "feet",
              primaryAreaUnit = "sqmiles",
              activeColor = "red",
              completedColor = "green" ) %>%
            leafem::addMouseCoordinates()
          # addLabelOnlyMarkers(
          #      data = centers,
          #      label = ~DISTRICT,
          #      labelOptions = labelOptions(
          #          noHide = TRUE,
          #          direction = 'center',
          #          textOnly = T,
          #          style = list(
          #                    "color" = "black",
          #                    "font-weight" = "1000",
          #                    "font-size" = "9px",
          #                    "border" = "1.5px solid black",
          #                    "background-color"="LightCyan",
          #                    "border-color" = "rgba(0,0,0,0.9)"
          #             )
          #          )
          #      ) %>%
          # addLegend(
          #   data = centroids,
          #   "bottomright",
          #   pal = pal,
          #   values = ~af_total,
          #   title = "Coefficient",
          #   labFormat = labelFormat(digits = 10,),
          #   opacity = 1
          # )

}
coeff_map <- function(shp, pts) {
    pal <- colorNumeric("viridis", domain = pts$af_total, n = 10)
    # pal2 <- colorNumeric(c("darkred", "brown1", "yellow"), domain = centroids$af_total)

    leaflet() %>%
          addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
          addLayersControl(
              options = layersControlOptions(collapsed = FALSE),
              baseGroups = c('Topographic', "Relief")
            ) %>%
          addPolygons(
              data = shp,
              fillColor = 'black',
              col = "black",
              weight = 3
          ) %>%
          addCircleMarkers(
              data = pts,
              fillColor = ~pal(af_total),
              radius = ~ size*30,
              stroke = FALSE,
              fillOpacity = 0.7
          ) %>%
          addLegend(
              data = centroids,
              "bottomright",
              pal = pal,
              values = ~af_total,
              title = "Coefficient",
              labFormat = labelFormat(digits = 10,),
              opacity = 1
      )
          leafem::addMouseCoordinates()

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
timeseries_data <- function(agg_df) {
  df <- agg_df %>%
    group_by(date) %>%
    mutate(tmax = mean(tmax),
           tmin = mean(tmin),
           prcp = mean(prcp),
           rhmin = mean(rhmin),
           rhmax = mean(rhmax),
           srad = mean(srad)) %>%
    slice(n = 1)
  df$date <- as.Date(df$date)
  df <- data.frame(df)
  rownames(df) <- df$date
  df
}




make_timeseries <- function(xts_df, param) {
  dygraph(data = dplyr::select(xts_df, param)) %>%
    dyHighlight(highlightCircleSize = 4,
                highlightSeriesBackgroundAlpha = .4) %>%
    dyOptions(colors = c("darkred"),
              fillGraph = TRUE)
}

# Takes map click --> creates extent polygon --> gets climate raster for prediction --> outputs point raster
click_to_AOI <- function(pt) {
  buffer <- pt %>%
    st_transform(5070) %>%
    st_buffer(4000) %>%
    st_transform(4326)

  bb = buffer %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_transform(4326) %>%
    st_as_sf()
}
# stepwise regression on model data, either on whole data set or by individual district, direction of stepwise direction can be selected, either "forward" (default) or "backward"
rm_collinearity <- function(model, df, vif_thresh = 2.5) {
  # VIF threshold. variable w/ higher VIF than threshold are dropped from the model
  threshold <- vif_thresh

  # Sequentially drop the variable with the largest VIF until all variables have VIF less than threshold
  drop = TRUE

  after_vif=data.frame()
  while(drop==TRUE) {
    vfit=car::vif(model)
    after_vif=plyr::rbind.fill(after_vif,as.data.frame(t(vfit)))
    if(max(vfit) > threshold) {
      model <- update(
        model, as.formula(paste(".","~",".","-",names(which.max(vfit))))
      )
    }
    else { drop=FALSE }
  }
  model
}

step_districts <- function(df, distr = NULL, direct = "forward") {
  if (is.null(distr) & direct == "forward") {
    data <- dplyr::select(df, -wyear, -district, -short_log)

    lm_fit <- lm(short_mean ~., data = data)
    y_int <- lm(short_mean ~ 1, data = data)

    lm_forward <- stats::step(y_int, direction = "forward", scope= formula(lm_fit), trace = 0)
    lm_forward$call
    lm <- lm_forward$call
    form <- lm$formula
   lm(formula = lm$formula, data = data)

   # old_lm <- lm(formula = form, data = data)
    # new_lm <- update.formula(old_lm, ~. + season)
    # lm(formula = new_lm, data = data)

  } else if(is.null(distr) & direct == "backward") {
    data <- dplyr::select(distr, -wyear, -district, -short_log)

    lm_fit <- lm(short_mean ~., data = data)
    y_int <- lm(short_mean ~ 1, data = data)

    lm_backward <- stats::step(lm_fit, direction = "backward", scope= formula(lm_fit), trace = 0)
    lm <- lm_backward$call
    lm
    form2 <- lm$formula
    lm(formula = form2, data = data)
    # old_lm <- lm(formula = form2, data = data)
    # new_lm <- update.formula(old_lm, ~. + season)
    # lm(formula = new_lm, data = data)
  } else if(!is.null(distr) & direct == "forward") {
    data <- df %>%
      filter(district == distr) %>%
      dplyr::select(-wyear, -district, -short_log)

    lm_fit <- lm(short_mean ~., data = data)
    y_int <- lm(short_mean ~ 1, data = data)

    lm_forward <- stats::step(y_int, direction = "forward", scope= formula(lm_fit), trace = 0)
    lm <- lm_forward$call
    form <- lm$formula
    lm(formula = form, data = data)
    # old_lm <- lm(formula = form, data = data)
    # new_lm <- update.formula(old_lm, ~. + season)
    # lm(formula = new_lm, data = data)

  } else if(!is.null(distr) & direct == "backward") {
    data <- df %>%
      filter(district == distr) %>%
      dplyr::select(-wyear, -district, -short_log)

    lm_fit <- lm(short_mean ~., data = data)
    y_int <- lm(short_mean ~ 1, data = data)

    lm_backward <- stats::step(lm_fit, direction = "backward", scope= formula(lm_fit), trace = 0)
    lm <- lm_backward$call
    form2 <- lm$formula
    lm(formula = form2, data = data)
    # old_lm <- lm(formula = form2, data = data)
    # new_lm <- update.formula(old_lm, ~. + season)
    # lm(formula = new_lm, data = data)
  }
}


# stepwise regression on model data, either on whole data set or by individual district, direction of stepwise direction can be selected, either "forward" (default) or "backward"
step_districts_log <- function(df, distr = NULL, direct = "forward") {
  if (is.null(distr) & direct == "forward") {
    data <- dplyr::select(df, -wyear, -district, -short_mean)

    lm_fit <- lm(short_log ~., data = data)
    y_int <- lm(short_log ~ 1, data = data)

    lm_forward <- stats::step(y_int, direction = "forward", scope= formula(lm_fit), trace = 0)
    lm <- lm_forward$call
    form <- lm$formula

    lm(formula = form, data = data)

  } else if(is.null(distr) & direct == "backward") {
    data <- dplyr::select(df, -wyear, -district)

    lm_fit <- lm(short_log ~., data = data)
    y_int <- lm(short_log ~ 1, data = data)

    lm_backward <- stats::step(lm_fit, direction = "backward", scope= formula(lm_fit), trace = 0)
    lm <- lm_backward$call
    form2 <- lm$formula

    lm(formula = form2, data = data)
  } else if(!is.null(distr) & direct == "forward") {
    data <- df %>%
      filter(district == distr) %>%
      dplyr::select(-wyear, -district)

    lm_fit <- lm(short_log ~., data = data)
    y_int <- lm(short_log ~ 1, data = data)

    lm_forward <- stats::step(y_int, direction = "forward", scope= formula(lm_fit), trace = 0)
    lm <- lm_forward$call
    form <- lm$formula

    lm(formula = form, data = data)

  } else if(!is.null(distr) & direct == "backward") {
    data <- df %>%
      filter(district == distr) %>%
      dplyr::select(-wyear, -district)

    lm_fit <- lm(short_log ~., data = data)
    y_int <- lm(short_log ~ 1, data = data)

    lm_backward <- stats::step(lm_fit, direction = "backward", scope= formula(lm_fit), trace = 0)
    lm <- lm_backward$call
    form2 <- lm$formula

    lm(formula = form2, data = data)
  }
}
# aggregate, clean impacts & indicators to year timescale
aggreg_short <- function(df){
  season_short <- df %>%
    filter(wyear != 2013) %>%
    group_by(wyear, district) %>% # calculate averages during growing and not growing seasons
    summarize(
        short_total      = sum(short),
        short_dir_total  = sum(short_dir),
        demand_total     = sum(demand),
        dry_events       = max(dry),
        prcp             = sum(prcp),
        soilm            = mean(soilm),
        pdsi             = mean(pdsi),
        pdsi_gridmet     = mean(pdsi_gridmet),
        eddi1            = mean(eddi1),
        eddi3            = mean(eddi3),
        eddi6            = mean(eddi6),
        eddi12           = mean(eddi12),
        tavg             = mean(tavg),
        tmax             = mean(tmax),
        tmin             = mean(tmin),
        spi1             = mean(spi1),
        spi3             = mean(spi3),
        spi6             = mean(spi6),
        spi9             = mean(spi9),
        spi12            = mean(spi12),
        water_deficit    = mean(water_deficit),
        aet              = mean(aet),
        pet              = mean(pet)
      ) %>%
    ungroup() %>%
    group_by(district) %>%
    mutate(
        prcp_norm = mean(prcp)
    ) %>%
    group_by(district, wyear) %>%
    mutate(
        prcp_pct_norm = 100*(prcp/prcp_norm)
    ) %>%
    ungroup() %>%
    mutate(
        district = as.factor(district)
    )
}
# SUMMARIZE BY BASIN AND WATER RIGHT PER YEAR
aggreg_basins <- function(df) {
  shortage <- df %>%
    group_by(basin, seniority, wyear) %>%
    summarise(
        short_total        = sum(short_total),
        short_dir_total    = sum(short_dir_total),
        demand_total       = sum(demand_total),
        short_norm         = short_total/demand_total,
        short_dir_norm     = short_dir_total/demand_total,
        af_total           = sum(af_total),
        prcp_total         = sum(prcp),
        swe_max            = mean(swe_max),
        decree             = sum(decree)
    )
}

vif_func<-function(in_frame,thresh=10,trace=T,...){

  library(fmsb)

  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)

  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)

  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{

    in_dat<-in_frame

    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){

      vif_vals<-NULL
      var_names <- names(in_dat)

      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]

      vif_max<-as.numeric(vif_vals[max_row,2])

      if(vif_max<thresh) break

      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }

      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]

    }

    return(names(in_dat))

  }

}



get_eddi <- function(basin, start_date, end_date = NULL,
                     shp_path, timestep = 1) {
  # dates for EDDI
  time = seq(
    lubridate::ymd(start_date),
    lubridate::ymd(end_date),
    by = '1 month'
  )
  # read shapefile, filter to a basin, cast to a multipolygon for masking later on
  shp <- sf::st_read(shp_path, quiet = TRUE) %>%
    dplyr::filter(BASIN == !!basin) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON")

  # pull EDDI data for each month looking at the 1 month prior, iterate over each date and input into getEDDI()
  eddi <- lapply(X = time, function(x)
    climateR::getEDDI(
      AOI = shp,
      startDate = x,
      timestep = timestep,
      timescale = "month")
  ) %>%
    raster::stack()

  # mask stacks to district boundaries
  eddi <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(eddi, shp[x, ])
    }
  )

  stack_list     <- lapply(X = eddi, FUN = raster::stack)       # stack list of masked rasterstacks
  district_names <- paste0(shp$DISTRICT)                        # district number
  stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks

  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_eddi <- lapply(X = stack_list, FUN = tidy_drought_raster) %>%
    lapply(FUN = function(x) {
      dplyr::mutate(x,
                    year   = stringr::str_sub(date, 15, 18),
                    month = stringr::str_sub(date, 19, 20),
                    doy    = stringr::str_sub(date, 21, -1),
                    date = as.Date(with(x, paste(year,month,doy,sep="-")),"%Y-%m-%d")
      )
    })
  tidy_eddi <- lapply(
    X = names(tidy_eddi),
    FUN = function(x) {
      dplyr::mutate(
        tidy_eddi[[x]],
        district = x
      )
    }
  ) %>%
    bind_rows()
}
tidy_drought_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    # dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# tidy each raster stack in gridMET (exluding PDSI) list of Raster stacks into a tidy tibble
tidy_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# tidy each raster stack in terraclim list of Raster stacks into a tidy tibble
tidy_terra_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        paste0(stringr::str_sub(colnames(.)[-(1:2)], start = 2L), ".1")
      )) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# # tidy each raster stack in ridMET's PDSI and getEDDI() list of Raster stacks into a tidy tibble
tidy_drought_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_drought_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

tidy_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
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



















