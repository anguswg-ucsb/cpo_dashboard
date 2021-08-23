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
mlr_map <- function(shp, pts = NULL) {

      pal <- colorNumeric("YlOrRd", domain = shp$variance_rsq, n = 21)
      # Leaflet map
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap, group = "Topographic") %>%
        addPolygons(
          data = shp,
          color = "black",
          fillOpacity = 0.8,
          fillColor = ~pal(variance_rsq),
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
        addLegend(
          data = shp,
          "bottomright",
          pal = pal,
          values = ~variance_rsq,
          title = "Variance x R2",
          labFormat = labelFormat(digits = 10,),
          opacity = 1
        ) %>%
        addScaleBar("bottomleft") %>%
        addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "feet",
          primaryAreaUnit = "sqmiles",
          activeColor = "red",
          completedColor = "green" ) %>%
        leafem::addMouseCoordinates()
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



















