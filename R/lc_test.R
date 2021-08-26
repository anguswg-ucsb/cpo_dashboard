utah <- USAboundaries::us_states() %>%
  filter(name == "Utah")

nlcd_path    = 'D:/NLCD/NLCD_2016_Land_Cover_L48_20190424.img'
lc           = raster(nlcd_path)
utah2         = st_transform(utah, st_crs(lc))

# crop to state
crop <- crop(lc, utah2)

# mask NLCD raster to shape
lc_mask <- mask(crop, utah2)


t = exactextractr::exact_extract(lc, utah2, function(value, cov_frac) {
  data.frame(value = value, cov_frac = cov_frac) %>%
    group_by(CLASS = as.character(substr(value,1,1))) %>%
    summarize(c = sum(cov_frac*(900/1e6))) %>%
    right_join(data.frame(CLASS = as.character(c('0', '1', '2', '3',
                                                 '4', '5', '7', '8', '9')),
                          tmp = rep(NA, 9)), by = 'CLASS') %>%
    pull(c)
})

shp[1,] %>% st_area()
