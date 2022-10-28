# Angus Watters
# Upload CPO dashboard data to S3 Bucket
library(aws.s3)
library(tidyverse)


# **********************************
# ---- Upload data to S3 Bucket ----
# **********************************

# list buckets
aws.s3::bucketlist()

# model_data <- readRDS("model_data_all.rds")
# model_data      <- readRDS("statemod_climate_year.rds")

# Name of S3 bucket to write to
bucket_name <- "cpo-shiny-data"

# rds data paths
rds_paths <- list.files(here::here(), pattern = ".rds", full.names = TRUE)

# loop through files in directory and write all RDS files to S3
for(i in 1:length(rds_paths)) {

  message(paste0("Writing ",basename(rds_paths[i]), " to ", bucket_name,  " S3 bucket"))

  # write rds objects to S3
  aws.s3::s3write_using(
    readRDS(rds_paths[i]),
    FUN    = saveRDS,
    bucket = bucket_name,
    object = basename(rds_paths[i])
  )

}

# geojson data paths
gj_paths <- list.files(here::here(), pattern = ".geojson", full.names = TRUE)

# loop through files in directory and write all RDS files to S3
for(i in 1:length(gj_paths)) {

  message(paste0("Writing ",basename(gj_paths[i]), " to ", bucket_name,  " S3 bucket"))

  # write rds objects to S3
  aws.s3::s3write_using(
    sf::read_sf(gj_paths[i]),
    FUN    = sf::write_sf,
    bucket = bucket_name,
    object = gsub("geojson", "gpkg", basename(gj_paths[i]))
  )

}

# ****************************
# ---- Read in S3 objects ----
# ****************************

bucket_objs <- aws.s3::get_bucket_df(bucket_name)
bucket_objs$Key
bucket_objs$Key[1]
bucket_objs$Key[1]
aws.s3::s3readRDS(
  object = "statemod_climate_year.rds",
  bucket = bucket_name
)
system.time(
model_data      <- readRDS("statemod_climate_year.rds")
)
system.time(
model_data      <- aws.s3::s3readRDS(
  object = "statemod_climate_year.rds",
  bucket = bucket_name
)
)
# ********************************
# ---- Empty buckets & delete ----
# ********************************

# bucket list dataframe
bl_df <- aws.s3::bucket_list_df()

# buckets to delete
bl_del <-
  bl_df %>%
  dplyr::filter(!Bucket %in% c("cpo-shiny-data", "nfl-win-predictor-api")) %>%
  .$Bucket

# loop through buckets, delete objects and then delete bucket
for(i in 1:length(bl_del)) {
  message(paste0("S3 Bucket: ", bl_del[i]))
  # files to delete within bucket
  files_to_delete <-
    aws.s3::get_bucket_df(bl_del[i]) %>%
    dplyr::select(Key) %>%
    .$Key

    # Loop through each S3 object list
    for(z in 1:length(files_to_delete)){

      message(paste0("Deleting S3 Object...",
                     "\nObject: ", files_to_delete[z]
                     )
              )
      aws.s3::delete_object(
        object = files_to_delete[z],
        bucket = bl_del[i]
        )
    }

  message(paste0("Deleting S3 bucket: ", bl_del[i]))

  aws.s3::delete_bucket(bl_del[i])

}

