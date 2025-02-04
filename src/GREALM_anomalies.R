library(dplyr)
library(sf)
library(tidyr)
library(zoo)
library(data.table)

GREALM_30_files = list.files("input/lake.30.tar/lake.30", pattern = ".txt", full.names = TRUE)
GREALM_10_files = list.files("input/lake.10.tar/lake.10", pattern = ".txt", full.names = TRUE)
GREALM_file = GREALM_10_files[1]

read_GREALM_data <- function(GREALM_file, skip_count = 50){
  data_string = readLines(GREALM_file, n = 1)[1]
  GREALM_version = strsplit(data_string, " ")[[1]][1]
  site_info = readLines(GREALM_file, n = 2)[2]
  site_no = strsplit(site_info, " ")[[1]][1]
  site_name = strsplit(site_info, " ")[[1]][2]
  
  latlon_string = readLines(GREALM_file, n = 3)[3]
  lat = as.numeric(strsplit(latlon_string, "[ ]")[[1]][2])
  lon = as.numeric(strsplit(latlon_string, "[ ]")[[1]][3]) - 360
  
  GREALM_cols = c(
    "Satellite_mission_name",
    "Satellite_repeat_cycle",
    "Calendar_year/month/day",
    "Hour",
    "Minutes",
    "Target_height_variation_wrt_Jason-2",
    "Estimated_error_of_target_height_variation",
    "Mean_along_track_Ku-band_backscatter_coef",
    "Wet_tropospheric_correction",
    "Ionosphere_correction",
    "Dry_tropospheric_correction",
    "Instrument_operating_mode_1",
    "Instrument_operating_mode_2",
    "Ice_flag",
    "Target_height_variation_in_EGM2008_datum",
    "Flag_for_data_source"
  )
  
  GREALM_df = fread(GREALM_file, skip = skip_count)
  colnames(GREALM_df) = GREALM_cols
  GREALM_df = GREALM_df %>% mutate(longitude = lon, latitude = lat, site_no = site_no, site_name = site_name)
  
  return(GREALM_df)
}

GREALM_data_30 = do.call(rbind, lapply(GREALM_30_files, read_GREALM_data, skip_count = 49))
GREALM_data_10 = do.call(rbind, lapply(GREALM_10_files, read_GREALM_data, skip_count = 50))

GREALM_data = rbind(GREALM_data_10, GREALM_data_30) 
GREALM_data = GREALM_data %>% 
  mutate(Target_height_variation_wrt_Jason2 = na_if(GREALM_data$`Target_height_variation_wrt_Jason-2`, 999.99)) %>% 
  mutate(date = as.Date(as.character(GREALM_data$`Calendar_year/month/day`), format = "%Y%m%d")) %>%
  drop_na(Target_height_variation_wrt_Jason2) %>% 
  select(-`Calendar_year/month/day`, -Hour, -Minutes, -`Target_height_variation_wrt_Jason-2`)
