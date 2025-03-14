library(dplyr)
library(sf)
library(tidyr)
library(zoo)
library(data.table)
library(lubridate)

GREALM_10day_url <- "https://ipad.fas.usda.gov/lakes/images/lakes.TPJOJ.2.smooth.txt.tar.gz"
GREALM_destfile <- "lakes.TPJOJ.2.smooth.txt.tar.gz"

# Download the file
download.file(GREALM_10day_url, GREALM_destfile, mode = "wb")

# Extract the contents of the tar.gz file
untar(GREALM_destfile, exdir = "data/GREALM")

read_GREALM_data <- function(GREALM_file, skip_count = 12){
  data_string = readLines(GREALM_file, n = 1)[1]
  GREALM_version = strsplit(data_string, " ")[[1]][1]
  site_info = readLines(GREALM_file, n = 2)[2]
  site_no = strsplit(site_info, " ")[[1]][1]
  site_name = strsplit(site_info, " ")[[1]][2]
  
  latlon_string = readLines(GREALM_file, n = 3)[3]
  latlon_chars = strsplit(latlon_string, "[ ]")[[1]]
  latlon_cleaned = latlon_chars[latlon_chars != ""]
  lat = as.numeric(latlon_cleaned[1])
  lon = as.numeric(latlon_cleaned[2])
  if(lon >= 180){
    lon = lon - 360
  }
  
  GREALM_cols = c(
    "Calendar_year/month/day",
    "Hour",
    "Minutes",
    "Smoothed_target_height_wrtJ2",
    "Smoothed_target_height_EGM2008"
  )
  
  GREALM_df = fread(GREALM_file, skip = skip_count)
  colnames(GREALM_df) = GREALM_cols
  GREALM_df = GREALM_df %>% mutate(longitude = lon, latitude = lat, site_no = site_no, site_name = site_name)
  
  return(GREALM_df)
}

GREALM_files = list.files("data/GREALM", full.names = TRUE)
GREALM_data = do.call(rbind, lapply(GREALM_files, read_GREALM_data, skip_count = 12))

GREALM_df = GREALM_data %>% 
  mutate(Smoothed_target_height_EGM2008 = na_if(Smoothed_target_height_EGM2008, 9999.99)) %>% 
  mutate(date = as.Date(as.character(`Calendar_year/month/day`), format = "%Y%m%d")) %>%
  drop_na(Smoothed_target_height_EGM2008) %>% 
  select(date, Smoothed_target_height_EGM2008, longitude, latitude, site_no, site_name)

HydroLAKES = read_sf("data/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") %>% 
  mutate(ID = Hylak_id)

HydroBASINS_list = list.files("data/HydroBASINS", pattern = "*shp$", full.names = TRUE, recursive = TRUE)
HydroBASINS_df = do.call(rbind, lapply(HydroBASINS_list, read_sf)) %>% st_make_valid() %>% select(HYBAS_ID)

combined_df_ID = GREALM_df %>%
  select(site_no) %>%
  distinct()

three_year_gap = GREALM_df %>% 
  mutate(Year = year(date)) %>%
  select(site_no, Smoothed_target_height_EGM2008, Year) %>%
  group_by(Year, site_no) %>% 
  summarize(obs_count = length(which(!is.na(Smoothed_target_height_EGM2008)))) %>% 
  mutate(obs_count_three = rollmean(obs_count, k = 4, fill = NA, align = "right")) %>%
  filter(obs_count_three == 0)

three_year_IDs = unique(three_year_gap$site_no)

df_2024 = GREALM_df  %>% 
  mutate(Year = year(date)) %>%
  select(site_no, Smoothed_target_height_EGM2008, Year) %>%
  filter(Year == 2024) %>%
  group_by(site_no) %>%
  summarize(obs_count_2024 = length(which(!is.na(Smoothed_target_height_EGM2008)))) %>%
  filter(obs_count_2024 > 2)

df_2024_IDs = unique(df_2024$site_no)

df_20yrs = GREALM_df %>% 
  mutate(Year = year(date)) %>%
  select(site_no, Smoothed_target_height_EGM2008, Year) %>%
  group_by(Year, site_no) %>% 
  summarize(obs_count = length(which(!is.na(Smoothed_target_height_EGM2008)))) %>% 
  filter(obs_count > 0) %>%
  group_by(site_no) %>% summarize(year_count = length(Year),
                             start_year = min(Year),
                             end_year = max(Year)) %>%
  filter(year_count >= 20)

combined_df <- GREALM_df %>% 
  mutate(Year = year(date)) %>%
  select(site_no, Smoothed_target_height_EGM2008, Year) %>%
  filter(!site_no %in% three_year_IDs) %>%
  filter(site_no %in% df_2024_IDs) %>%
  filter(site_no %in% df_20yrs$site_no) %>%
  group_by(Year, site_no) %>% 
  summarize(obs_count = length(which(!is.na(Smoothed_target_height_EGM2008)))) %>% 
  group_by(site_no) %>% summarize(obs_bl_median = median(obs_count)) %>%
  filter(obs_bl_median >= 3) %>%
  left_join(df_20yrs) %>%
  left_join(df_2024)

combined_data <- GREALM_df %>% 
  mutate(Year = year(date)) %>%
  select(site_no, Smoothed_target_height_EGM2008, Year, date) %>%
  filter(site_no %in% combined_df$site_no) %>%
  filter(!is.na(Smoothed_target_height_EGM2008))

anomaly_calc <- function(rownum){
  print(paste0(rownum, " out of ", nrow(combined_df)))
  level_i = combined_data %>% filter(site_no == as.character(combined_df[rownum, "site_no"]))
  bl = as.numeric(unlist(level_i %>% filter(Year %in% c(1993:2020)) %>% select(Smoothed_target_height_EGM2008)))
  current = as.numeric(unlist(level_i %>% filter(Year == 2024) %>% select(Smoothed_target_height_EGM2008)))
  level_long_term_mean = mean(bl, na.rm = TRUE)
  level_long_term_mean_smooth = mean(smooth(bl), na.rm = TRUE)
  level_long_term_median = median(bl, na.rm = TRUE)
  level_long_term_sd = sd(bl, na.rm = TRUE)
  level_2024_mean = mean(current, na.rm = TRUE)
  level_2024_mean_smooth = mean(smooth(current), na.rm = TRUE)
  level_2024_median = median(current, na.rm = TRUE)
  level_2024_sd = sd(current, na.rm = TRUE)
  level_anomaly = level_2024_mean-level_long_term_mean
  level_anomaly_smooth = level_2024_mean_smooth-level_long_term_mean_smooth
  df = data.frame(site_no = as.character(combined_df[rownum, "site_no"]), 
                  level_long_term_mean = level_long_term_mean, 
                  level_long_term_mean_smooth = level_long_term_mean_smooth,
                  level_long_term_median = level_long_term_median,
                  level_long_term_sd = level_long_term_sd,
                  level_2024_mean = level_2024_mean,
                  level_2024_mean_smooth = level_2024_mean_smooth,
                  level_2024_median = level_2024_median,
                  level_2024_sd = level_2024_sd,
                  level_anomaly = level_anomaly,
                  level_anomaly_smooth = level_anomaly_smooth)
  return(df)
}

start.time = Sys.time()
combined_anomaly = do.call(rbind, lapply(c(1:nrow(combined_df)), anomaly_calc))
end.time = Sys.time()
print(end.time - start.time)

GREALM_sp = combined_anomaly %>% 
  left_join(GREALM_data %>% select(site_no, latitude, longitude) %>% distinct(), by = "site_no") %>%
  mutate(lat = latitude, lon = longitude) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_make_valid() 

write_sf(GREALM_sp, "data/GREALM_sp.gpkg")

HydroLAKES_fixed = read_sf("data/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp") %>%
  st_make_valid()

GREALM_buffered = GREALM_sp %>% st_buffer(dist = 1000)

ptm = proc.time()
GREALM_HydroLAKES = GREALM_buffered %>% st_intersects(HydroLAKES_fixed)
proc.time() - ptm

HydroLAKES_intersecting = HydroLAKES_fixed[unique(unlist(GREALM_HydroLAKES)), ]

GREALM_HydroLAKES_merged = GREALM_buffered %>% 
  st_join(HydroLAKES_intersecting, join = st_intersects)

GREALM_duplicate = GREALM_HydroLAKES_merged %>% 
  st_drop_geometry() %>%
  group_by(site_no) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  select(site_no, Hylak_id, Lake_name) %>%
  left_join(GREALM_data %>% select(site_no, site_name) %>% distinct()) %>%
  select(site_no, Hylak_id, Lake_name, site_name) %>%
  filter(!is.na(Hylak_id))

HydroLAKES_duplicate = GREALM_HydroLAKES_merged %>% 
  st_drop_geometry() %>%
  group_by(Hylak_id) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  select(site_no, Hylak_id, Lake_name) %>%
  left_join(GREALM_data %>% select(site_no, site_name) %>% distinct()) %>%
  select(site_no, Hylak_id, Lake_name, site_name) %>%
  filter(!is.na(Hylak_id))

GREALM_na = GREALM_HydroLAKES_merged %>% 
  st_drop_geometry() %>%
  select(site_no, Hylak_id, Lake_name) %>%
  left_join(GREALM_data %>% select(site_no, site_name) %>% distinct()) %>%
  select(site_no, Hylak_id, Lake_name, site_name) %>%
  filter(is.na(Hylak_id))

GREALM_manual = rbind(GREALM_duplicate, HydroLAKES_duplicate, GREALM_na) %>% 
  distinct()
  
write.csv(GREALM_manual, "data/GREALM_manual.csv", row.names = FALSE)

GREALM_HydroLAKES_pairing = GREALM_HydroLAKES_merged %>%
  filter(!site_no %in% GREALM_manual$site_no) %>%
  filter(site_no %in% combined_anomaly$site_no) %>%
  select(site_no, Hylak_id) %>%
  st_drop_geometry()

GREALM_manual_corrected = read.csv("data/GREALM_manual_corrected.csv", colClasses = c("site_no" = "character")) %>%
  filter(!is.na(Hylak_id)) %>%
  select(-site_name)

GREALM_HydroLAKES_df = rbind(GREALM_HydroLAKES_pairing, GREALM_manual_corrected) %>%
  left_join(combined_anomaly) %>%
  left_join(HydroLAKES_fixed) %>% 
  mutate(ID = Hylak_id) %>% 
  st_drop_geometry() %>%
  select(-geometry) %>%
  mutate(lat = Pour_lat) %>%
  mutate(lon = Pour_long) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(HydroBASINS_df, join = st_intersects)

df_out = GREALM_HydroLAKES_df

write.csv(df_out %>% st_drop_geometry(), "out/GREALM_anomaly_1993_2020BL.csv", row.names = FALSE)
write.csv(combined_data %>% 
            filter(site_no %in% df_out$site_no) %>% 
            left_join(df_out %>% st_drop_geometry() %>% select(site_no, Hylak_id)), 
          "out/GREALM_timeseries_1993_2020BL.csv", row.names = FALSE)
