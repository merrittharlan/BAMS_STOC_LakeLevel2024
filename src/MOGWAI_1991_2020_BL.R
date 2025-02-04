library(dplyr)
library(sf)
library(tidyr)
library(zoo)
library(data.table)

MOGWAI_data = read.csv("data/MOGWAI/HydroLakes_polys_v10_10km2_global_results_dswe.csv")


lake_levels_nc <- function(nc_file, altimetry){
  nc <- ncdf4::nc_open(nc_file)
  print(nc)
  lat <- ncvar_get(nc, "latitude")
  lon <- ncvar_get(nc, "longitude")
  lake_name <- ncvar_get(nc, "lake_name")
  state_name <- ncvar_get(nc, "state_name")
  country_name <- ncvar_get(nc, "country_name")
  basin_id <- ncvar_get(nc, "basin_id")
  basin_name <- ncvar_get(nc, "basin_name")
  lake_storage <- ncvar_get(nc, "lake_storage")
  
  time <- nc$dim$time$vals
  time <- as.Date('1984-01-01')+time
  ID <- nc$dim$ID$vals
  
  ID_df <- data.frame(lat=lat,lon=lon,lake_name=lake_name,country_name=country_name,
                      basin_id=basin_id,basin_name=basin_name,ID=ID,altimetry=altimetry)
  
  df <- data.frame(ID=rep(ID, each = length(time)),lake_storage=as.numeric(unlist(t(lake_storage))),
                   time=rep(time, times = length(ID))) %>%
    mutate(Year = format(as.Date(time),"%Y")) %>%
    filter(Year %in% c(1991:2024))
  
  df_out <- df %>% left_join(ID_df)
  
  return(df_out)
}

HydroLAKES <- read_sf("data/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.dbf") %>% 
  mutate(ID = Hylak_id) %>% st_drop_geometry()

HydroBASINS_list = list.files("data/HydroBASINS", pattern = "*shp$", full.names = TRUE, recursive = TRUE)
HydroBASINS_df = do.call(rbind, lapply(HydroBASINS_list, read_sf)) %>% st_make_valid() %>% select(HYBAS_ID)

combined_df_ID = rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>%
  select(ID, lat, lon, lake_name, country_name, basin_id, basin_name) %>%
  distinct()

three_year_gap = rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>% 
  select(ID, lake_storage, Year) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(lake_storage)))) %>% 
  mutate(obs_count_three = rollmean(obs_count, k = 4, fill = NA, align = "right")) %>%
  filter(obs_count_three == 0)

three_year_IDs = unique(three_year_gap$ID)

df_2024 = rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>% 
  select(ID, lake_storage, Year) %>%
  filter(Year == 2024) %>%
  group_by(ID) %>%
  summarize(obs_count_2024 = length(which(!is.na(lake_storage)))) %>%
  filter(obs_count_2024 > 2)

df_2024_IDs = unique(df_2024$ID)

df_20yrs = rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>% 
  select(ID, lake_storage, Year) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(lake_storage)))) %>% 
  filter(obs_count > 0) %>%
  group_by(ID) %>% summarize(year_count = length(Year),
                             start_year = min(Year),
                             end_year = max(Year)) %>%
  filter(year_count >= 20)

combined_df <- rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>% 
  select(ID, lake_storage, Year) %>%
  filter(!ID %in% three_year_IDs) %>%
  filter(ID %in% df_2024_IDs) %>%
  filter(ID %in% df_20yrs$ID) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(lake_storage)))) %>% 
  group_by(ID) %>% summarize(obs_bl_median = median(obs_count)) %>%
  filter(obs_bl_median >= 3) %>%
  left_join(df_20yrs) %>%
  left_join(df_2024)

# total: 25,718 (100%)
# three year: 18,516, 71.99%
# 20 years: 25,630 99.66%
# obs count 2024: 6,923, 26.92%
# all filtering: 4,309, 16.75%

combined_data <- rbind(LandsatSentinel2_df, LandsatGREALM_df, LandsatICESat2_df) %>% 
  select(ID, lake_storage, Year, time) %>%
  filter(ID %in% combined_df$ID) %>%
  filter(!is.na(lake_storage))

anomaly_calc <- function(rownum){
  print(paste0(rownum, " out of ", nrow(combined_df)))
  storage_i = combined_data %>% filter(ID == as.numeric(combined_df[rownum, "ID"]))
  bl = as.numeric(unlist(storage_i %>% filter(Year %in% c(1991:2020)) %>% select(lake_storage)))
  current = as.numeric(unlist(storage_i %>% filter(Year == 2024) %>% select(lake_storage)))
  storage_long_term_mean = mean(bl)
  storage_long_term_mean_smooth = mean(smooth(bl))
  storage_long_term_median = median(bl)
  storage_long_term_sd = sd(bl)
  storage_2024_mean = mean(current)
  storage_2024_mean_smooth = mean(smooth(current))
  storage_2024_median = median(current)
  storage_2024_sd = sd(current)
  storage_anomaly_volume = storage_2024_mean-storage_long_term_mean
  storage_anomaly_percent = 100*storage_anomaly_volume/storage_long_term_mean
  storage_anomaly_volume_smooth = storage_2024_mean_smooth-storage_long_term_mean_smooth
  storage_anomaly_percent_smooth =  100*storage_anomaly_volume_smooth/storage_long_term_mean_smooth
  df = data.frame(ID = as.numeric(combined_df[rownum, "ID"]), 
                  storage_long_term_mean = storage_long_term_mean, 
                  storage_long_term_mean_smooth = storage_long_term_mean_smooth,
                  storage_long_term_median = storage_long_term_median,
                  storage_long_term_sd = storage_long_term_sd,
                  storage_2024_mean = storage_2024_mean,
                  storage_2024_mean_smooth = storage_2024_mean_smooth,
                  storage_2024_median = storage_2024_median,
                  storage_2024_sd = storage_2024_sd,
                  storage_anomaly_volume = storage_anomaly_volume,
                  storage_anomaly_volume_smooth = storage_anomaly_volume_smooth,
                  storage_anomaly_percent = storage_anomaly_percent,
                  storage_anomaly_percent_smooth = storage_anomaly_percent_smooth)
  return(df)
}

start.time = Sys.time()
combined_anomaly = do.call(rbind, lapply(c(1:nrow(combined_df)), anomaly_calc))
end.time = Sys.time()
print(end.time - start.time)

df_basins = combined_df %>% 
  left_join(combined_anomaly) %>%
  left_join(combined_df_ID) %>% 
  mutate(latitude = lat) %>%
  mutate(longitude = lon) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(HydroBASINS_df, join = st_intersects)

df_out = df_basins %>%
  left_join(HydroLAKES)
