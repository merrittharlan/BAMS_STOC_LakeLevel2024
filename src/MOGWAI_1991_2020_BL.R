library(dplyr)
library(sf)
library(tidyr)
library(zoo)
library(data.table)

MOGWAI_data = read.csv("data/MOGWAI/HydroLakes_polys_v10_10km2_global_results_dswe.csv", check.names = FALSE)
MOGWAI_df = MOGWAI_data %>%
  select(c(-`Unnamed: 0`, -FileName)) %>%
  pivot_longer(cols = -Dates,
              names_to = "ID",
              values_to = "DSWE_Area_km2") %>%
  mutate(Year = as.numeric(format(as.Date(Dates),"%Y"))) %>%
  filter(Year %in% c(2001:2024)) %>%
  filter(!is.na(DSWE_Area_km2))

HydroLAKES = read_sf("data/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.dbf") %>% 
  mutate(ID = Hylak_id) %>% st_drop_geometry()

HydroBASINS_list = list.files("data/HydroBASINS", pattern = "*shp$", full.names = TRUE, recursive = TRUE)
HydroBASINS_df = do.call(rbind, lapply(HydroBASINS_list, read_sf)) %>% st_make_valid() %>% select(HYBAS_ID)

combined_df_ID = MOGWAI_df %>%
  select(ID) %>%
  distinct()

three_year_gap = MOGWAI_df %>% 
  select(ID, DSWE_Area_km2, Year) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(DSWE_Area_km2)))) %>% 
  mutate(obs_count_three = rollmean(obs_count, k = 4, fill = NA, align = "right")) %>%
  filter(obs_count_three == 0)

three_year_IDs = unique(three_year_gap$ID)

df_2024 = MOGWAI_df  %>% 
  select(ID, DSWE_Area_km2, Year) %>%
  filter(Year == 2024) %>%
  group_by(ID) %>%
  summarize(obs_count_2024 = length(which(!is.na(DSWE_Area_km2)))) %>%
  filter(obs_count_2024 > 2)

df_2024_IDs = unique(df_2024$ID)

df_15yrs = MOGWAI_df %>% 
  select(ID, DSWE_Area_km2, Year) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(DSWE_Area_km2)))) %>% 
  filter(obs_count > 0) %>%
  group_by(ID) %>% summarize(year_count = length(Year),
                             start_year = min(Year),
                             end_year = max(Year)) %>%
  filter(year_count >= 15)

combined_df <- MOGWAI_df %>% 
  select(ID, DSWE_Area_km2, Year) %>%
  filter(!ID %in% three_year_IDs) %>%
  filter(ID %in% df_2024_IDs) %>%
  filter(ID %in% df_15yrs$ID) %>%
  group_by(Year, ID) %>% 
  summarize(obs_count = length(which(!is.na(DSWE_Area_km2)))) %>% 
  group_by(ID) %>% summarize(obs_bl_median = median(obs_count)) %>%
  filter(obs_bl_median >= 3) %>%
  left_join(df_15yrs) %>%
  left_join(df_2024)

# all filtering: 11,028, 100%

combined_data <- MOGWAI_df %>% 
  select(ID, DSWE_Area_km2, Year, Dates) %>%
  filter(ID %in% combined_df$ID) %>%
  filter(!is.na(DSWE_Area_km2))

anomaly_calc <- function(rownum){
  print(paste0(rownum, " out of ", nrow(combined_df)))
  area_i = combined_data %>% filter(ID == as.numeric(combined_df[rownum, "ID"]))
  bl = as.numeric(unlist(area_i %>% filter(Year %in% c(2001:2020)) %>% select(DSWE_Area_km2)))
  current = as.numeric(unlist(area_i %>% filter(Year == 2024) %>% select(DSWE_Area_km2)))
  area_long_term_mean = mean(bl, na.rm = TRUE)
  area_long_term_mean_smooth = mean(smooth(bl), na.rm = TRUE)
  area_long_term_median = median(bl, na.rm = TRUE)
  area_long_term_sd = sd(bl, na.rm = TRUE)
  area_2024_mean = mean(current, na.rm = TRUE)
  area_2024_mean_smooth = mean(smooth(current), na.rm = TRUE)
  area_2024_median = median(current, na.rm = TRUE)
  area_2024_sd = sd(current, na.rm = TRUE)
  area_anomaly_volume = area_2024_mean-area_long_term_mean
  area_anomaly_percent = 100*area_anomaly_volume/area_long_term_mean
  area_anomaly_volume_smooth = area_2024_mean_smooth-area_long_term_mean_smooth
  area_anomaly_percent_smooth =  100*area_anomaly_volume_smooth/area_long_term_mean_smooth
  df = data.frame(ID = as.numeric(combined_df[rownum, "ID"]), 
                  area_long_term_mean = area_long_term_mean, 
                  area_long_term_mean_smooth = area_long_term_mean_smooth,
                  area_long_term_median = area_long_term_median,
                  area_long_term_sd = area_long_term_sd,
                  area_2024_mean = area_2024_mean,
                  area_2024_mean_smooth = area_2024_mean_smooth,
                  area_2024_median = area_2024_median,
                  area_2024_sd = area_2024_sd,
                  area_anomaly_volume = area_anomaly_volume,
                  area_anomaly_volume_smooth = area_anomaly_volume_smooth,
                  area_anomaly_percent = area_anomaly_percent,
                  area_anomaly_percent_smooth = area_anomaly_percent_smooth)
  return(df)
}

start.time = Sys.time()
combined_anomaly = do.call(rbind, lapply(c(1:nrow(combined_df)), anomaly_calc))
end.time = Sys.time()
print(end.time - start.time)

df_basins = combined_df %>% 
  mutate(ID = as.numeric(ID)) %>%
  left_join(combined_anomaly) %>%
  left_join(combined_df_ID %>%
              mutate(ID = as.numeric(ID))) %>% 
  left_join(HydroLAKES %>% 
              mutate(ID = as.numeric(Hylak_id))) %>%
  mutate(latitude = Pour_lat) %>%
  mutate(longitude = Pour_long) %>%
  drop_na(c(latitude, longitude)) %>%
  st_as_sf(coords = c("Pour_long", "Pour_lat"), crs = 4326) %>%
  st_join(HydroBASINS_df, join = st_intersects)

df_out = df_basins

write.csv(df_out %>% st_drop_geometry(), "out/MOGWAI_anomaly_2001_2020BL.csv", row.names = FALSE)
write.csv(combined_data, "data/MOGWAI/MOGWAI_timeseries_2001_2020BL.csv", row.names = FALSE)
