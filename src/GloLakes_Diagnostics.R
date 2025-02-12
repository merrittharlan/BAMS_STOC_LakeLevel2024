library(ncdf4)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)

lake_levels_df <- function(nc_file, output){
  nc <- ncdf4::nc_open(nc_file)
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
  
  ID_2024 <- data.frame(ID=rep(ID, each = length(time)),lake_storage=as.numeric(unlist(t(lake_storage))),
                   time=rep(time, times = length(ID))) %>% 
    mutate(year = format(as.Date(time),"%Y")) %>%
    group_by(year, ID) %>% summarize(obs_count = length(which(!is.na(lake_storage)))) %>% 
    filter(obs_count > 0) %>% group_by(ID) %>% summarize(year_count = length(year))
  
  ID_df <- data.frame(lat=lat,lon=lon,lake_name=lake_name,country_name=country_name,
                      basin_id=basin_id,basin_name=basin_name,ID=ID) %>%
    filter(ID %in% ID_2024$ID)
  
  df <- data.frame(ID=rep(ID, each = length(time)),lake_storage=as.numeric(unlist(t(lake_storage))),
                   time=rep(time, times = length(ID))) %>% 
    mutate(year = format(as.Date(time),"%Y")) %>%
    group_by(year, ID) %>% summarize(obs_count = length(which(!is.na(lake_storage))),
                                     mean_annual_storage = mean(lake_storage, na.rm = T),
                                     sd_annual_storage = sd(lake_storage, na.rm = T)) %>%
    filter(obs_count > 0)
  
  write.csv(df, paste0("out/",output), row.names = FALSE)
  
  ID_df$storage_long_term_mean <- 0
  ID_df$storage_2024_mean <- 0
  ID_df$storage_2024_mean_smooth <- 0
  ID_df$storage_2024_sd <- 0
  ID_df$storage_anomaly_volume <- 0
  ID_df$storage_anomaly_percent <- 0
  ID_df$storage_anomaly_volume_smooth <- 0
  ID_df$storage_anomaly_percent_smooth <- 0
  
  for(i in 1:nrow(ID_df)){
    storage_i <- lake_storage[i,]
    ID_df$storage_long_term_mean[i] <- mean(storage_i[lubridate::year(time) %in% c(1991:2020)],na.rm=T)
    ID_df$storage_long_term_mean_smooth[i] <- mean(smooth(as.numeric(na.omit(storage_i[lubridate::year(time) %in% c(1991:2020)]))))
    ID_df$storage_2024_mean[i] <- mean(storage_i[lubridate::year(time) == 2024],na.rm=T)
    ID_df$storage_2024_mean_smooth[i] <- mean(smooth(as.numeric(na.omit(storage_i[lubridate::year(time) == 2024]))))
    ID_df$storage_2024_median[i] <- median(storage_i[lubridate::year(time) == 2024],na.rm=T)
    ID_df$storage_2024_sd[i] <- sd(storage_i[lubridate::year(time) == 2024],na.rm=T)
    ID_df$storage_anomaly_volume[i] <- ID_df$storage_2024_mean[i]-ID_df$storage_long_term_mean[i]
    ID_df$storage_anomaly_percent[i] <-  100*ID_df$storage_anomaly_volume[i]/ID_df$storage_long_term_mean[i]
    ID_df$storage_anomaly_volume_smooth[i] <- ID_df$storage_2024_mean_smooth[i]-ID_df$storage_long_term_mean_smooth[i]
    ID_df$storage_anomaly_percent_smooth[i] <-  100*ID_df$storage_anomaly_volume_smooth[i]/ID_df$storage_long_term_mean_smooth[i]
  }
  
  df_year_count <- df %>% filter(obs_count > 0) %>% group_by(ID) %>% 
    summarize(year_count = length(year), start_year = min(year), end_year = max(year))
  
  df = df_year_count %>% left_join(ID_df) %>% filter(year_count > 9)
  
  return(df)
}

LandsatSentinel2file <- "data/Global_Lake_Absolute_Storage_LandsatPlusSentinel2 (1984-present).nc"
LandsatGREALMfile <- "data/Global_Lake_Absolute_Storage_LandsatPlusGREALM (1984-present).nc"
LandsatICESat2file <- "data/Global_Lake_Absolute_Storage_LandsatPlusICESat2 (1984-present).nc"

LandsatSentinel2_df <- lake_levels_df(LandsatSentinel2file, output = "GloLakes_LandsatSentinel2_data.csv")
LandsatGREALM_df <- lake_levels_df(LandsatGREALMfile, output = "GloLakes_LandsatGREALM_data.csv")
LandsatICESat2_df <- lake_levels_df(LandsatICESat2file, output = "GloLakes_LandsatICESat2_data.csv")

HydroLAKES <- read_sf("data/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.dbf") %>% 
  mutate(ID = Hylak_id) %>% st_drop_geometry()

LandsatGREALM_data = read.csv("out/GloLakes_LandsatGREALM_data.csv")

LandsatGREALM_cor = cor(LandsatGREALM_df %>% left_join(HydroLAKES) %>% mutate("Year Count" = year_count,
                                                            "Start Year" = start_year,
                                                            "Latitude" = lat,
                                                            "Storage Longterm Mean" = storage_long_term_mean,
                                                            "Storage 2024 Mean" = storage_2024_mean,
                                                            "Lake Area" = Lake_area,
                                                            "Elevation" = Elevation,
                                                            "Storage Anomaly (%)" = storage_anomaly_percent) %>%
  select(c("Year Count", "Start Year", "Latitude", "Storage Longterm Mean", "Storage 2024 Mean", 
           "Lake Area", "Elevation", "Storage Anomaly (%)")) %>%
  mutate_if(is.character,as.numeric))

LandsatSentinel2_data = read.csv("out/GloLakes_LandsatSentinel2_data.csv")

LandsatSentinel2_cor = cor(LandsatSentinel2_df %>% left_join(HydroLAKES) %>% mutate("Year Count" = year_count,
                                                                              "Start Year" = start_year,
                                                                              "Latitude" = lat,
                                                                              "Storage Longterm Mean" = storage_long_term_mean,
                                                                              "Storage 2024 Mean" = storage_2024_mean,
                                                                              "Lake Area" = Lake_area,
                                                                              "Elevation" = Elevation,
                                                                              "Storage Anomaly (%)" = storage_anomaly_percent) %>%
                          select(c("Year Count", "Start Year", "Latitude", "Storage Longterm Mean", "Storage 2024 Mean", 
                                   "Lake Area", "Elevation", "Storage Anomaly (%)")) %>%
                          mutate_if(is.character,as.numeric), use = "complete.obs")

LandsatICESat2_data = read.csv("out/GloLakes_LandsatICESat2_data.csv")

LandsatICESat2_cor = cor(LandsatICESat2_df %>% left_join(HydroLAKES) %>% mutate("Year Count" = year_count,
                                                                                    "Start Year" = start_year,
                                                                                    "Latitude" = lat,
                                                                                    "Storage Longterm Mean" = storage_long_term_mean,
                                                                                    "Storage 2024 Mean" = storage_2024_mean,
                                                                                    "Lake Area" = Lake_area,
                                                                                    "Elevation" = Elevation,
                                                                                    "Storage Anomaly (%)" = storage_anomaly_percent) %>%
                             select(c("Year Count", "Start Year", "Latitude", "Storage Longterm Mean", "Storage 2024 Mean", 
                                      "Lake Area", "Elevation", "Storage Anomaly (%)")) %>%
                             mutate_if(is.character,as.numeric), use = "complete.obs")

#generate some basic plots
plot_fun <- function(df = LandsatGREALM_df, data_df = LandsatGREALM_data, 
                     cor_df = LandsatGREALM_cor,
                     title = "Landsat_GREALM"){
  
  hist_plot = ggplot(df %>% mutate(year_bins = cut(year_count, breaks = c(10,15,20,25,30,35,40))),
                     aes(x = storage_anomaly_percent, colour = as.factor(year_bins))) +
                stat_ecdf() + 
                theme_bw() +
                xlab("Lake Storage Anomaly % (2024 vs. 1984-2022)") + 
                ylab("Cumulative Density Function") +
                ggtitle(title) + 
                scale_color_viridis_d(name = "Number of years with data \n between 1984-2022")
  
  bar_plot = ggplot(data_df %>% mutate(ID_factor = as.factor(ID)), 
                     aes(x = year, y = mean_annual_storage, color = ID_factor)) + 
                geom_bar(stat = "identity", fill = "lightgrey") +
                theme_bw() + 
                xlab("Year") + 
                ylab("Mean Annual Lake Storage (MCM) by Lake") +
                theme(legend.position="none") +
                ggtitle(title) + 
                scale_color_viridis_d()
  
  line_plot = ggplot(data_df %>% mutate(ID_factor = as.factor(ID)), 
                     aes(x = year, y = mean_annual_storage, color = ID_factor)) + 
                geom_line(stat = "identity") + 
                theme_bw() +
                xlab("Year") +
                ylab("Mean Annual Lake Storage (MCM)") +
                theme(legend.position="none") +
                ggtitle(title) +
                scale_color_grey(start = 0, end = 0.7)
              
  pdf(paste0("out/", title, ".pdf"))
  par(mfrow = c(2,2))
  plot(hist_plot)
  plot(bar_plot)
  plot(line_plot)
  corrplot(cor_df[c(1:8),8, drop=FALSE], cl.pos = "r", cl.ratio = 1, tl.srt=0, tl.col="black")
  dev.off()
}

plot_fun(df = LandsatGREALM_df, data_df = LandsatGREALM_data, 
         cor_df = LandsatGREALM_cor,
         title = "Landsat_GREALM")

plot_fun(df = LandsatSentinel2_df, data_df = LandsatSentinel2_data, 
         cor_df = LandsatSentinel2_cor,
         title = "Landsat_Sentinel2")

plot_fun(df = LandsatICESat2_df, data_df = LandsatICESat2_data, 
         cor_df = LandsatICESat2_cor,
         title = "Landsat_ICESat2")

#Compare same ID waterbodies
GS2 = LandsatGREALM_df %>% inner_join(LandsatSentinel2_df, by = "ID") %>% 
  select(storage_anomaly_percent.x, year_count.x, storage_anomaly_percent.y, year_count.y)

GS2_data = LandsatGREALM_data %>% inner_join(LandsatSentinel2_data, by = c("ID", "year")) %>% 
  select(ID, year, mean_annual_storage.x, mean_annual_storage.y)

GI2 = LandsatGREALM_df %>% inner_join(LandsatICESat2_df, by = "ID") %>% 
  select(storage_anomaly_percent.x, year_count.x, storage_anomaly_percent.y, year_count.y)

S12 = LandsatSentinel2_df %>% inner_join(LandsatICESat2_df, by = "ID") %>% 
  select(storage_anomaly_percent.x, year_count.x, storage_anomaly_percent.y, year_count.y)

GREALM_S2 = ggplot(data.frame(GS2), aes(x = storage_anomaly_percent.x, y = storage_anomaly_percent.y)) + 
  geom_point() + theme_minimal() +
  xlab("Landsat GREALM") + ylab("Landsat Sentinel2") +
  geom_abline(slope = 1, intercept = 0, color = "grey", lty = 2) +
  tune::coord_obs_pred()

GREALM_ICESAT2 = ggplot(data.frame(GI2), aes(x = storage_anomaly_percent.x, y = storage_anomaly_percent.y)) + 
  geom_point() + theme_minimal() +
  xlab("Landsat GREALM") + ylab("Landsat ICESat2") +
  geom_abline(slope = 1, intercept = 0, color = "grey", lty = 2) +
  tune::coord_obs_pred(xlim = c(-100, 100), ylim = c(-100, 100))

S2_ICESAT2 = ggplot(data.frame(S12), aes(x = storage_anomaly_percent.x, y = storage_anomaly_percent.y)) + 
  geom_point() + theme_minimal() +
  xlab("Landsat Sentinel-2") + ylab("Landsat ICESat2") +
  geom_abline(slope = 1, intercept = 0, color = "grey", lty = 2) +
  tune::coord_obs_pred(xlim = c(-100, 500), ylim = c(-100, 500))

pdf("out/GloLakes_same_lakes.pdf")
print(GREALM_S2)
print(GREALM_ICESAT2)
print(S2_ICESAT2)
dev.off()