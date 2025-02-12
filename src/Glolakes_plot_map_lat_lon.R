library(ggplot2)
library(dplyr)
library(sf)
library(RColorBrewer)
library(gridExtra)

#  Define Robinson projection
cr1 = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#  Load data
anomaly_data = read.csv("out/Glolakes_anomaly_1991_2020BL.csv")
anomaly_data_sp = st_as_sf(anomaly_data, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(cr1)

#  Get Continents data
shp_robinson = read_sf('data/World_Continents.shp', stringsAsFactors = F) %>% st_transform(cr1)

volume_breaks = c(seq(-5, 5, 1))
anomaly_volume_sp = anomaly_data_sp %>% filter(storage_anomaly_volume < 5) %>% filter(storage_anomaly_volume > -5)

# Storage Anomaly Volume Plot
size_sf = 0.2

anomaly_volume_plot = ggplot() +
  geom_sf(data = shp_robinson, colour = "black", fill = 'gray80')+ 
  geom_sf(anomaly_volume_sp, 
          mapping=aes(fill = storage_anomaly_volume, 
                      color = storage_anomaly_volume),
          size=size_sf,
          pch=21)+
  scale_color_fermenter(breaks = volume_breaks, palette = "BrBG", name = "Anomalies from 1991-2020 (MCM)", direction = 1) +
  scale_fill_fermenter(breaks = volume_breaks, palette = "BrBG", name = "Anomalies from 1991-2020 (MCM)", direction = 1) +
  theme_light() +
  guides(fill = guide_colorbar(
    title.position = "bottom",
    frame.linewidth = 0.55,
    frame.colour = "black",
    ticks.colour = "black",
    ticks.linewidth = 0.3,
    barwidth = 25
  ),
  color = "none") +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_rect(fill = NA, color=NA),
        legend.position = "bottom")


anomaly_volume_df = anomaly_data %>% filter(storage_anomaly_volume < 5) %>% filter(storage_anomaly_volume > -5) %>%
  mutate(lat = as.factor(round(latitude, 0))) %>%
  mutate(lon = as.factor(round(longitude, 0)))

write.csv(anomaly_data %>% filter(storage_anomaly_volume < 5) %>% filter(storage_anomaly_volume > -5), "out/anomaly_volume_filtered.csv", 
          row.names = FALSE)
write.csv(anomaly_data, "out/anomaly_volume_all.csv", row.names = FALSE)

anomaly_latitude_blank = data.frame(lat = c(as.factor(c(-45:73)), as.factor(c(-45:73))), 
                                    sign = rep(c("Negative", "Positive"), each = 119), latitude = c(c(-45:73), c(-45:73))) %>%
  filter(!lat %in% anomaly_volume_df$lat)

anomaly_latitude =  anomaly_volume_df %>% 
  mutate(sign = ifelse(storage_anomaly_volume <= 0, "Negative", "Positive")) %>%
  group_by(lat, sign) %>% 
  summarize(mean_anomaly = mean(storage_anomaly_volume),
            sd_anomaly = sd(storage_anomaly_volume),
            sum_anomaly = sum(storage_anomaly_volume)) %>%
  mutate(latitude = as.numeric(as.character(lat))) %>%
  full_join(anomaly_latitude_blank)

write.csv(anomaly_latitude, "out/anomaly_latitude.csv", row.names = FALSE)

anomaly_latitude_avg = anomaly_latitude %>%
  group_by(lat) %>%
  summarize(mean_anomaly = mean(mean_anomaly),
            sum_anomaly = sum(sum_anomaly)) %>%
  mutate(latitude = as.numeric(as.character(lat)))

write.csv(anomaly_latitude_avg, "out/anomaly_latitude_avg.csv", row.names = FALSE)

anomaly_longitude_blank = data.frame(lon = c(as.factor(c(-180:180)), as.factor(c(-180:180))), 
                                    sign = rep(c("Negative", "Positive"), each = 361), longitude = c(c(-180:180), c(-180:180))) %>%
  filter(!lon %in% anomaly_volume_df$lon)

anomaly_longitude =  anomaly_volume_df %>% 
  mutate(sign = ifelse(storage_anomaly_volume <= 0, "Negative", "Positive")) %>%
  group_by(lon, sign) %>% 
  summarize(mean_anomaly = mean(storage_anomaly_volume),
            sd_anomaly = sd(storage_anomaly_volume),
            sum_anomaly = sum(storage_anomaly_volume)) %>%
  mutate(longitude = as.numeric(as.character(lon))) %>%
  full_join(anomaly_longitude_blank)

write.csv(anomaly_longitude, "out/anomaly_longitude.csv", row.names = FALSE)

anomaly_longitude_avg = anomaly_longitude %>%
  group_by(lon) %>%
  summarize(mean_anomaly = mean(mean_anomaly),
            sum_anomaly = sum(sum_anomaly)) %>%
  mutate(longitude = as.numeric(as.character(lon)))

write.csv(anomaly_longitude_avg, "out/anomaly_longitude_avg.csv", row.names = FALSE)

lat_plot = ggplot() +
  geom_line(data = anomaly_volume_df, 
            aes(x = latitude, y = storage_anomaly_volume),
            linewidth = 0.1, color = "grey80") +
  geom_ribbon(data = anomaly_latitude, alpha = 0.5,
              aes(x = latitude, ymin = mean_anomaly - sd_anomaly, ymax = mean_anomaly + sd_anomaly, fill = sign)) + 
  geom_line(data = anomaly_latitude, aes(x = latitude, y = mean_anomaly, color = sign), linewidth = 0.5) +
  geom_line(data = anomaly_latitude_avg, aes(x = latitude, y = mean_anomaly), linewidth = 0.3, color = "grey40") +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "bottom" )+
  labs(x ='Latitude [°]', y = "\n Mean Lake Storage \n Anomaly (MCM)") +
  theme(panel.spacing = unit(1, "lines"))+
  scale_color_manual(values = c("Positive" = "#01665E", "Negative" = "#8C510A")) +
  scale_fill_manual(values = c("Positive" = "#80CDC1", "Negative" = "#DFC27D")) +
  labs(fill="", color = "") +
  geom_hline(yintercept = 0, lty = 3, linewidth = 0.2, color = "black")

lat_plot_sum = ggplot() +
  geom_line(data = anomaly_volume_df, 
            aes(x = latitude, y = storage_anomaly_volume),
            linewidth = 0.1, color = "grey80") +
  geom_line(data = anomaly_latitude, aes(x = latitude, y = sum_anomaly, color = sign), linewidth = 0.5) +
  geom_line(data = anomaly_latitude_avg, aes(x = latitude, y = sum_anomaly), linewidth = 0.3, color = "grey40") +
  coord_flip() +
  theme_classic()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "bottom" )+
  labs(x ='Latitude [°]', y = "\n Cumulative Lake Storage \n Anomaly (MCM)") +
  theme(panel.spacing = unit(1, "lines"))+
  scale_color_manual(values = c("Positive" = "#01665E", "Negative" = "#8C510A")) +
  labs(color = "") +
  geom_hline(yintercept = 0, lty = 3, linewidth = 0.2, color = "black")

lon_plot = ggplot() +
  geom_line(data = anomaly_volume_df,
            aes(x = longitude, y = storage_anomaly_volume),
            linewidth = 0.1, color = "grey80") +
  geom_ribbon(data = anomaly_longitude, alpha = 0.5,
              aes(x = longitude, ymin = mean_anomaly - sd_anomaly, ymax = mean_anomaly + sd_anomaly, fill = sign)) + 
  geom_line(data = anomaly_longitude, aes(x = longitude, y = mean_anomaly, color = sign), linewidth = 0.5) +
  geom_line(data = anomaly_longitude_avg, aes(x = longitude, y = mean_anomaly), linewidth = 0.3, color = "grey40") +
  theme_classic()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "none" )+
  labs(x ='Longitude [°]', y = "\n \n \n \n",) +
  ylim(c(-5, 5)) +
  theme(panel.spacing = unit(1, "lines"))+
  scale_color_manual(values = c("Positive" = "#01665E", "Negative" = "#8C510A")) +
  scale_fill_manual(values = c("Positive" = "#80CDC1", "Negative" = "#DFC27D")) +
  geom_hline(yintercept = 0, lty = 3, linewidth = 0.2, color = "black")

lon_plot_sum = ggplot() +
  geom_line(data = anomaly_volume_df,
            aes(x = longitude, y = storage_anomaly_volume),
            linewidth = 0.1, color = "grey80") +
  geom_line(data = anomaly_longitude, aes(x = longitude, y = sum_anomaly, color = sign), linewidth = 0.5) +
  geom_line(data = anomaly_longitude_avg, aes(x = longitude, y = sum_anomaly), linewidth = 0.3, color = "grey40") +
  theme_classic()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  theme(legend.position = "none" )+
  labs(x ='Longitude [°]', y = "\n \n \n \n",) +
  theme(panel.spacing = unit(1, "lines"))+
  scale_color_manual(values = c("Positive" = "#01665E", "Negative" = "#8C510A")) +
  geom_hline(yintercept = 0, lty = 3, linewidth = 0.2, color = "black")

pdf(paste0("out/Lake_Anomaly_Volume_mean_lat_lon.pdf"), width = 9, height = 6)
grid.arrange(lon_plot, grid::nullGrob(), anomaly_volume_plot, lat_plot, nrow = 2, ncol =2, widths = c(3, 1), heights = c(1, 3))
dev.off()

pdf(paste0("out/Lake_Anomaly_Volume_sum_lat_lon.pdf"), width = 9, height = 6)
grid.arrange(lon_plot_sum, grid::nullGrob(), anomaly_volume_plot, lat_plot_sum, nrow = 2, ncol =2, widths = c(3, 1), heights = c(1, 3))
dev.off()