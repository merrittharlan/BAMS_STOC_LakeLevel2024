library(ggplot2)
library(dplyr)
library(sf)
library(RColorBrewer)
library(gridExtra)

in_path = "out/"
out_path = "out/"

#  Define Robinson projection
cr1 = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#  Load data
anomaly_volume = read.csv(paste0(in_path, "anomaly_volume_filtered.csv"))
anomaly_volume_sp = st_as_sf(anomaly_volume, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(cr1)
anomaly_latitude = read.csv(paste0(in_path, "anomaly_latitude.csv"))
anomaly_latitude_avg = read.csv(paste0(in_path, "anomaly_latitude_avg.csv"))
anomaly_longitude = read.csv(paste0(in_path, "anomaly_longitude.csv"))
anomaly_longitude_avg = read.csv(paste0(in_path, "anomaly_longitude_avg.csv"))

#  Get Continents data
# https://hub.arcgis.com/datasets/esri::world-continents/explore?location=0.005807%2C-36.562506%2C0.00 
shp_robinson = read_sf('input/World_Continents.shp', stringsAsFactors = F) %>% st_transform(cr1)

# Storage Anomaly Volume Plot
size_sf = 0.2
volume_breaks = c(seq(-5, 5, 1))

anomaly_volume_plot = ggplot() +
  geom_sf(data = shp_robinson, colour = "black", fill = 'gray80')+ 
  geom_sf(anomaly_volume_sp, 
          mapping=aes(fill = storage_anomaly_volume, 
                      color = storage_anomaly_volume),
          size=size_sf,
          pch=21)+
  scale_color_fermenter(breaks = volume_breaks, palette = "BrBG", name = "Anomalies from 1995-2020 (MCM)", direction = 1) +
  scale_fill_fermenter(breaks = volume_breaks, palette = "BrBG", name = "Anomalies from 1995-2020 (MCM)", direction = 1) +
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

lat_plot = ggplot() +
  geom_line(data = anomaly_volume, 
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

lon_plot = ggplot() +
  geom_line(data = anomaly_volume,
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

pdf(paste0("out/Lake_Anomaly_Volume_lat_lon_mean2.pdf"), width = 9, height = 6)
grid.arrange(lon_plot, NULL, anomaly_volume_plot, lat_plot, nrow = 2, ncol =2, widths = c(3, 1), heights = c(1, 3))
dev.off()