library(ggplot2)
library(dplyr)
library(sf)
library(RColorBrewer)
library(gridExtra)

#Updated to baseline 1991

#  Define Robinson projection
cr1 = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#  Load data
anomaly_data = read.csv("out/lakes_anomaly_1991_2020BL.csv")
anomaly_data_sp = st_as_sf(anomaly_data, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(cr1)
anomaly_data_sp = anomaly_data_sp %>% filter(storage_anomaly_percent < 100)

anomaly_data_filtered = anomaly_data %>% filter(storage_anomaly_percent < 100) %>% select(storage_anomaly_percent, ID, latitude, longitude)
write.csv(anomaly_data_filtered, "out/DATA_SotC_LakeWaterStorage_plate_v2.csv", row.names = FALSE)
write.csv(anomaly_data, "out/lake_storage_percent_plate_all.csv", row.names = FALSE)


#  Get Continents data
shp_robinson = read_sf('input/World_Continents.shp', stringsAsFactors = F) %>% st_transform(cr1)

size_sf = 0.1
percent_breaks = c(seq(-100, 100, 20))

# Storage Anomaly % Plot
anomaly_percent_plot = ggplot() +
  geom_sf(data = shp_robinson, colour = "black", fill = 'gray80')+ 
  geom_sf(anomaly_data_sp, 
          mapping=aes(fill = storage_anomaly_percent, 
                      color = storage_anomaly_percent),
          size=size_sf, pch=21)+
  scale_color_fermenter(breaks = percent_breaks, palette = "BrBG", name = "Anomaly from 1991-2020 (%)", direction = 1) +
  scale_fill_fermenter(breaks = percent_breaks, palette = "BrBG", name = "Anomaly from 1991-2020 (%)", direction = 1) +
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
  ggtitle("Lake Storage") +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_rect(fill = NA, color=NA),
        legend.position = "bottom")

pdf(paste0("out/Lake_Storage_Plate.pdf"), width = 6, height = 4)
plot(anomaly_percent_plot)
dev.off()

