library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(zoo)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(scales)
library(purrr)

GREALM_timeseries = read.csv("out/GREALM_timeseries_1993_2020BL.csv") %>%
  filter(Year >= 1993)
GREALM_anomaly = read.csv("out/GREALM_anomaly_1993_2020BL.csv")

GloLakes_timeseries = read.csv("out/Glolakes_timeseries_1993_2020BL.csv") %>%
  filter(Year >= 1993)
GloLakes_anomaly = read.csv("out/Glolakes_anomaly_1993_2020BL.csv")

### Calculate GREALM anomalies
# Create a complete grid of all sites and years
GREALM_complete_grid <- expand_grid(
  Year = seq(min(GREALM_timeseries$Year), max(GREALM_timeseries$Year)),
  site_no = unique(GREALM_timeseries$site_no)
)

# Join your original data to this complete grid
GREALM_complete_df <- GREALM_complete_grid %>%
  left_join(
    GREALM_timeseries %>%
      select(Year, site_no, Smoothed_target_height_EGM2008),
    by = c("Year", "site_no")
  )

# Interpolate & Fill Leading/Trailing NAs
GREALM_filled_df <- GREALM_complete_df %>%
  group_by(site_no) %>%
  arrange(Year) %>%
  mutate(
    Smoothed_target_height_EGM2008 = na.approx(Smoothed_target_height_EGM2008, Year, na.rm = FALSE),
    Smoothed_target_height_EGM2008 = na.locf(Smoothed_target_height_EGM2008, na.rm = FALSE),             # Fill leading NAs forward
    Smoothed_target_height_EGM2008 = na.locf(Smoothed_target_height_EGM2008, fromLast = TRUE, na.rm = FALSE) # Fill trailing NAs backward
  ) %>%
  ungroup() %>%
  distinct(.keep_all = TRUE)

GREALM_baseline_avg <- GREALM_filled_df %>%
  filter(Year >= 1993, Year <= 2020) %>%
  group_by(site_no) %>%
  summarize(baseline_mean = mean(Smoothed_target_height_EGM2008, na.rm = TRUE))

GREALM_anomaly_df <- GREALM_filled_df %>%
  left_join(GREALM_baseline_avg, by = "site_no") %>%
  mutate(
    percentage_anomaly = ((Smoothed_target_height_EGM2008 - baseline_mean) / baseline_mean) * 100,
    ID_factor = as.factor(site_no)
  )

GREALM_median_anomaly_df <- GREALM_anomaly_df %>%
  group_by(Year) %>%
  summarize(
    median_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE),
    sd_percentage_anomaly = sd(percentage_anomaly, na.rm = TRUE)
  )

###Calculate GloLakes anomalies
# Create a complete grid of all sites and years
GloLakes_complete_grid <- expand_grid(
  Year = seq(min(GloLakes_timeseries$Year), max(GloLakes_timeseries$Year)),
  ID = unique(GloLakes_timeseries$ID)
)

# Join your original data to this complete grid
GloLakes_complete_df <- GloLakes_complete_grid %>%
  left_join(
    GloLakes_timeseries %>%
      select(Year, ID, lake_storage),
    by = c("Year", "ID")
  )

# Interpolate & Fill Leading/Trailing NAs
GloLakes_filled_df <- GloLakes_complete_df %>%
  group_by(ID) %>%
  arrange(Year) %>%
  mutate(
    lake_storage = na.approx(lake_storage, Year, na.rm = FALSE),
    lake_storage = na.locf(lake_storage, na.rm = FALSE),             # Fill leading NAs forward
    lake_storage = na.locf(lake_storage, fromLast = TRUE, na.rm = FALSE) # Fill trailing NAs backward
  ) %>%
  ungroup() %>%
  distinct(.keep_all = TRUE)

GloLakes_baseline_avg <- GloLakes_filled_df %>%
  filter(Year >= 1993, Year <= 2020) %>%
  group_by(ID) %>%
  summarize(baseline_mean = mean(lake_storage, na.rm = TRUE))

GloLakes_anomaly_df <- GloLakes_filled_df %>%
  left_join(GloLakes_baseline_avg, by = "ID") %>%
  mutate(
    percentage_anomaly = ((lake_storage - baseline_mean) / baseline_mean) * 100,
    ID_factor = as.factor(ID)
  )

GloLakes_median_anomaly_df <- GloLakes_anomaly_df %>%
  group_by(Year) %>%
  summarize(
    median_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE),
    sd_percentage_anomaly = sd(percentage_anomaly, na.rm = TRUE)
  )

### Plot anomaly time series
timeseries_plot = ggplot() +
  geom_point(data = GREALM_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "GREALM (lake level)")) +
  geom_smooth(data = GREALM_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "GREALM (lake level)")) +
  geom_point(data = GloLakes_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "GloLakes (lake storage)")) +
  geom_smooth(data = GloLakes_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "GloLakes (lake storage)")) +
  theme_classic() +
  scale_color_manual(values = c("GREALM (lake level)" = "grey50", "GloLakes (lake storage)" = "black"), name = "") +
  xlab("Year") + ylab("Percent Anomaly from \n 1993-2020 Baseline (%)") +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "plain"),
        text = element_text(size = 12))

### Plot absolute anomalies
GREALM_anomaly_df_level = GREALM_filled_df %>%
  left_join(GREALM_baseline_avg, by = "site_no") %>%
  mutate(
    level_anomaly = Smoothed_target_height_EGM2008 - baseline_mean,
    ID_factor = as.factor(site_no)
  )

GREALM_median_anomaly_df_level = GREALM_anomaly_df_level %>%
  group_by(Year) %>%
  summarize(
    median_level_anomaly = median(level_anomaly, na.rm = TRUE),
    sd_level_anomaly = sd(level_anomaly, na.rm = TRUE)
  )

GloLakes_anomaly_df_storage <- GloLakes_filled_df %>%
  left_join(GloLakes_baseline_avg, by = "ID") %>%
  mutate(
    storage_anomaly = lake_storage - baseline_mean,
    ID_factor = as.factor(ID)
  )

GloLakes_median_anomaly_df_storage <- GloLakes_anomaly_df_storage %>%
  group_by(Year) %>%
  summarize(
    median_storage_anomaly = median(storage_anomaly, na.rm = TRUE),
    sd_storage_anomaly = sd(storage_anomaly, na.rm = TRUE)
  )

# Determine a scaling factor based on the range of anomalies
scaling_factor <- max(abs(GREALM_median_anomaly_df_level$median_level_anomaly), na.rm = TRUE) / 
  max(abs(GloLakes_median_anomaly_df_storage$median_storage_anomaly), na.rm = TRUE)

absolute_plot = ggplot() +
  # Lake Level Anomaly (Primary Y-Axis)
  geom_point(data = GREALM_median_anomaly_df_level, aes(x = Year, y = median_level_anomaly, color = "GREALM (lake level)")) +
  geom_smooth(data = GREALM_median_anomaly_df_level, aes(x = Year, y = median_level_anomaly, color = "GREALM (lake level)")) +
  
  # Lake Storage Anomaly (Secondary Y-Axis, Adjusted for Scaling)
  geom_point(data = GloLakes_median_anomaly_df_storage, aes(x = Year, y = median_storage_anomaly * scaling_factor, color = "GloLakes (lake storage)")) +
  geom_smooth(data = GloLakes_median_anomaly_df_storage, aes(x = Year, y = median_storage_anomaly * scaling_factor, color = "GloLakes (lake storage)")) +
  
  theme_classic() +
  scale_color_manual(values = c("GREALM (lake level)" = "grey50", "GloLakes (lake storage)" = "black"), name = "") +
  
  # Define Dual Y-Axes
  scale_y_continuous(
    name = "Lake Level Anomaly (m)",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Lake Storage Anomaly (MCM)")
  ) +
  
  xlab("Year") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "plain"),
    text = element_text(size = 12)
  )

# Save Plot
png("out/timeseries_plot.png", width = 7, height = 3.5, units = "in", res = 300)
ggarrange(timeseries_plot, absolute_plot, ncol = 2, labels = c("a)", "b)"), widths = c(0.8, 1), common.legend = TRUE, legend = "bottom")
dev.off()


### Plot anomaly plate
#  Define Robinson projection
cr1 = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#  Load data
GREALM_2024_anomaly = GREALM_anomaly_df %>% 
  left_join(GREALM_anomaly) %>% 
  filter(abs(percentage_anomaly) < 100) %>% 
  filter(Year == 2024) %>%
  mutate(Source = "GREALM") %>%
  select(Source, percentage_anomaly, ID, Pour_lat, Pour_long, Country, Grand_id, Continent, Elevation, Depth_avg, Lake_area)

GloLakes_2024_anomaly = GloLakes_anomaly_df %>% 
  left_join(GloLakes_anomaly) %>% 
  filter(abs(percentage_anomaly) < 100) %>% 
  filter(Year == 2024) %>%
  mutate(Source = "GloLakes") %>%
  select(Source, percentage_anomaly, ID, Pour_lat, Pour_long, Country, Grand_id, Continent, Elevation, Depth_avg, Lake_area)

GREALM_GloLakes_df = rbind(GREALM_2024_anomaly, GloLakes_2024_anomaly) %>% 
  drop_na() %>%
  st_as_sf(coords = c("Pour_long", "Pour_lat"), crs = 4326)

#  Get Continents data
shp_robinson = read_sf('data/World_Continents.shp', stringsAsFactors = F) %>% st_transform(cr1)

size_sf = 0.5
percent_breaks = c(seq(-100, 100, 20))

# Storage Anomaly % Plot
anomaly_percent_plot = ggplot() +
  geom_sf(data = shp_robinson, colour = "black", fill = 'gray80')+ 
  geom_sf(GREALM_GloLakes_df, 
          mapping=aes(fill = percentage_anomaly, 
                      color = percentage_anomaly),
          size=size_sf, pch=21)+
  scale_color_fermenter(breaks = percent_breaks, palette = "BrBG", name = "Anomalies from 1993-2020 (%)", direction = 1) +
  scale_fill_fermenter(breaks = percent_breaks, palette = "BrBG", name = "Anomalies from 1993-2020 (%)", direction = 1) +
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
  ggtitle("Lake Storage and Level") +
  theme_light() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        panel.border = element_rect(fill = NA, color=NA),
        legend.position = "bottom")

png(paste0("out/Lake_Plate.png"), width = 6, height = 4, units = "in", res = 300)
anomaly_percent_plot
dev.off()

### Plot map showing GREALM vs GloLakes, reservoirs vs natural lakes
# Define bins for lake area
bin_breaks <- c(0, 1, 10, 100, 1000, Inf)  
bin_labels <- c("<1", "1-10", "10-100", "100-1,000", ">1,000")

# Categorize lakes as Reservoir or Natural
GREALM_GloLakes_df <- GREALM_GloLakes_df %>%
  mutate(
    lake_area_bin = cut(Lake_area, breaks = bin_breaks, labels = bin_labels, include.lowest = TRUE),
    Reservoir = ifelse(Grand_id > 0, "Reservoir", "Natural")
  )

# Compute sample sizes per bin and reservoir type
sample_sizes <- GREALM_GloLakes_df %>%
  group_by(lake_area_bin, Reservoir) %>%
  summarise(count = n(), .groups = 'drop')

# Determine y-position for labels
y_max <- max(GREALM_GloLakes_df$percentage_anomaly, na.rm = TRUE) 
y_natural <- y_max + 24  # Higher position for Natural
y_reservoir <- y_max + 10  # Even higher for Reservoir

# Create the box plot with side-by-side grouping
reservoir_size_plot = ggplot(GREALM_GloLakes_df, aes(x = lake_area_bin, y = percentage_anomaly, fill = Reservoir)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.fill = NA, outlier.size = 0.5, outlier.alpha = 0.5) +  # Side-by-side grouping
  # Text for Natural (first line)
  geom_text(data = sample_sizes %>% filter(Reservoir == "Natural"), 
            aes(x = lake_area_bin, y = y_natural, label = paste0("n=", comma(count))), 
            position = position_dodge(width = 0.8), size = 4, color = "black") +
  # Text for Reservoir (second line)
  geom_text(data = sample_sizes %>% filter(Reservoir == "Reservoir"), 
            aes(x = lake_area_bin, y = y_reservoir, label = paste0("n=", comma(count))), 
            position = position_dodge(width = 0.8), size = 4, color = "grey50") +
  scale_fill_manual(values = c("Reservoir" = "white", "Natural" = "black")) +  # Boxplot colors
  labs(x = expression("Lake Area (binned) km" ^2), y = "Anomalies from Baseline 1993-2001 (%)", 
       title = "Anomalies by Lake Area and Type",
       fill = "Lake Type", color = "Lake Type") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "plain"),
        text = element_text(size = 12))

png(paste0("out/reservoir_area.png"), width = 6, height = 4, units = "in", res = 300)
reservoir_size_plot
dev.off()