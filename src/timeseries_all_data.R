library(ncdf4)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)

GREALM_timeseries = read.csv("out/GREALM_timeseries_1993_2020BL.csv")

GREALM_baseline_avg <- GREALM_timeseries %>%
  filter(Year >= 1992, Year <= 2020) %>%
  group_by(site_no) %>%
  summarize(baseline_mean = mean(Smoothed_target_height_EGM2008, na.rm = TRUE))

# Step 2: Join baseline mean with the full dataset and calculate anomaly
GREALM_anomaly_df <- GREALM_timeseries %>%
  group_by(Year, site_no) %>%
  summarize(annual_level = mean(Smoothed_target_height_EGM2008, na.rm = TRUE)) %>%
  left_join(GREALM_baseline_avg, by = "site_no") %>%
  mutate(
    percentage_anomaly = ((annual_level - baseline_mean) / baseline_mean) * 100,
    ID_factor = as.factor(site_no)
  )

GREALM_median_anomaly_df <- GREALM_anomaly_df %>%
  group_by(Year) %>%
  summarize(median_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE))

MOGWAI_timeseries = read.csv("data/MOGWAI/MOGWAI_timeseries_2001_2020BL.csv")

MOGWAI_baseline_avg <- MOGWAI_timeseries %>%
  filter(Year >= 2001, Year <= 2020) %>%
  group_by(ID) %>%
  summarize(baseline_mean = mean(DSWE_Area_km2, na.rm = TRUE))

# Step 2: Join baseline mean with the full dataset and calculate anomaly
MOGWAI_anomaly_df <- MOGWAI_timeseries %>%
  group_by(Year, ID) %>%
  summarize(annual_area = mean(DSWE_Area_km2, na.rm = TRUE)) %>%
  left_join(MOGWAI_baseline_avg, by = "ID") %>%
  mutate(
    percentage_anomaly = ((annual_area - baseline_mean) / baseline_mean) * 100,
    ID_factor = as.factor(ID)
  )

MOGWAI_median_anomaly_df <- MOGWAI_anomaly_df %>%
  group_by(Year) %>%
  summarize(median_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE))

median_anomaly_plot <- ggplot() +
  geom_line(data = GREALM_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "GREALM")) +
  geom_line(data = MOGWAI_median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "MOGWAI")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Baseline reference line
  theme_bw() +
  xlab("Year") +
  ylab("Percent Anomaly from Baseline (%)") +
  ggtitle("Median Percent Anomaly Over Time") +
  scale_color_manual(values = c("GREALM" = "darkgreen", "MOGWAI" = "purple"))

median_anomaly_plot
