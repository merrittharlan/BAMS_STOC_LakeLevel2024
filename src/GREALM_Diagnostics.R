library(ncdf4)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)

GREALM_timeseries = read.csv("out/GREALM_timeseries_1992_2020BL.csv")
GREALM_year_count = GREALM_timeseries %>%
  group_by(Year, site_no) %>% summarize(obs_count = length(which(!is.na(Smoothed_target_height_EGM2008)))) %>% 
  filter(obs_count > 0) %>% group_by(site_no) %>% summarize(year_count = length(Year))

#generate some basic plots
plot_fun <- function(data_df = GREALM_timeseries,
                     title = "GREALM_diagnostics"){
  
  line_plot = ggplot(data_df %>% group_by(Year, site_no) %>%
                       summarize(annual_level = mean(Smoothed_target_height_EGM2008)) %>%
                       mutate(ID_factor = as.factor(site_no)), 
                     aes(x = Year, y = annual_level, color = ID_factor)) + 
    geom_line() + 
    theme_bw() +
    xlab("Year") +
    ylab("Mean Annual Lake Level (m)") +
    theme(legend.position="none") +
    scale_color_grey(start = 0, end = 0.7) +
    scale_y_log10()
  
  # Step 1: Calculate baseline average for each lake (2000-2020)
  baseline_avg <- data_df %>%
    filter(Year >= 1992, Year <= 2020) %>%
    group_by(site_no) %>%
    summarize(baseline_mean = mean(Smoothed_target_height_EGM2008, na.rm = TRUE))
  
  # Step 2: Join baseline mean with the full dataset and calculate anomaly
  anomaly_df <- data_df %>%
    group_by(Year, site_no) %>%
    summarize(annual_level = mean(Smoothed_target_height_EGM2008, na.rm = TRUE)) %>%
    left_join(baseline_avg, by = "site_no") %>%
    mutate(
      percentage_anomaly = ((annual_level - baseline_mean) / baseline_mean) * 100,
      ID_factor = as.factor(site_no)
    )
  
  # Step 3: Plot the percentage anomaly
  anomaly_plot <- ggplot(anomaly_df, aes(x = Year, y = percentage_anomaly, color = ID_factor)) +
    geom_line() +
    theme_bw() +
    xlab("Year") +
    ylab("Percentage Anomaly from 1992-2020 Baseline (%)") +
    theme(legend.position = "none") +
    scale_color_grey(start = 0, end = 0.7)
  
  median_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(median_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE))
  
  mean_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(mean_percentage_anomaly = mean(percentage_anomaly, na.rm = TRUE))
  
  # Step 4: Plot the average anomaly over time
  mean_anomaly_plot <- ggplot() +
    geom_line(data = median_anomaly_df, aes(x = Year, y = median_percentage_anomaly, color = "median")) +
    geom_line(data = mean_anomaly_df, aes(x = Year, y = mean_percentage_anomaly, color = "mean"), lty = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Baseline reference line
    theme_bw() +
    xlab("Year") +
    ylab("Percent Anomaly from 2000-2020 Baseline (%)") +
    ggtitle("Average Percentage Anomaly of Lake Area Over Time") +
    scale_color_manual(values = c("median" = "grey50", "mean" = "grey10"))
  
  # Step 1: Calculate baseline mean (2000-2020) for each lake (ID)
  baseline_avg <- data_df %>%
    filter(Year >= 1992, Year <= 2020) %>%
    group_by(site_no) %>%
    summarize(baseline_mean = mean(Smoothed_target_height_EGM2008, na.rm = TRUE))
  
  # Step 2: Calculate area anomaly for each Year and ID
  anomaly_df <- data_df %>%
    group_by(Year, site_no) %>%
    summarize(annual_level = mean(Smoothed_target_height_EGM2008, na.rm = TRUE)) %>%
    left_join(baseline_avg, by = "site_no") %>%
    mutate(level_anomaly = annual_level - baseline_mean)
  
  anomaly_level_plot <- ggplot(anomaly_df, aes(x = Year, y = level_anomaly, color = as.factor(site_no))) +
    geom_line() +
    theme_bw() +
    xlab("Year") +
    ylab("Level Anomaly (m)") +
    theme(legend.position = "none") +
    scale_color_grey(start = 0, end = 0.7)
  
  # Step 3: Average area anomaly across all IDs for each year
  mean_level_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(mean_level_anomaly = mean(level_anomaly, na.rm = TRUE))
  
  median_level_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(median_level_anomaly = median(level_anomaly, na.rm = TRUE))
  
  # Step 4: Plot the average area anomaly over time
  level_anomaly_plot <- ggplot() +
    geom_line(data = median_level_anomaly_df, aes(x = Year, y = median_level_anomaly, color = "median")) +
    geom_line(data = mean_level_anomaly_df, aes(x = Year, y = mean_level_anomaly, color = "mean"), lty = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Baseline reference line
    theme_bw() +
    xlab("Year") +
    ylab("Mean Level Anomaly (m)") +
    ggtitle("Mean Lake Level Anomaly Over Time (Relative to 1992-2020 Baseline)") +
    scale_color_manual(values = c("median" = "grey50", "mean" = "grey10"))
  
  pdf(paste0("out/", title, ".pdf"))
  par(mfrow = c(3,2))
  plot(line_plot)
  print(anomaly_plot)
  print(mean_anomaly_plot)
  print(anomaly_level_plot)
  print(level_anomaly_plot)
  dev.off()
}

plot_fun(data_df = GREALM_timeseries,
         title = "GREALM_diagnostics")

#Compare same ID waterbodies
GloLakes_LandsatGREALM_data = read.csv("out/GloLakes_LandsatGREALM_data.csv")
GloLakes_LandsatICESat2_data = read.csv("out/GloLakes_LandsatICESat2_data.csv")
GloLakes_LandsatSentinel2_data = read.csv("out/GloLakes_LandsatSentinel2_data.csv")

GREALM_anomaly = read.csv("out/GREALM_anomaly_1992_2020BL.csv")
GREALM_yearly = GREALM_timeseries %>%
  mutate(year = Year) %>%
  left_join(GREALM_anomaly) %>%
  group_by(ID, year) %>%
  summarize(mean_level = mean(Smoothed_target_height_EGM2008),
            sd_level = sd(Smoothed_target_height_EGM2008))

GREALM_GloLakesGREALM = GREALM_yearly %>% inner_join(GloLakes_LandsatGREALM_data, by = c("ID", "year"))
GREALM_GloLakesICESat2 = GREALM_yearly %>% inner_join(GloLakes_LandsatICESat2_data, by = c("ID", "year"))
GREALM_GloLakesSentinel2 = GREALM_yearly %>% inner_join(GloLakes_LandsatSentinel2_data, by = c("ID", "year"))

GREALM_GloLakesGREALM_plot = ggplot(GREALM_GloLakesGREALM, aes(x = mean_level, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("GREALM level") + ylab("GloLakes GREALM Storage")

GREALM_GloLakesICESat2_plot = ggplot(GREALM_GloLakesICESat2, aes(x = mean_level, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("GREALM level") + ylab("GloLakes ICESat2 Storage")

GREALM_GloLakesSentinel2_plot = ggplot(GREALM_GloLakesSentinel2, aes(x = mean_level, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("GREALM level") + ylab("GloLakes Sentinel2 Storage")

# Calculate R² for each unique ID
GREALM_GloLakesGREALM_r2_df <- GREALM_GloLakesGREALM %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_level, mean_annual_storage)^2,
    .groups = "drop"
  )

GREALM_GloLakesICESat2_r2_df <- GREALM_GloLakesICESat2 %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_level, mean_annual_storage)^2,
    .groups = "drop"
  )

GREALM_GloLakesSentinel2_r2_df <- GREALM_GloLakesSentinel2 %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_level, mean_annual_storage)^2,
    .groups = "drop"
  )

GREALM_GloLakesGREALM_r2 = ggplot(GREALM_GloLakesGREALM_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "GREALM level ~ GloLakes GREALM Storage")) +
  xlab("")

GREALM_GloLakesICESat2_r2 = ggplot(GREALM_GloLakesICESat2_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "GREALM level ~ GloLakes ICESat2 Storage")) +
  xlab("")

GREALM_GloLakesSentinel2_r2= ggplot(GREALM_GloLakesSentinel2_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "GREALM level ~ GloLakes Sentinel2 Storage")) +
  xlab("")

pdf("out/GREALM_GloLakes_same_lakes.pdf")
print(GREALM_GloLakesGREALM_plot)
print(GREALM_GloLakesICESat2_plot)
print(GREALM_GloLakesSentinel2_plot)
print(GREALM_GloLakesGREALM_r2)
print(GREALM_GloLakesICESat2_r2)
print(GREALM_GloLakesSentinel2_r2)
dev.off()