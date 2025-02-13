library(ncdf4)
library(dplyr)
library(ggplot2)
library(corrplot)
library(sf)

MOGWAI_timeseries = read.csv("data/MOGWAI/MOGWAI_timeseries_2001_2020BL.csv")
MOGWAI_year_count = MOGWAI_timeseries %>%
  group_by(Year, ID) %>% summarize(obs_count = length(which(!is.na(DSWE_Area_km2)))) %>% 
  filter(obs_count > 0) %>% group_by(ID) %>% summarize(year_count = length(Year))

#generate some basic plots
plot_fun <- function(data_df = MOGWAI_timeseries,
                     title = "MOGWAI_diagnostics"){
  
  bar_plot = ggplot(data_df %>% group_by(Year) %>%
                      summarize(Cumulative_Area = sum(DSWE_Area_km2)),
                    aes(x = Year, y = Cumulative_Area)) + 
                geom_bar(stat = "identity") +
                theme_bw() + 
                xlab("Year") + 
                ylab("Mean Annual Lake Area (km2) by Lake") +
                theme(legend.position="none") +
                scale_color_viridis_d()
  
  line_plot = ggplot(data_df %>% group_by(Year, ID) %>%
                       summarize(annual_area = mean(DSWE_Area_km2)) %>%
                       mutate(ID_factor = as.factor(ID)), 
                     aes(x = Year, y = annual_area, color = ID_factor)) + 
                geom_line() + 
                theme_bw() +
                xlab("Year") +
                ylab("Mean Annual Lake Area (km2)") +
                theme(legend.position="none") +
                scale_color_grey(start = 0, end = 0.7) +
                scale_y_log10()
  
  # Step 1: Calculate baseline average for each lake (2000-2020)
  baseline_avg <- data_df %>%
    filter(Year >= 2000, Year <= 2020) %>%
    group_by(ID) %>%
    summarize(baseline_mean = mean(DSWE_Area_km2, na.rm = TRUE))
  
  # Step 2: Join baseline mean with the full dataset and calculate anomaly
  anomaly_df <- data_df %>%
    group_by(Year, ID) %>%
    summarize(annual_area = mean(DSWE_Area_km2, na.rm = TRUE)) %>%
    left_join(baseline_avg, by = "ID") %>%
    mutate(
      percentage_anomaly = ((annual_area - baseline_mean) / baseline_mean) * 100,
      ID_factor = as.factor(ID)
    )
  
  # Step 3: Plot the percentage anomaly
  anomaly_plot <- ggplot(anomaly_df, aes(x = Year, y = percentage_anomaly, color = ID_factor)) +
    geom_line() +
    theme_bw() +
    xlab("Year") +
    ylab("Percentage Anomaly from 2000-2020 Baseline (%)") +
    theme(legend.position = "none") +
    scale_color_grey(start = 0, end = 0.7) #+
    #coord_cartesian(ylim = c(50, 500))
  
  median_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(mean_percentage_anomaly = median(percentage_anomaly, na.rm = TRUE))
  
  mean_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(mean_percentage_anomaly = mean(percentage_anomaly, na.rm = TRUE))
  
  # Step 4: Plot the average anomaly over time
  mean_anomaly_plot <- ggplot() +
    geom_line(data = median_anomaly_df, aes(x = Year, y = mean_percentage_anomaly, color = "median")) +
    geom_line(data = mean_anomaly_df, aes(x = Year, y = mean_percentage_anomaly, color = "mean"), lty = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Baseline reference line
    theme_bw() +
    xlab("Year") +
    ylab("Percent Anomaly from 2000-2020 Baseline (%)") +
    ggtitle("Average Percentage Anomaly of Lake Area Over Time") +
    scale_color_manual(values = c("median" = "grey50", "mean" = "grey10"))
  
  # Step 1: Calculate baseline mean (2000-2020) for each lake (ID)
  baseline_avg <- data_df %>%
    filter(Year >= 2000, Year <= 2020) %>%
    group_by(ID) %>%
    summarize(baseline_mean = mean(DSWE_Area_km2, na.rm = TRUE))
  
  # Step 2: Calculate area anomaly for each Year and ID
  anomaly_df <- data_df %>%
    group_by(Year, ID) %>%
    summarize(annual_area = mean(DSWE_Area_km2, na.rm = TRUE)) %>%
    left_join(baseline_avg, by = "ID") %>%
    mutate(area_anomaly = annual_area - baseline_mean)
  
  anomaly_area_plot <- ggplot(anomaly_df, aes(x = Year, y = area_anomaly, color = as.factor(ID))) +
    geom_line() +
    theme_bw() +
    xlab("Year") +
    ylab("Area Anomaly (km²)") +
    theme(legend.position = "none") +
    scale_color_grey(start = 0, end = 0.7)
  
  # Step 3: Average area anomaly across all IDs for each year
  mean_area_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(mean_area_anomaly = mean(area_anomaly, na.rm = TRUE))
  
  median_area_anomaly_df <- anomaly_df %>%
    group_by(Year) %>%
    summarize(median_area_anomaly = median(area_anomaly, na.rm = TRUE))
  
  # Step 4: Plot the average area anomaly over time
  area_anomaly_plot <- ggplot() +
    geom_line(data = median_area_anomaly_df, aes(x = Year, y = median_area_anomaly, color = "median")) +
    geom_line(data = mean_area_anomaly_df, aes(x = Year, y = mean_area_anomaly, color = "mean"), lty = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Baseline reference line
    theme_bw() +
    xlab("Year") +
    ylab("Mean Area Anomaly (km²)") +
    ggtitle("Mean Lake Area Anomaly Over Time (Relative to 2000-2020 Baseline)") +
    scale_color_manual(values = c("median" = "grey50", "mean" = "grey10"))
  
  pdf(paste0("out/", title, ".pdf"))
  par(mfrow = c(3,2))
  plot(bar_plot)
  plot(line_plot)
  print(anomaly_plot)
  print(mean_anomaly_plot)
  print(anomaly_area_plot)
  print(area_anomaly_plot)
  dev.off()
}

plot_fun(data_df = MOGWAI_timeseries,
         title = "MOGWAI_diagnostics")

#Compare same ID waterbodies
GloLakes_LandsatGREALM_data = read.csv("out/GloLakes_LandsatGREALM_data.csv")
GloLakes_LandsatICESat2_data = read.csv("out/GloLakes_LandsatICESat2_data.csv")
GloLakes_LandsatSentinel2_data = read.csv("out/GloLakes_LandsatSentinel2_data.csv")

MOGWAI_yearly = MOGWAI_timeseries %>%
  mutate(year = Year) %>%
  group_by(ID, year) %>%
  summarize(mean_area = mean(DSWE_Area_km2),
            sd_area = sd(DSWE_Area_km2))

MOGWAI_GloLakesGREALM = MOGWAI_yearly %>% inner_join(GloLakes_LandsatGREALM_data, by = c("ID", "year"))
MOGWAI_GloLakesICESat2 = MOGWAI_yearly %>% inner_join(GloLakes_LandsatICESat2_data, by = c("ID", "year"))
MOGWAI_GloLakesSentinel2 = MOGWAI_yearly %>% inner_join(GloLakes_LandsatSentinel2_data, by = c("ID", "year"))

MOGWAI_GloLakesGREALM_plot = ggplot(MOGWAI_GloLakesGREALM, aes(x = mean_area, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("MOGWAI Area") + ylab("GloLakes GREALM Storage")

MOGWAI_GloLakesICESat2_plot = ggplot(MOGWAI_GloLakesICESat2, aes(x = mean_area, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("MOGWAI Area") + ylab("GloLakes ICESat2 Storage")

MOGWAI_GloLakesSentinel2_plot = ggplot(MOGWAI_GloLakesSentinel2, aes(x = mean_area, y = mean_annual_storage)) + 
  geom_point(aes(color = ID)) + 
  theme_minimal() +
  xlab("MOGWAI Area") + ylab("GloLakes Sentinel2 Storage")

# Calculate R² for each unique ID
MOGWAI_GloLakesGREALM_r2_df <- MOGWAI_GloLakesGREALM %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_area, mean_annual_storage)^2,
    .groups = "drop"
  )

MOGWAI_GloLakesICESat2_r2_df <- MOGWAI_GloLakesICESat2 %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_area, mean_annual_storage)^2,
    .groups = "drop"
  )

MOGWAI_GloLakesSentinel2_r2_df <- MOGWAI_GloLakesSentinel2 %>%
  group_by(ID) %>%
  summarize(
    r_squared = cor(mean_area, mean_annual_storage)^2,
    .groups = "drop"
  )

MOGWAI_GloLakesGREALM_r2 = ggplot(MOGWAI_GloLakesGREALM_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "MOGWAI Area ~ GloLakes GREALM Storage")) +
  xlab("")

MOGWAI_GloLakesICESat2_r2 = ggplot(MOGWAI_GloLakesICESat2_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "MOGWAI Area ~ GloLakes ICESat2 Storage")) +
  xlab("")

MOGWAI_GloLakesSentinel2_r2= ggplot(MOGWAI_GloLakesSentinel2_r2_df, aes(x = "", y = r_squared)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.7) +
  theme_minimal() +
  ylab(expression(R^2 ~ "MOGWAI Area ~ GloLakes Sentinel2 Storage")) +
  xlab("")

pdf("out/MOGWAI_GloLakes_same_lakes.pdf")
print(MOGWAI_GloLakesGREALM_plot)
print(MOGWAI_GloLakesICESat2_plot)
print(MOGWAI_GloLakesSentinel2_plot)
print(MOGWAI_GloLakesGREALM_r2)
print(MOGWAI_GloLakesICESat2_r2)
print(MOGWAI_GloLakesSentinel2_r2)
dev.off()