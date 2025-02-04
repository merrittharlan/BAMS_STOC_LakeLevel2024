library(ncdf4)
library(dplyr)
library(ggplot2)
require(ggridges)
library(tidyr)
library(matrixStats)

LandsatSentinel2file <- "input/Global_Lake_Absolute_Storage_LandsatPlusSentinel2 (1984-present).nc"
LandsatGREALMfile <- "input/Global_Lake_Absolute_Storage_LandsatPlusGREALM (1984-present).nc"
LandsatICESat2file <- "input/Global_Lake_Absolute_Storage_LandsatPlusICESat2 (1984-present).nc"

nc <- nc_open(LandsatSentinel2file)
ID_LandsatPlusSentinel2 <- nc$dim$ID$vals

nc <- ncdf4::nc_open(LandsatGREALMfile)
ID_LandsatPlusGREALM <- nc$dim$ID$vals

nc <- ncdf4::nc_open(LandsatICESat2file)
ID_LandsatPlusICESat2<- nc$dim$ID$vals

unique_ids <- unique(c(ID_LandsatPlusICESat2,ID_LandsatPlusSentinel2,ID_LandsatPlusGREALM))
common_ids <- intersect(intersect(ID_LandsatPlusSentinel2,ID_LandsatPlusGREALM),ID_LandsatPlusICESat2)

common_lake_levels_df <- function(nc_file){
  nc <- nc_open(nc_file)
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
  
  common_df <- data.frame(ID=rep(ID, each = length(time)), lake_name =rep(lake_name, each = length(time)), 
                          lake_storage=as.numeric(unlist(t(lake_storage))),
                        time=rep(time, times = length(ID))) %>% 
    filter(ID %in% common_ids) %>% filter(!is.na(lake_storage))
  
  return(common_df)
}

Sentinel2_common = common_lake_levels_df(LandsatSentinel2file) %>% mutate(altimetry = "Sentinel2")
GREALM_common = common_lake_levels_df(LandsatGREALMfile) %>% mutate(altimetry = "GREALM")
ICESat2_common = common_lake_levels_df(LandsatICESat2file) %>% mutate(altimetry = "ICESat2")

common_df = rbind(Sentinel2_common, GREALM_common, ICESat2_common)


timeseries_plot = ggplot(common_df, aes(x = time, y = lake_storage, color = altimetry)) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~ID, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 10)) +
  xlab("Date") +
  ylab("Lake Storage (MCM)") +
  ggtitle("Lake Storage Volume Timeseries") + 
  guides(color=guide_legend(title="Altimetry Source"))

distribution_plot = ggplot(common_df, aes(x=lake_storage,y=altimetry, fill = altimetry)) +
  geom_density_ridges2(rel_min_height = 0.005)+
  facet_wrap(~ID) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 10)) +
  coord_cartesian(xlim = c(0,5000)) + 
  xlab("Lake Storage (MCM)") +
  scale_x_continuous(breaks = c(0, 2000, 4000)) +
  ylab("Altimetry Source") +
  ggtitle("Lake Storage Volume Distributions") + 
  guides(fill=guide_legend(title="Altimetry Source"))

#Number of observations
numobs_plot = ggplot(common_df, aes(x=altimetry, fill = altimetry)) +
  geom_bar() +
  facet_wrap(~ID, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 9),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Altimetry Source") +
  ylab("Number of Observations") +
  ggtitle("Number of Observations per Lake") + 
  guides(fill=guide_legend(title="Altimetry Source"))

#mean and sd
mean_plot = ggplot(common_df %>% 
         group_by(ID, altimetry) %>% 
         summarize(mean_lake_storage = mean(lake_storage), sd_lake_storage = sd(lake_storage)), 
       aes(x=altimetry, y = mean_lake_storage,fill = altimetry)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_lake_storage - sd_lake_storage, ymax = mean_lake_storage + sd_lake_storage), 
                position = "dodge") +
  facet_wrap(~ID, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 8),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Altimetry Source") +
  ylab("Mean Lake Storage") +
  ggtitle("Mean Lake Storage & Standard Deviation") + 
  guides(fill=guide_legend(title="Altimetry Source"))

pdf(paste0("out/Lake_Comparison.pdf"))
plot(timeseries_plot)
plot(distribution_plot)
plot(numobs_plot)
plot(mean_plot)
dev.off()

# compare lake Mohave
Mohave_2023 = common_df %>% filter(ID == 9360) %>% mutate(year = format(as.Date(time),"%Y")) %>% filter(year == 2023)
ggplot(Mohave_2023, aes(x = time, y = lake_storage, color = altimetry)) + geom_point() + geom_line() + geom_abline(slope = 0, intercept = 1973)

# find percent difference
common_df_wide = common_df %>% mutate(year = format(as.Date(time),"%Y")) %>% 
  filter(year > 2019) %>%
  group_by(ID, altimetry, year) %>% 
  summarize(median_lake_storage = median(lake_storage, na.rm = TRUE)) %>%
  pivot_wider(values_from = median_lake_storage, names_from = altimetry)

common_df_wide$row_sd = sqrt(rowVars(as.matrix(common_df_wide[,c(3:5)]), na.rm = TRUE))
common_df_wide$row_mean = rowMeans(as.matrix(common_df_wide[,c(3:5)]), na.rm = TRUE)
common_df_wide$coeff_of_var = common_df_wide$row_sd/common_df_wide$row_mean
common_df_wide$GREALM_percent_diff = (common_df_wide$GREALM - common_df_wide$row_mean)/common_df_wide$row_mean*100
common_df_wide$Sentinel2_percent_diff = (common_df_wide$Sentinel2 - common_df_wide$row_mean)/common_df_wide$row_mean*100
common_df_wide$ICESat2_percent_diff = (common_df_wide$ICESat2 - common_df_wide$row_mean)/common_df_wide$row_mean*100

summary(common_df_wide$GREALM_percent_diff)
summary(common_df_wide$Sentinel2_percent_diff)
summary(common_df_wide$ICESat2_percent_diff)
