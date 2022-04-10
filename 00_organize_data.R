#---------------------------------------------------------------------------------#
#                             00. Organize data                         
#
# Code for editing and organizing the datasets that will be used in the analysis 
# This script will read in raw data from the input directory, and write clean
# datasets to the output directory
#
#---------------------------------------------------------------------------------#

# Load packages

library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)

####################################################################################
#                       REMOTE SENSING DATA                                        #
####################################################################################

# Import plot coordinates:

plots_cf_coords <- read.csv("input/RS/plots_cf_coords.csv")

plots_cf_coords <- plots_cf_coords %>%
  mutate(plot_id = as.character(X)) %>%
  select(-X)

# Import Landsat 5 data:

# NDVI: 

ndvi_l5 <- read_csv("input/RS/ndvi_cfplots_all.csv", col_names = FALSE)
head(ndvi_l5)

ndvi_l5 <- ndvi_l5%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ndvi = as.numeric(X5))

head(ndvi_l5)

ndvi_l5 <- ndvi_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# CI1:

ci1_l5 <- read_csv("input/RS/ci1_cfplots_all.csv", col_names = FALSE)
head(ci1_l5)

ci1_l5 <- ci1_l5%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ci1 = as.numeric(X5))

head(ci1_l5)

ci1_l5 <- ci1_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# CI2:

ci2_l5 <- read_csv("input/RS/ci2_cfplots_all.csv", col_names = FALSE)
head(ci2_l5)

ci2_l5 <- ci2_l5%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ci2 = as.numeric(X5))

head(ci2_l5)

ci2_l5 <- ci2_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# Repeat with landsat 7:

# NDVI:

ndvi_l7 <- read_csv("input/RS/ndvil7_cfplots_all.csv", col_names = FALSE)
head(ndvi_l7)

ndvi_l7 <- ndvi_l7 %>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ndvi = as.numeric(X5))

head(ndvi_l7)

ndvi_l7 <- ndvi_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# CI1:

ci1_l7 <- read_csv("input/RS/ci1l7_cfplots_all.csv", col_names = FALSE)

ci1_l7 <- ci1_l7%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ci1 = as.numeric(X5))

# CI2:

ci2_l7 <- read_csv("input/RS/ci2l7_cfplots_all.csv", col_names = FALSE)
head(ci2_l7)

ci2_l7 <- ci2_l7%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ci2 = as.numeric(X5))


ci1_l7 <- ci1_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

ci2_l7 <- ci2_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# Bind all datasets.
# First join ndvi, ci1 and ci1, then bind landsat 5 and landsat 7 data

l5 <- left_join(ndvi_l5, ci1_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ci2_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time"))

l7 <- left_join(ndvi_l7, ci1_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ci2_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time"))

time_series <- bind_rows(l5, l7) %>%
  arrange(plot_id, date)

time_series %>%
  filter(is.na(ndvi)) %>%
  group_by(landsat) %>%
  dplyr::summarize(na_number = n())

# Most NA in dataset are landsat 7 (perhaps due to l7 stripes)

# Get rid of duplicated dates

time_series <- time_series %>% # 187,961
  filter(!is.na(ndvi)) %>% #157,652 rows without NAs
  group_by(plot_id, date) %>%
  dplyr::summarize(date = unique(date),
                   landsat = unique(landsat),
                   lon = mean(lon),
                   lat = mean(lat),
                   ndvi = mean(ndvi),
                   ci1 = mean(ci1),
                   ci2 = mean(ci2)) #145,771 rows
