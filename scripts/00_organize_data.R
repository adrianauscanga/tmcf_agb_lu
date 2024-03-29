#---------------------------- 00. Organize data --------------------------
#
# Code for editing and organizing the datasets that will be used in the analysis 
# This script will read in raw data from the input directory, and write clean
# datasets to the output directory
#
#-------------------------------------------------------------------------#


# Load packages -----------------------------------------------------------

library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(fs)


#  ....................  ---------------------------------------------------------------------


# 1. REMOTE SENSING DATA -----------------------------------------------------


# 1.1. Import Landsat 5 data ---------------------------------------------------


# NDVI: 

# 1.1.1. NDVI --------------------------------------------------------------------


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

# 1.1.2. CI1 ---------------------------------------------------------------------


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

# 1.1.3. CI2 ---------------------------------------------------------------------


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

# EVI:

# 1.1.4. EVI ---------------------------------------------------------------------


evi_l5 <- read_csv("input/RS/evi_cfplots_all.csv", col_names = FALSE)
head(evi_l5)

evi_l5 <- evi_l5%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            evi = as.numeric(X5))

head(evi_l5)

evi_l5 <- evi_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# NDWI:

# 1.1.5. NDWI --------------------------------------------------------------------

ndwi_l5 <- read_csv("input/RS/ndwi_cfplots_all.csv", col_names = FALSE)
head(ndwi_l5)

ndwi_l5 <- ndwi_l5%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ndwi = as.numeric(X5))

head(ndwi_l5)

ndwi_l5 <- ndwi_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))


# 1.1.6. RGB Bands ---------------------------------------------------------------------

rgb_l5 <- read_csv("input/RS/rgb_cfplots_all.csv", col_names = FALSE)
head(rgb_l5)

rgb_l5 <- rgb_l5%>%
  transmute(plot_id = as.character(X8),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            b1 = as.numeric(X5),
            b2 = as.numeric(X6),
            b3 = as.numeric(X7))

head(rgb_l5)

rgb_l5 <- rgb_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))


# 1.1.7. IR Bands ----------------------------------------------------------------

ir_l5 <- read_csv("input/RS/ir_cfplots_all.csv", col_names = FALSE)
head(ir_l5)

ir_l5 <- ir_l5%>%
  transmute(plot_id = as.character(X9),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            b4 = as.numeric(X5),
            b5 = as.numeric(X6),
            b6 = as.numeric(X7),
            b7 = as.numeric(X8))

head(ir_l5)

ir_l5 <- ir_l5 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# .................... -----------------------------------------------------------------------


# 1.2. Import landsat 7 data ---------------------------------------------------

# Repeat with landsat 7:

# NDVI:

# 1.2.1. NDVI -------------------------------------------------------------


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
tail(ndvi_l7)

ndvi_l7 <- ndvi_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# CI1:

# 1.2.2. CI1 --------------------------------------------------------------


ci1_l7 <- read_csv("input/RS/ci1l7_cfplots_all.csv", col_names = FALSE)

ci1_l7 <- ci1_l7%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ci1 = as.numeric(X5))

# CI2:

# 1.2.3. CI2 --------------------------------------------------------------


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

# EVI:

# 1.2.4. EVI --------------------------------------------------------------


evi_l7 <- read_csv("input/RS/evil7_cfplots_all.csv", col_names = FALSE)
head(evi_l7)

evi_l7 <- evi_l7%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            evi = as.numeric(X5))

evi_l7 <- evi_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# NDWI:

# 1.2.5. NDWI -------------------------------------------------------------


ndwi_l7 <- read_csv("input/RS/ndwil7_cfplots_all.csv", col_names = FALSE)
head(ndwi_l7)

ndwi_l7 <- ndwi_l7%>%
  transmute(plot_id = as.character(X6),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            ndwi = as.numeric(X5))

ndwi_l7 <- ndwi_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))


# 1.2.6. RGB Bands ---------------------------------------------------------------


rgb_l7 <- read_csv("input/RS/rgb_cfplots_all_l7.csv", col_names = FALSE)
head(rgb_l7)

rgb_l7 <- rgb_l7%>%
  transmute(plot_id = as.character(X8),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            b1 = as.numeric(X5),
            b2 = as.numeric(X6),
            b3 = as.numeric(X7))

head(rgb_l7)

rgb_l7 <- rgb_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))


# 1.2.7. IR Bands ----------------------------------------------------------------

ir_l7 <- read_csv("input/RS/ir_cfplots_all_l7.csv", col_names = FALSE)
head(ir_l7)

ir_l7 <- ir_l7%>%
  transmute(plot_id = as.character(X7),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            b4 = as.numeric(X5),
            b5 = as.numeric(X6))

head(ir_l7)

ir2_l7 <- read_csv("input/RS/ir2_cfplots_all_l7.csv", col_names = FALSE)
head(ir2_l7)

ir2_l7 <- ir2_l7%>%
  transmute(plot_id = as.character(X7),
            scene = as.character(X1),
            lon = as.numeric(X2),
            lat = as.numeric(X3),
            sat_time = as.numeric(X4),
            b6 = as.numeric(X5),
            b7 = as.numeric(X6))

head(ir2_l7)

ir_l7 <- ir_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

ir2_l7 <- ir2_l7 %>%
  separate(scene, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date))

# Add time series that have been filtered with cloud mask

#  ....................  ---------------------------------------------------------------------



# 1.3. Add ts that have been filtered with cloud mask --------------------------


# Landsat 5

ndvinc_ts_files <- "input/RS/l5_noclouds/"  # file directory
ndvinc_l5_ts <- ndvinc_ts_files %>%   # make a list of all files
  dir_ls() %>%
  map(
    .f = function(path) {
      read_csv(
        path,
        col_names = c("id", "lon", "lat", "sat_time", "ndvinc", "plot_id"),
        col_types = cols(
          plot_id = col_character(),
          id = col_character(),
          lon = col_double(),
          lat = col_double(),
          sat_time = col_double(),
          ndvinc = col_double()
        )
      )
    }
  )

ndvinc_l5_ts_tbl <- ndvinc_l5_ts %>%
  set_names(dir_ls(ndvinc_ts_files)) %>%
  bind_rows(.id = "file_path")
  
dim(ndvinc_l5_ts_tbl)

ndvinc_l5_ts_tbl <- ndvinc_l5_ts_tbl %>%
  separate(id, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date)) %>%
  select(-file_path) %>%
  filter(!is.na(ndvinc)) %>%
  select(-lon, -lat)

head(ndvinc_l5_ts_tbl)

# Landsat 7

ndvincl7_ts_files <- "input/RS/l7_noclouds/"  # file directory
ndvinc_l7_ts <- ndvincl7_ts_files %>%   # make a list of all files
  dir_ls() %>%
  map(
    .f = function(path) {
      read_csv(
        path,
        col_names = c("id", "lon", "lat", "sat_time", "ndvinc", "plot_id"),
        col_types = cols(
          plot_id = col_character(),
          id = col_character(),
          lon = col_double(),
          lat = col_double(),
          sat_time = col_double(),
          ndvinc = col_double()
        )
      )
    }
  )

ndvinc_l7_ts_tbl <- ndvinc_l7_ts %>%
  set_names(dir_ls(ndvincl7_ts_files)) %>%
  bind_rows(.id = "file_path")

dim(ndvinc_l7_ts_tbl)

ndvinc_l7_ts_tbl <- ndvinc_l7_ts_tbl %>%
  separate(id, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date)) %>%
  select(-file_path) %>%
  filter(!is.na(ndvinc)) %>% # remove cloudy pixels
  select(-lon, -lat)


#  ....................  ---------------------------------------------------------------------


# 1.4. Bind datasets -----------------------------------------------------------
# Bind all datasets.
# First join ndvi, evi, ndwi, ndvinc, ci1 and ci1, then bind landsat 5 and landsat 7 data

#  ....................  ---------------------------------------------------------------------



l5 <- left_join(ndvinc_l5_ts_tbl, ndvi_l5, by = c("plot_id", "sat_time", "landsat", "row_path", "date"))
head(l5)

l5 <- l5 %>%
  left_join(ci1_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ci2_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(evi_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ndwi_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(rgb_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ir_l5, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) 

head(l5)


l7 <- left_join(ndvinc_l7_ts_tbl, ndvi_l7, by = c("plot_id", "landsat", "row_path", "date", "sat_time")) %>%
  left_join(ci1_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ci2_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(evi_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ndwi_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(rgb_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ir_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) %>%
  left_join(ir2_l7, by = c("plot_id", "landsat", "row_path", "date", "lon", "lat", "sat_time")) 

head(l7)

time_series <- bind_rows(l5, l7) %>%
  arrange(plot_id, date) %>%
  relocate(c("plot_id", "lat", "lon"), .before = landsat)

head(time_series)

time_series <- time_series %>%
  filter(date > "1992-12-31")

head(time_series)
tail(time_series)

 time_series %>%
   filter(is.na(ndvi)) %>%
   group_by(landsat) %>%
   dplyr::summarize(na_number = n())
 
# Most NA in dataset are landsat 7 (perhaps due to l7 stripes and because there's more l7 data in general)

# Get rid of duplicated dates

time_series <- time_series %>%
   distinct(plot_id, date, .keep_all= TRUE) # From 84,698 to 75,945 

# 1.5. Remove cloudy pixels ----------------------------------------------------

time_series <- time_series %>%
  filter(ci1 > 2.8) # From 75,945 to 67,017

#  ....................  ---------------------------------------------------------------------


# 1.6. Add SAVI -----------------------------------------------------------

# ((NIR - R) / (NIR + R + L)) * (1 + L)
# In Landsat 4-7, SAVI = ((Band 4 – Band 3) / (Band 4 + Band 3 + 0.5)) * (1.5)

time_series <- time_series %>%
  mutate(savi = (((b4 - b3) / (b4 + b3 + 0.5)) * (1.5))) 

head(time_series)

# 1.7. Export long dataset  ----------------------------------------------------

# as csv file and RData

save(time_series, file = "output/time_series.RData")

write.csv(time_series, file = "output/time_series.csv")

# Split dataset into individual plots and export dataset that way

time_series_plots <- split(time_series, time_series$plot_id)

time_series_plots2 <- time_series_plots[-c(1:143)]

lapply(names(time_series_plots), function(x){
  write_csv(time_series_plots[[x]], path = paste("output/ts_plots/wclouds", x, ".csv", sep = ""))
})


# .................... -------------------------------------------------------------------------


# 1.8. Training data ------------------------------------------------------

tpoints_ndvi_l7 <- read_csv("input/RS/ndvinc_tpoints.csv", col_names = T)
head(tpoints_ndvi)

tpoints_ndvi_l5 <- read_csv("input/RS/ndvi_tpoints_l5.csv", col_names = T)
head(tpoints_ndvi_l5)

tpoints_fc <- read_csv("input/RS/tpoints_fc.csv")
head(tpoints_fc)

tpoints_fc <- tpoints_fc %>%
  transmute(tp = as.character(`system:index`),
            coords = as.character(.geo)) %>%
  separate(coords,into = c("pre", "lon", "lat"), sep = ",") %>%
  select(-pre) %>%
  separate(lon, into = c("pre", "lon"), sep = ":") %>%
  select(-pre) %>%
  mutate(lon = substring(lon, first= 2),
         lat = substring(lat, first= 1, last = nchar(lat)-2)) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  mutate(tp = paste0("tp",tp))

tpoints_fc <- tpoints_fc %>%
  mutate(latitude = as.numeric(lat),
         longitude = as.numeric(lon))

head(tpoints_ndvi_l5)

tps <- tpoints_ndvi_l5 %>%
  mutate(coords = paste0(latitude,", ", longitude)) %>%
  group_by(coords) %>%
  summarize(tps_coords = unique(coords))

tps$tp <- 1:nrow(tps)

tps <- tps %>%
  mutate(tp = paste0("tp_", tp))

tpoints_ndvi_l5 <- tpoints_ndvi_l5 %>%
  mutate(tps_coords = paste0(latitude,", ", longitude))
head(tpoints_ndvi_l5)

tpoints_ndvi_l5 <- left_join(tpoints_ndvi_l5, tps, by = "tps_coords")
head(tpoints_ndvi_l5)

tps_l7 <- tpoints_ndvi_l7 %>%
  mutate(coords = paste0(latitude,", ", longitude)) %>%
  group_by(coords) %>%
  summarize(tps_coords = unique(coords))

tps_l7$tp <- 1:nrow(tps_l7)

tps_l7 <- tps_l7 %>%
  mutate(tp = paste0("tp_", tp))

tpoints_ndvi_l7 <- tpoints_ndvi_l7 %>%
  mutate(tps_coords = paste0(latitude,", ", longitude)) %>%
  left_join(tps_l7, by = "tps_coords")
head(tpoints_ndvi_l5)

tpoints_ndvi <- full_join(tpoints_ndvi_l5, tpoints_ndvi_l7)

tpoints_ndvi %>%
  group_by(tp) %>%
  summarize(tps_total = unique(tps_coords)) #great!

# Repeat with cloud index

tpoints_ci1_l7 <- read_csv("input/RS/ci1_tpoints.csv", col_names = T)

tpoints_ci1_l5 <- read_csv("input/RS/ci1_tpoints_l5.csv", col_names = T)

tps_ci1_l7 <- tpoints_ci1_l7 %>%
  mutate(coords = paste0(latitude,", ", longitude)) %>%
  group_by(coords) %>%
  summarize(tps_coords = unique(coords))

tps_ci1_l7$tp <- 1:nrow(tps_ci1_l7)

tps_ci1_l7 <- tps_ci1_l7 %>%
  mutate(tp = paste0("tp_", tp))

tpoints_ci1_l7 <- tpoints_ci1_l7 %>%
  mutate(tps_coords = paste0(latitude,", ", longitude)) %>%
  left_join(tps_ci1_l7, by = "tps_coords")
head(tpoints_ci1_l7)

tps_ci1_l5 <- tpoints_ci1_l5 %>%
  mutate(coords = paste0(latitude,", ", longitude)) %>%
  group_by(coords) %>%
  summarize(tps_coords = unique(coords))

tps_ci1_l5$tp <- 1:nrow(tps_ci1_l5)

tps_ci1_l5 <- tps_ci1_l5 %>%
  mutate(tp = paste0("tp_", tp))

tpoints_ci1_l5 <- tpoints_ci1_l5 %>%
  mutate(tps_coords = paste0(latitude,", ", longitude)) %>%
  left_join(tps_ci1_l5, by = "tps_coords")
head(tpoints_ci1_l5)

tpoints_ci1 <- full_join(tpoints_ci1_l5, tpoints_ci1_l7)

# Join datasets

tpoints_time_series <- full_join(tpoints_ndvi, tpoints_ci1)
head(tpoints_time_series)

# Filter out clouds

tpoints_time_series <- tpoints_time_series %>%
  filter(!is.na(NDVI)) %>%
  filter(CI1 > 2.8)

# Filter dates

tpoints_time_series <- tpoints_time_series %>% 
  separate(id, into = c("landsat", "row_path", "date"), sep = "_", remove = TRUE) %>%
  mutate(date = ymd(date)) %>%
  filter(date > "1992-12-31")

save(tpoints_time_series, file = "output/tps_time_series.RData")

# Split dataset into individual tps and export data that way

time_series_tps <- split(tpoints_time_series, tpoints_time_series$tp)

lapply(names(time_series_tps), function(x){
  write_csv(time_series_tps[[x]], path = paste("output/ts_tps/", x, ".csv", sep = ""))
})


#  ....................  ---------------------------------------------------------------------

# 2. FI DATA -----------------------------------------------------
# 
# load("input/FI/plots_cf4.RData")
# head(plots_cf)
# 
# load("input/FI/trees_cf4.RData")
# head(trees_cf)


a <- ndvi_l5 %>%
  filter(date > "1992-12-12") %>%
  group_by(plot_id) %>%
  summarize(length_l5 = n())

b <- ndvi_l7 %>%
  group_by(plot_id) %>%
  summarize(length_l7 = n())

c <- left_join(a, b) %>%
  mutate(length = length_l5 + length_l7)

mean(c$length)
