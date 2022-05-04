#---------------------- 01. Remote Sensing Analisis ------------------------
#
# 
#
#-------------------------------------------------------------------------#

# Load packages and files -------------------------------------------------


library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(bfast)
library(tseries)
library(fs)

# Load individual plots
# Make a loop to read all ts plots at once

ts_plots <- dir_ls(path="output/ts_plots/")

#2. Make a for loop for reading each file txt file
#First, create empty variables
table <- NULL
names <- NULL

#loop for reading all plots:
for (i in ts_plots){
  #Read data
  table <- read_csv(i, col_names = T) 
  
  assign(paste0("p",substr(i,17,23)),table) #assign name "p+plot_id" to table
  names<- c(names,paste0("p",substr(i,17,23))) #make a list of all plots
}

# Load full data set (all plots, indices and bands)

load("output/time_series.RData")

# Load FI plots

load("input/FI/plots_cf4.RData")

# 1. BFAST NDVI ----------------------------------------------------------------

# Run bfast ---------------------------------------------------------------

# Loop for running bfast

ts <- NULL
ndvi <- NULL
date <- NULL
ts_list <- NULL
ts_tsp <- NULL
ts_h <- NULL
freq <- NULL
length_ts <- NULL
bfastx <- NULL
bfastlist <- NULL
bfastoutput <- NULL
lastiter <- NULL
breaks <- NULL
breaks2 <- NULL
totalbreaks <- NULL
sumbreaks <- NULL
bfast_breaks <- NULL
nobreaks <- NULL
break_moments <- NULL
magnitudes <- NULL
mag_list <- NULL

for(i in names){
  plotx <- mget(i) #search and "call" i(which is already in the environment)
  plotx <- data.frame(plotx)
  ndvi <- plotx[,9]
  date <- plotx[,6]
  ts <- bfastts(ndvi, date, type = "irregular")
  ts <- na.remove(ts)
  ts_tsp <- attr(ts, "tsp")
  freq <- ts_tsp[3]
  length_ts <- length(ts)
  ts_h <- (freq/length_ts)*1
  bfastx <- bfast(ts, h = ts_h, season = "dummy", max.iter = 10, breaks = NULL, hpc = "none", level = 0.05, type = "OLS-MOSUM") 
  assign(paste0("bfast_",i), bfastx)
  bfastlist <- c(bfastlist, paste0("bfast_",i)) #create a list of bfast files 
  bfastoutput <- bfastx$output #get output of bfast
  lastiter <- tail(bfastoutput, n = 1) #get the last iteration 
  #breaks <- lastiter[[1]]$bp.Vt #get breakpoint info from last iteration
  breaks<- lastiter[[c(1,5)]]
  magnitudes <- bfastx[["Mags"]]
  if(is.na(breaks)) {
    nobreaks <- c(nobreaks, paste0("bfast_",i),0)
  } else {
    breaks2 <- breaks$breakpoints #get only the breakpoints
    break_moments <- c(break_moments, paste0("bfast_",i,",",breaks2))
    totalbreaks <- length(breaks2) #calculate no. of breakpoints
    bfast_breaks <- c(bfast_breaks, paste0("bfast_",i), totalbreaks)
    mag_list <- c(mag_list, paste0("bfast_",i,",",magnitudes[,3]))
  }
  
}


# Extract relevant information from bfast ---------------------------------

# No. of breaks per plot

bfast_breaks <- c(bfast_breaks, nobreaks)
breaksxplot <- matrix(bfast_breaks, ncol=2, byrow=T) #must have 300 rows

# Dates of breaks

break_moments <- matrix(break_moments, ncol=1, byrow = T)
break_moments <- as.data.frame(break_moments)
break_moments <- separate(break_moments, V1, into = c("plot_id", "ts_index"), sep = ",", remove = TRUE)

# Clean table

break_moments <- break_moments %>%
  mutate(ts_index= as.numeric(ts_index)) %>%
  separate(plot_id, into = c("bfast", "plot_id"), sep = "bfast_", remove = TRUE) %>%
  select(-bfast)

# Function and loop to extract break dates
# Make a function that selects the row number equivalent to ts_index in each plot dataset

f <- function(df, x){
  df[x,6]
}

# Make a list of plots with breaks

plotwblist <- break_moments$plot_id
plotwblist <- unique(plotwblist)

# Loop applying f function

breakdates <- NULL

for(i in 1:nrow(break_moments)){
  plotid <- break_moments[i, "plot_id"]
  index <- break_moments[i, "ts_index"]
  for(j in plotwblist){
    plotx <- mget(j)
    plotx <- as.data.frame(plotx)
    if(j == plotid){
      breakdate <- f(plotx, index)
      breakdates <- c(breakdates, paste(j, breakdate, sep = ", "))
    }
  }
}

# Great!

# Edit breakdates data and combine it with break moments


breakdates <- matrix(breakdates, ncol=1, byrow = T)
breakdates <- as.data.frame(breakdates)
breakdates <- separate(breakdates, V1, into = c("plot_id", "break_dates"), sep = ",", remove = TRUE)

# Clean table
breakdates <- breakdates %>%
  mutate(break_dates = as.Date(break_dates))

break_moments <- cbind(break_moments, breakdates$break_dates)
colnames(break_moments) <- c("plot_id", "ts_index", "break_dates")

# Extract break magnitudes:

magnitudes_m <- matrix(mag_list, ncol=1, byrow = T)
magnitudes_m <- as.data.frame(magnitudes_m)
magnitudes_m <- separate(magnitudes_m, V1, into = c("plot_id", "magnitude"), sep = ",", remove = TRUE)

# Bind datasets

plots_breaks_dummy <- cbind(break_moments, magnitudes_m)
plots_breaks_dummy <- plots_breaks_dummy[,c(-4)]

#write.csv(plots_breaks_harmonic, file = "output/plots_breaks_harmonic.csv")
write.csv(plots_breaks_dummy, file = "output/plots_breaks_dummy.csv")
save(plots_breaks_dummy, file = "output/plots_breaks_dummy.RData")

# 1. BFAST NDWI ----------------------------------------------------------------

# Run bfast ---------------------------------------------------------------

# Loop for running bfast

ts <- NULL
ndwi <- NULL
date <- NULL
ts_list <- NULL
ts_tsp <- NULL
ts_h <- NULL
freq <- NULL
length_ts <- NULL
bfastx <- NULL
bfastlist <- NULL
bfastoutput <- NULL
lastiter <- NULL
breaks <- NULL
breaks2 <- NULL
totalbreaks <- NULL
sumbreaks <- NULL
bfast_breaks <- NULL
nobreaks <- NULL
break_moments <- NULL
magnitudes <- NULL
mag_list <- NULL

for(i in names){
  plotx <- mget(i) #search and "call" i(which is already in the environment)
  plotx <- data.frame(plotx)
  ndwi <- plotx[,13]
  date <- plotx[,6]
  ts <- bfastts(ndwi, date, type = "irregular")
  ts <- na.remove(ts)
  ts_tsp <- attr(ts, "tsp")
  freq <- ts_tsp[3]
  length_ts <- length(ts)
  ts_h <- (freq/length_ts)*1
  bfastx <- bfast(ts, h = ts_h, season = "dummy", max.iter = 10, breaks = NULL, hpc = "none", level = 0.05, type = "OLS-MOSUM") 
  assign(paste0("bfast_",i), bfastx)
  bfastlist <- c(bfastlist, paste0("bfast_",i)) #create a list of bfast files 
  bfastoutput <- bfastx$output #get output of bfast
  lastiter <- tail(bfastoutput, n = 1) #get the last iteration 
  #breaks <- lastiter[[1]]$bp.Vt #get breakpoint info from last iteration
  breaks<- lastiter[[c(1,5)]]
  magnitudes <- bfastx[["Mags"]]
  if(is.na(breaks)) {
    nobreaks <- c(nobreaks, paste0("bfast_",i),0)
  } else {
    breaks2 <- breaks$breakpoints #get only the breakpoints
    break_moments <- c(break_moments, paste0("bfast_",i,",",breaks2))
    totalbreaks <- length(breaks2) #calculate no. of breakpoints
    bfast_breaks <- c(bfast_breaks, paste0("bfast_",i), totalbreaks)
    mag_list <- c(mag_list, paste0("bfast_",i,",",magnitudes[,3]))
  }
  
}


# Extract relevant information from bfast ---------------------------------

# No. of breaks per plot

bfast_breaks <- c(bfast_breaks, nobreaks)
breaksxplot <- matrix(bfast_breaks, ncol=2, byrow=T) #must have 300 rows

# Dates of breaks

break_moments <- matrix(break_moments, ncol=1, byrow = T)
break_moments <- as.data.frame(break_moments)
break_moments <- separate(break_moments, V1, into = c("plot_id", "ts_index"), sep = ",", remove = TRUE)

# Clean table

break_moments <- break_moments %>%
  mutate(ts_index= as.numeric(ts_index)) %>%
  separate(plot_id, into = c("bfast", "plot_id"), sep = "bfast_", remove = TRUE) %>%
  select(-bfast)

# Function and loop to extract break dates
# Make a function that selects the row number equivalent to ts_index in each plot dataset

f <- function(df, x){
  df[x,6]
}

# Make a list of plots with breaks

plotwblist <- break_moments$plot_id
plotwblist <- unique(plotwblist)

# Loop applying f function

breakdates <- NULL

for(i in 1:nrow(break_moments)){
  plotid <- break_moments[i, "plot_id"]
  index <- break_moments[i, "ts_index"]
  for(j in plotwblist){
    plotx <- mget(j)
    plotx <- as.data.frame(plotx)
    if(j == plotid){
      breakdate <- f(plotx, index)
      breakdates <- c(breakdates, paste(j, breakdate, sep = ", "))
    }
  }
}

# Great!

# Edit breakdates data and combine it with break moments


breakdates <- matrix(breakdates, ncol=1, byrow = T)
breakdates <- as.data.frame(breakdates)
breakdates <- separate(breakdates, V1, into = c("plot_id", "break_dates"), sep = ",", remove = TRUE)

# Clean table
breakdates <- breakdates %>%
  mutate(break_dates = as.Date(break_dates))

break_moments <- cbind(break_moments, breakdates$break_dates)
colnames(break_moments) <- c("plot_id", "ts_index", "break_dates")

# Extract break magnitudes:

magnitudes_m <- matrix(mag_list, ncol=1, byrow = T)
magnitudes_m <- as.data.frame(magnitudes_m)
magnitudes_m <- separate(magnitudes_m, V1, into = c("plot_id", "magnitude"), sep = ",", remove = TRUE)

# Bind datasets

plots_breaks_ndwi <- cbind(break_moments, magnitudes_m)
plots_breaks_ndwi <- plots_breaks_ndwi[,c(-4)]



# BFAST EVI ---------------------------------------------------------------


# Loop for running bfast

ts <- NULL
evi <- NULL
date <- NULL
ts_list <- NULL
ts_tsp <- NULL
ts_h <- NULL
freq <- NULL
length_ts <- NULL
bfastx <- NULL
bfastlist <- NULL
bfastoutput <- NULL
lastiter <- NULL
breaks <- NULL
breaks2 <- NULL
totalbreaks <- NULL
sumbreaks <- NULL
bfast_breaks <- NULL
nobreaks <- NULL
break_moments <- NULL
magnitudes <- NULL
mag_list <- NULL

for(i in names){
  plotx <- mget(i) #search and "call" i(which is already in the environment)
  plotx <- data.frame(plotx)
  evi <- plotx[,12]
  date <- plotx[,6]
  ts <- bfastts(evi, date, type = "irregular")
  ts <- na.remove(ts)
  ts_tsp <- attr(ts, "tsp")
  freq <- ts_tsp[3]
  length_ts <- length(ts)
  ts_h <- (freq/length_ts)*1
  bfastx <- bfast(ts, h = ts_h, season = "dummy", max.iter = 10, breaks = NULL, hpc = "none", level = 0.05, type = "OLS-MOSUM") 
  assign(paste0("bfast_",i), bfastx)
  bfastlist <- c(bfastlist, paste0("bfast_",i)) #create a list of bfast files 
  bfastoutput <- bfastx$output #get output of bfast
  lastiter <- tail(bfastoutput, n = 1) #get the last iteration 
  #breaks <- lastiter[[1]]$bp.Vt #get breakpoint info from last iteration
  breaks<- lastiter[[c(1,5)]]
  magnitudes <- bfastx[["Mags"]]
  if(is.na(breaks)) {
    nobreaks <- c(nobreaks, paste0("bfast_",i),0)
  } else {
    breaks2 <- breaks$breakpoints #get only the breakpoints
    break_moments <- c(break_moments, paste0("bfast_",i,",",breaks2))
    totalbreaks <- length(breaks2) #calculate no. of breakpoints
    bfast_breaks <- c(bfast_breaks, paste0("bfast_",i), totalbreaks)
    mag_list <- c(mag_list, paste0("bfast_",i,",",magnitudes[,3]))
  }
  
}


# Extract relevant information from bfast ---------------------------------

# No. of breaks per plot

bfast_breaks <- c(bfast_breaks, nobreaks)
breaksxplot <- matrix(bfast_breaks, ncol=2, byrow=T) #must have 300 rows

# Dates of breaks

break_moments <- matrix(break_moments, ncol=1, byrow = T)
break_moments <- as.data.frame(break_moments)
break_moments <- separate(break_moments, V1, into = c("plot_id", "ts_index"), sep = ",", remove = TRUE)

# Clean table

break_moments <- break_moments %>%
  mutate(ts_index= as.numeric(ts_index)) %>%
  separate(plot_id, into = c("bfast", "plot_id"), sep = "bfast_", remove = TRUE) %>%
  select(-bfast)

# Function and loop to extract break dates
# Make a function that selects the row number equivalent to ts_index in each plot dataset

f <- function(df, x){
  df[x,6]
}

# Make a list of plots with breaks

plotwblist <- break_moments$plot_id
plotwblist <- unique(plotwblist)

# Loop applying f function

breakdates <- NULL

for(i in 1:nrow(break_moments)){
  plotid <- break_moments[i, "plot_id"]
  index <- break_moments[i, "ts_index"]
  for(j in plotwblist){
    plotx <- mget(j)
    plotx <- as.data.frame(plotx)
    if(j == plotid){
      breakdate <- f(plotx, index)
      breakdates <- c(breakdates, paste(j, breakdate, sep = ", "))
    }
  }
}

# Great!

# Edit breakdates data and combine it with break moments


breakdates <- matrix(breakdates, ncol=1, byrow = T)
breakdates <- as.data.frame(breakdates)
breakdates <- separate(breakdates, V1, into = c("plot_id", "break_dates"), sep = ",", remove = TRUE)

# Clean table
breakdates <- breakdates %>%
  mutate(break_dates = as.Date(break_dates))

break_moments <- cbind(break_moments, breakdates$break_dates)
colnames(break_moments) <- c("plot_id", "ts_index", "break_dates")

# Extract break magnitudes:

magnitudes_m <- matrix(mag_list, ncol=1, byrow = T)
magnitudes_m <- as.data.frame(magnitudes_m)
magnitudes_m <- separate(magnitudes_m, V1, into = c("plot_id", "magnitude"), sep = ",", remove = TRUE)

# Bind datasets

plots_breaks_evi <- cbind(break_moments, magnitudes_m)
plots_breaks_evi <- plots_breaks_ndwi[,c(-4)]

#write.csv(plots_breaks_harmonic, file = "output/plots_breaks_harmonic.csv")
write.csv(plots_breaks_evi, file = "output/plots_breaks_evi.csv")
save(plots_breaks_evi, file = "output/plots_breaks_evi.RData")

head(plots_breaks_evi)


# BFAST comparison --------------------------------------------------------

load("output/plots_breaks_dummy.RData")
load("output/plots_breaks_ndwi.RData")
load("output/plots_breaks_evi.RData")

ndvi_breaks <- plots_breaks_dummy %>%
  group_by(plot_id) %>%
  summarise(ndvi_breaks = n())

ndwi_breaks <- plots_breaks_ndwi %>%
  group_by(plot_id) %>%
  summarise(ndwi_breaks = n())

plots_breaks_evi <- plots_breaks_evi[,-4]

evi_breaks <- plots_breaks_evi %>%
  group_by(plot_id) %>%
  summarise(evi_breaks = n())

total_breaks <- full_join(ndvi_breaks, evi_breaks, by = "plot_id") %>%
  full_join(ndwi_breaks, by = "plot_id")


# Time Series: Summary Statistics -----------------------------------------

# NDVI

time_series %>%
  mutate(min_ndvi = min(ndvi),
         max_ndvi = max(ndvi),
         mean_ndvi = mean(ndvi),
         median_ndvi = median(ndvi)) %>%
  select(plot_id, min_ndvi, max_ndvi, mean_ndvi, median_ndvi) %>%
  head()
  
# ndvi min: 0.241, max: 0.996, mean: 0.751, median: 0.767

# EVI

time_series %>%
  mutate(min_evi = min(evi),
         max_evi = max(evi),
         mean_evi = mean(evi),
         median_evi = median(evi)) %>%
  select(plot_id, min_evi, max_evi, mean_evi, median_evi) %>%
  head()

# evi min: -2710, max: 2488, mean: 2.31, median: 2.20

# NDWI

time_series %>%
  mutate(min_ndwi = min(ndwi),
         max_ndwi = max(ndwi),
         mean_ndwi = mean(ndwi),
         median_ndwi = median(ndwi)) %>%
  select(plot_id, min_ndwi, max_ndwi, mean_ndwi, median_ndwi) %>%
  head()

# ndwi min: -0.324, max: 0.734, mean: 0.294, median: 0.315


time_series %>%
  ggplot(aes(x = ndvi)) +
  geom_histogram()

time_series %>%
  filter(evi > 0 & evi < 7.5) %>%
  ggplot(aes(x = evi)) +
  geom_histogram()

time_series %>%
  ggplot(aes(x = ndwi)) +
  geom_histogram()

# should I make a relationship between ndvi/evi/ndwi and agb? It would have to be at the time of data collection and probably at site level
# time to recovery to previous mean ndvi
# min ndvi in time series
# max ndvi in time series
# mean ndvi
# interannual and intrannual variation

ggplot(p72496_4, aes(x = date, y = ndwi)) +
  geom_point() +
  geom_line ()


# VI in FI plots ----------------------------------------------------------


average_vi <- time_series %>%
  filter(date > "2008-12-31") %>%
  filter(date < "2015-01-01") %>%
  mutate(year = year(date)) %>% 
  group_by(plot_id, year) %>%
  summarize(mean_ndvi = mean(ndvi),
            sd_ndvi = sd(ndvi),
            mean_evi = mean(evi),
            sd_evi = sd(evi),
            mean_ndwi = mean(ndwi),
            sd_ndwi = sd(ndwi))

vi_plots <- average_vi %>%  group_by(plot_id) %>% summarise(plots = n())

anti_join(vi_plots, plots_cf)

average_vi <- average_vi %>%
  filter(!plot_id %in% c("67883_2", "67883_3", "67883_4", "68398_3", "68398_4", "70076_1", "70306_3", "70306_4", "72284_1", "72284_2", "72284_3"))

plots_cf_vi <- plots_cf %>%
  inner_join(average_vi, by = c("plot_id", "year"))

plots_cf_vi %>%
  pivot_longer(cols = c("mean_ndvi", "mean_evi", "mean_ndwi"), names_to = "vi", values_to = "vi_annual_mean") %>%
  ggplot(aes(x= tree_no, y = vi_annual_mean)) +
  geom_point() +
  facet_grid(vi ~ ., scales = "free")

plots_cf_vi %>%
  ggplot(aes(x = basal_area_ha, y = mean_ndvi)) +
  geom_point()
  geom_smooth(method = "lm")
  
plots_cf_vi %>%
  mutate(t_ndvi = (mean_ndvi)^2) %>%
  group_by(site) %>%
  summarize(no_plots = n(),
            ndvi_site = mean(mean_ndvi),
            evi_site = mean(mean_evi),
            ndwi_site = mean(mean_ndwi),
            t_ndvi_site = mean(t_ndvi),
            agb_site = mean(agb_plot_ha),
            tree_density_site = mean(tree_density),
            tree_height_site = mean(loreys_height),
            ba_site = mean(basal_area_ha)) %>%
  filter(no_plots > 2) %>%
  filter(evi_site > 0) %>%
  #select(t_ndvi, mean_ndvi)
  ggplot(aes(x= agb_site, y = ndvi_site)) +
  geom_point() +
  #scale_x_continuous(limits = c(0,200)) +
  geom_smooth(method = "loess") +
  scale_x_log10()



