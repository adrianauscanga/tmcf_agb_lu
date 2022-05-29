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
library(ggpubr)
library(scales)
library(leaps)
library(asbio)
library(MASS)

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
load("input/FI/sites_cf4.RData")

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

ggplot(p72496_1, aes(x = sat_time, y = ndvi)) +
  geom_line(color= "gray") +
  geom_point() +
  #geom_vline(xintercept = 1.545497e+12, color = "red") +
  geom_vline(xintercept = 1.517849e+12, color = "purple") +
  geom_vline(xintercept = 859133672899, color = "purple") +
  geom_vline(xintercept = 1.422464e+12, color = "purple") +
  geom_vline(xintercept = 1.241369e+12, color = "orange") 
  #geom_hline(yintercept = 0.7554, color = "darkgray")

mean(p72496_1$ndwi)
median(p72496_1$ndvi)

ggplot(p68397_3, aes(x = sat_time, y = ndvi)) +
  geom_line(color= "gray") +
  geom_point() 
  # geom_vline(xintercept = 1.548262e+12, color = "red") +
  # geom_vline(xintercept = 1.466701e+12, color = "blue") +
  # geom_vline(xintercept = 1.439658e+12, color = "blue") +
  # geom_vline(xintercept = 1.306342e+12, color = "blue") +
  # geom_vline(xintercept = 1.294505e+12, color = "red") +
  # geom_vline(xintercept = 1.1211e+12, color = "blue") +
  # geom_vline(xintercept = 9.93919e+11, color = "blue") +
  # geom_vline(xintercept = 9.58667e+11, color = "blue") +
  # geom_vline(xintercept = 933783944820, color = "blue") +
  # geom_vline(xintercept = 899223943383, color = "blue") +
  # geom_vline(xintercept = 868033436415, color = "blue") +
  # geom_vline(xintercept = 837014507403, color = "blue") +
  # geom_vline(xintercept = 792777541151, color = "red")



# .................... ----------------------------------------------------


# Training Points ---------------------------------------------------------

# Make a for loop for reading each file csv file

tps_dir <- dir_ls(path="output/ts_tps/")

# First, create empty variables
table <- NULL
names <- NULL

#loop for reading all plots:
for (i in tps_dir){
  #Read data
  table <- read_csv(i, col_names = T) 
  
  assign(substring(i, first= 15, last = nchar(i)-4), table) #assign name "tp_#" to table
  names<- c(names,substring(i, first= 15, last = nchar(i)-4)) #make a list of all plots
}

# Run BFAST

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
  ndvi <- plotx[,7]
  date <- plotx[,3]
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

# No. of breaks per plot

bfast_breaks <- c(bfast_breaks, nobreaks)
breaksxplot <- matrix(bfast_breaks, ncol=2, byrow=T) #must have 300 rows

# Dates of breaks

break_moments <- matrix(break_moments, ncol=1, byrow = T)
break_moments <- as.data.frame(break_moments)
break_moments <- separate(break_moments, V1, into = c("tp", "ts_index"), sep = ",", remove = TRUE)

# Clean table

break_moments <- break_moments %>%
  mutate(ts_index= as.numeric(ts_index)) %>%
  separate(tp, into = c("bfast", "tp"), sep = "bfast_", remove = TRUE) %>%
  select(-bfast)

# Function and loop to extract break dates
# Make a function that selects the row number equivalent to ts_index in each plot dataset

f <- function(df, x){
  df[x,3]
}

# Make a list of plots with breaks

tpwblist <- break_moments$tp
tpwblist <- unique(tpwblist)

# Loop applying f function

breakdates <- NULL

for(i in 1:nrow(break_moments)){
  tp <- break_moments[i, "tp"]
  index <- break_moments[i, "ts_index"]
  for(j in tpwblist){
    plotx <- mget(j)
    plotx <- as.data.frame(plotx)
    if(j == tp){
      breakdate <- f(plotx, index)
      breakdates <- c(breakdates, paste(j, breakdate, sep = ", "))
    }
  }
}

# Great!

# Edit breakdates data and combine it with break moments


breakdates <- matrix(breakdates, ncol=1, byrow = T)
breakdates <- as.data.frame(breakdates)
breakdates <- separate(breakdates, V1, into = c("tp", "break_dates"), sep = ",", remove = TRUE)

# Clean table
breakdates <- breakdates %>%
  mutate(break_dates = as.Date(break_dates))

break_moments <- cbind(break_moments, breakdates$break_dates)
colnames(break_moments) <- c("tp", "ts_index", "break_dates")

# Extract break magnitudes:

magnitudes_m <- matrix(mag_list, ncol=1, byrow = T)
magnitudes_m <- as.data.frame(magnitudes_m)
magnitudes_m <- separate(magnitudes_m, V1, into = c("tp", "magnitude"), sep = ",", remove = TRUE)

# Bind datasets

tp_breaks <- cbind(break_moments, magnitudes_m)
tp_breaks <- tp_breaks[,c(-4)]

tp_breaks %>%
  group_by(tp) %>%
  summarize(tp_list = unique(tp))

save(tp_breaks, file = "output/tp_breaks.RData")

# .................... --------------------------------------------------------------


# Breaks validation -------------------------------------------------------

# After validating breaks with training points and actual plots, I found a threshold of [0.15] (absolute value)
# Note that gradual 'clearings' are not picked up by bfast, example tp_8 in 1995-05-05, that's why 
# it's important to to include positive breaks

load("output/plots_breaks_dummy.RData")

ts_breaks <- plots_breaks_dummy %>%
  mutate(magnitude = as.numeric(magnitude)) %>%
  filter(magnitude < -0.15 | magnitude > 0.15)

# Compute number of breaks and extract date of last break per plot

dc_year <- plots_cf %>%  # Year of data collection
  select(plot_id, year) %>%
  transmute(plot_id = as.character(plot_id),
            dc_year = as.numeric(year))

ts_breaks_plot <- ts_breaks %>%
  arrange(break_dates) %>%
  mutate(year = year(break_dates)) %>%
  mutate(id = as.character(plot_id)) %>%
  select(-plot_id) %>%
  separate(id, into = c("p", "plot_id"), sep = "p") %>%
  select(-p) %>%
  left_join(dc_year, by = "plot_id") %>%
  filter(year < dc_year+1) %>%
  mutate(n_break = ifelse(magnitude < 0, 1, 0),
         p_break = ifelse(magnitude > 0, 1, 0)) %>% 
  group_by(plot_id) %>%
  summarize(number_breaks = n(),
            number_n_breaks = sum(n_break),
            number_p_breaks = sum(p_break),
            last_break = last(break_dates),
            magnitude = last(magnitude))

# 76 plots have breaks, min number of breaks is 1, max is 9, mean value 2.066


# Join dataset with plots_cf

plots_cf_breaks <- ts_breaks_plot %>%
  right_join(plots_cf, by = "plot_id")

plots_cf_breaks <- plots_cf_breaks %>%
  mutate(break_year = year(last_break)) %>%
  mutate(age = year - break_year) %>%
  mutate(number_breaks = ifelse(is.na(number_breaks), 0, number_breaks))

# Filter out pixels acquired after data collection

time_series_dc <- time_series %>%
  mutate(year = year(date)) %>%
  mutate(year = as.numeric(year)) %>%
  left_join(dc_year, by = "plot_id") %>%
  group_by(plot_id) %>%
  filter(year < dc_year+1)


# VI data -----------------------------------------------------------------

# Max VI when data collection (vi_max_dc)
# Min VI when data collection (vi_min_dc)
# Average annual VI when data collection (and sd)

# Max VI through time (vi_max_ts)
# Min VI through time (vi_min_ts)
# Average VI through time

# Average VI before last break
# Average VI after break
# VI before break
# VI after break
# Time to reach VI level before break


# should I make a relationship between ndvi/evi/ndwi and agb? It would have to be at the time of data collection and probably at site level

# interannual and intrannual variation

# VI when data collection:

time_series_dc %>%
  filter(evi < 10 & evi > 0) %>%
  ggplot(aes(y = ndvi, x = evi)) +
  geom_point()

vi_dc <- time_series %>%
  filter(date > "2008-12-31") %>%
  filter(date < "2015-01-01") %>%
  mutate(year = year(date)) %>% 
  group_by(plot_id, year) %>%
  summarize(ndvi_mean_dc = mean(ndvi),
            ndvi_median_dc = median(ndvi),
            ndvi_sd_dc = sd(ndvi),
            ndvi_max_dc = max(ndvi),
            ndvi_min_dc = min(ndvi),
            evi_mean_dc = mean(evi),
            evi_median_dc = median(evi),
            evi_sd_dc = sd(evi),
            evi_max_dc = max(evi),
            evi_min_dc = min(evi),
            ndwi_mean_dc = mean(ndwi),
            ndwi_median_dc = median(ndwi),
            ndwi_sd_dc = sd(ndwi),
            ndwi_max_dc = max(ndwi),
            ndwi_min_dc = min(ndwi))
            
# Not all plots are in both time series and plots_cf data sets
vi_plots <- vi_dc %>%  group_by(plot_id) %>% summarise(plots = n())

# These plots are in the time series data set but not in plots_cf (meaning we don't have structure info for them)
anti_join(vi_plots, plots_cf)

# Remove plots that are not in plots_cf:
vi_dc <- vi_dc %>%
  filter(!plot_id %in% c("67883_2", "67883_3", "67883_4", "68398_3", "68398_4", "70076_1", "70306_3", "70306_4", "72284_1", "72284_2", "72284_3"))

# Join data sets:

plots_cf_vi <- plots_cf %>%
  inner_join(vi_dc, by = c("plot_id", "year"))

# VI through time:

vi_ts <- time_series_dc %>%
  group_by(plot_id) %>%
  summarize(ndvi_mean_ts = mean(ndvi),
            ndvi_median_ts = median(ndvi),
            ndvi_sd_ts = sd(ndvi),
            ndvi_max_ts = max(ndvi),
            ndvi_min_ts = min(ndvi),
            ndvi_cv_ts = sd(ndvi)/mean(ndvi) *100,
            evi_mean_ts = mean(evi),
            evi_median_ts = median(evi),
            evi_sd_ts = sd(evi),
            evi_max_ts = max(evi),
            evi_min_ts = min(evi),
            evi_cv_ts = sd(evi)/mean(evi) *100,
            ndwi_mean_ts = mean(ndwi),
            ndwi_median_ts = median(ndwi),
            ndwi_sd_ts = sd(ndwi),
            ndwi_max_ts = max(ndwi),
            ndwi_min_ts = min(ndwi),
            ndwi_cv_ts = sd(ndwi)/mean(ndwi) *100,
            # savi_mean_ts = mean(savi),
            # savi_median_ts = median(savi),
            # savi_sd_ts = sd(savi),
            # savi_max_ts = max(savi),
            # savi_min_ts = min(savi),
            # savi_cv_ts = sd(savi)/mean(savi) *100)
  )

vi_annual_ts <- time_series_dc %>%
  group_by(plot_id, year) %>%
  summarize(ndvi_annual_min = min(ndvi),
            evi_annual_min = min(evi),
            ndwi_annual_min = min(ndwi),
            #savi_annual_min = min(savi),
            ndvi_annual_max = max(ndvi),
            evi_annual_max = max(evi),
            ndwi_annual_max = max(ndwi),
            #savi_annual_max = max(savi),
            ndvi_annual_sd = sd(ndvi),
            evi_annual_sd = sd(evi),
            ndwi_annual_sd = sd(ndwi),
            #savi_annual_sd = sd(savi)) %>%
  ) %>%
  group_by(plot_id) %>%
  summarize(ndvi_annual_min = mean(na.omit(ndvi_annual_min)),
            evi_annual_min = mean(na.omit(evi_annual_min)),
            ndwi_annual_min = mean(na.omit(ndwi_annual_min)),
            #savi_annual_min = mean(na.omit(savi_annual_min)),
            ndvi_annual_max = mean(na.omit(ndvi_annual_max)),
            evi_annual_max = mean(na.omit(evi_annual_max)),
            ndwi_annual_max = mean(na.omit(ndwi_annual_max)),
            #savi_annual_max = mean(na.omit(savi_annual_max)),
            ndvi_annual_sd = mean(na.omit(ndvi_annual_sd)),
            evi_annual_sd = mean(na.omit(evi_annual_sd)),
            ndwi_annual_sd = mean(na.omit(ndwi_annual_sd)),
            #savi_annual_sd = mean(na.omit(savi_annual_sd))) 
  )

# Remove plots that are not in plots_cf:
vi_ts <- vi_ts %>%
  filter(!plot_id %in% c("67883_2", "67883_3", "67883_4", "68398_3", "68398_4", "70076_1", "70306_3", "70306_4", "72284_1", "72284_2", "72284_3"))

# Join data sets:

plots_cf_vi <- plots_cf_vi %>%
  inner_join(vi_ts, by = "plot_id") %>%
  inner_join(vi_annual_ts, by = "plot_id")

# Join breaks and VI data:

plots_cf_rs <- inner_join(plots_cf_breaks, plots_cf_vi) #plots_cf with remote sensing data

plots_cf_rs <- plots_cf_rs %>%
  mutate(age = ifelse(is.na(age), year-1993, age))
  
save(plots_cf_rs, file = "output/plots_cf_rs.RData")

# Calculate SAVI *CHECK
# Calculate average of the lowest and highest vi value per year *CHECK
# PCA with rs values and AGB and structure?

plots_cf_rs %>%
  mutate(age_fixed = ifelse(number_breaks == 0, 20, age)) %>%
  mutate(no_breaks_class = ifelse(number_breaks == 0, "0",
                                  ifelse(number_breaks == 1, "1",
                                         ifelse(number_breaks == 2, "2", ">2")))) %>%
  ggplot(aes(y = agb_plot, x = age_fixed)) +
  geom_jitter(aes(color= as.factor(no_breaks_class))) +
  geom_smooth(method = "lm", span = 1) +
  scale_y_log10() +
  scale_color_brewer(type = "qual")

plots_cf_rs %>%
  mutate(age_fixed = ifelse(number_breaks == 0, 20, age)) %>%
  mutate(no_breaks_class = ifelse(number_breaks == 0, "a",
                                  ifelse(number_breaks == 1, "b",
                                         ifelse(number_breaks == 2, "c", "d")))) %>%
  ggplot(aes(y = agb_plot_ha, x = no_breaks_class)) +
  geom_boxplot() +
  stat_compare_means(comparisons = list(c("a", "b"), c("a", "c"), c("a", "d"), c("b", "c"), c("b", "d"), c("c", "d")),
                     label = "p.signif")


plots_cf_rs %>%
  mutate(no_breaks_class = ifelse(number_breaks == 0, "none",
                                  ifelse(number_breaks == 1, "one", "several"))) %>%
  ggplot(aes(y = agb_plot_ha, x = age)) +
  geom_point(aes(color = no_breaks_class), size = 2) +
  scale_y_log10() +
  #scale_color_gradient(low = "forestgreen", high =  "lightgoldenrod2")
  scale_color_brewer(type = "qual") +
  geom_smooth(method = "lm", color = "black", alpha = 0.3)

weirdplots <- plots_cf_rs %>%
  filter(age > 15,
         agb_plot_ha < 100,
         str_class_plots == 1,
         number_breaks == 0)


# Comparison of RS data ---------------------------------------------------

rs <- plots_cf_rs[,c(1:2,27,28,30,34,42:84)]

rs_corr <- rs[,-1] %>%
  scale() %>%
  na.omit() %>%
  cor()

rs_corr_cov <- rs_corr %>%
  as_tibble(rownames = "cov1") %>%
  pivot_longer(c(-cov1), names_to = "cov2", values_to = "value") %>%
  mutate(abs_value = abs(value)) %>%
  filter(abs_value > 0.3) %>% #play around with this value to get more (or less) variables
  filter(!abs_value == 1) %>%
  filter(cov1 == "tree_density" |
           cov1 == "loreys_height" |
           cov1 == "basal_area_ha" |
           cov1 == "agb_plot_ha") %>%
  filter(!cov2 == "tree_density" &
          !cov2 == "loreys_height" &
          !cov2 == "basal_area_ha" &
          !cov2 == "agb_plot_ha") %>%
  filter(!grepl('savi', cov2)) %>%
  pull(cov2) %>%
  unique()

rs_corr_matrix <- rs_corr %>%
  as_tibble(rownames = "cov1") %>%
  pivot_longer(c(-cov1), names_to = "cov2", values_to = "value") %>%
  mutate(abs_value = abs(value)) %>%
  filter(abs_value > 0.3) %>% #play around with this value to get more (or less) variables
  filter(!abs_value == 1) %>%
  filter(cov1 == "tree_density" |
           cov1 == "loreys_height" |
           cov1 == "basal_area_ha" |
           cov1 == "agb_plot_ha") %>%
  filter(!cov2 == "tree_density" &
           !cov2 == "loreys_height" &
           !cov2 == "basal_area_ha" &
           !cov2 == "agb_plot_ha") %>%
  filter(!grepl('savi', cov2))


rs_short <- rs %>%
  select(plot_id,
         tree_density,
         loreys_height,
         basal_area_ha,
         agb_plot_ha,
         number_breaks,
         age,
         ndvi_annual_sd,
         ndvi_sd_ts,
         ndvi_min_ts,
         ndvi_cv_ts,
         ndwi_sd_ts,
         ndwi_min_ts,
         ndwi_annual_min,
         ndwi_annual_sd)

rs_short_cor <- cor(drop_na(rs_short[,-1]))
library(corrplot)
corrplot(rs_short_cor, method = 'ellipse')
corrplot(rs_short_cor, method = 'number')  
plot(rs_short$ndvi_annual_sd, rs_short$tree_density)

# Reducing even more the dataset: 

rs_short %>%
  # filter(age > 15,
  #        number_breaks == 0,
  #        agb_plot_ha < 100) %>%
  ggplot(aes(y= agb_plot_ha, x= age)) +
  geom_point() +
  geom_smooth(method= "lm")

biplot(rda(drop_na(rs_short[,-c(1:5)]), scale = T), display = 'species')

# ndvi_sd_ts and ndvi_cv_ts are VERY similar but the former has slightly 
# higher correlations with forest structure
# ndvi and ndwi behave almost he same except for ndwi_annual_min, which I'm keeping
# I'll drop all other ndwi values and keep ndvi ones:
# ndvi_annual_sd: average of every year's sd
# ndvi_sd_ts: sd of all values throughout the entire ts
# ndvi_min_ts: min ndvi found in ts
# ndwi_annual_min: average of every year's min value

# Shorter data set:

rs_shorter <- rs_short %>%
  select(plot_id,
         tree_density,
         loreys_height,
         basal_area_ha,
         agb_plot_ha,
         number_breaks,
         age,
         ndvi_annual_sd,
         ndvi_sd_ts,
         ndvi_min_ts,
         ndwi_annual_min)

rs_shorter <- rs_shorter %>%
  mutate(is_unexpected = ifelse(age > 15 &
                                  number_breaks == 0 &
                                  agb_plot_ha < 100, "unexpected", "normal")) 

rs_shorter %>%
  mutate(normal_b = ifelse(number_breaks > 0 & is_unexpected == "normal",
                           "n_b",
                           ifelse(number_breaks == 0 & is_unexpected == "normal",
                                  "n_nb", "un_nb"))) %>%
  ggplot(aes(x = normal_b, y = ndvi_sd_ts)) +
  geom_boxplot() +
  stat_compare_means(method = "anova") +
  geom_jitter(aes(color = log1p(agb_plot_ha)))
  

# By site:

rs_sites <- rs_shorter %>%
  separate(plot_id, into = c("site", "plot"), sep = "_") %>%
  group_by(site) %>%
  summarize(no_plots = n(),
            tree_density = mean(tree_density),
            loreys_height = mean(loreys_height),
            basal_area = mean(basal_area_ha),
            agb = mean(agb_plot_ha),
            total_breaks = sum(number_breaks),
            mean_breaks = mean(number_breaks),
            min_age = min(age),
            mean_age = mean(age),
            ndvi_annual_sd = mean(ndvi_annual_sd),
            ndvi_sd_ts = mean(ndvi_sd_ts),
            ndvi_min_ts = mean(ndvi_min_ts),
            ndwi_annual_min = mean(ndwi_annual_min)) %>%
  mutate(is_unexpected = ifelse(mean_age > 15 &
                                  mean_breaks == 0 &
                                  agb < 100, "unexpected", "normal"))


rs_sites %>%
  mutate(normal_b = ifelse(mean_breaks > 0 & is_unexpected == "normal",
                           "n_b",
                           ifelse(mean_breaks == 0 & is_unexpected == "normal",
                                  "n_nb", "un_nb"))) %>%
  filter(no_plots > 3) %>%
  ggplot(aes(x = mean_age, y = basal_area)) +
  geom_point(aes(color= mean_breaks)) +
  geom_smooth(method = "loess", se = F)

rs_sites_4p <- rs_sites %>%
  filter(no_plots > 3)

summary(lm(rs_sites_4p$ndwi_annual_min ~ rs_sites_4p$loreys_height))

rs_sites_4p <- sites_cf %>%
  select(altitude, slope_gee) %>%
  right_join(rs_sites_4p)

save(rs_sites_4p, file = "output/rs_sites_4p.RData")


# Multiple Linear Regression Models ---------------------------------------


# Models

library(leaps)

# LM basal area

ms <- regsubsets(basal_area ~ altitude + slope_gee + mean_age + min_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts + ndwi_annual_min, data = rs_sites_4p, nvmax = 9)
summary(ms)

ms_sum <- summary(ms)
# Best model:
data.frame(
  Adj.R2 = (ms_sum$adjr2),
  CP = (ms_sum$cp),
  BIC = (ms_sum$bic)
)
data.frame(
  Adj.R2 = which.max(ms_sum$adjr2),
  CP = which.min(ms_sum$cp),
  BIC = which.min(ms_sum$bic)
)

ms_sum

lm_ba <- lm(basal_area ~ altitude + slope_gee + ndvi_annual_sd + ndvi_sd_ts + ndwi_annual_min, data = rs_sites_4p)
summary(lm_ba)

plot(lm_ba)
partial.resid.plot(lm_ba)
bc<- boxcox(lm_ba)
(lambda <- bc$x[which.max(bc$y)])
# log(y) if lambda = 0
# y^lambda-1)/lambda if lambda != 0


lm_ba_t <- lm(((basal_area^lambda-1)/lambda) ~ altitude + slope_gee + ndvi_annual_sd + ndvi_sd_ts + ndwi_annual_min, data = rs_sites_4p)
summary(lm_ba_t)
plot(lm_ba_t)

# This is the good one:
lm_ba_t2 <- lm((basal_area^(1/3)) ~ altitude + slope_gee + ndvi_annual_sd + ndvi_sd_ts + ndwi_annual_min, data = rs_sites_4p)
summary(lm_ba_t2)
plot(lm_ba_t2)

#Q-Q plot for original model
qqnorm(lm_ba$residuals)
qqline(lm_ba$residuals)

#Q-Q plot for Box-Cox transformed model
qqnorm(lm_ba_t2$residuals)
qqline(lm_ba_t2$residuals)

hist(((rs_sites_4p$basal_area)^lambda-1)/lambda)
hist((rs_sites_4p$basal_area)^(1/3), breaks = 8)
hist(rs_sites_4p$basal_area)
hist(sqrt(rs_sites_4p$basal_area))
hist(lm_ba$residuals)
hist((lm_ba$residuals)^(1/3))
hist(sqrt(lm_ba$residuals))

shapiro.test(lm_ba$residuals) #W = 0.96834, p-value = 0.3334     
shapiro.test(((rs_sites_4p$basal_area)^lambda-1)/lambda)
shapiro.test((rs_sites_4p$basal_area)^(1/3)) # Not perfect but better
shapiro.test(rs_sites_4p$basal_area)

# LM tree height

ms <- regsubsets(loreys_height ~ altitude + slope_gee + min_age + mean_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts + ndwi_annual_min, data = rs_sites_4p, nvmax = 9)
summary(ms)

ms_sum <- summary(ms)

# Best model:
data.frame(
  Adj.R2 = (ms_sum$adjr2),
  CP = (ms_sum$cp),
  BIC = (ms_sum$bic)
)
data.frame(
  Adj.R2 = which.max(ms_sum$adjr2),
  CP = which.min(ms_sum$cp),
  BIC = which.min(ms_sum$bic)
)

ms_sum

lm_lh <- lm(loreys_height ~ altitude + slope_gee + ndvi_sd_ts, data = rs_sites_4p)
summary(lm_lh)
plot(lm_lh)

bc<- boxcox(lm_lh)
(lambda <- bc$x[which.max(bc$y)])
# log transform could work

hist(rs_sites_4p$loreys_height)
hist(log(rs_sites_4p$loreys_height))
hist(lm_lh$residuals)
shapiro.test(lm_lh$residuals)
shapiro.test(rs_sites_4p$loreys_height)
shapiro.test(log(rs_sites_4p$loreys_height))

lm_lh_t <- lm(log(loreys_height) ~ altitude + slope_gee + mean_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts, data = rs_sites_4p)
summary(lm_lh_t)
plot(lm_lh_t)

lm_lh2 <- lm(loreys_height ~ altitude + slope_gee + min_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts, data = rs_sites_4p)
summary(lm_lh2)
plot(lm_lh2)

## WINNER!!
lm_lh3 <- lm(loreys_height ~ altitude + ndvi_sd_ts, data = rs_sites_4p)
summary(lm_lh3)
plot(lm_lh3)

#Q-Q plot for original model
qqnorm(lm_lh$residuals)
qqline(lm_lh$residuals)

#Q-Q plot for Box-Cox transformed model
qqnorm(lm_lh_t$residuals)
qqline(lm_lh_t$residuals)

# LM tree density

ms <- regsubsets(tree_density ~ altitude + slope_gee + min_age + mean_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts + ndwi_annual_min, data = rs_sites_4p, nvmax = 9)
summary(ms)

ms_sum <- summary(ms)

# Best model:
data.frame(
  Adj.R2 = (ms_sum$adjr2),
  CP = (ms_sum$cp),
  BIC = (ms_sum$bic)
)
data.frame(
  Adj.R2 = which.max(ms_sum$adjr2),
  CP = which.min(ms_sum$cp),
  BIC = which.min(ms_sum$bic)
)

ms_sum

lm_td <- lm(tree_density ~ altitude + ndwi_annual_min, data = rs_sites_4p)
summary(lm_td)
plot(lm_td)


bc<- boxcox(lm_td$residuals)
(lambda <- bc$x[which.max(bc$y)])

hist(rs_sites_4p$tree_density)
hist(log(rs_sites_4p$tree_density))
hist(sqrt(rs_sites_4p$tree_density))
hist((rs_sites_4p$tree_density)^(1/3))

hist(lm_td$residuals)
shapiro.test(lm_td$residuals) #W = 0.92631, p-value = 0.01371

shapiro.test(rs_sites_4p$tree_density)
shapiro.test(log(rs_sites_4p$tree_density))
shapiro.test(sqrt(rs_sites_4p$tree_density)) # This is the good one
shapiro.test((rs_sites_4p$tree_density)^(1/3))

lm_td_t <- lm(sqrt(tree_density) ~ altitude + ndwi_annual_min, data = rs_sites_4p)
summary(lm_td_t)
plot(lm_td_t)

lm_td_t2 <- lm(sqrt(tree_density) ~ altitude + mean_age + mean_breaks + ndvi_sd_ts + ndwi_annual_min, data = rs_sites_4p)
summary(lm_td_t2)
plot(lm_td_t2)

# LM AGB

ms <- regsubsets(log1p(agb) ~ altitude + slope_gee + min_age + mean_age + mean_breaks + ndvi_annual_sd + ndvi_sd_ts + ndvi_min_ts + ndwi_annual_min, data = rs_sites_4p, nvmax = 9)
summary(ms)

ms_sum <- summary(ms)

# Best model:
data.frame(
  Adj.R2 = (ms_sum$adjr2),
  CP = (ms_sum$cp),
  BIC = (ms_sum$bic)
)
data.frame(
  Adj.R2 = which.max(ms_sum$adjr2),
  CP = which.min(ms_sum$cp),
  BIC = which.min(ms_sum$bic)
)

ms_sum

lm_agb <- lm(agb ~ altitude + slope_gee + ndvi_sd_ts, data = rs_sites_4p)
summary(lm_agb)
plot(lm_agb)

bc<- boxcox(lm_agb)
(lambda <- bc$x[which.max(bc$y)])

hist(rs_sites_4p$agb)
hist(log1p(rs_sites_4p$agb))
hist(sqrt(rs_sites_4p$agb))
hist((rs_sites_4p$agb)^(1/3))
hist(lm_agb$residuals)
shapiro.test(lm_agb$residuals)
shapiro.test(rs_sites_4p$agb)
shapiro.test(log1p(rs_sites_4p$agb)) #the least worse but not normal
shapiro.test(sqrt(rs_sites_4p$agb))
shapiro.test((rs_sites_4p$agb)^(1/3))

lm_agb_t <- lm(log1p(agb) ~ altitude + slope_gee +  ndvi_sd_ts, data = rs_sites_4p)
summary(lm_agb_t)
plot(lm_agb_t)

hist(lm_agb_t$residuals)
shapiro.test(lm_agb_t$residuals)
# -> do I need to transform data (to normal distribution)?

# 1. Compare 'historical' VI trends vs VI when data collection
# 2. Compare model with and without (only climate) VI trends
# 3. Linear mixed effects model with and without breaks?

# Individual plot visualization -------------------------------------------

p66139_2 <- read_csv("output/ts_plots/66139_2.csv")
View(p66139_2)

# looks disturbed and feels like it has too many trees, 
# the other plot in this site has way less AGB (but no breaks!)

p66139_2 %>%
  ggplot(aes(x= sat_time)) +
  geom_point(aes(y = ndvi), color = "darkgreen") +
  geom_line(aes(y = ndvi), color = "darkgreen") +
  geom_point(aes(y = ndwi), color = "blue") +
  geom_line(aes(y = ndwi), color = "blue") +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept = 892311875560, color = "red") +
  geom_vline(xintercept = 976638420716, color = "red") +
  geom_vline(xintercept = 1.555779e+12, color = "red") +
  geom_vline(xintercept = 1.33183e+12, color = "yellow")

p68397_1 <- read_csv("output/ts_plots/68397_1.csv")
View(p68397_1)

p68397_1 %>%
  ggplot(aes(x= sat_time, y = ndvi)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,1))

# Analysis in sites may be better given these variances



# Sites -------------------------------------------------------------------


# Summarize break info by site:

sites_breaks <- plots_cf_rs %>%
  group_by(site) %>%
  summarize(total_breaks = sum(number_breaks),
            is_break = ifelse(total_breaks == 0, 'no break', 'break'),
            mean_breaks = mean(number_breaks),
            mean_age = mean(age),
            ndvi_mean_dc = mean(ndvi_mean_dc),
            ndvi_sd_dc = mean(ndvi_sd_dc),
            evi_mean_dc = mean(evi_mean_dc),
            evi_sd_dc = mean(evi_sd_dc),
            ndwi_mean_dc = mean(ndwi_mean_dc),
            ndwi_sd_dc = mean(ndwi_sd_dc))

# Summarize VI data by site:

sites_vi <- time_series_dc %>%
  separate(plot_id, into = c("site", "plot"), sep = "_") %>%
  group_by(site) %>%
  summarize(ndvi_mean_ts = mean(ndvi),
            ndvi_median_ts = median(ndvi),
            ndvi_sd_ts = sd(ndvi),
            ndvi_max_ts = max(ndvi),
            ndvi_min_ts = min(ndvi),
            ndvi_cv_ts = sd(ndvi)/mean(ndvi) *100,
            evi_mean_ts = mean(evi),
            evi_median_ts = median(evi),
            evi_sd_ts = sd(evi),
            evi_max_ts = max(evi),
            evi_min_ts = min(evi),
            evi_cv_ts = sd(evi)/mean(ndvi) *100,
            ndwi_mean_ts = mean(ndwi),
            ndwi_median_ts = median(ndwi),
            ndwi_sd_ts = sd(ndwi),
            ndwi_max_ts = max(ndwi),
            ndwi_min_ts = min(ndwi),
            ndwi_cv_ts = sd(ndwi)/mean(ndvi) *100)

sites_cf_rs <- inner_join(sites_cf, sites_breaks) %>%
  inner_join(sites_vi)

# Explore relationships with sd, why are so many pixels with no breaks but very few trees?

sites_cf_rs %>%
  #filter(av_agb_site < 500) %>%
  filter(plot_no >2) %>%
  #filter(is_break == "break") %>%
  #select(landscape, total_breaks, mean_breaks, mean_age, is_break, av_agb_site, av_treeheight_site, av_treedensity_site, av_basalarea_site, ndvi_mean_ts, ndvi_cf_ts, ndwi_mean_ts, ndwi_cf_ts, evi_mean_ts, evi_cf_ts) %>%
  mutate(age_class = ifelse(mean_age < 15, "<15", ">15")) %>%
  mutate(breaks_class = ifelse(mean_breaks == 0, "0",
                               ifelse(mean_breaks > 0 & mean_breaks < 1.1, "1", "2"))) %>%
  ggplot(aes(y = av_agb_site, x = age_class))+
  geom_boxplot() +
  geom_point(aes(color = is_break))

# aov_data <- sites_cf_rs %>%
#   filter(plot_no >2) %>%
#   mutate(agb_class = ifelse(av_agb_site < 100, "low", "high")) %>%
#   mutate(age_class = ifelse(mean_age < 15, "<15", ">15")) %>%
#   mutate(weird = ifelse(av_agb_site < 100 & mean_age > 15 & is_break == "no break", "weird", "normal"))
# 
# 
# summary(aov(ndvi_cv_ts ~ weird * is_break, data = aov_data))
# 
# sites_cf_rs %>%
#   filter(plot_no >2) %>%
#   mutate(agb_class = ifelse(av_agb_site < 100, "low", "high")) %>%
#   mutate(age_class = ifelse(mean_age < 15, "<15", ">15")) %>%
#   mutate(weird = ifelse(av_agb_site < 100 & mean_age > 15 & is_break == "no break", "weird", "normal")) %>%
#   group_by(age_class, is_break, weird) %>%
#   summarize(counts = n(),
#             vi = mean(ndvi_mean_ts)) # ANOVAS ?

summary(lm(sites_cf_rs$av_agb_site ~ sites_cf_rs$ndvi_mean_ts))

sites_cf_rs %>%
  mutate(age_fixed = ifelse(is_break == "no break", 20, mean_age)) %>%
  ggplot(aes(y = av_agb_site, x = age_fixed)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm")

sites_cf_rs %>%
  mutate(age_fixed = ifelse(is_break == "no break", 20, mean_age)) %>%
  ggplot(aes(y = av_agb_site, x = age_fixed)) +
  geom_point(aes(color= mean_breaks)) +
  scale_y_log10() +
  #scale_x_log10() +
  geom_smooth(method = "lm")


# .................... ----------------------------------------------------


# Summary Stats -----------------------------------------------------------

# Time series

time_series_dc %>%
  group_by(plot_id) %>%
  summarize(ts_length = n()) %>%
  mutate(mean_ts_l = mean(ts_length),
         sd = sd(ts_length),
         max = max(ts_length),
         min = min(ts_length))

time_series_dc %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month > 4 & month < 11,
                         "rainy", "dry")) %>%
  group_by(year, season) %>%
  summarize(obs= n()) %>%
  pivot_wider(names_from= season, values_from = obs) %>%
  filter(year > 2002)

time_series_dc %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month > 4 & month < 11,
                         "rainy", "dry")) %>%
  group_by(year, season, plot_id) %>%
  summarize(obs= n()) %>%
  group_by(year, season) %>%
  summarize(m = mean(obs),
            sd = sd(obs)) %>%
  pivot_wider(names_from= season, values_from = c("m", "sd")) %>%
  filter(year > 2002)

time_series_dc %>%
  group_by(plot_id) %>%
  summarize(ts_length = n(),
            ndvi_min = min(ndvi),
            ndvi_mean = mean(ndvi),
            ndvi_max = max(ndvi),
            ndwi_min = min(ndwi),
            ndwi_mean = mean(ndwi),
            ndwi_max = max(ndwi)) %>%
  mutate(mean_ts_l = mean(ts_length),
       max = max(ts_length),
       min = min(ts_length),
       ndvi_min = min(ndvi_min),
       ndvi_mean = mean(ndvi_mean),
       ndvi_max = max(ndvi_max),
       ndwi_min = min(ndwi_min),
       ndwi_mean = mean(ndwi_mean),
       ndwi_max = max(ndwi_max)) %>%
  select(mean_ts_l, max, min,ndvi_min, ndvi_mean, ndvi_max, ndwi_min, ndwi_mean, ndwi_max)

 
# Plots

plots_cf_rs %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  group_by(is_break) %>%
  summarize(counts = n(),
            min = min(ndwi_annual_min),
            max = max(ndwi_annual_min),
            mean = mean(ndwi_annual_min),
            sd = sd(ndwi_annual_min))

plots_cf_rs %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  select(plot_id, is_break) %>%
  left_join(time_series_dc) %>%
  group_by(plot_id, is_break, year) %>%
  summarize(l = n()) %>%
  group_by(is_break) %>%
  summarize(count = n(),
            min = min(l),
            mean = mean(l),
            sd = sd(l),
            max = max(l))



plots_cf_rs %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  mutate(is_unexpected = ifelse(age > 15 &
                                number_breaks == 0 &
                                agb_plot_ha < 100, "unexpected", "normal")) %>%
  filter(is_unexpected == "unexpected") %>%
  ggplot(aes(x = agb_plot_ha, y = ndvi_annual_sd)) +
  geom_point() +
  geom_smooth(method = "lm")


# Sites

sd(rs_sites_4p$ndwi_annual_min)
  

# .................... ----------------------------------------------------


# T-tests -----------------------------------------------------------------

library(rstatix)

plots_cf_rs %>%
  mutate(log_agb = log1p(agb_plot_ha)) %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  #group_by(is_break) %>%
  t_test(loreys_height ~ is_break)
  

# Linear regressions TSLD -------------------------------------------------

b <- plots_cf_rs %>%
  mutate(log_agb = log1p(agb_plot_ha)) %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  dplyr::filter(is_break == "break")

nb <- plots_cf_rs %>%
  mutate(log_agb = log1p(agb_plot_ha)) %>%
  mutate(is_break = ifelse(number_breaks == 0, "no break", "break")) %>%
  dplyr::filter(is_break == "no break")
  
summary(lm(nb$tree_density ~ nb$age))

# Seasonal variation ------------------------------------------------------

s <- time_series %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month == 1 | month == 2 | month == 3, "winter",
                         ifelse(month == 4 | month == 5 | month == 6, "spring",
                                ifelse(month == 7 | month == 8 | month == 9, "summer",
                                       "fall")))) 

a <- aov(s$ndvi ~ s$season)

TukeyHSD(a)
  
time_series %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month == 1 | month == 2 | month == 3, "winter",
                         ifelse(month == 4 | month == 5 | month == 6, "spring",
                                ifelse(month == 7 | month == 8 | month == 9, "summer",
                                       "fall")))) %>%
  ggplot(aes(x= month, y = ndvi)) +
  geom_point(aes(color = season)) 

time_series %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month == 1 | month == 2 | month == 3, "winter",
                         ifelse(month == 4 | month == 5 | month == 6, "spring",
                                ifelse(month == 7 | month == 8 | month == 9, "summer",
                                       "fall")))) %>%
  ggplot(aes(x= season, y = ndvi)) +
  geom_boxplot() +
  stat_compare_means(method = "anova")

time_series %>%
  mutate(month = month(date)) %>%
  mutate(season = ifelse(month > 4 & month < 11,
                         "rainy", "dry")) %>%
  ggplot(aes(x = season, y = ndvi)) +
  geom_boxplot() +
  stat_compare_means(method = "t.test",
                     label.x = 1.4)
  
# Notes -------------------------------------------------------------------


# Bart: seasonal variation -- make month/year plot

# histogram of redisuals
# 
# check what's happening with outliers
# 
# 
# Frontiers in Biogeo
# Frontiers in Ecology 

plots_cf_rs %>%
  select(ndvi_sd_ts, ndvi_annual_sd)

x <- c(5,9,1,3,0,2,6,9)
y <- c(0,4,7,2,9,7,5,0)
z <- c(9,2,8,4,7,8,3,7)

mean(x)
mean(y)
mean(z)

s <- c(x,y,z)

mean(s)
mean(c(mean(x),mean(y),mean(z)))

sd(s)
sd(c(sd(x),sd(y),sd(z)))

min(s)
mean(c(min(x), min(y), min(z)))
