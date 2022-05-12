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

load("~/OregonPhD/Tesis/chapter2/tmcf_agb_lu/output/plots_breaks_dummy.RData")

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
  filter(year < dc_year) %>%
  group_by(plot_id) %>%
  summarize(number_breaks = n(),
            last_break = last(break_dates),
            magnitude = last(magnitude))

# 75 plots have breaks, min number of breaks is 1, max is 9, mean value 2.066


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
            ndvi_sd_dc = sd(ndvi),
            ndvi_max_dc = max(ndvi),
            ndvi_min_dc = min(ndvi),
            evi_mean_dc = mean(evi),
            evi_sd_dc = sd(evi),
            evi_max_dc = max(evi),
            evi_min_dc = min(evi),
            ndwi_mean_dc = mean(ndwi),
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
            ndvi_sd_ts = sd(ndvi),
            ndvi_max_ts = max(ndvi),
            ndvi_min_ts = min(ndvi),
            ndvi_cv_ts = sd(ndvi)/mean(ndvi) *100,
            evi_mean_ts = mean(evi),
            evi_sd_ts = sd(evi),
            evi_max_ts = max(evi),
            evi_min_ts = min(evi),
            evi_cv_ts = sd(evi)/mean(ndvi) *100,
            ndwi_mean_ts = mean(ndwi),
            ndwi_sd_ts = sd(ndwi),
            ndwi_max_ts = max(ndwi),
            ndwi_min_ts = min(ndwi),
            ndwi_cv_ts = sd(ndwi)/mean(ndvi) *100)

# Remove plots that are not in plots_cf:
vi_ts <- vi_ts %>%
  filter(!plot_id %in% c("67883_2", "67883_3", "67883_4", "68398_3", "68398_4", "70076_1", "70306_3", "70306_4", "72284_1", "72284_2", "72284_3"))

# Join data sets:

plots_cf_vi <- plots_cf_vi %>%
  inner_join(vi_ts, by = c("plot_id"))

# Join breaks and VI data:

plots_cf_rs <- inner_join(plots_cf_breaks, plots_cf_vi) #plots_cf with remote sensing data

plots_cf_rs <- plots_cf_rs %>%
  mutate(age = ifelse(is.na(age), year-1993, age))
  
# Summarize break info by site:

sites_breaks <- plots_cf_rs %>%
  group_by(site) %>%
  summarize(total_breaks = sum(number_breaks),
            is_break = ifelse(total_breaks == 0, 'no break', 'break'),
            mean_breaks = mean(number_breaks),
            mean_age = mean(age),
            ndvi_mean_dc = mean(ndvi_mean_dc),
            ndvi_sd_dc = sd(ndvi_mean_dc),
            evi_mean_dc = mean(evi_mean_dc),
            evi_sd_dc = sd(evi_mean_dc),
            ndwi_mean_dc = mean(ndwi_mean_dc),
            ndwi_sd_dc = sd(ndwi_mean_dc))

# Summarize VI data by site:

sites_vi <- time_series_dc %>%
  separate(plot_id, into = c("site", "plot"), sep = "_") %>%
  group_by(site) %>%
  summarize(ndvi_mean_ts = mean(ndvi),
            ndvi_sd_ts = sd(ndvi),
            ndvi_max_ts = max(ndvi),
            ndvi_min_ts = min(ndvi),
            ndvi_cf_ts = sd(ndvi)/mean(ndvi) *100,
            evi_mean_ts = mean(evi),
            evi_sd_ts = sd(evi),
            evi_max_ts = max(evi),
            evi_min_ts = min(evi),
            evi_cf_ts = sd(evi)/mean(ndvi) *100,
            ndwi_mean_ts = mean(ndwi),
            ndwi_sd_ts = sd(ndwi),
            ndwi_max_ts = max(ndwi),
            ndwi_min_ts = min(ndwi),
            ndwi_cf_ts = sd(ndwi)/mean(ndvi) *100)

sites_cf_rs <- inner_join(sites_cf, sites_breaks) %>%
  inner_join(sites_vi)

# Explore relationships with sd, why are so many pixels with no breaks but very few trees?

sites_cf_rs %>%
  filter(plot_no >2) %>%
  #filter(is_break == "break") %>%
  #select(landscape, total_breaks, mean_breaks, mean_age, is_break, av_agb_site, av_treeheight_site, av_treedensity_site, av_basalarea_site, ndvi_mean_ts, ndvi_cf_ts, ndwi_mean_ts, ndwi_cf_ts, evi_mean_ts, evi_cf_ts) %>%
  mutate(age_class = ifelse(mean_age < 15, "<15", ">15")) %>%
  mutate(breaks_class = ifelse(mean_breaks == 0, "0",
                               ifelse(mean_breaks > 0 & mean_breaks < 1.1, "1", "2"))) %>%
  ggplot(aes(y = av_agb_site, x = is_break))+
  geom_boxplot() +
  geom_point()

# Filter sites with low agb (agb < 100) and mean_age > 15

weird <- sites_cf_rs %>%
  filter(av_agb_site < 100,
         mean_age > 15)

weird %>%
  group_by(is_break) %>%
  summarize(breaks_total = n())


sites_cf_rs %>%
  #filter(is_break == "break",
  #        plot_no > 2) %>%
  ggplot(aes(y = av_agb_site, x = mean_age)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm")

plots_cf_rs %>%
  mutate(no_breaks_class = ifelse(number_breaks == 0, "none",
                                  ifelse(number_breaks == 1, "one", "several"))) %>%
  ggplot(aes(y = agb_plot_ha, x = age)) +
  geom_point(aes(color = no_breaks_class), size = 2) +
  scale_y_log10() +
  #scale_color_gradient(low = "forestgreen", high =  "lightgoldenrod2")
  scale_color_brewer(type = "qual")

weirdplots <- plots_cf_rs %>%
  filter(age > 15,
         agb_plot_ha < 100,
         str_class_plots == 1,
         number_breaks == 0)

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
