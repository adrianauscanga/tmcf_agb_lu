#---------------------------- 01. BFAST analysis --------------------------
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
  if(is.na(breaks)) {
    nobreaks <- c(nobreaks, paste0("bfast_",i),0)
  } else {
    breaks2 <- breaks$breakpoints #get only the breakpoints
    break_moments <- c(break_moments, paste0("bfast_",i,",",breaks2))
    totalbreaks <- length(breaks2) #calculate no. of breakpoints
    bfast_breaks <- c(bfast_breaks, paste0("bfast_",i), totalbreaks)
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

# To know which breaks are positive or negative, 
# I need to get Tt (the fitted trend component)
# from bfast ouput.