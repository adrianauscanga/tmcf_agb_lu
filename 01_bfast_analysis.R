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

