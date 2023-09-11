# Linear mixed models

# Load libraries

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(leaps)
library(asbio)
library(MASS)
library(lme4)
library(stargazer)
library(sjPlot)
library(cAIC4)

# Load data sets

load("output/rs_shorter_4p.RData")

# Basal area ------------------------------------------------------------------

## Null model

lmer_ba_0 <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                  REML = F,
                  data = rs_shorter_4p)

cAIC(lmer_ba_0)
summary(lmer_ba_0)

## 1 predictor

lmer_ba_1a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1b <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   REML = F,
                   data = rs_shorter_4p)

lmer_ba_1c <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1d <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1e <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1g <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_1_cAIC <- anocAIC(lmer_ba_1a,
                          lmer_ba_1b,
                          lmer_ba_1c,
                          lmer_ba_1d,
                          lmer_ba_1e,
                          lmer_ba_1f,
                          lmer_ba_1g,
                          digits = 3)

lmer_ba_1_cAIC <- as_tibble(lmer_ba_1_cAIC, rownames = "model")

lmer_ba_1_cAIC %>%
  arrange(cAIC)


## 2 predictors
# Two predictors
combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 2, simplify = T)

lmer_ba_2a <- lmer(sqrt(basal_area_ha) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    #scale(number_breaks) + 
                    #ndvi_annual_sd + 
                    #ndvi_sd_ts + 
                    #ndvi_min_ts + 
                    #ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

lmer_ba_2b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2g <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   REML = F,
                   data = rs_shorter_4p)

lmer_ba_2h <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2i <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2j <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2k <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2l <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2m <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2n <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2o <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2p <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2q <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2r <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2s <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_2_cAIC <- anocAIC(lmer_ba_2a,
                          lmer_ba_2b,
                          lmer_ba_2c,
                          lmer_ba_2d,
                          lmer_ba_2e,
                          lmer_ba_2f,
                          lmer_ba_2g,
                          lmer_ba_2h,
                          lmer_ba_2i,
                          lmer_ba_2j,
                          lmer_ba_2k,
                          lmer_ba_2l,
                          lmer_ba_2m,
                          lmer_ba_2n,
                          lmer_ba_2o,
                          lmer_ba_2p,
                          lmer_ba_2q,
                          lmer_ba_2r,
                          lmer_ba_2s,
                          lmer_ba_2t,
                          digits = 3)

lmer_ba_2_cAIC <- as_tibble(lmer_ba_2_cAIC, rownames = "model")

lmer_ba_2_cAIC %>%
  arrange(cAIC)

## 3 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 3, simplify = T)

lmer_ba_3a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   REML = F,
                   data = rs_shorter_4p)

lmer_ba_3b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3g <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3h <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3i <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3j <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3k <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3l <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3m <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3n <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3o <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3p <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3q <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3r <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3s <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3u <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3v <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3w <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3x <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3y <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3z <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3a2 <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_3b2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3c2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3d2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3e2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3f2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3g2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3h2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3i2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_3_cAIC <- anocAIC(lmer_ba_3a,
                          lmer_ba_3b,
                          lmer_ba_3c,
                          lmer_ba_3d,
                          lmer_ba_3e,
                          lmer_ba_3f,
                          lmer_ba_3g,
                          lmer_ba_3h,
                          lmer_ba_3i,
                          lmer_ba_3j,
                          lmer_ba_3k,
                          lmer_ba_3l,
                          lmer_ba_3m,
                          lmer_ba_3n,
                          lmer_ba_3o,
                          lmer_ba_3p,
                          lmer_ba_3q,
                          lmer_ba_3r,
                          lmer_ba_3s,
                          lmer_ba_3t,
                          lmer_ba_3u,
                          lmer_ba_3v,
                          lmer_ba_3w,
                          lmer_ba_3x,
                          lmer_ba_3y,
                          lmer_ba_3z,
                          lmer_ba_3a2,
                          lmer_ba_3b2,
                          lmer_ba_3c2,
                          lmer_ba_3d2,
                          lmer_ba_3e2,
                          lmer_ba_3f2,
                          lmer_ba_3g2,
                          lmer_ba_3h2,
                          lmer_ba_3i2,
                          digits = 3)

lmer_ba_3_cAIC <- as_tibble(lmer_ba_3_cAIC, rownames = "model")

lmer_ba_3_cAIC %>%
  arrange(cAIC)

## 4 predictors 

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 4, simplify = T)

lmer_ba_4a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     scale(ndvi_annual_sd) + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4g <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4h <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4i <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4j <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4k <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4l <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4m <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4n <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4o <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4p <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4q <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4r <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4s <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4t <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4u <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4v <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4w <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4x <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4y <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4z <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_4a2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4b2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4c2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4d2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4e2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4f2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4g2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4h2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4i2 <- lmer(sqrt(basal_area_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_ba_4_cAIC <- anocAIC(lmer_ba_4a,
                          lmer_ba_4b,
                          lmer_ba_4c,
                          lmer_ba_4d,
                          lmer_ba_4e,
                          lmer_ba_4f,
                          lmer_ba_4g,
                          lmer_ba_4h,
                          lmer_ba_4i,
                          lmer_ba_4j,
                          lmer_ba_4k,
                          lmer_ba_4l,
                          lmer_ba_4m,
                          lmer_ba_4n,
                          lmer_ba_4o,
                          lmer_ba_4p,
                          lmer_ba_4q,
                          lmer_ba_4r,
                          lmer_ba_4s,
                          lmer_ba_4t,
                          lmer_ba_4u,
                          lmer_ba_4v,
                          lmer_ba_4w,
                          lmer_ba_4x,
                          lmer_ba_4y,
                          lmer_ba_4z,
                          lmer_ba_4a2,
                          lmer_ba_4b2,
                          lmer_ba_4c2,
                          lmer_ba_4d2,
                          lmer_ba_4e2,
                          lmer_ba_4f2,
                          lmer_ba_4g2,
                          lmer_ba_4h2,
                          lmer_ba_4i2,
                          digits = 3)

lmer_ba_4_cAIC <- as_tibble(lmer_ba_4_cAIC, rownames = "model")

lmer_ba_4_cAIC %>%
  arrange(cAIC)

## 5 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 5, simplify = T)

lmer_ba_5a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5g <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5h <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5i <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5j <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5k <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5l <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5m <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5n <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5o <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5p <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5q <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5r <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5s <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5u <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_5_cAIC <- anocAIC(lmer_ba_5a,
                          lmer_ba_5b,
                          lmer_ba_5c,
                          lmer_ba_5d,
                          lmer_ba_5e,
                          lmer_ba_5f,
                          lmer_ba_5g,
                          lmer_ba_5h,
                          lmer_ba_5i,
                          lmer_ba_5j,
                          lmer_ba_5k,
                          lmer_ba_5l,
                          lmer_ba_5m,
                          lmer_ba_5n,
                          lmer_ba_5o,
                          lmer_ba_5p,
                          lmer_ba_5q,
                          lmer_ba_5r,
                          lmer_ba_5s,
                          lmer_ba_5t,
                          lmer_ba_5u,
                          digits = 3)

lmer_ba_5_cAIC <- as_tibble(lmer_ba_5_cAIC, rownames = "model")

lmer_ba_5_cAIC %>%
  arrange(cAIC)

## 6 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 6, simplify = T)

lmer_ba_6a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6g <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_ba_6_cAIC <- anocAIC(lmer_ba_6a,
                          lmer_ba_6b,
                          lmer_ba_6c,
                          lmer_ba_6d,
                          lmer_ba_6e,
                          lmer_ba_6f,
                          lmer_ba_6g,
                          digits = 3)

lmer_ba_6_cAIC <- as_tibble(lmer_ba_6_cAIC, rownames = "model")

lmer_ba_6_cAIC %>%
  arrange(cAIC)

## 7 predictors

lmer_ba_7 <- lmer(sqrt(basal_area_ha) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

# Best models:

# 1 predictor. cAIC: 975.186
lmer_ba_1b <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 2 predictors. cAIC: 973.851
lmer_ba_2a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 3 predictors. cAIC: 975.344
lmer_ba_3a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 4 predictors. cAIC: 981.525
lmer_ba_4a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 5 predictors. cAIC: 982.252
lmer_ba_5d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 6 predictors. cAIC: 983.643
lmer_ba_6a <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   REML = F,
                   data = rs_shorter_4p)
# 7 predictors. cAIC: 985.92
lmer_ba_7 <- lmer(sqrt(basal_area_ha) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  REML = F,
                  data = rs_shorter_4p)

# Model comparison:
anocAIC(lmer_ba_1b, lmer_ba_2g, lmer_ba_3a, lmer_ba_4a, lmer_ba_5d, lmer_ba_6d, lmer_ba_7, digits = 3) 
rsq.lmm(lmer_ba_1b, adj= T)
rsq.lmm(lmer_ba_2g, adj= T)
rsq.lmm(lmer_ba_3a, adj= T)
rsq.lmm(lmer_ba_4a, adj= T)
rsq.lmm(lmer_ba_5d, adj= T)
rsq.lmm(lmer_ba_6d, adj= T)
rsq.lmm(lmer_ba_7, adj= T)

summary(lmer_ba_3a)

cAIC(lmer_ba_3a)
stargazer(lmer_ba_3a, type = "text", report = ("vc*p"))
rsq.lmm(lmer_ba_5d, adj= T)
plot(lmer_ba_3a)
plot_model(lmer_ba_5d, show.p = T, show.values = T)
qqnorm(resid(lmer_ba_3a))
qqline(resid(lmer_ba_3a))

rs_shorter_4p %>%
  mutate("Elevation (m asl)" = as.numeric(altitude),
         "NDVIa S.D." = as.numeric(ndvi_annual_sd),
         "NDVI S.D." = as.numeric(ndvi_sd_ts),
         "NDWIa min" = as.numeric(ndwi_annual_min),
         "Slope (degrees)" = as.numeric(slope),
         "Breaks" = as.numeric(number_breaks)) %>%
  pivot_longer(c(-site, -plot_id, -tree_density, -loreys_height, -basal_area_ha, -agb_plot_ha, -is_unexpected ), names_to = "covariates", values_to = "value") %>%
  filter(covariates == "Elevation (m asl)" | 
           covariates == "NDVIa S.D." |
           covariates == "NDVI S.D." |
           covariates == "NDWIa min" |
           covariates == "Slope (degrees)" |
           covariates == "Breaks") %>%
  ggplot(aes(value, basal_area_ha)) +
  geom_point() +
  geom_smooth(method = "lm", color = "gray30", alpha = 0.15) +
  theme_bw() +
  facet_wrap(~covariates,
             scales = "free",
             ncol = 6) +
  xlab(NULL) +
  labs(y = (bquote('Basal Area')))

# Tree height -------------------------------------------------------------------

lmer_th_0 <- lmer(sqrt(loreys_height) ~ 
                    #scale(altitude) + 
                    #scale(slope) + 
                    #scale(number_breaks) + 
                    #ndvi_annual_sd + 
                    #ndvi_sd_ts + 
                    #ndvi_min_ts + 
                    #ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

cAIC(lmer_th_0)
summary(lmer_th_0)

## 1 predictor

lmer_th_1a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1b <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_th_1c <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1d <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1e <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1g <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_1_cAIC <- anocAIC(lmer_th_1a,
                          lmer_th_1b,
                          lmer_th_1c,
                          lmer_th_1d,
                          lmer_th_1e,
                          lmer_th_1f,
                          lmer_th_1g,
                          digits = 3)

lmer_th_1_cAIC <- as_tibble(lmer_th_1_cAIC, rownames = "model")

lmer_th_1_cAIC %>%
  arrange(cAIC)


## 2 predictors
# Two predictors
combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 2, simplify = T)

lmer_th_2a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2b <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2c <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2e <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2g <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_th_2h <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2i <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2j <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2k <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2l <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2m <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2n <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2o <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2p <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2q <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2r <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2s <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2t <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2t <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_2_cAIC <- anocAIC(lmer_th_2a,
                          lmer_th_2b,
                          lmer_th_2c,
                          lmer_th_2d,
                          lmer_th_2e,
                          lmer_th_2f,
                          lmer_th_2g,
                          lmer_th_2h,
                          lmer_th_2i,
                          lmer_th_2j,
                          lmer_th_2k,
                          lmer_th_2l,
                          lmer_th_2m,
                          lmer_th_2n,
                          lmer_th_2o,
                          lmer_th_2p,
                          lmer_th_2q,
                          lmer_th_2r,
                          lmer_th_2s,
                          lmer_th_2t,
                          digits = 3)

lmer_th_2_cAIC <- as_tibble(lmer_th_2_cAIC, rownames = "model")

lmer_th_2_cAIC %>%
  arrange(cAIC)

## 3 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 3, simplify = T)

lmer_th_3a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_th_3b <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3c <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3e <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3g <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3h <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3i <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3j <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3k <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3l <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3m <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3n <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3o <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3p <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3q <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3r <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3s <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3t <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3u <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3v <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3w <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3x <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3y <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3z <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_3a2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3b2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3c2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3d2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3e2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3f2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3g2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3h2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3i2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_3_cAIC <- anocAIC(lmer_th_3a,
                          lmer_th_3b,
                          lmer_th_3c,
                          lmer_th_3d,
                          lmer_th_3e,
                          lmer_th_3f,
                          lmer_th_3g,
                          lmer_th_3h,
                          lmer_th_3i,
                          lmer_th_3j,
                          lmer_th_3k,
                          lmer_th_3l,
                          lmer_th_3m,
                          lmer_th_3n,
                          lmer_th_3o,
                          lmer_th_3p,
                          lmer_th_3q,
                          lmer_th_3r,
                          lmer_th_3s,
                          lmer_th_3t,
                          lmer_th_3u,
                          lmer_th_3v,
                          lmer_th_3w,
                          lmer_th_3x,
                          lmer_th_3y,
                          lmer_th_3z,
                          lmer_th_3a2,
                          lmer_th_3b2,
                          lmer_th_3c2,
                          lmer_th_3d2,
                          lmer_th_3e2,
                          lmer_th_3f2,
                          lmer_th_3g2,
                          lmer_th_3h2,
                          lmer_th_3i2,
                          digits = 3)

lmer_th_3_cAIC <- as_tibble(lmer_th_3_cAIC, rownames = "model")

lmer_th_3_cAIC %>%
  arrange(cAIC)

## 4 predictors 

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 4, simplify = T)

lmer_th_4a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_th_4b <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4c <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4e <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4g <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4h <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4i <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4j <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4k <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4l <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4m <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4n <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4o <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4p <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4q <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4r <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4s <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4t <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4u <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4v <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4w <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4x <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4y <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4z <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_4a2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4b2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4c2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4d2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4e2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4f2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4g2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4h2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4i2 <- lmer(sqrt(loreys_height) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_th_4_cAIC <- anocAIC(lmer_th_4a,
                          lmer_th_4b,
                          lmer_th_4c,
                          lmer_th_4d,
                          lmer_th_4e,
                          lmer_th_4f,
                          lmer_th_4g,
                          lmer_th_4h,
                          lmer_th_4i,
                          lmer_th_4j,
                          lmer_th_4k,
                          lmer_th_4l,
                          lmer_th_4m,
                          lmer_th_4n,
                          lmer_th_4o,
                          lmer_th_4p,
                          lmer_th_4q,
                          lmer_th_4r,
                          lmer_th_4s,
                          lmer_th_4t,
                          lmer_th_4u,
                          lmer_th_4v,
                          lmer_th_4w,
                          lmer_th_4x,
                          lmer_th_4y,
                          lmer_th_4z,
                          lmer_th_4a2,
                          lmer_th_4b2,
                          lmer_th_4c2,
                          lmer_th_4d2,
                          lmer_th_4e2,
                          lmer_th_4f2,
                          lmer_th_4g2,
                          lmer_th_4h2,
                          lmer_th_4i2,
                          digits = 3)

lmer_th_4_cAIC <- as_tibble(lmer_th_4_cAIC, rownames = "model")

lmer_th_4_cAIC %>%
  arrange(cAIC)

## 5 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 5, simplify = T)

lmer_th_5a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5b <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5c <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5e <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5g <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5h <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5i <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5j <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5k <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5l <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5m <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5n <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5o <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5p <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5q <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5r <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5s <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5t <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5u <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_5_cAIC <- anocAIC(lmer_th_5a,
                          lmer_th_5b,
                          lmer_th_5c,
                          lmer_th_5d,
                          lmer_th_5e,
                          lmer_th_5f,
                          lmer_th_5g,
                          lmer_th_5h,
                          lmer_th_5i,
                          lmer_th_5j,
                          lmer_th_5k,
                          lmer_th_5l,
                          lmer_th_5m,
                          lmer_th_5n,
                          lmer_th_5o,
                          lmer_th_5p,
                          lmer_th_5q,
                          lmer_th_5r,
                          lmer_th_5s,
                          lmer_th_5t,
                          lmer_th_5u,
                          digits = 3)

lmer_th_5_cAIC <- as_tibble(lmer_th_5_cAIC, rownames = "model")

lmer_th_5_cAIC %>%
  arrange(cAIC)

## 6 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 6, simplify = T)

lmer_th_6a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6b <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6c <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6e <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6f <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6g <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_th_6_cAIC <- anocAIC(lmer_th_6a,
                          lmer_th_6b,
                          lmer_th_6c,
                          lmer_th_6d,
                          lmer_th_6e,
                          lmer_th_6f,
                          lmer_th_6g,
                          digits = 3)

lmer_th_6_cAIC <- as_tibble(lmer_th_6_cAIC, rownames = "model")

lmer_th_6_cAIC %>%
  arrange(cAIC)

## 7 predictors

lmer_th_7 <- lmer(sqrt(loreys_height) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

# Best models:

# 1 predictor. 
lmer_th_1b <- lmer(sqrt(loreys_height) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 2 predictors. 
lmer_th_2a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 3 predictors. cAIC: 975.344
lmer_th_3a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 4 predictors. cAIC: 981.525
lmer_th_4a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 5 predictors. cAIC: 982.252
lmer_th_5d <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)
# 6 predictors. cAIC: 983.643
lmer_th_6a <- lmer(sqrt(loreys_height) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)
# 7 predictors. cAIC: 985.92
lmer_th_7 <- lmer(sqrt(loreys_height) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  #REML = F,
                  data = rs_shorter_4p)

# Model comparison:
anocAIC(lmer_th_1b, lmer_th_2a, lmer_th_3e, lmer_th_4d, lmer_th_5f, lmer_th_6c, lmer_th_7, digits = 3) 
rsq.lmm(lmer_th_1b, adj= T)
rsq.lmm(lmer_th_2a, adj= T)
rsq.lmm(lmer_th_3e, adj= T)
rsq.lmm(lmer_th_4d, adj= T)
rsq.lmm(lmer_th_5f, adj= T)
rsq.lmm(lmer_th_6c, adj= T)
rsq.lmm(lmer_th_7, adj= T)

summary(lmer_th_3e) # Best model 
summary(lmer_th_4d)

cAIC(lmer_th_7)
stargazer(lmer_th_7, type = "text", report = ("vc*p"))
rsq.lmm(lmer_th_7, adj= T)
plot(lmer_th_3e)
plot_model(lmer_th_3e, show.p = T, show.values = T)
qqnorm(resid(lmer_th_3e))
qqline(resid(lmer_th_3e))

rs_shorter_4p %>%
  mutate("Elevation (m asl)" = as.numeric(altitude),
         "NDVIa S.D." = as.numeric(ndvi_annual_sd),
         "NDVI S.D." = as.numeric(ndvi_sd_ts),
         "NDWIa min" = as.numeric(ndwi_annual_min),
         "Slope (degrees)" = as.numeric(slope),
         "Breaks" = as.numeric(number_breaks)) %>%
  pivot_longer(c(-site, -plot_id, -tree_density, -basal_area_ha, -loreys_height, -agb_plot_ha, -is_unexpected ), names_to = "covariates", values_to = "value") %>%
  filter(covariates == "Elevation (m asl)" | 
           covariates == "NDVIa S.D." |
           covariates == "NDVI S.D." |
           covariates == "NDWIa min" |
           covariates == "Slope (degrees)" |
           covariates == "Breaks") %>%
  ggplot(aes(value, loreys_height)) +
  geom_point() +
  geom_smooth(method = "lm", color = "gray30", alpha = 0.15) +
  theme_bw() +
  facet_wrap(~covariates,
             scales = "free",
             ncol = 6) +
  xlab(NULL) +
  labs(y = (bquote('Tree height')))

# Tree density -----------------------------------------------------------------

lmer_td_0 <- lmer(log1p(agb_plot_ha) ~ 
                    #scale(altitude) + 
                    #scale(slope) + 
                    #scale(number_breaks) + 
                    #ndvi_annual_sd + 
                    #ndvi_sd_ts + 
                    #ndvi_min_ts + 
                    #ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

cAIC(lmer_td_0)
summary(lmer_td_0)

## 1 predictor

lmer_td_1a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1b <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_td_1c <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1d <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1e <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_1_cAIC <- anocAIC(lmer_td_1a,
                          lmer_td_1b,
                          lmer_td_1c,
                          lmer_td_1d,
                          lmer_td_1e,
                          lmer_td_1f,
                          lmer_td_1g,
                          digits = 3)

lmer_td_1_cAIC <- as_tibble(lmer_td_1_cAIC, rownames = "model")

lmer_td_1_cAIC %>%
  arrange(cAIC)


## 2 predictors
# Two predictors
combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 2, simplify = T)

lmer_td_2a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_td_2h <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2i <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2j <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2k <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2l <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2m <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2n <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2o <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_2_cAIC <- anocAIC(lmer_td_2a,
                          lmer_td_2b,
                          lmer_td_2c,
                          lmer_td_2d,
                          lmer_td_2e,
                          lmer_td_2f,
                          lmer_td_2g,
                          lmer_td_2h,
                          lmer_td_2i,
                          lmer_td_2j,
                          lmer_td_2k,
                          lmer_td_2l,
                          lmer_td_2m,
                          lmer_td_2n,
                          lmer_td_2o,
                          lmer_td_2p,
                          lmer_td_2q,
                          lmer_td_2r,
                          lmer_td_2s,
                          lmer_td_2t,
                          digits = 3)

lmer_td_2_cAIC <- as_tibble(lmer_td_2_cAIC, rownames = "model")

lmer_td_2_cAIC %>%
  arrange(cAIC)

## 3 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 3, simplify = T)

lmer_td_3a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_td_3b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3v <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3w <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3x <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3y <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3z <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_3a2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3b2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3c2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3d2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3e2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3f2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3g2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3h2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3i2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_3_cAIC <- anocAIC(lmer_td_3a,
                          lmer_td_3b,
                          lmer_td_3c,
                          lmer_td_3d,
                          lmer_td_3e,
                          lmer_td_3f,
                          lmer_td_3g,
                          lmer_td_3h,
                          lmer_td_3i,
                          lmer_td_3j,
                          lmer_td_3k,
                          lmer_td_3l,
                          lmer_td_3m,
                          lmer_td_3n,
                          lmer_td_3o,
                          lmer_td_3p,
                          lmer_td_3q,
                          lmer_td_3r,
                          lmer_td_3s,
                          lmer_td_3t,
                          lmer_td_3u,
                          lmer_td_3v,
                          lmer_td_3w,
                          lmer_td_3x,
                          lmer_td_3y,
                          lmer_td_3z,
                          lmer_td_3a2,
                          lmer_td_3b2,
                          lmer_td_3c2,
                          lmer_td_3d2,
                          lmer_td_3e2,
                          lmer_td_3f2,
                          lmer_td_3g2,
                          lmer_td_3h2,
                          lmer_td_3i2,
                          digits = 3)

lmer_td_3_cAIC <- as_tibble(lmer_td_3_cAIC, rownames = "model")

lmer_td_3_cAIC %>%
  arrange(cAIC)

## 4 predictors 

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 4, simplify = T)

lmer_td_4a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_td_4b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4p <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4q <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4r <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4s <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4t <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4v <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4w <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4x <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4y <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4z <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_4a2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4b2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4c2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4d2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4e2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4f2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4g2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4h2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4i2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_td_4_cAIC <- anocAIC(lmer_td_4a,
                          lmer_td_4b,
                          lmer_td_4c,
                          lmer_td_4d,
                          lmer_td_4e,
                          lmer_td_4f,
                          lmer_td_4g,
                          lmer_td_4h,
                          lmer_td_4i,
                          lmer_td_4j,
                          lmer_td_4k,
                          lmer_td_4l,
                          lmer_td_4m,
                          lmer_td_4n,
                          lmer_td_4o,
                          lmer_td_4p,
                          lmer_td_4q,
                          lmer_td_4r,
                          lmer_td_4s,
                          lmer_td_4t,
                          lmer_td_4u,
                          lmer_td_4v,
                          lmer_td_4w,
                          lmer_td_4x,
                          lmer_td_4y,
                          lmer_td_4z,
                          lmer_td_4a2,
                          lmer_td_4b2,
                          lmer_td_4c2,
                          lmer_td_4d2,
                          lmer_td_4e2,
                          lmer_td_4f2,
                          lmer_td_4g2,
                          lmer_td_4h2,
                          lmer_td_4i2,
                          digits = 3)

lmer_td_4_cAIC <- as_tibble(lmer_td_4_cAIC, rownames = "model")

lmer_td_4_cAIC %>%
  arrange(cAIC)

## 5 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 5, simplify = T)

lmer_td_5a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_5_cAIC <- anocAIC(lmer_td_5a,
                          lmer_td_5b,
                          lmer_td_5c,
                          lmer_td_5d,
                          lmer_td_5e,
                          lmer_td_5f,
                          lmer_td_5g,
                          lmer_td_5h,
                          lmer_td_5i,
                          lmer_td_5j,
                          lmer_td_5k,
                          lmer_td_5l,
                          lmer_td_5m,
                          lmer_td_5n,
                          lmer_td_5o,
                          lmer_td_5p,
                          lmer_td_5q,
                          lmer_td_5r,
                          lmer_td_5s,
                          lmer_td_5t,
                          lmer_td_5u,
                          digits = 3)

lmer_td_5_cAIC <- as_tibble(lmer_td_5_cAIC, rownames = "model")

lmer_td_5_cAIC %>%
  arrange(cAIC)

## 6 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 6, simplify = T)

lmer_td_6a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_td_6_cAIC <- anocAIC(lmer_td_6a,
                          lmer_td_6b,
                          lmer_td_6c,
                          lmer_td_6d,
                          lmer_td_6e,
                          lmer_td_6f,
                          lmer_td_6g,
                          digits = 3)

lmer_td_6_cAIC <- as_tibble(lmer_td_6_cAIC, rownames = "model")

lmer_td_6_cAIC %>%
  arrange(cAIC)

## 7 predictors

lmer_td_7 <- lmer(log1p(agb_plot_ha) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

# Best models:
# lmer_td_1b
# lmer_td_2g
# lmer_td_3a ## Best model
# lmer_td_4a
# lmer_td_5f
# lmer_td_6c

# Model comparison:
anocAIC(lmer_td_1b, lmer_td_2g, lmer_td_3a, lmer_td_4a, lmer_td_5f, lmer_td_6c, lmer_td_7, digits = 3) 
rsq.lmm(lmer_td_1b, adj= T)
rsq.lmm(lmer_td_2g, adj= T)
rsq.lmm(lmer_td_3a, adj= T)
rsq.lmm(lmer_td_4a, adj= T)
rsq.lmm(lmer_td_5f, adj= T)
rsq.lmm(lmer_td_6c, adj= T)
rsq.lmm(lmer_td_7, adj= T)

summary(lmer_td_4a)

cAIC(lmer_td_7)
stargazer(lmer_td_7, type = "text", report = ("vc*p"))
rsq.lmm(lmer_td_7, adj= T)
plot(lmer_td_3a)
plot_model(lmer_td_3a, show.p = T, show.values = T)
qqnorm(resid(lmer_td_3a))
qqline(resid(lmer_td_3a))

rs_shorter_4p %>%
  mutate("Elevation (m asl)" = as.numeric(altitude),
         "NDVIa S.D." = as.numeric(ndvi_annual_sd),
         "NDVI S.D." = as.numeric(ndvi_sd_ts),
         "NDWIa min" = as.numeric(ndwi_annual_min),
         "Slope (degrees)" = as.numeric(slope),
         "Breaks" = as.numeric(number_breaks)) %>%
  pivot_longer(c(-site, -plot_id, -tree_density, -basal_area_ha, -loreys_height, -agb_plot_ha, -is_unexpected ), names_to = "covariates", values_to = "value") %>%
  filter(covariates == "Elevation (m asl)" | 
           covariates == "NDVIa S.D." |
           covariates == "NDVI S.D." |
           covariates == "NDWIa min" |
           covariates == "Slope (degrees)" |
           covariates == "Breaks") %>%
  ggplot(aes(value, tree_density)) +
  geom_point() +
  geom_smooth(method = "lm", color = "gray30", alpha = 0.15) +
  theme_bw() +
  facet_wrap(~covariates,
             scales = "free",
             ncol = 6) +
  xlab(NULL) +
  labs(y = (bquote('Tree density')))

# AGB --------------------------------------------------------------------------

lmer_agb_0 <- lmer(log1p(agb_plot_ha) ~ 
                    #scale(altitude) + 
                    #scale(slope) + 
                    #scale(number_breaks) + 
                    #ndvi_annual_sd + 
                    #ndvi_sd_ts + 
                    #ndvi_min_ts + 
                    #ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

cAIC(lmer_agb_0)
summary(lmer_agb_0)

## 1 predictor

lmer_agb_1a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1b <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_agb_1c <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1d <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1e <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_1_cAIC <- anocAIC(lmer_agb_1a,
                          lmer_agb_1b,
                          lmer_agb_1c,
                          lmer_agb_1d,
                          lmer_agb_1e,
                          lmer_agb_1f,
                          lmer_agb_1g,
                          digits = 3)

lmer_agb_1_cAIC <- as_tibble(lmer_agb_1_cAIC, rownames = "model")

lmer_agb_1_cAIC %>%
  arrange(cAIC)


## 2 predictors
# Two predictors
combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 2, simplify = T)

lmer_agb_2a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_agb_2h <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2i <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2j <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2k <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2l <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2m <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2n <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2o <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_2_cAIC <- anocAIC(lmer_agb_2a,
                          lmer_agb_2b,
                          lmer_agb_2c,
                          lmer_agb_2d,
                          lmer_agb_2e,
                          lmer_agb_2f,
                          lmer_agb_2g,
                          lmer_agb_2h,
                          lmer_agb_2i,
                          lmer_agb_2j,
                          lmer_agb_2k,
                          lmer_agb_2l,
                          lmer_agb_2m,
                          lmer_agb_2n,
                          lmer_agb_2o,
                          lmer_agb_2p,
                          lmer_agb_2q,
                          lmer_agb_2r,
                          lmer_agb_2s,
                          lmer_agb_2t,
                          digits = 3)

lmer_agb_2_cAIC <- as_tibble(lmer_agb_2_cAIC, rownames = "model")

lmer_agb_2_cAIC %>%
  arrange(cAIC)

## 3 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 3, simplify = T)

lmer_agb_3a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_agb_3b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3v <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3w <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3x <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3y <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3z <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_3a2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3b2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3c2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3d2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3e2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3f2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3g2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3h2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3i2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_3_cAIC <- anocAIC(lmer_agb_3a,
                          lmer_agb_3b,
                          lmer_agb_3c,
                          lmer_agb_3d,
                          lmer_agb_3e,
                          lmer_agb_3f,
                          lmer_agb_3g,
                          lmer_agb_3h,
                          lmer_agb_3i,
                          lmer_agb_3j,
                          lmer_agb_3k,
                          lmer_agb_3l,
                          lmer_agb_3m,
                          lmer_agb_3n,
                          lmer_agb_3o,
                          lmer_agb_3p,
                          lmer_agb_3q,
                          lmer_agb_3r,
                          lmer_agb_3s,
                          lmer_agb_3t,
                          lmer_agb_3u,
                          lmer_agb_3v,
                          lmer_agb_3w,
                          lmer_agb_3x,
                          lmer_agb_3y,
                          lmer_agb_3z,
                          lmer_agb_3a2,
                          lmer_agb_3b2,
                          lmer_agb_3c2,
                          lmer_agb_3d2,
                          lmer_agb_3e2,
                          lmer_agb_3f2,
                          lmer_agb_3g2,
                          lmer_agb_3h2,
                          lmer_agb_3i2,
                          digits = 3)

lmer_agb_3_cAIC <- as_tibble(lmer_agb_3_cAIC, rownames = "model")

lmer_agb_3_cAIC %>%
  arrange(cAIC)

## 4 predictors 

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 4, simplify = T)

lmer_agb_4a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     scale(ndvi_annual_sd) + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   #REML = F,
                   data = rs_shorter_4p)

lmer_agb_4b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4p <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4q <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4r <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4s <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4t <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4v <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4w <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4x <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4y <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4z <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_4a2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4b2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4c2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4d2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      scale(slope) + 
                      #scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4e2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      #ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4f2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      #ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4g2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      ndvi_annual_sd + 
                      #ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4h2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      scale(number_breaks) + 
                      #ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4i2 <- lmer(log1p(agb_plot_ha) ~ 
                      #scale(altitude) + 
                      #scale(slope) + 
                      #scale(number_breaks) + 
                      ndvi_annual_sd + 
                      ndvi_sd_ts + 
                      ndvi_min_ts + 
                      ndwi_annual_min +
                      (1|site),
                    data = rs_shorter_4p)

lmer_agb_4_cAIC <- anocAIC(lmer_agb_4a,
                          lmer_agb_4b,
                          lmer_agb_4c,
                          lmer_agb_4d,
                          lmer_agb_4e,
                          lmer_agb_4f,
                          lmer_agb_4g,
                          lmer_agb_4h,
                          lmer_agb_4i,
                          lmer_agb_4j,
                          lmer_agb_4k,
                          lmer_agb_4l,
                          lmer_agb_4m,
                          lmer_agb_4n,
                          lmer_agb_4o,
                          lmer_agb_4p,
                          lmer_agb_4q,
                          lmer_agb_4r,
                          lmer_agb_4s,
                          lmer_agb_4t,
                          lmer_agb_4u,
                          lmer_agb_4v,
                          lmer_agb_4w,
                          lmer_agb_4x,
                          lmer_agb_4y,
                          lmer_agb_4z,
                          lmer_agb_4a2,
                          lmer_agb_4b2,
                          lmer_agb_4c2,
                          lmer_agb_4d2,
                          lmer_agb_4e2,
                          lmer_agb_4f2,
                          lmer_agb_4g2,
                          lmer_agb_4h2,
                          lmer_agb_4i2,
                          digits = 3)

lmer_agb_4_cAIC <- as_tibble(lmer_agb_4_cAIC, rownames = "model")

lmer_agb_4_cAIC %>%
  arrange(cAIC)

## 5 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 5, simplify = T)

lmer_agb_5a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5g <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5h <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5i <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5j <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5k <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5l <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5m <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5n <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5o <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5p <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5q <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5r <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5s <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5t <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5u <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_5_cAIC <- anocAIC(lmer_agb_5a,
                          lmer_agb_5b,
                          lmer_agb_5c,
                          lmer_agb_5d,
                          lmer_agb_5e,
                          lmer_agb_5f,
                          lmer_agb_5g,
                          lmer_agb_5h,
                          lmer_agb_5i,
                          lmer_agb_5j,
                          lmer_agb_5k,
                          lmer_agb_5l,
                          lmer_agb_5m,
                          lmer_agb_5n,
                          lmer_agb_5o,
                          lmer_agb_5p,
                          lmer_agb_5q,
                          lmer_agb_5r,
                          lmer_agb_5s,
                          lmer_agb_5t,
                          lmer_agb_5u,
                          digits = 3)

lmer_agb_5_cAIC <- as_tibble(lmer_agb_5_cAIC, rownames = "model")

lmer_agb_5_cAIC %>%
  arrange(cAIC)

## 6 predictors

combn(c("altitude","slope","number_breaks", "ndvi_annual_sd","ndvi_sd_ts","ndvi_min_ts","ndwi_annual_min"), 6, simplify = T)

lmer_agb_6a <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6b <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6c <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6d <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6e <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6f <- lmer(log1p(agb_plot_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6g <- lmer(log1p(agb_plot_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter_4p)

lmer_agb_6_cAIC <- anocAIC(lmer_agb_6a,
                          lmer_agb_6b,
                          lmer_agb_6c,
                          lmer_agb_6d,
                          lmer_agb_6e,
                          lmer_agb_6f,
                          lmer_agb_6g,
                          digits = 3)

lmer_agb_6_cAIC <- as_tibble(lmer_agb_6_cAIC, rownames = "model")

lmer_agb_6_cAIC %>%
  arrange(cAIC)

## 7 predictors

lmer_agb_7 <- lmer(log1p(agb_plot_ha) ~ 
                    scale(altitude) + 
                    scale(slope) + 
                    scale(number_breaks) + 
                    ndvi_annual_sd + 
                    ndvi_sd_ts + 
                    ndvi_min_ts + 
                    ndwi_annual_min +
                    (1|site),
                  data = rs_shorter_4p)

# Best models:
# lmer_agb_1b
# lmer_agb_2g
# lmer_agb_3a
# lmer_agb_4a
# lmer_agb_5d
# lmer_agb_6d

# Model comparison:
anocAIC(lmer_agb_1b, lmer_agb_2g, lmer_agb_3a, lmer_agb_4a, lmer_agb_5d, lmer_agb_6d, lmer_agb_7, digits = 3) 
rsq.lmm(lmer_agb_1b, adj= T)
rsq.lmm(lmer_agb_2g, adj= T)
rsq.lmm(lmer_agb_3a, adj= T)
rsq.lmm(lmer_agb_4a, adj= T)
rsq.lmm(lmer_agb_5d, adj= T)
rsq.lmm(lmer_agb_6c, adj= T)
rsq.lmm(lmer_agb_7, adj= T)

summary(lmer_agb_4a) # Best model

cAIC(lmer_agb_7)
stargazer(lmer_agb_7, type = "text", report = ("vc*p"))
rsq.lmm(lmer_agb_7, adj= T)
plot(lmer_agb_4a)
plot_model(lmer_agb_4a, show.p = T, show.values = T)
qqnorm(resid(lmer_agb_4a))
qqline(resid(lmer_agb_4a))
