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

# Basal area

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
                   data = rs_shorter)

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
                   data = rs_shorter)

lmer_ba_1b <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_1c <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_1d <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_1e <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_1f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_1g <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

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
                  data = rs_shorter)

lmer_ba_2b <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2c <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2d <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2e <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2f <- lmer(sqrt(basal_area_ha) ~ 
                     scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2g <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2h <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2i <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2j <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2k <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2l <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2m <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2n <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2o <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2p <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2q <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2r <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2s <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     ndvi_min_ts + 
                     #ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     ndvi_sd_ts + 
                     #ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

lmer_ba_2t <- lmer(sqrt(basal_area_ha) ~ 
                     #scale(altitude) + 
                     #scale(slope) + 
                     #scale(number_breaks) + 
                     #ndvi_annual_sd + 
                     #ndvi_sd_ts + 
                     ndvi_min_ts + 
                     ndwi_annual_min +
                     (1|site),
                   data = rs_shorter)

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

## 3 predictores

## 4 predictors 

## 5 predictors

## 6 predictors


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
                  data = rs_shorter)

summary(lmer_ba_7)
cAIC(lmer_ba_7)
stargazer(lmer_ba_7, type = "text", report = ("vc*p"))
rsq.lmm(lmer_ba_7, adj= T)
plot(lmer_ba_7)
plot_model(lmer_ba_7, show.p = T, show.values = T)
qqnorm(resid(lmer_ba_7))
qqline(resid(lmer_ba_7))

# Model comparison:
anocAIC(lmm_all, lmm_all6, lmm_all5, lmm_all4, digits = 3)
