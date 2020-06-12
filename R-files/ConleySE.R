rm(list=ls())
setwd("/Users/amandaAmanda/Desktop")
data <- read.csv("1_26_2019 - Bad_Neighbor_Balanced.csv") 

# Standardize
data$WEF_defacto_score <- scale(data$WEF_defacto_score)
data$EF_PR_dejure_score <- scale(data$EF_PR_dejure_score)
data$EF_overall_score <- scale(data$EF_overall_score)
data$EF_trade_freedom <- scale(data$EF_trade_freedom)

library(lfe)
library(Rcpp)
library(RcppArmadillo)
library(geosphere)
library(foreign)
library(reshape)
library(dplyr)
source("code/conley.R")

# Now the fun begins
m <- felm(EF_overall_score ~ treatment_TRIPS - 1
              | year + country | 0 | centroid.lat + centroid.lon,
              data = data, keepCX = TRUE)
coefficients(m) %>%round(3) 

# Neighbors are specified as those within 500 kilometers 

SE <-ConleySEs(reg = m,
               unit = "country", 
               time = "year",
               lat = "centroid.lat", lon = "centroid.lon",
               dist_fn = "SH", dist_cutoff = 500, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = FALSE) 

sapply(SE, sqrt) %>%round(3) 

# 4 regressions - 4 same but with controls
didreg1 <- lm(WEF_defacto_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data)
didreg2 <- lm(WEF_defacto_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data)
didreg3 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data) 
didreg4 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data) 
didreg5 <- lm(EF_trade_freedom ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data) 
didreg6 <- lm(EF_trade_freedom ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data)
didreg7 <- lm(EF_overall_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data) 
didreg8 <- lm(EF_overall_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data) 

