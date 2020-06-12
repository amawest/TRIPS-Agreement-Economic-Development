# ---------------------------------------------------------------------------
# Final Analysis Code
# ---------------------------------------------------------------------------
rm(list=ls())
setwd("/Users/amandaAmanda/Desktop")

# Unbalanced
data.nas <- read.csv("1_26_2019 - Unbalanced_Panel.csv") 
# 1367 observations, 145 countries total, 13 years 

# Balanced
data.no.nas <- read.csv("1_26_2019 - Balanced_Panel.csv")  
# 1378 observations, 13 years, 106 countries, perfect information, balanced
# ---------------------------------------------------------------------------
# Standardizing (mean 0 and SD 1 for all columns)
# ---------------------------------------------------------------------------
#install.packages("psycho")
library(psycho)
#install.packages("tidyverse")
library(tidyverse)

data.nas$EF_Perception <- data.nas$WEF_defacto_score
data.no.nas$ER_Perception <- data.no.nas$WEF_defacto_score
data.nas$PR_Law <- data.nas$EF_PR_dejure_score
data.no.nas$PR_Law <- data.no.nas$EF_PR_dejure_score
#-------
# Unbalanced
data.nas.2 <- data.nas[,c(1,11, 12, 8, 9)]
data.nas.2 %>% 
  dplyr::select(-year) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0)) +
  theme_bw() +
  scale_fill_brewer(palette="Spectral")
#-------
# Balanced
data.no.nas.2 <- data.no.nas[,c(1,11, 12, 8, 9)]
data.no.nas.2 %>% 
  dplyr::select(-year) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
  geom_density(alpha=0.5) +
  geom_vline(aes(xintercept=0)) +
  theme_bw() +
  scale_fill_brewer(palette="Spectral")
# ---------------------------------------------------------------------------
# WEF / De Facto  (perception of local business owners; dummy and continuous)
# ---------------------------------------------------------------------------
#Balanced
didreg1 <- lm(WEF_defacto_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.no.nas)
didreg2 <- lm(WEF_defacto_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data.no.nas)
#Unbalanced
didreg3 <- lm(WEF_defacto_score  ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.nas)  

summary(didreg1) # ~= ***
summary(didreg2) # ~= ***
summary(didreg3) # ~= ***
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------------------------------
# EF / De Jure (laws on the books; dummy and continuous)
# ---------------------------------------------------------------------------
didreg4 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.no.nas) 
didreg5 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1
              + non_TRIPS_sum, data = data.no.nas) 
didreg6 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.nas)  

summary(didreg4) # ~= 
summary(didreg5) # ~= *
summary(didreg6) # ~= 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------------------------------
# Trade Freedom, EF Index (laws on the books; dummy and continuous)
# ---------------------------------------------------------------------------
# Balanced
didreg10 <- lm(EF_trade_freedom ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 
didreg11 <- lm(EF_trade_freedom ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1
               + non_TRIPS_sum, data = data.no.nas)
# Unbalanced
didreg12 <- lm(EF_trade_freedom ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.nas)  

summary(didreg10) # ~= 
summary(didreg11) # ~= 
summary(didreg12) # ~= 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------------------------------
# EF Overall (laws on the books; dummy and continuous)
# ---------------------------------------------------------------------------
# Balanced
didreg7 <- lm(EF_overall_score ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 
didreg8 <- lm(EF_overall_score ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1
               + non_TRIPS_sum, data = data.no.nas) 
#Unbalanced
didreg9 <- lm(EF_overall_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.nas)  

summary(didreg7) # ~= 
summary(didreg8) # ~= 
summary(didreg9) # ~= 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------------------------------
