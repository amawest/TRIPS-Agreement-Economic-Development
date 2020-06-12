# ------------------------------------------------------
# False Experiment, De Facto
# ------------------------------------------------------
rm(list=ls())
setwd("/Users/amandaAmanda/Desktop")
data.bal <- read.csv("5.5.2020 - False_Balanced.csv") 
data.unbal <- read.csv("5.5.2020 - False_Unbalanced.csv") 
# ------------------------------------------------------

# De Jure ----------
#Balanced
didreg3 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.bal) 
#Unbalanced
didreg4 <- lm(EF_PR_dejure_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.unbal)  
summary(didreg3) 
summary(didreg4) 

# De Facto ----------

#Balanced
didreg1 <- lm(WEF_defacto_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.bal)
#Unbalanced
didreg2 <- lm(WEF_defacto_score  ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.unbal)
summary(didreg1) 
summary(didreg2) 

# Trade Freedom ----------
#Balanced
didreg5 <- lm(EF_trade_freedom ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.bal) 
# Unbalanced
didreg6 <- lm(EF_trade_freedom ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.unbal)  
summary(didreg5) 
summary(didreg6)


# Overall ----------
# Balanced
didreg7 <- lm(EF_overall_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.bal) 
#Unbalanced
didreg8 <- lm(EF_overall_score ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.unbal)  

summary(didreg7) 
summary(didreg8) 

# ------------------------------------------------------

