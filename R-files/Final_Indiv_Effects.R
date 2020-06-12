# -----------------------------------------------------
# Individual Graphs
# -----------------------------------------------------
remove(list = ls())
setwd("/Users/amandaAmanda/Desktop")
data.nas <- read.csv("1_26_2019 - Unbalanced_Panel.csv") 
data.nas <- na.omit(data.nas)   
data.no.nas <- read.csv("1_26_2019 - Balanced_Panel.csv") 

# --------- Standardize
data.nas$WEF_defacto_score <- scale(data.nas$WEF_defacto_score)
data.nas$EF_PR_dejure_score <- scale(data.nas$EF_PR_dejure_score)
data.nas$EF_overall_score <- scale(data.nas$EF_overall_score)
data.nas$EF_trade_freedom <- scale(data.nas$EF_trade_freedom)
data.no.nas$WEF_defacto_score <- scale(data.no.nas$WEF_defacto_score)
data.no.nas$EF_PR_dejure_score <- scale(data.no.nas$EF_PR_dejure_score)
data.no.nas$EF_overall_score <- scale(data.no.nas$EF_overall_score)
data.no.nas$EF_trade_freedom <- scale(data.no.nas$EF_trade_freedom)
# ----------------------
# Only Used Balanced Countries

# ----------- Australia
subset <- data.nas[data.nas$country == "Australia",]
subset <- subset[,c("country",
                    "year",
                    "WEF_defacto_score",
                    "EF_PR_dejure_score",
                    "EF_overall_score",
                    "EF_trade_freedom")]
library(dplyr)
subset <- subset %>% arrange(country, year)
plot(subset$year,subset$WEF_defacto_score,
     type = "l",
     lwd=2,col="firebrick",
     xlab = "Time",
     ylim=c(-1, 2),
     ylab = "Standardized Value",
     main = "Australia")
lines(subset$year,subset$EF_PR_dejure_score, col="forestgreen",lwd=2)
lines(subset$year,subset$EF_overall_score, col="dodgerblue",lwd=2)
lines(subset$year,subset$EF_trade_freedom, col="gold",lwd=2)
abline(v = 2013.75, col="black", lwd=1, lty=3)
abline(v = 2013.40, col="black", lwd=1, lty=3)
abline(v = 2013.50, col="black", lwd=1, lty=3)
abline(v = 2012.4, col="black", lwd=1, lty=3)
abline(v = 2012.3, col="black", lwd=1, lty=3)
legend("bottomright",cex = 0.4, legend=c("De Facto", 
                                     "De Jure", 
                                     "Overall",
                                     "Trade Freedom", 
                                     "Dispute"),
       lty=c(1,1,1,1,3,3), 
       col=c("firebrick", 
             "forestgreen", 
             "dodgerblue",
             "gold", 
             "black"))
# ----------- Bahrain
subset <- data.nas[data.nas$country == "Bahrain",]
subset <- subset[,c("country",
                    "year",
                    "WEF_defacto_score",
                    "EF_PR_dejure_score",
                    "EF_overall_score",
                    "EF_trade_freedom")]
library(dplyr)
subset <- subset %>% arrange(country, year)
plot(subset$year,subset$WEF_defacto_score,
     type = "l",
     lwd=2,col="firebrick",
     xlab = "Time",
     ylim=c(-1, 2),
     ylab = "Standardized Value",
     main = "Bahrain")
lines(subset$year,subset$EF_PR_dejure_score, col="forestgreen",lwd=2)
lines(subset$year,subset$EF_overall_score, col="dodgerblue",lwd=2)
lines(subset$year,subset$EF_trade_freedom, col="gold",lwd=2)
abline(v = 2017.5, col="black", lwd=1, lty=3)
legend("topleft",cex = 0.4, legend=c("De Facto", 
                                     "De Jure", 
                                     "Overall",
                                     "Trade Freedom", 
                                     "Dispute"),
       lty=c(1,1,1,1,3,3), 
       col=c("firebrick", 
             "forestgreen", 
             "dodgerblue",
             "gold", 
             "black"))
# ----------- EU Countries
subset <- read.csv("EU Countries - Sheet2.csv") 
subset <- subset[,c("country",
                    "year",
                    "WEF_defacto_score",
                    "EF_PR_dejure_score",
                    "EF_overall_score",
                    "EF_trade_freedom")]
plot(subset$year,subset$WEF_defacto_score,
     type = "l",
     lwd=2,col="firebrick",
     xlab = "Time",
     ylim=c(0, 1.5),
     ylab = "Standardized Value",
     main = "EU Countries")
lines(subset$year,subset$EF_PR_dejure_score, col="forestgreen",lwd=2)
lines(subset$year,subset$EF_overall_score, col="dodgerblue",lwd=2)
lines(subset$year,subset$EF_trade_freedom, col="gold",lwd=2)
abline(v = 2010.5, col="black", lwd=1, lty=3)
abline(v = 2010.55, col="black", lwd=1, lty=3)
legend("bottomright",cex = 0.4, legend=c("De Facto", 
                                     "De Jure", 
                                     "Overall",
                                     "Trade Freedom", 
                                     "Dispute"),
       lty=c(1,1,1,1,3,3), 
       col=c("firebrick", 
             "forestgreen", 
             "dodgerblue",
             "gold", 
             "black"))
# ----------- United Arab Emirates
subset <- data.nas[data.nas$country == "United.Arab.Emirates",]
subset <- subset[,c("country",
                    "year",
                    "WEF_defacto_score",
                    "EF_PR_dejure_score",
                    "EF_overall_score",
                    "EF_trade_freedom")]
library(dplyr)
subset <- subset %>% arrange(country, year)
plot(subset$year,subset$WEF_defacto_score,
     type = "l",
     lwd=2,col="firebrick",
     xlab = "Time",
     ylim=c(-1, 2),
     ylab = "Standardized Value",
     main = "United Arab Emirates")
lines(subset$year,subset$EF_PR_dejure_score, col="forestgreen",lwd=2)
lines(subset$year,subset$EF_overall_score, col="dodgerblue",lwd=2)
lines(subset$year,subset$EF_trade_freedom, col="gold",lwd=2)
abline(v = 2017.5, col="black", lwd=1, lty=3)
legend("topleft",cex = 0.4, legend=c("De Facto", 
                                     "De Jure", 
                                     "Overall",
                                     "Trade Freedom", 
                                     "Dispute"),
       lty=c(1,1,1,1,3,3), 
       col=c("firebrick", 
             "forestgreen", 
             "dodgerblue",
             "gold", 
             "black"))
# ----------- China
subset <- data.nas[data.nas$country == "China",]
subset <- subset[,c("country",
                    "year",
                      "WEF_defacto_score",
                      "EF_PR_dejure_score",
                      "EF_overall_score",
                      "EF_trade_freedom")]
library(dplyr)
subset <- subset %>% arrange(country, year)
plot(subset$year,subset$WEF_defacto_score,
     type = "l",
     lwd=2,col="firebrick",
     xlab = "Time",
     ylim=c(-2, 2),
     ylab = "Standardized Value",
     main = "China")
lines(subset$year,subset$EF_PR_dejure_score, col="forestgreen",lwd=2)
lines(subset$year,subset$EF_overall_score, col="dodgerblue",lwd=2)
lines(subset$year,subset$EF_trade_freedom, col="gold",lwd=2)
legend("topleft",cex = 0.4, legend=c("De Facto", 
                                     "De Jure", 
                                     "Overall",
                                     "Trade Freedom", 
                                     "Trade War", 
                                     "Dispute"),
                            lty=c(1,1,1,1,3,3), 
                            col=c("firebrick", 
                                  "forestgreen", 
                                  "dodgerblue",
                                  "gold", 
                                  "red", 
                                  "black"))
abline(v = 2018.25, col="black", lwd=1, lty=3)
abline(v = 2008, col="black", lwd=1, lty=3)
abline(v = 2007.333, col="black", lwd=1, lty=3)
abline(v = 2018.5, col="black", lwd=1, lty=3)
abline(v = 2017, col="black", lwd=1, lty=3)
abline(v = 2016, col="red", lwd=1, lty=3)
# ----------------------------------------------------------

