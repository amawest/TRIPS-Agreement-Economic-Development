#--------------------------------------------------------------------------
# Final Bad Neighbor Centroid Mapping using Geospatial Data
#--------------------------------------------------------------------------
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "rgeos",
#                   "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rgeos")

# The package rnaturalearth provides a map of countries of the entire world.
# Use ne_countries to pull country data and choose the scale (rnaturalearthhires is necessary for scale = "large")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
ggplot(data = world) +
  geom_sf() + xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

#--------------------------------------------------------------------------
# Step 2: Adding Centroid Points in Red
#--------------------------------------------------------------------------
# Tutorial: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# For centroids: https://ropensci.github.io/CoordinateCleaner/reference/countryref.html#examples

library("tidyverse")
library("maps")
library("tools")
library("CoordinateCleaner") # contains countryref data

# There's 80 countries in the balanced panel
# 78 here - no EU, other 1 ???
#data(countryref)

rm(list=ls())
setwd("/Users/amandaAmanda/Desktop")
countryref <- read.csv("1_26_2019 - Bad_Neighbor_Balanced.csv") 

countryref <- countryref[,c(2,4:5)]
# Countries are put in twice; need to remove duplicate data
countryref <- countryref[duplicated(countryref$country) == FALSE,]
countryref <- countryref[countryref$name == "Zimbabwe" |
                           countryref$name == "Zambia" |
                           countryref$name == "Vietnam" |
                           countryref$name == "Venezuela" |
                           countryref$name == "Uruguay" |
                           countryref$name == "United States" |
                           countryref$name == "United Arab Emirates" |
                           countryref$name == "EU" |
                           countryref$name == "Ukraine" |
                           countryref$name == "Uganda" |
                           countryref$name == "Turkey" |
                           countryref$name == "Trinidad and Tobago" |
                           countryref$name == "Thailand" |
                           countryref$name == "Tanzania" |
                           countryref$name == "Taiwan" |
                           countryref$name == "Switzerland" |
                           countryref$name == "Sri Lanka" |
                           countryref$name == "South Africa" |
                           countryref$name == "South Korea" |
                           countryref$name == "Singapore" |
                           countryref$name == "Russia" |
                           countryref$name == "Qatar" |
                           countryref$name == "Philippines" |
                           countryref$name == "Peru" |
                           countryref$name == "Paraguay" |
                           countryref$name == "Panama" |
                           countryref$name == "Pakistan" |
                           countryref$name == "Norway" |
                           countryref$name == "Nigeria" |
                           countryref$name == "Nicaragua" |
                           countryref$name == "New Zealand" |
                           countryref$name == "Nepal" |
                           countryref$name == "Namibia" |
                           countryref$name == "Mozambique" |
                           countryref$name == "Morocco" |
                           countryref$name == "Mongolia" |
                           countryref$name == "Mexico" |
                           countryref$name == "Mauritius" |
                           countryref$name == "Mauritania" |
                           countryref$name == "Mali" |
                           countryref$name == "Malaysia" |
                           countryref$name == "Lesotho" |
                           countryref$name == "Kuwait" |
                           countryref$name == "Kenya" |
                           countryref$name == "Kazakhstan" |
                           countryref$name == "Jordan" |
                           countryref$name == "Japan" |
                           countryref$name == "Jamaica" |
                           countryref$name == "Israel" |
                           countryref$name == "Indonesia" |
                           countryref$name == "India" |
                           countryref$name == "Iceland" |
                           countryref$name == "Hong Kong SAR China" |
                           countryref$name == "Honduras" |
                           countryref$name == "Guatemala" |
                           countryref$name == "Georgia" |
                           countryref$name == "EU" |
                           countryref$name == "Ethiopia" |
                           countryref$name == "El Salvador" |
                           countryref$name == "Egypt" |
                           countryref$name == "Dominican Republic" |
                           countryref$name == "Costa Rica" |
                           countryref$name == "Colombia" |
                           countryref$name == "China" |
                           countryref$name == "Chile" |
                           countryref$name == "Chad" |
                           countryref$name == "Canada" |
                           countryref$name == "Cameroon" |
                           countryref$name == "Cambodia" |
                           countryref$name == "Burundi" |
                           countryref$name == "Brazil" |
                           countryref$name == "Botswana" |
                           countryref$name == "Bangladesh" |
                           countryref$name == "Bahrain" |
                           countryref$name == "Azerbaijan" |
                           countryref$name == "Australia" |
                           countryref$name == "Armenia" |
                           countryref$name == "Argentina" |
                           countryref$name == "Algeria" |
                           countryref$name == "Albania"
                           ,]
countryref <- na.omit(countryref)   
#write.csv(countryref, "spatial_data.csv")

(sites <- data.frame(longitude = countryref$centroid.lon, 
                     latitude = countryref$centroid.lat))
ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 1, 
             shape = 23, fill = "darkred") + 
  ggtitle("Map 1: World map with Centroids, Balanced Panel", subtitle = paste0("(", length(unique(countryref$country)), " countries)"))

#--------------------------------------------------------------------------
# Final Bad Neighbor, Standard Errors thru Clustering by Geographic Region
#--------------------------------------------------------------------------
data <- read.csv("1_26_2019 - Bad_Neighbor_Balanced.csv") 
# already standardized.

library(lfe)
library(Rcpp)
library(RcppArmadillo)
library(geosphere)
library(foreign)
library(reshape)
library(dplyr)
source("code/conley.R")

# WEF De Facto
m <- felm(WEF_defacto_score ~ treatment_TRIPS - 1
          | year + country | 0 | centroid.lat + centroid.lon,
          data = data, keepCX = TRUE)
coefficients(m) %>%round(3) 
summary(m)
SE <-ConleySEs(reg = m,
               unit = "country", 
               time = "year",
               lat = "centroid.lat", lon = "centroid.lon",
               dist_fn = "SH", dist_cutoff = 1000, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = FALSE) 
sapply(SE, sqrt) %>%round(3) 

# EF De Jure PR 
m <- felm(EF_PR_dejure_score ~ treatment_TRIPS - 1
          | year + country | 0 | centroid.lat + centroid.lon,
          data = data, keepCX = TRUE)
coefficients(m) %>%round(3) 
summary(m)
SE <-ConleySEs(reg = m,
               unit = "country", 
               time = "year",
               lat = "centroid.lat", lon = "centroid.lon",
               dist_fn = "SH", dist_cutoff = 1000, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = FALSE) 
sapply(SE, sqrt) %>%round(3) 

# Trade Freedom
m <- felm(EF_trade_freedom ~ treatment_TRIPS - 1
          | year + country | 0 | centroid.lat + centroid.lon,
          data = data, keepCX = TRUE)
coefficients(m) %>%round(3) 
summary(m)
SE <-ConleySEs(reg = m,
               unit = "country", 
               time = "year",
               lat = "centroid.lat", lon = "centroid.lon",
               dist_fn = "SH", dist_cutoff = 1000, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = FALSE) 
sapply(SE, sqrt) %>%round(3) 

# Overall Score
m <- felm(EF_overall_score ~ treatment_TRIPS - 1
          | year + country | 0 | centroid.lat + centroid.lon,
          data = data, keepCX = TRUE)
coefficients(m) %>%round(3) 
summary(m)
SE <-ConleySEs(reg = m,
               unit = "country", 
               time = "year",
               lat = "centroid.lat", lon = "centroid.lon",
               dist_fn = "SH", dist_cutoff = 1000, 
               lag_cutoff = 5,
               cores = 1, 
               verbose = FALSE) 
sapply(SE, sqrt) %>%round(3) 

# P-Values
z <- -.053 / .043
P <- exp(-0.717*z - 0.416*z^2)
1.286284

z <- .08/.043
P <- exp(-0.717*z - 0.416*z^2)
0.06242016

z <- .152/.036
P <- exp(-0.717*z - 0.416*z^2)
2.913903e-05

z <- .036/.05
P <- exp(-0.717*z - 0.416*z^2)
0.4809969

