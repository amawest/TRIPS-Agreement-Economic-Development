# Messy Code That I Should Fix / Take Out Entirely
# ---------------------------------------------------------------------------
# Standardizing to 100 instead of 7 (for easy interpretation)
# Doesn't work like this, lol. 
# ---------------------------------------------------------------------------
data.nas$WEF_GCI_score <- data.nas$WEF_GCI_score * 14.2857142857
data.no.nas$WEF_GCI_score <- data.no.nas$WEF_GCI_score * 14.2857142857
max(data.no.nas$WEF_GCI_score) # 94.28571 looks good
colSums(is.na(data.nas))
colSums(is.na(data.no.nas))
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# GCI Score from WEF (perception of local businessmen; dummy and continuous)
# ---------------------------------------------------------------------------
didreg9 <- lm(EF_property_rights_law ~ treatment_TRIPS
              + factor(country) - 1 
              + factor(year) - 1, data = data.nas)  
didreg10 <- lm(EF_property_rights_law ~ treatment_TRIPS
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 
didreg11 <- lm(EF_property_rights_law ~ borders_treated
               + factor(country) - 1 
               + factor(year) - 1, data = data.nas)  
didreg12 <- lm(EF_property_rights_law ~ borders_treated
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 
summary(didreg9) # ~= 11.043*  
summary(didreg10) # ~= 0.4412 
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# --------------------------------------------------------------------
# HIGH INCOME COUNTRIES
data.1 <- data[data$country == "Australia" |
                 data$country == "Brunei" |
                 data$country == "Canada" | 
                 data$country == "Chile" | 
                 data$country == "France" | 
                 data$country == "Germany" | 
                 data$country == "Hungary" |
                 data$country == "Ireland" | 
                 data$country == "Israel" | 
                 data$country == "Italy" | 
                 data$country == "Japan"|
                 data$country == "Netherlands"|
                 data$country == "New Zealand"|
                 data$country == "Poland"|
                 data$country == "Saudi Arabia"|
                 data$country == "Singapore"|
                 data$country == "South Korea"|
                 data$country == "Spain"|
                 data$country == "Sweden"|
                 data$country == "Switzerland"|
                 data$country == "Taiwan"|
                 data$country == "United Arab Emirates"|
                 data$country == "United Kingdom"|
                 data$country == "United States",]

plotmeans(WEF_defacto_score ~ country,
          main="Figure 3. Heterogeneity across high income countries", 
          xlab = "Country",
          ylab = "Average Country Score",
          barcol="white", 
          data=data.1)

# LOW AND MIDDLE INCOME COUNTRIES
data.2 <- data[data$country =="Egypt"|
                 data$country == "India"|
                 data$country == "Indonesia"|
                 data$country == "Kenya"|
                 data$country == "Morocco"|
                 data$country == "Nigeria"|
                 data$country == "Pakistan"|
                 data$country == "Philippines"|
                 data$country == "Ukraine"|
                 data$country == "Vietnam"|
                 data$country == "Argentina"|
                 data$country == "Brazil"|
                 data$country == "China"|
                 data$country == "Colombia"|
                 data$country == "Costa Rica"|
                 data$country == "Ecuador"|
                 data$country == "Jordan"|
                 data$country == "Malaysia"|
                 data$country == "Mexico"|
                 data$country == "Peru"|
                 data$country == "Russia"|
                 data$country == "South Africa"|
                 data$country == "Thailand"|
                 data$country == "Turkey"|
                 data$country == "Venezuela",]

plotmeans(GCI_score ~ country,
          main="Figure 3. Heterogeneity across low and middle income countries", 
          xlab = "Country",
          ylab = "Average Country Score",
          barcol="white", 
          data=data.2)

#make the names
labs <- paste(names(table(data.nas$country)), "") 
#create new plot
par(new=T)
#count the unique factors to make a vector of their numbers
# to serve as the positions on the x axis
a<-rev(as.numeric(unique(data$country))) 
#insert the text on the graph.
text(cex=1, x=a, y=0.2, labs, xpd=TRUE, srt=35) 