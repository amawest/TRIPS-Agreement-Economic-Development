# --------------------------------------------------------
# Final Fixed Effects Graph
# ADD ERROR BANDS / Can I use plotmeans for multiple graphs?
# Also add individual country graphs that were affected by TRIPS
# !!Add a line in the year they incurred a TRIPS violation
# --------------------------------------------------------
#install.packages("gplots")
#install.packages("data.1")
library(gplots)
par(mfrow=c(1,2)) 

# Time Fixed Effects -- include these graphs small in paper.
# ----
# Overall
plotmeans(EF_overall_score ~ year, 
          main = "Laws Overall, Balanced",
          data = data.no.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
plotmeans(EF_overall_score ~ year, 
          main = "Laws Overall, Unbalanced",
          data = data.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
# De Jure
plotmeans(EF_PR_dejure_score ~ year, 
          main = "PR Law, Balanced", 
          data = data.no.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
plotmeans(EF_PR_dejure_score ~ year, 
          main = "PR Law, Unbalanced",
          data = data.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)

# De Facto
plotmeans(WEF_defacto_score ~ year, 
          main = "Perception, Balanced",
          data = data.no.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
plotmeans(WEF_defacto_score ~ year, 
          main = "Perception, Unbalanced",
          data = data.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)

# Trade Freedom
plotmeans(EF_trade_freedom ~ year, 
          main = "Trade Freedom, Balanced",
          data = data.no.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
plotmeans(EF_trade_freedom ~ year, 
          main = "Trade Freedom, Unbalanced",
          data = data.nas,
          ylab = "Average Score Across Countries",
          xlab = "Year",
          n.label = FALSE)
# --------------------------------------------------------