# ---------------------------------------------------------------------------
# Borders / Neighbor States
# ---------------------------------------------------------------------------
didreg9 <- lm(WEF_defacto_score ~ borders_treated
              + factor(country) - 1 
              + factor(year) - 1, data = data.nas)  
didreg10 <- lm(WEF_defacto_score ~ borders_treated
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 

didreg11 <- lm(EF_PR_dejure_score ~ borders_treated
               + factor(country) - 1 
               + factor(year) - 1, data = data.nas)  
didreg12 <- lm(EF_PR_dejure_score ~ borders_treated
               + factor(country) - 1 
               + factor(year) - 1, data = data.no.nas) 
