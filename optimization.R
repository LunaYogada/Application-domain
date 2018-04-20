source("data_clean.R")
library(lpSolve)

retail_tib<-retail%>%
  group_by(group)%>%
  nest()%>%
  mutate(model = map(data, ~lm(`Items Sold` ~ `Shelf Spaces` + `Average Price`+0,data = .x))) %>%
  mutate(summary = map(model,summary))

retail_tib$summary

#result-----

# Call:
#   lm(formula = `Items Sold` ~ `Shelf Spaces` + `Average Price` + 
#        0, data = .x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1614.7  -375.1   258.2   731.2  2591.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# `Shelf Spaces`    108.32      22.84   4.742 6.64e-05 ***
#   `Average Price`   -33.46      16.02  -2.089   0.0466 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1204 on 26 degrees of freedom
# Multiple R-squared:  0.6255,	Adjusted R-squared:  0.5967 
# F-statistic: 21.72 on 2 and 26 DF,  p-value: 2.848e-06

# Call:
#   lm(formula = `Items Sold` ~ `Shelf Spaces` + `Average Price` + 
#        0, data = .x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -463.50 -145.81  -40.71  240.15  946.73 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# `Shelf Spaces`    54.255      7.663   7.080 1.61e-07 ***
#   `Average Price`   -7.468      3.963  -1.884   0.0708 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 350.1 on 26 degrees of freedom
# Multiple R-squared:  0.8788,	Adjusted R-squared:  0.8695 
# F-statistic: 94.27 on 2 and 26 DF,  p-value: 1.217e-12
# 
# 
# Call:
#   lm(formula = `Items Sold` ~ `Shelf Spaces` + `Average Price` + 
#        0, data = .x)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1355.7  -334.0   337.0   479.4  2234.2 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# `Shelf Spaces`   114.632     14.726   7.784 2.94e-08 ***
#   `Average Price`  -10.550      6.816  -1.548    0.134    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 823.8 on 26 degrees of freedom
# Multiple R-squared:  0.8689,	Adjusted R-squared:  0.8588 
# F-statistic: 86.13 on 2 and 26 DF,  p-value: 3.392e-12

constraint.matrix <- matrix(c(1,0,0,0,0,0,
                              0,1,0,0,0,0,
                              0,0,1,0,0,0,
                              1,1,1,0,0,0,
                              0,0,0,1,0,0,
                              0,0,0,0,1,0,
                              0,0,0,0,0,1,
                              0,0,0,1,0,0,
                              0,0,0,0,1,0,
                              0,0,0,0,0,1),nrow = 10,byrow = T)

constraint.rhs <- c(5,5,5,60,16,19,22,40,48,55)

retail_lp<-lp(direction = "max",
   objective.in = c(108.32, -33.46, 54.253, -7.468, 114.632, -10.55),
   const.mat = constraint.matrix,
   const.dir = c(">=",">=", ">=","<=", ">=",">=", ">=", "<=","<=","<="),
   const.rhs = constraint.rhs
   )
retail_lp$solution

# 50  5  5 16 48 22



         