#### Make Sure to run Cleaning_data to have a clean data set

# This linear regression will be focusing on the number of housing units in each county
# And looking to see how it effects the total estimate cost of living


# Extracting out two variables 

df <- final_dataset[,c(2,29:30)]

# Graphinh a scatter plot of it 
library(ggplot2)


# Looking at the graph there are outlines and those need to be adjusted so we will
# using IQR to filter them out 

#### Before Any Data Cleaning ####

lm.fit <- lm(Average.Cost.Of.Rent~Estimate..HOUSING.OCCUPANCY..Total.housing.units, data = df)
summary(lm.fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -14461  -4256  -2211   2095  28920 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      1.238e+04  3.161e+02  39.176   <2e-16 ***
#   Estimate..HOUSING.OCCUPANCY..Total.housing.units 5.939e-03  6.387e-04   9.298   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6647 on 591 degrees of freedom
# Multiple R-squared:  0.1276,	Adjusted R-squared:  0.1261 
# F-statistic: 86.45 on 1 and 591 DF,  p-value: < 2.2e-16

coeff <- coefficients(lm.fit)
inter <- coeff[1]
Slope <- coeff[2]

ggplot(df, aes(x = Estimate..HOUSING.OCCUPANCY..Total.housing.units, y =Average.Cost.Of.Rent)) +
  geom_point(aes(fill =State, color = State)) + 
  xlab("Estimate Number Of Units") + 
  ylab("Estimate Average Rent Cost (USD) ") + 
  ggtitle("Scatter Plot of Housing Units and Average Rent Cross For CA, NY, WA")+
  geom_abline(intercept = inter, slope = Slope, color = "black", size = 1.5 )

#### Beging Data Cleaning ####


summary(df)
# Estimate..HOUSING.OCCUPANCY..Total.housing.units Average.Cost.Of.Rent
# Min.   :  20960                                  Min.   : 6397       
# 1st Qu.:  49684                                  1st Qu.: 8781       
# Median : 100617                                  Median :11342       
# Mean   : 249523                                  Mean   :13865       
# 3rd Qu.: 286905                                  3rd Qu.:16672       
# Max.   :3620201                                  Max.   :41966   

Q1 <- 8781
Q3 <- 16672
IQR_Average_Rent_Cost <- Q3 - Q1

Lower_Average_Rent_Fence <- Q1 - (1.5*IQR_Average_Rent_Cost)
Upper_Average_Rent_Fence <- Q3 + (1.5*IQR_Average_Rent_Cost)

# Filtering out data using our IQR value (Fences method)

lower <- df[df$Average.Cost.Of.Rent < Lower_Average_Rent_Fence,]
upper <- df[df$Average.Cost.Of.Rent > Upper_Average_Rent_Fence,]

print(nrow(lower) + nrow(upper))

# We have 27 rows that meet this critiria and it will be removed because of this reason

df <- df[df$Average.Cost.Of.Rent >= Lower_Average_Rent_Fence,]
df <- df[df$Average.Cost.Of.Rent <= Upper_Average_Rent_Fence,]
nrow(df)


# We still have some outliners for the total number of housing so we will also remove those 
Q1 <- 49684
Q3 <- 286905
IQR_Unit_Total <- Q3 - Q1

Lower_Units <- Q1 - (1.5*IQR_Unit_Total)
Upper_Units <- Q3 + (1.5*IQR_Unit_Total)

# Filtering out data using our IQR value (Fences method)

lower <- df[df$Estimate..HOUSING.OCCUPANCY..Total.housing.units < Lower_Units,]
upper <- df[df$Estimate..HOUSING.OCCUPANCY..Total.housing.units > Upper_Units,]

print(nrow(lower) + nrow(upper))

# We have 60 rows that meet this critiria and it will be removed because of this reason

df <- df[df$Estimate..HOUSING.OCCUPANCY..Total.housing.units >= Lower_Units,]
df <- df[df$Estimate..HOUSING.OCCUPANCY..Total.housing.units <= Upper_Units,]

#### Model Building With Filtered Data ####

lm.fit <- lm(Average.Cost.Of.Rent~Estimate..HOUSING.OCCUPANCY..Total.housing.units, data = df)
summary(lm.fit)
# Call:
#   lm(formula = Average.Cost.Of.Rent ~ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 
#      data = df)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -10537  -2620  -1038   1932  14121 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      9.608e+03  2.637e+02   36.43   <2e-16 ***
#   Estimate..HOUSING.OCCUPANCY..Total.housing.units 1.899e-02  1.346e-03   14.11   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4232 on 511 degrees of freedom
# Multiple R-squared:  0.2804,	Adjusted R-squared:  0.279 
# F-statistic: 199.1 on 1 and 511 DF,  p-value: < 2.2e-16

coeff <- coefficients(lm.fit)
inter <- coeff[1]
Slope <- coeff[2]

ggplot(df, aes(x = Estimate..HOUSING.OCCUPANCY..Total.housing.units, y =Average.Cost.Of.Rent)) +
  geom_point(aes(fill =State, color = State)) + 
  xlab("Estimate Number Of Units") + 
  ylab("Estimate Average Rent Cost (USD) ") + 
  ggtitle("Scatter Plot of Housing Units and Average Rent Cost For CA, NY, WA")+
  geom_abline(intercept = inter, slope = Slope, color = "black", size = 1.5 )


df_california <- df[df$State == "CA",]
summary(df_california)
# We still have some outliners for the total number of housing so we will also remove those 
Q1 <- 63160
Q3 <- 339106
IQR_Unit_Total <- Q3 - Q1

Lower_Units <- Q1 - (1.5*IQR_Unit_Total)
Upper_Units <- Q3 + (1.5*IQR_Unit_Total)


# We have 60 rows that meet this critiria and it will be removed because of this reason

df_california <- df_california[df_california$Estimate..HOUSING.OCCUPANCY..Total.housing.units >= Lower_Units,]
df_california <- df_california[df_california$Estimate..HOUSING.OCCUPANCY..Total.housing.units <= Upper_Units,]


lm.fit <- lm(Average.Cost.Of.Rent~Estimate..HOUSING.OCCUPANCY..Total.housing.units, data = df_california)
summary(lm.fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -8043  -3660  -1639   3973  11576 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      1.221e+04  5.175e+02   23.60   <2e-16 ***
#   Estimate..HOUSING.OCCUPANCY..Total.housing.units 1.609e-02  2.435e-03    6.61    4e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4770 on 185 degrees of freedom
# Multiple R-squared:  0.191,	Adjusted R-squared:  0.1867 
# F-statistic: 43.69 on 1 and 185 DF,  p-value: 3.996e-10

coeff <- coefficients(lm.fit)
inter <- coeff[1]
Slope <- coeff[2]

ggplot(df_california, aes(x = Estimate..HOUSING.OCCUPANCY..Total.housing.units, y =Average.Cost.Of.Rent)) +
  geom_point(aes(fill = State, color = State)) + 
  xlab("Estimate Number Of Units") + 
  ylab("Estimate Average Rent Cost (USD) ") + 
  ggtitle("Scatter Plot of Housing Units and Average Rent Cost For CA")+
  geom_abline(intercept = inter, slope = Slope, color = "black", size = 1.5 )


df_NY <- df[df$State == "NY",]

summary(df_NY)
# We still have some outliners for the total number of housing so we will also remove those 
Q1 <- 47369
Q3 <- 205492
IQR_Unit_Total <- Q3 - Q1

Lower_Units <- Q1 - (1.5*IQR_Unit_Total)
Upper_Units <- Q3 + (1.5*IQR_Unit_Total)


# We have 60 rows that meet this critiria and it will be removed because of this reason

df_NY <- df_NY[df_NY$Estimate..HOUSING.OCCUPANCY..Total.housing.units >= Lower_Units,]
df_NY <- df_NY[df_NY$Estimate..HOUSING.OCCUPANCY..Total.housing.units <= Upper_Units,]


lm.fit <- lm(Average.Cost.Of.Rent~Estimate..HOUSING.OCCUPANCY..Total.housing.units, data = df_NY)
summary(lm.fit)

# Call:
#   lm(formula = Average.Cost.Of.Rent ~ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 
#      data = df_NY)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9152.0 -1638.9  -591.7  1900.5 12914.5 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      8.217e+03  3.106e+02   26.46   <2e-16 ***
#   Estimate..HOUSING.OCCUPANCY..Total.housing.units 1.900e-02  1.513e-03   12.56   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3337 on 213 degrees of freedom
# Multiple R-squared:  0.4254,	Adjusted R-squared:  0.4227 
# F-statistic: 157.7 on 1 and 213 DF,  p-value: < 2.2e-16

coeff <- coefficients(lm.fit)
inter <- coeff[1]
Slope <- coeff[2]

ggplot(df_NY, aes(x = Estimate..HOUSING.OCCUPANCY..Total.housing.units, y =Average.Cost.Of.Rent)) +
  geom_point(fill = "green", color = "green4") + 
  xlab("Estimate Number Of Units") + 
  ylab("Estimate Average Rent Cost (USD) ") + 
  ggtitle("Scatter Plot of Housing Units and Average Rent Cost For NY")+
  geom_abline(intercept = inter, slope = Slope, color = "black", size = 1.5 )


df_WA <- df[df$State == "WA",]

summary(df_WA)
# We still have some outliners for the total number of housing so we will also remove those 
Q1 <- 37459                                   
Q3 <- 174714                                                      
IQR_Unit_Total <- Q3 - Q1

Lower_Units <- Q1 - (1.5*IQR_Unit_Total)
Upper_Units <- Q3 + (1.5*IQR_Unit_Total)


# We have 60 rows that meet this critiria and it will be removed because of this reason

df_WA <- df_WA[df_WA$Estimate..HOUSING.OCCUPANCY..Total.housing.units >= Lower_Units,]
df_WA <- df_WA[df_WA$Estimate..HOUSING.OCCUPANCY..Total.housing.units <= Upper_Units,]


lm.fit <- lm(Average.Cost.Of.Rent~Estimate..HOUSING.OCCUPANCY..Total.housing.units, data = df_WA)
summary(lm.fit)

# Call:
#   lm(formula = Average.Cost.Of.Rent ~ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 
#      data = df_WA)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6396  -2128   -642   1662  11387 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      8.633e+03  4.610e+02  18.728  < 2e-16 ***
#   Estimate..HOUSING.OCCUPANCY..Total.housing.units 1.939e-02  3.278e-03   5.914  3.9e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3265 on 109 degrees of freedom
# Multiple R-squared:  0.2429,	Adjusted R-squared:  0.236 
# F-statistic: 34.97 on 1 and 109 DF,  p-value: 3.901e-08


coeff <- coefficients(lm.fit)
inter <- coeff[1]
Slope <- coeff[2]

ggplot(df_WA, aes(x = Estimate..HOUSING.OCCUPANCY..Total.housing.units, y =Average.Cost.Of.Rent)) +
  geom_point(color = "steelblue3") + 
  xlab("Estimate Number Of Units") + 
  ylab("Estimate Average Rent Cost (USD) ") + 
  ggtitle("Scatter Plot of Housing Units and Average Rent Cost For WA")+
  geom_abline(intercept = inter, slope = Slope, color = "black", size = 1.5 )
