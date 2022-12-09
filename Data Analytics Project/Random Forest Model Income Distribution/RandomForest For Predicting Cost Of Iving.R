## We are going to be generating random data points for each county
# For each state. We will be using the household income data
# Along side the total cost of essentials for each county.
# The goal of this is can we predict was the ratio of income/total cost will 
# be depending on the area we are in. 
# Three models will be built one for each state 

generate_data <- function(df, sampleSize) { 
  county <- c()
  state <- c()
  total <- c()
  income <-c()
  ratio <-c()
  for(row in 1:nrow(df)){
    
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate..Less.than..10.000"]/100) ){
      randomIncome <- runif(1,0,9999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    for(i in 1:(sampleSize * df[row, "Households..Estimate...10.000.to..14.999"]/100) ){
      randomIncome <- runif(1,10000,14999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...15.000.to..24.999"]/100) ){
      randomIncome <- runif(1,15000,24999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...25.000.to..34.999"]/100) ){
      randomIncome <- runif(1,25000,34999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...35.000.to..49.999"]/100) ){
      randomIncome <- runif(1,35000,49999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...50.000.to..74.999"]/100) ){
      randomIncome <- runif(1,50000,74999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...75.000.to..99.999"]/100) ){
      randomIncome <- runif(1,75000,99999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...100.000.to..149.999"]/100) ){
      randomIncome <- runif(1,100000,149999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...150.000.to..199.999"]/100) ){
      randomIncome <- runif(1,150000,199999)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }
    
    for(i in 1:(sampleSize * df[row, "Households..Estimate...100.000.to..149.999"]/100) ){
      randomIncome <- runif(1,200000,1000000)
      county <- c(county,df[row,"Geographic.Area.Name"])
      state <- c(state,df[row,"State"])
      total <- c(total, df[row,"Total.Cost"])
      income <- c(income,randomIncome )
      ratio <- c(ratio, randomIncome/df[row,"Total.Cost"])
    }

  }
  generated_df <- data.frame(county,state,total,income,ratio)
  return(generated_df)
  
}

df_california <- final_dataset[final_dataset$State == "CA",]
generated_data_points <- generate_data(df_california, 100)


library(ggplot2)

ggplot(generated_data_points, aes(x = ratio)) + 
  geom_histogram(binwidth = 1)

summary(generated_data_points)

# county             state               total           income             ratio         
# Length:24510       Length:24510       Min.   :21979   Min.   :     9.9   Min.   : 0.00041  
# Class :character   Class :character   1st Qu.:27014   1st Qu.: 36277.2   1st Qu.: 1.11258  
# Mode  :character   Mode  :character   Median :31307   Median : 73804.3   Median : 2.24521  
# Mean   :34219   Mean   :152033.0   Mean   : 4.57500  
# 3rd Qu.:38889   3rd Qu.:141867.0   3rd Qu.: 4.22250  
# Max.   :65213   Max.   :999864.3   Max.   :42.88231

# We need to filter some of our own generated data points as it has become to large
Q1 <- 1.11258  
Q3 <- 4.22250  
IQR_ratio <- Q3 - Q1

Lower_ratio <- Q1 - (1.5*IQR_ratio)
Upper_ratio <- Q3 + (1.5*IQR_ratio)

# Filtering out data using our IQR value (Fences method)

lower <- generated_data_points[generated_data_points$ratio < Lower_ratio,]
upper <- generated_data_points[generated_data_points$ratio > Upper_ratio,]

print(nrow(lower) + nrow(upper))

# Getting Rid of 3188 data entries 

clean_generated_data <- generated_data_points[generated_data_points$ratio >= Lower_ratio,]
clean_generated_data <- generated_data_points[generated_data_points$ratio <= Upper_ratio,]

ggplot(clean_generated_data, aes(x = ratio, fill = county)) + 
  geom_histogram(binwidth = 0.5) + 
  xlab("Ratio") + 
  ylab("Count") + 
  ggtitle("Distribution of Ratios, binsize = 0.5")

clean_generated_data <- clean_generated_data[,-c(2)]
clean_generated_data <- clean_generated_data[,-c(4)]

#### Modeling ####
library(rpart)

Train <- createDataPartition(clean_generated_data$total, p = 0.7, list = FALSE)

training <- clean_generated_data[ Train, ]
testing <- clean_generated_data[ - Train, ]

set.seed(123)

random_forest <- randomForest(total~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)
random_forest

# Call:
#   randomForest(formula = total ~ ., data = training, ntree = 50,      importance = TRUE, na.action = na.exclude) 
# Type of random forest: regression
# Number of trees: 50
# No. of variables tried at each split: 1
# 
# Mean of squared residuals: 5506245
# % Var explained: 93.78

plot(random_forest)
importance_frame <- measure_importance(random_forest)
importance_frame

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value")



##################################################################################
#### New York #### 
df_NY <- final_dataset[final_dataset$State == "NY",]
generated_data_points <- generate_data(df_NY, 100)


ggplot(generated_data_points, aes(x = ratio)) + 
  geom_histogram(binwidth = 1)+
  ggtitle("Distribution of Ratios for NY")

summary(generated_data_points)

# county             state               total           income             ratio         
# Length:24154       Length:24154       Min.   :19904   Min.   :     2.8   Min.   : 0.00009  
# Class :character   Class :character   1st Qu.:22744   1st Qu.: 33865.0   1st Qu.: 1.32929  
# Mode  :character   Mode  :character   Median :24487   Median : 70530.5   Median : 2.74506  
# Mean   :26355   Mean   :145335.8   Mean   : 5.60795  
# 3rd Qu.:28695   3rd Qu.:135010.7   3rd Qu.: 5.13944  
# Max.   :44547   Max.   :999890.9   Max.   :49.39495  

# We need to filter some of our own generated data points as it has become to large
Q1 <- 1.32929    
Q3 <- 5.13944   
IQR_ratio <- Q3 - Q1

Lower_ratio <- Q1 - (1.5*IQR_ratio)
Upper_ratio <- Q3 + (1.5*IQR_ratio)

# Filtering out data using our IQR value (Fences method)

lower <- generated_data_points[generated_data_points$ratio < Lower_ratio,]
upper <- generated_data_points[generated_data_points$ratio > Upper_ratio,]

print(nrow(lower) + nrow(upper))

# Getting Rid of 3043 data entries 

clean_generated_data <- generated_data_points[generated_data_points$ratio >= Lower_ratio,]
clean_generated_data <- generated_data_points[generated_data_points$ratio <= Upper_ratio,]

ggplot(clean_generated_data, aes(x = ratio, fill = county)) + 
  geom_histogram(binwidth = 0.5) + 
  xlab("Ratio") + 
  ylab("Count") + 
  ggtitle("Distribution of Ratios, binsize = 0.5")

clean_generated_data <- clean_generated_data[,-c(2,5)]


#### Modeling ####

Train <- createDataPartition(clean_generated_data$total, p = 0.7, list = FALSE)

training <- clean_generated_data[ Train, ]
testing <- clean_generated_data[ - Train, ]

set.seed(123)

random_forest <- randomForest(total~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)
random_forest
# Call:
#   randomForest(formula = total ~ ., data = training, ntree = 50,      importance = TRUE, na.action = na.exclude) 
# Type of random forest: regression
# Number of trees: 50
# No. of variables tried at each split: 1
# 
# Mean of squared residuals: 2791977
# % Var explained: 88.93

plot(random_forest)

importance_frame <- measure_importance(random_forest)
importance_frame
# variable mean_min_depth no_of_nodes mse_increase node_purity_increase no_of_trees times_a_root p_value
# 1   county           0.78       27854   42589142.7         306797016053          50           21       1
# 2   income           0.56       46485     660330.1          43176728619          50           29       0

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value")


##################################################################################

#### WA #### 
df_WA <- final_dataset[final_dataset$State == "WA",]
generated_data_points <- generate_data(df_WA, 100)


ggplot(generated_data_points, aes(x = ratio)) + 
  geom_histogram(binwidth = 1)+
  ggtitle("Distribution of Ratios for WA")

summary(generated_data_points)

# county             state               total           income             ratio         
# Length:12346       Length:12346       Min.   :20077   Min.   :     0.7   Min.   : 0.00002  
# Class :character   Class :character   1st Qu.:23408   1st Qu.: 37719.9   1st Qu.: 1.47486  
# Mode  :character   Mode  :character   Median :25094   Median : 71636.2   Median : 2.79449  
# Mean   :25859   Mean   :147863.4   Mean   : 5.75902  
# 3rd Qu.:27814   3rd Qu.:133451.6   3rd Qu.: 5.16346  
# Max.   :38271   Max.   :999614.3   Max.   :49.10453 

# We need to filter some of our own generated data points as it has become to large
Q1 <- 1.47486      
Q3 <- 5.16346     
IQR_ratio <- Q3 - Q1

Lower_ratio <- Q1 - (1.5*IQR_ratio)
Upper_ratio <- Q3 + (1.5*IQR_ratio)

# Filtering out data using our IQR value (Fences method)

lower <- generated_data_points[generated_data_points$ratio < Lower_ratio,]
upper <- generated_data_points[generated_data_points$ratio > Upper_ratio,]

print(nrow(lower) + nrow(upper))

# Getting Rid of 3043 data entries 

clean_generated_data <- generated_data_points[generated_data_points$ratio >= Lower_ratio,]
clean_generated_data <- generated_data_points[generated_data_points$ratio <= Upper_ratio,]

ggplot(clean_generated_data, aes(x = ratio, fill = county)) + 
  geom_histogram(binwidth = 0.5) + 
  xlab("Ratio") + 
  ylab("Count") + 
  ggtitle("Distribution of Ratios Outliers Removed, binsize = 0.5")

clean_generated_data <- clean_generated_data[,-c(2,5)]


#### Modeling ####

Train <- createDataPartition(clean_generated_data$total, p = 0.7, list = FALSE)

training <- clean_generated_data[ Train, ]
testing <- clean_generated_data[ - Train, ]

set.seed(123)

random_forest <- randomForest(total~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)
random_forest
# Call:
#   randomForest(formula = total ~ ., data = training, ntree = 50,      importance = TRUE, na.action = na.exclude) 
# Type of random forest: regression
# Number of trees: 50
# No. of variables tried at each split: 1
# 
# Mean of squared residuals: 2336829
# % Var explained: 81.37

plot(random_forest)


importance_frame <- measure_importance(random_forest)
importance_frame

plot_multi_way_importance(importance_frame, x_measure = "mse_increase", y_measure = "node_purity_increase", size_measure = "p_value")









