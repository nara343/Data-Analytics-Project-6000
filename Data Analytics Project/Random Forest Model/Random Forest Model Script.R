# Using random forest to determine if we can predict each state
# Then take it a step further to see if we can classify each county for each state

library(caret)
library(randomForest)
library(randomForestExplainer)

# Using all the data that is available and removing some that capture total
# Population values 

df <- final_dataset[,-c(1:4, 18)]


plot_multi_way_importance(df)

Train <- createDataPartition(df$State, p = 0.7, list = FALSE)

training <- df[ Train, ]
testing <- df[ - Train, ]

set.seed(123)

random_forest <- randomForest(as.factor(State)~., 
                              data = training, 
                              ntree = 50,
                              importance =TRUE,
                              na.action=na.exclude)
random_forest

# No. of variables tried at each split: 5
# OOB estimate of  error rate: 6.24%
# Confusion matrix:
#   CA  NY WA class.error
# CA 157   6  8  0.08187135
# NY   3 161  0  0.01829268
# WA   8   1 73  0.10975610

plot(random_forest)

# Used this documentation to follow along and better understand the model that was 
# built 
# https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html

pred_w_random <- predict(random_forest, testing, type = "class")
tableCheck <- table(pred_w_random, testing$State)
tableCheck
# pred_w_random CA NY WA
# CA 72  0  3
# NY  0 69  2
# WA  0  0 30
sum(diag(tableCheck))/sum(tableCheck)
#0.972

#### Minimal Depth ####
min_depth_frame <- min_depth_distribution(random_forest)
plot_min_depth_distribution(min_depth_frame)

#### Importance Frame ####
importance_frame <- measure_importance(random_forest)

plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "gini_decrease", size_measure = "p_value", no_of_labels = 5)

plot_importance_ggpairs(importance_frame)

plot_importance_rankings(importance_frame)


vars <- important_variables(importance_frame, k = 10, measures = c("mean_min_depth", "no_of_trees"))
# [1] "Average.Cost.of.Insurance.With.Employer"                                                          "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit"
# [3] "Average.Misc.Expense"                                                                             "Average.Transportation.Expense"                                                                  
# [5] "Percent..HOUSING.TENURE..Occupied.housing.units..Owner.occupied"                                  "Percent..HOUSING.TENURE..Occupied.housing.units..Renter.occupied"                                
# [7] "Total.Cost"                                                                                       "Percent..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units"                         
# [9] "Households..Estimate...50.000.to..74.999"                                                         "Households..Estimate...75.000.to..99.999"


interactions_frame <- min_depth_interactions(random_forest, vars)


plot_predict_interaction(random_forest, grid=100,df, "Average.Cost.of.Insurance.With.Employer", "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit")

