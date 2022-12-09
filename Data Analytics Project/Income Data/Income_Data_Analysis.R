# Removing columns that contain lots of null values 
library(ggplot2)
return_col_means <- function(df){
  names <- c()
  val <- c()
  for(i in colnames(df)) { 
    print(i)
      mn <- mean(df[[i]])
      names<- append(names, c(i))
      val <- append(val, c(mn))
  }
  return(data.frame(group = names,
                    values = val))
}
return_subset_by_state <- function(df, state){
  
  subset_df <- df[grep(state, df$Geographic.Area.Name), ]
  
  return (subset_df)
}


remove_nas <- function(df, Null) {
  totalRows <- nrow(df)
  print(totalRows)
  index = c()
  for ( i in colnames(df) ){
    found = nrow(df[df[[i]] == Null, ])
    if(found >= totalRows*0.5){
      index <- append(index, c(FALSE))
    }
    else{ index <- append(index, c(TRUE))}
  }
  return(index)
}

#Removing Null Values Based On if There are more than 50, "NULL" values 
# entry in the estimate and margin of error columns indicates that data for this geographic area cannot be displayed because the number of sample cases is too small

Income_data_ACS_2015 <- Income_data_ACS_2015[, remove_nas(Income_data_ACS_2015, "null")]
Income_data_ACS_2016 <- Income_data_ACS_2016[, remove_nas(Income_data_ACS_2016, "null")]
Income_data_ACS_2017 <- Income_data_ACS_2017[, remove_nas(Income_data_ACS_2017, "null")]
Income_data_ACS_2018 <- Income_data_ACS_2018[, remove_nas(Income_data_ACS_2018, "null")]
Income_data_ACS_2019 <- Income_data_ACS_2019[, remove_nas(Income_data_ACS_2019, "null")]
Income_data_ACS_2021 <- Income_data_ACS_2021[, remove_nas(Income_data_ACS_2021, "null")]



household_features <- c("Geographic.Area.Name",
                        "Households..Estimate..Total",
                        "Households..Estimate..Less.than..10.000",  
                        "Households..Estimate...10.000.to..14.999",   
                        "Households..Estimate...15.000.to..24.999",  
                        "Households..Estimate...25.000.to..34.999",  
                        "Households..Estimate...35.000.to..49.999",                                                                       
                        "Households..Estimate...50.000.to..74.999",  
                        "Households..Estimate...75.000.to..99.999", 
                        "Households..Estimate...100.000.to..149.999",  
                        "Households..Estimate...150.000.to..199.999",  
                        "Households..Estimate...150.000.to..199.999", 
                        "Households..Estimate...200.000.or.more")  

colnames(Income_data_ACS_2018)

feature_names_updated <- c("Geographic.Area.Name",
                           "Estimate..Households..Total",
                           "Estimate..Households..Total..Less.than..10.000",  
                           "Estimate..Households..Total...10.000.to..14.999",   
                           "Estimate..Households..Total...15.000.to..24.999",  
                           "Estimate..Households..Total...25.000.to..34.999",  
                           "Estimate..Households..Total...35.000.to..49.999",                                                                       
                           "Estimate..Households..Total...50.000.to..74.999",  
                           "Estimate..Households..Total...75.000.to..99.999", 
                           "Estimate..Households..Total...100.000.to..149.999",  
                           "Estimate..Households..Total...150.000.to..199.999", 
                           "Estimate..Households..Total...200.000.or.more")

idx <- match(feature_names_updated, names(Income_data_ACS_2018))
idx
Household_2018 <-Income_data_ACS_2018[, idx ]
Household_2019 <- Income_data_ACS_2019[, idx ]
Household_2021 <- Income_data_ACS_2021[, idx ]

#### California  ####
library(ggplot2)
# 2018
california_2018_household <- return_subset_by_state(Household_2018, "California")

california_2018_household <- california_2018_household[, -c(1:2)]

means_for_each_category <- return_col_means(california_2018_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

california_income_brackets_2018 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In California") + 
  ylab("Income Bracket ") + 
  labs(title = "Percent of Total Household Borken Down by Income Bracket 2018")
california_income_brackets_2018
  
# 2019
california_2019_household <- return_subset_by_state(Household_2019, "California")

california_2019_household <- california_2019_household[, -c(1:2)]

means_for_each_category <- return_col_means(california_2019_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

california_income_brackets_2019 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In California") + 
  ylab("Income Bracket ") + 
  labs(title = "Percent of Total Household Borken Down by Income Bracket 2019")

california_income_brackets_2019

# 2021

california_2021_household <- return_subset_by_state(Household_2021, "California")

california_2021_household <- california_2021_household[, -c(1:2)]

means_for_each_category <- return_col_means(california_2021_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

california_income_brackets_2021 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "green4", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In California") + 
  ylab("Income Bracket ") + 
  labs(title = "Percent of Total Household Borken Down by Income Bracket 2021")

california_income_brackets_2021

#### New York  ####

# 2018

NY_2018_household <- return_subset_by_state(Household_2018, "New York")

NY_2018_household <- NY_2018_household[, -c(1:2)]

means_for_each_category <- return_col_means(NY_2018_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

NY_income_brackets_2018 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In New York") + 
  ylab("Income Bracket ") + 
  labs(title = "New York Percent of Total Household Borken Down by Income Bracket 2018")

NY_income_brackets_2018

# 2019

NY_2019_household <- return_subset_by_state(Household_2019, "New York")

NY_2019_household <- NY_2019_household[, -c(1:2)]

means_for_each_category <- return_col_means(NY_2019_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

NY_income_brackets_2019 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In New York") + 
  ylab("Income Bracket ") + 
  labs(title = "New York Percent of Total Household Borken Down by Income Bracket 2019")

NY_income_brackets_2019

# 2021

NY_2021_household <- return_subset_by_state(Household_2021, "New York")

NY_2021_household <- NY_2021_household[, -c(1:2)]

means_for_each_category <- return_col_means(NY_2021_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

NY_income_brackets_2021 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "green4", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In New York") + 
  ylab("Income Bracket ") + 
  labs(title = "New York Percent of Total Household Borken Down by Income Bracket 2021")

NY_income_brackets_2021

#### Washington  ####

# 2018

WA_2018_household <- return_subset_by_state(Household_2018, "Washington")

WA_2018_household <- WA_2018_household[, -c(1:2)]

means_for_each_category <- return_col_means(WA_2018_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

WA_income_brackets_2018 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In Washington") + 
  ylab("Income Bracket ") + 
  labs(title = "Washington Percent of Total Household Borken Down by Income Bracket 2018")

WA_income_brackets_2018

# 2019 
WA_2019_household <- return_subset_by_state(Household_2019, "Washington")

WA_2019_household <- WA_2019_household[, -c(1:2)]

means_for_each_category <- return_col_means(WA_2019_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

WA_income_brackets_2019 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In New York") + 
  ylab("Income Bracket ") + 
  labs(title = "Washington Percent of Total Household Borken Down by Income Bracket 2019")

WA_income_brackets_2019

#2021
WA_2021_household <- return_subset_by_state(Household_2021, "Washington")

WA_2021_household <- WA_2021_household[, -c(1:2)]

means_for_each_category <- return_col_means(WA_2021_household)
means_for_each_category$group <- factor(means_for_each_category$group, 
                                        levels = means_for_each_category$group)

WA_income_brackets_2021 <- ggplot(means_for_each_category, aes(x = values, y = group)) +
  geom_bar(stat = "identity", fill = "green4", alpha = 0.5) + 
  scale_y_discrete(limits=rev) + 
  xlab("Percent of Total Household In Washington") + 
  ylab("Income Bracket ") + 
  labs(title = "Washington Percent of Total Household Borken Down by Income Bracket 2021")

WA_income_brackets_2021
