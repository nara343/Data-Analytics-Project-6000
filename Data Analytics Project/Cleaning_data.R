adjust_cost_by_inflation <- function(df1, df2, rate){
  
  df2$Average.Cost.Of.Rent = df1$Average.Cost.Of.Rent/ (1+rate)
  df2$Average.Cost.of.Insurance.With.Employer = df1$Average.Cost.of.Insurance.With.Employer/ (1+rate)
  df2$Average.Transportation.Expense = df1$Average.Transportation.Expense/ (1+rate)
  df2$Average.Misc.Expense = df1$Average.Misc.Expense/ (1+rate)
  df2$Total.Cost = df1$Total.Cost / (1+rate)
  
  return(df2)
  
}

remove_nas <- function(df, Null, rate=0.5) {
  totalRows <- nrow(df)
  index = c()
  for ( i in colnames(df) ){
    found = nrow(df[df[[i]] == Null, ])
    if(found >= totalRows*rate){
      index <- append(index, c(FALSE))
    }
    else{ index <- append(index, c(TRUE))}
  }
  return(index)
}

return_subset_by_state <- function(df, state){
  
  subset_df <- df[grep(state, df$Geographic.Area.Name), ]
  
  return (subset_df)
}

return_feature_subset_without_given_string <- function(df, subset){

  return (df[ , !grepl( subset , names( df ) ) ])
}

return_feature_subset_with_given_string <- function(df, subset){
  
  return (df[ , grepl( subset , names( df ) ) ])
}

return_joined_data <- function(first_df, second_df, feature_name){
  
  
  
  return (subset_df)
}


create_data_entries <- function(df, count){
  
  
  
}

#Estimates for average cost 

# Housing_data_ACS_2021$Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.owner.occupied.unit - 
# Housing_data_ACS_2021$Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit -  

#Removing Null Values Based On if There are more than 50, "NULL" values 
# entry in the estimate and margin of error columns indicates that data for this geographic area cannot be displayed because the number of sample cases is too small
Housing_data_ACS_2015 <- Housing_data_ACS_2015[, remove_nas(Housing_data_ACS_2015, "null")]
Housing_data_ACS_2016 <- Housing_data_ACS_2016[, remove_nas(Housing_data_ACS_2016, "null")]
Housing_data_ACS_2017 <- Housing_data_ACS_2017[, remove_nas(Housing_data_ACS_2017, "null")]
Housing_data_ACS_2018 <- Housing_data_ACS_2018[, remove_nas(Housing_data_ACS_2018, "null")]
Housing_data_ACS_2019 <- Housing_data_ACS_2019[, remove_nas(Housing_data_ACS_2019, "null")]
Housing_data_ACS_2021 <- Housing_data_ACS_2021[, remove_nas(Housing_data_ACS_2021, "null")]

#Removing Null Values Based On if There are more than 50, "Values with (X)"
# means that the estimate is not applicable or not available

Housing_data_ACS_2015 <- Housing_data_ACS_2015[, remove_nas(Housing_data_ACS_2015, "(X)")]
Housing_data_ACS_2016 <- Housing_data_ACS_2016[, remove_nas(Housing_data_ACS_2016, "(X)")]
Housing_data_ACS_2017 <- Housing_data_ACS_2017[, remove_nas(Housing_data_ACS_2017, "(X)")]
Housing_data_ACS_2018 <- Housing_data_ACS_2018[, remove_nas(Housing_data_ACS_2018, "(X)")]
Housing_data_ACS_2019 <- Housing_data_ACS_2019[, remove_nas(Housing_data_ACS_2019, "(X)")]
Housing_data_ACS_2021 <- Housing_data_ACS_2021[, remove_nas(Housing_data_ACS_2021, "(X)")]

# From looking at the data it looks like there are a lot of repretitive data, 
# We are given the total number for the type of unit and then the margins 
# And from there it is represented in a percentage. To simplify the data this might be broken down into
# by the feature type and might only keep the percent values. 

# Updating name for cost of living data 

colnames(cost_of_living_data_by_county)[2] <- "Geographic.Area.Name"

# Features wanted from Housing data 
colnames(Housing_data_ACS_2018)

#Removing the Margin of Error Features
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_data_ACS_2015, "Margin.of.Error")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_data_ACS_2016, "Margin.of.Error")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_data_ACS_2017, "Margin.of.Error")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_data_ACS_2018, "Margin.of.Error")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_data_ACS_2019, "Margin.of.Error")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_data_ACS_2021, "Margin.of.Error")

#Removing Year Built
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "YEAR.STRUCTURE.BUILT")

#Removing Unit Structure
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "UNITS.IN.STRUCTURE")

#Removing Unit BEDROOMS
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "BEDROOMS")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "BEDROOMS")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "BEDROOMS")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "BEDROOMS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "BEDROOMS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "BEDROOMS")

#Removing Unit ROOMS
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "ROOMS")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "ROOMS")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "ROOMS")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "ROOMS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "ROOMS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "ROOMS")


#Removing Unit VEHICLES.AVAILABLE
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "VEHICLES.AVAILABLE")

#Removing Unit HOUSE.HEATING.FUEL
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "HOUSE.HEATING.FUEL")

#Removing Unit MORTGAGE.STATUS
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "MORTGAGE.STATUS")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "MORTGAGE.STATUS")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "MORTGAGE.STATUS")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "MORTGAGE.STATUS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "MORTGAGE.STATUS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "MORTGAGE.STATUS")

#Removing Unit GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")

#Removing Unit SELECTED.MONTHLY.OWNER.COSTS
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "SELECTED.MONTHLY.OWNER.COSTS")

#Removing Unit VALUE
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "VALUE")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "VALUE")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "VALUE")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "VALUE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "VALUE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "VALUE")

#Removing Unit YEAR.HOUSHOLDER.MOVED.INTO
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")

#Removing Unit SELECTED.CHARACTERISTICS
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "SELECTED.CHARACTERISTICS")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "SELECTED.CHARACTERISTICS")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "SELECTED.CHARACTERISTICS")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "SELECTED.CHARACTERISTICS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "SELECTED.CHARACTERISTICS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "SELECTED.CHARACTERISTICS")

#Removing Unit OCCUPANTS.PER.ROOM
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "OCCUPANTS.PER.ROOM")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "OCCUPANTS.PER.ROOM")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "OCCUPANTS.PER.ROOM")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "OCCUPANTS.PER.ROOM")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "OCCUPANTS.PER.ROOM")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "OCCUPANTS.PER.ROOM")

#Removing Unit GROSS.RENT
Housing_Cleaned_data_2015 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2015, "GROSS.RENT")
Housing_Cleaned_data_2016 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2016, "GROSS.RENT")
Housing_Cleaned_data_2017 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2017, "GROSS.RENT")
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "GROSS.RENT")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "GROSS.RENT")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "GROSS.RENT")

# Feature names wanted from the income data 
feature_names_for_newer_datasets <- c("Geographic.Area.Name",
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

# Feature names wanted from the income data 

feature_names_for_older_datasets <- c("Geographic.Area.Name",
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
                                      "Households..Estimate...200.000.or.more")


idx_2015 <- match(feature_names_for_older_datasets, names(Income_data_ACS_2015))
idx_2016 <- match(feature_names_for_older_datasets, names(Income_data_ACS_2016))
idx_2017 <- match(feature_names_for_older_datasets, names(Income_data_ACS_2017))
idx_2018 <- match(feature_names_for_newer_datasets, names(Income_data_ACS_2018))
idx_2019 <- match(feature_names_for_newer_datasets, names(Income_data_ACS_2019))
idx_2021 <- match(feature_names_for_newer_datasets, names(Income_data_ACS_2021))

Income_Households_2015 <-Income_data_ACS_2015[, idx_2015 ]
Income_Households_2016 <-Income_data_ACS_2016[, idx_2016 ]
Income_Households_2017 <-Income_data_ACS_2017[, idx_2017 ]
Income_Households_2018 <-Income_data_ACS_2018[, idx_2018 ]
Income_Households_2019 <- Income_data_ACS_2019[, idx_2019 ]
Income_Households_2021 <- Income_data_ACS_2021[, idx_2021 ]

# Renaming the columns so each has the same column name so when merging it doesn't
# Cause an issue or create empty columns with NAs 
colnames(Income_Households_2018)[2] <- "Households..Estimate..Total"
colnames(Income_Households_2018)[3] <- "Households..Estimate..Less.than..10.000"
colnames(Income_Households_2018)[4] <- "Households..Estimate...10.000.to..14.999"
colnames(Income_Households_2018)[5] <- "Households..Estimate...15.000.to..24.999"
colnames(Income_Households_2018)[6] <- "Households..Estimate...25.000.to..34.999"
colnames(Income_Households_2018)[7] <- "Households..Estimate...35.000.to..49.999"
colnames(Income_Households_2018)[8] <- "Households..Estimate...50.000.to..74.999"
colnames(Income_Households_2018)[9] <- "Households..Estimate...75.000.to..99.999"
colnames(Income_Households_2018)[10] <- "Households..Estimate...100.000.to..149.999"
colnames(Income_Households_2018)[11] <- "Households..Estimate...150.000.to..199.999"
colnames(Income_Households_2018)[12] <- "Households..Estimate...200.000.or.more"

colnames(Income_Households_2019)[2] <- "Households..Estimate..Total"
colnames(Income_Households_2019)[3] <- "Households..Estimate..Less.than..10.000"
colnames(Income_Households_2019)[4] <- "Households..Estimate...10.000.to..14.999"
colnames(Income_Households_2019)[5] <- "Households..Estimate...15.000.to..24.999"
colnames(Income_Households_2019)[6] <- "Households..Estimate...25.000.to..34.999"
colnames(Income_Households_2019)[7] <- "Households..Estimate...35.000.to..49.999"
colnames(Income_Households_2019)[8] <- "Households..Estimate...50.000.to..74.999"
colnames(Income_Households_2019)[9] <- "Households..Estimate...75.000.to..99.999"
colnames(Income_Households_2019)[10] <- "Households..Estimate...100.000.to..149.999"
colnames(Income_Households_2019)[11] <- "Households..Estimate...150.000.to..199.999"
colnames(Income_Households_2019)[12] <- "Households..Estimate...200.000.or.more"


colnames(Income_Households_2021)[2] <- "Households..Estimate..Total"
colnames(Income_Households_2021)[3] <- "Households..Estimate..Less.than..10.000"
colnames(Income_Households_2021)[4] <- "Households..Estimate...10.000.to..14.999"
colnames(Income_Households_2021)[5] <- "Households..Estimate...15.000.to..24.999"
colnames(Income_Households_2021)[6] <- "Households..Estimate...25.000.to..34.999"
colnames(Income_Households_2021)[7] <- "Households..Estimate...35.000.to..49.999"
colnames(Income_Households_2021)[8] <- "Households..Estimate...50.000.to..74.999"
colnames(Income_Households_2021)[9] <- "Households..Estimate...75.000.to..99.999"
colnames(Income_Households_2021)[10] <- "Households..Estimate...100.000.to..149.999"
colnames(Income_Households_2021)[11] <- "Households..Estimate...150.000.to..199.999"
colnames(Income_Households_2021)[12] <- "Households..Estimate...200.000.or.more"


Housing_and_Income_data_2015 <- merge(Housing_Cleaned_data_2015, Income_Households_2015,
                                      by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                      all.x =  TRUE)

Housing_and_Income_data_2016 <- merge(Housing_Cleaned_data_2016, Income_Households_2016,
                                      by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                      all.x =  TRUE)

Housing_and_Income_data_2017 <- merge(Housing_Cleaned_data_2017, Income_Households_2017,
                                      by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                      all.x =  TRUE)


Housing_and_Income_data_2018 <- merge(Housing_Cleaned_data_2018, Income_Households_2018,
                                 by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                 all.x =  TRUE)

Housing_and_Income_data_2019 <- merge(Housing_Cleaned_data_2019, Income_Households_2019,
                                 by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                 all.x =  TRUE)

Housing_and_Income_data_2021 <- merge(Housing_Cleaned_data_2021, Income_Households_2021,
                                 by.x = "Geographic.Area.Name", by.y = "Geographic.Area.Name",
                                 all.x =  TRUE)

# Adjusting cost of living data
# For each to account for inflation or the change of prices we have to multiply it by the
# inflation rate that given year or the cumilation.
# The cost of living data has been adjusted to represent the cost of 2021
# To get the previous years we need to account for the inflation between those year
# (Cost)/(1+rate)

# From 2015 to 2021 using the average inflation rate for each year 
# 2021 - 4.7% 
# 2020 - 1.2%
# 2019 - 1.81%
# 2018 - 2.44%
# 2017 - 2.13%
# 2016 - 1.26%
# 2015 - 0.12%
cost_of_living_2015 <- cost_of_living_data_by_county
cost_of_living_2015 <- adjust_cost_by_inflation(cost_of_living_2015,cost_of_living_data_by_county, 0.1354 )

cost_of_living_2016 <- cost_of_living_data_by_county
cost_of_living_2016 <- adjust_cost_by_inflation(cost_of_living_2016,cost_of_living_data_by_county, 0.1228 )

cost_of_living_2017 <- cost_of_living_data_by_county
cost_of_living_2017 <- adjust_cost_by_inflation(cost_of_living_2017,cost_of_living_data_by_county, 0.1015 )

cost_of_living_2018 <- cost_of_living_data_by_county
cost_of_living_2018 <- adjust_cost_by_inflation(cost_of_living_2018,cost_of_living_data_by_county, 0.0771 )

cost_of_living_2019 <- cost_of_living_data_by_county
cost_of_living_2019 <- adjust_cost_by_inflation(cost_of_living_2015,cost_of_living_data_by_county, 0.059 )

# Adding Cost of Living Data 
Housing_and_Income_data_2015 <- merge(Housing_and_Income_data_2015, cost_of_living_2015,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")

Housing_and_Income_data_2016 <- merge(Housing_and_Income_data_2016, cost_of_living_2016,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")

Housing_and_Income_data_2017 <- merge(Housing_and_Income_data_2017, cost_of_living_2017,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")

Housing_and_Income_data_2018 <- merge(Housing_and_Income_data_2018, cost_of_living_2018,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")


Housing_and_Income_data_2019 <- merge(Housing_and_Income_data_2019, cost_of_living_2019,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")

Housing_and_Income_data_2021 <- merge(Housing_and_Income_data_2021, cost_of_living_data_by_county,
                                      by.x = "Geographic.Area.Name", by.y = "County.And.State")


# Combining all into one table 
# Updating columname for 2018 dataset

colnames(Housing_and_Income_data_2018)[13] <- "Percent..HOUSING.OCCUPANCY..Total.housing.units"
colnames(Housing_and_Income_data_2018)[14] <- "Percent..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units"
colnames(Housing_and_Income_data_2018)[15] <- "Percent..HOUSING.OCCUPANCY..Total.housing.units..Vacant.housing.units"
colnames(Housing_and_Income_data_2018)[16] <- "Percent..HOUSING.TENURE..Occupied.housing.units"
colnames(Housing_and_Income_data_2018)[17] <- "Percent..HOUSING.TENURE..Occupied.housing.units..Owner.occupied"
colnames(Housing_and_Income_data_2018)[18] <- "Percent..HOUSING.TENURE..Occupied.housing.units..Renter.occupied"

colnames(Housing_and_Income_data_2015)[9] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Homeowner.vacancy.rate"
colnames(Housing_and_Income_data_2015)[10] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Rental.vacancy.rate"
colnames(Housing_and_Income_data_2015)[17] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.owner.occupied.unit"
colnames(Housing_and_Income_data_2015)[18] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit"

colnames(Housing_and_Income_data_2016)[9] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Homeowner.vacancy.rate"
colnames(Housing_and_Income_data_2016)[10] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Rental.vacancy.rate"
colnames(Housing_and_Income_data_2016)[17] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.owner.occupied.unit"
colnames(Housing_and_Income_data_2016)[18] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit"

colnames(Housing_and_Income_data_2017)[9] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Homeowner.vacancy.rate"
colnames(Housing_and_Income_data_2017)[10] <- "Estimate..HOUSING.OCCUPANCY..Total.housing.units..Rental.vacancy.rate"
colnames(Housing_and_Income_data_2017)[17] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.owner.occupied.unit"
colnames(Housing_and_Income_data_2017)[18] <- "Estimate..HOUSING.TENURE..Occupied.housing.units..Average.household.size.of.renter.occupied.unit"



final_dataset <- rbind(Housing_and_Income_data_2015, Housing_and_Income_data_2016)
final_dataset <- rbind(final_dataset, Housing_and_Income_data_2017)
final_dataset <- rbind(final_dataset, Housing_and_Income_data_2018)
final_dataset <- rbind(final_dataset, Housing_and_Income_data_2019)
final_dataset <- rbind(final_dataset, Housing_and_Income_data_2021)


final_dataset <- final_dataset[,-c(2)]
final_dataset <- final_dataset[,-c(30)]
