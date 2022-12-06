remove_nas <- function(df, Null, rate=0.5) {
  totalRows <- nrow(df)
  print(totalRows)
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
Housing_data_ACS_2018 <- Housing_data_ACS_2018[, remove_nas(Housing_data_ACS_2018, "null")]
Housing_data_ACS_2019 <- Housing_data_ACS_2019[, remove_nas(Housing_data_ACS_2019, "null")]
Housing_data_ACS_2021 <- Housing_data_ACS_2021[, remove_nas(Housing_data_ACS_2021, "null")]

#Removing Null Values Based On if There are more than 50, "Values with (X)"
# means that the estimate is not applicable or not available

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
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_data_ACS_2018, "Margin.of.Error")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_data_ACS_2019, "Margin.of.Error")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_data_ACS_2021, "Margin.of.Error")

#Removing Year Built
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "YEAR.STRUCTURE.BUILT")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "YEAR.STRUCTURE.BUILT")

#Removing Unit Structure
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "UNITS.IN.STRUCTURE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "UNITS.IN.STRUCTURE")

#Removing Unit BEDROOMS
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "BEDROOMS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "BEDROOMS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "BEDROOMS")

#Removing Unit ROOMS
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "ROOMS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "ROOMS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "ROOMS")


#Removing Unit VEHICLES.AVAILABLE
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "VEHICLES.AVAILABLE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "VEHICLES.AVAILABLE")

#Removing Unit HOUSE.HEATING.FUEL
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "HOUSE.HEATING.FUEL")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "HOUSE.HEATING.FUEL")

#Removing Unit MORTGAGE.STATUS
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "MORTGAGE.STATUS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "MORTGAGE.STATUS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "MORTGAGE.STATUS")

#Removing Unit GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "GROSS.RENT.AS.A.PERCENTAGE.OF.HOUSEHOLD.INCOME..GRAPI")

#Removing Unit YEAR.HOUSHOLDER.MOVED.INTO
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "SELECTED.MONTHLY.OWNER.COSTS")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "SELECTED.MONTHLY.OWNER.COSTS")

#Removing Unit YEAR.HOUSHOLDER.MOVED.INTO
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "VALUE")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "VALUE")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "VALUE")

#Removing Unit YEAR.HOUSHOLDER.MOVED.INTO
Housing_Cleaned_data_2018 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2018, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2019 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2019, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")
Housing_Cleaned_data_2021 <- return_feature_subset_without_given_string(Housing_Cleaned_data_2021, "YEAR.HOUSEHOLDER.MOVED.INTO.UNIT")

#Removing Unit YEAR.HOUSHOLDER.MOVED.INTO
Housing_Cleaned_data_2018 <- return_feature_subset_with_given_string(Housing_Cleaned_data_2018, "Percent")
Housing_Cleaned_data_2019 <- return_feature_subset_with_given_string(Housing_Cleaned_data_2019, "Percent")
Housing_Cleaned_data_2021 <- return_feature_subset_with_given_string(Housing_Cleaned_data_2021, "Percent")


# Feature names wanted from the income data 
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

Income_Households_2018 <-Income_data_ACS_2018[, idx ]
Income_Households_2019 <- Income_data_ACS_2019[, idx ]
Income_Households_2021 <- Income_data_ACS_2021[, idx ]





