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


