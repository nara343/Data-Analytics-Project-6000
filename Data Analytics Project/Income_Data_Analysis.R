# Removing columns that contain lots of null values 

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
Income_data_ACS_2010 <- Income_data_ACS_2010[, remove_nas(Income_data_ACS_2010, "null")]
Income_data_ACS_2011 <- Income_data_ACS_2011[, remove_nas(Income_data_ACS_2011, "null")]
Income_data_ACS_2012 <- Income_data_ACS_2012[, remove_nas(Income_data_ACS_2012, "null")]
Income_data_ACS_2013 <- Income_data_ACS_2013[, remove_nas(Income_data_ACS_2013, "null")]
Income_data_ACS_2014 <- Income_data_ACS_2014[, remove_nas(Income_data_ACS_2014, "null")]
Income_data_ACS_2015 <- Income_data_ACS_2015[, remove_nas(Income_data_ACS_2015, "null")]
Income_data_ACS_2016 <- Income_data_ACS_2016[, remove_nas(Income_data_ACS_2016, "null")]
Income_data_ACS_2017 <- Income_data_ACS_2017[, remove_nas(Income_data_ACS_2017, "null")]
Income_data_ACS_2018 <- Income_data_ACS_2018[, remove_nas(Income_data_ACS_2018, "null")]
Income_data_ACS_2019 <- Income_data_ACS_2019[, remove_nas(Income_data_ACS_2019, "null")]
Income_data_ACS_2021 <- Income_data_ACS_2021[, remove_nas(Income_data_ACS_2021, "null")]


colnames(Income_data_ACS_2010)
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
                           "Estimate..Households..Total...150.000.to..199.999", 
                           "Estimate..Households..Total...200.000.or.more")

idx <- match(feature_names_updated, names(Income_data_ACS_2018))
idx
Household_2018 <-Income_data_ACS_2018[, idx ]
Household_2019 <- Income_data_ACS_2019[, idx ]
Household_2021 <- Income_data_ACS_2021[, idx ]

#### California  ####
library(ggplot2)

california_2018_household <- return_subset_by_state(Household_2018, "California")
california_2018_household <- california_2018_household[, -c(1)]

col_means <- colMeans(california_2018_household[paste0("Q", 1:12)])

col_means

#### New York  ####



#### Washington  ####