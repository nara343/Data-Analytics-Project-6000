### Purpose of this script is to load the data from the .xlsx/.csv files

# Loading Income Data # 

# Working on Desktop
setwd("C:/Users/Naran/Data-Analytics-Project-6000/Data Analytics Project/Income Data")

Income_data_ACS_2015 <- read.csv("ACSST1Y2015.S1901-Data.csv", skip=1)
Income_data_ACS_2016 <- read.csv("ACSST1Y2016.S1901-Data.csv", skip=1)
Income_data_ACS_2017 <- read.csv("ACSST1Y2017.S1901-Data.csv", skip=1)
Income_data_ACS_2018 <- read.csv("ACSST1Y2018.S1901-Data.csv", skip=1)
Income_data_ACS_2019 <- read.csv("ACSST1Y2019.S1901-Data.csv", skip=1)
Income_data_ACS_2021 <- read.csv("ACSST1Y2021.S1901-Data.csv", skip=1)

# Loading Housing Statistics # 

# Working on Desktop
setwd("C:/Users/Naran/Data-Analytics-Project-6000/Data Analytics Project/Housing Statistic")

Housing_data_ACS_2015 <- read.csv("ACSDP1Y2015.DP04-Data.csv", skip=1)
Housing_data_ACS_2016 <- read.csv("ACSDP1Y2016.DP04-Data.csv", skip=1)
Housing_data_ACS_2017 <- read.csv("ACSDP1Y2017.DP04-Data.csv", skip=1)
Housing_data_ACS_2018 <- read.csv("ACSDP1Y2018.DP04-Data.csv", skip=1)
Housing_data_ACS_2019 <- read.csv("ACSDP1Y2019.DP04-Data.csv", skip=1)
Housing_data_ACS_2021 <- read.csv("ACSDP1Y2021.DP04-Data.csv", skip=1)


#Load Cost of Living Data

setwd("C:/Users/Naran/Data-Analytics-Project-6000/Data Analytics Project")


cost_of_living_data <- read.csv("Cost_Of_Living_By_State.csv")
cost_of_living_data_by_county <- read.csv("Cost of Living/Cost_Of_Living_Atlanta_data.csv")

cost_of_living_data_by_county <- cost_of_living_data_by_county[,-c(6:7,10:14)]

