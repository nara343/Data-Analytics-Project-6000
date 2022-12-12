# Data-Analytics-Project-6000


## Project Overview

The objective of the project is to use 3-4 different Machine Learning models to investigate two or more datasets and uncover the hidden meaning of the data. For this project I will be focusing on two paticular fields: the first is focusing on the wealth & rent cost distribution across America and the second is the 

For the [cost of living](https://www.atlantafed.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/cost-of-living-database) data, we will be using a public dataset to avoid pay walls. The current CLD hasn't different values and has missing data for the more current years so to account for the missing years we are going to assume cost have been following inflation. 

 For the census data there are two different data sets that will be used; the first is the income distribution by county for California, New York, and Washington; second housing characteristic datasets. [census data collected](https://data.census.gov/cedsci/) these past century (2015 - 2021).




    
    
### Data Set Links
- [Census Data: Income](https://data.census.gov/cedsci/table?q=Income&g=0100000US%240400000&tid=ACSST1Y2021.S1901)
- [Census Data: Discription and Cost of Housing](https://data.census.gov/cedsci/table?q=Rent&g=0100000US%240400000&tid=ACSDP1Y2021.DP04)
- [](https://www.atlantafed.org/economic-mobility-and-resilience/advancing-careers-for-low-income-families/cost-of-living-database)

The four different models that will be built: 
- Linear Regression Model
    - Average Rent Cost ~ Estimate Number of Units
- Kmeans Clustering
    - Clustering based on the different state data.
- Random Forest Model
    - Predicting which state each data point is from. Using the distribution, housing occupancy data, and cost of living data.

- Random Forest Model
    - Predicting Estimated Total Cost based off of
    County and Income.
    - Estimated Total Cost ~ County Name + Income

