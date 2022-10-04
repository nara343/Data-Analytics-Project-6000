# Data-Analytics-Project-6000


## Project Overview

The objective of the project is to use 3-4 different Machine Learning models to investigate two or more datasets and uncover the hidden meaning of the data. For this project I will be focusing on two paticular fields: the first is focusing on the wealth & rent cost distribution across America and the second is the cost of living. The data for the wealth distribution and the cost of housing will be pulled from the [census data collected](https://data.census.gov/cedsci/) these past century (2020 - 1920). This means there will be missing data from previous decades but that will be handled during the implemenetation phase of the project. The second dataset will be data collected on the cost of living essentials such as the average amount an American Citizen spends on groceries, clothes, health care. From there the two data sets will be combined and we will generate a two new ratios: the first is the amount of income spent on rent and the second is the amount spent on living essentials. 

Based off of this we should see an linear increase in each section, as inflation gross we see an increase in income to combat it. If the cost of living increases we should also see a linear increase to income. 

### There will be three different models built: 
- The first model is a linear regression model
    - Using the data from the past century and the cost of essentials can we predict when or if the average cost of living will beging to consume someone's entire wage? If so when will we predict that event occuring? Will this ever be something we see or will a major event occur before we reach this point.
- Second a clustering model
    - Identifying different groups of people based off income and predicting when will they be considered the poverty. 
    - Motivation: In the past someone was able to live off a smaller wage comfortably ($30,000) but with the increase of cost of living they are no longer considered middle class but poverished. Are we able to predict when these groups will be considered "poverished" 
- An XGBoost model
    - With the cost of living increase we might also notice an increase in the number of household memebers and a decrease in children had by a family. 
    - Motivation: If cost of living increases will births decrease and the average household increase. What has been the trend from 1920 to  2020. 

