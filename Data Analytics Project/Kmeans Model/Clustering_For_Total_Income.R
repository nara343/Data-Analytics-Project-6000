library(ggplot2)
library(cluster)

# Can we identify certain states based off their distribution of household income 
# and yearly total cost of certain necessities 


# Need to normalize the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df <- final_dataset[,c(19:28,30:33)]
colnames(df)
# Normalize the data 

norm_data <- as.data.frame(lapply(df,normalize))


# Elbow Plot For Both 
tot.withinss <- vector(mode="character", length=10)
for (i in 1:10){
  obesity_ <- kmeans(norm_data, center=i, nstart=20)
  tot.withinss[i] <- obesity_$tot.withinss
}

plot(1:10, tot.withinss, type="b", pch=19)
title("Elbow Plot For Kmeans Model")

# Based off of the elbow plot we should 4 but we will first try with 3 clusters
# As to see if we can predict each state correctly


df_cluster <- kmeans(norm_data, center=3, nstart=20)
df_cluster

# K-means clustering with 3 clusters of sizes 89, 304, 200
# 
# Cluster means:
#   Households..Estimate..Less.than..10.000 Households..Estimate...10.000.to..14.999 Households..Estimate...15.000.to..24.999 Households..Estimate...25.000.to..34.999
# 1                               0.2225005                                0.1981177                                0.2209738                                0.1632959
# 2                               0.3784152                                0.3985324                                0.5682734                                0.5597862
# 3                               0.2646815                                0.2480769                                0.3354762                                0.3211250
# Households..Estimate...35.000.to..49.999 Households..Estimate...50.000.to..74.999 Households..Estimate...75.000.to..99.999 Households..Estimate...100.000.to..149.999
# 1                                0.1477030                                0.2200072                                0.2593291                                  0.6451962
# 2                                0.5464482                                0.6112267                                0.3766087                                  0.4037257
# 3                                0.3507595                                0.5084839                                0.4399270                                  0.6422460
# Households..Estimate...150.000.to..199.999 Households..Estimate...200.000.or.more Average.Cost.Of.Rent Average.Cost.of.Insurance.With.Employer Average.Transportation.Expense Average.Misc.Expense
# 1                                  0.7735785                             0.58956477           0.52757720                               0.5065447                      0.6184352            0.6479228
# 2                                  0.2517195                             0.09459127           0.09710653                               0.4370389                      0.6207222            0.4473222
# 3                                  0.5506439                             0.24507184           0.24008806                               0.4038997                      0.6561311            0.5027728
# 
# Within cluster sum of squares by cluster:
#   [1] 29.12549 68.57540 44.83188
# (between_SS / total_SS =  46.2 %)

# Results # 
table <- table(final_dataset$State, df_cluster$cluster)
table
#     1   2   3
# CA  48  98  97
# NY  37 143  53
# WA   4  63  50

#Clustering Accuracy 
sum(diag(table))/sum(table)
# 0.4064081


clusplot(norm_data, df_cluster$cluster, color=T, shade=T, labels=0, lines=0)
