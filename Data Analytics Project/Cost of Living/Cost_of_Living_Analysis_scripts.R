# Data From https://worldpopulationreview.com/state-rankings/cost-of-living-index-by-state

summary()




library(ggplot2)
require("ggrepel")

# Graphing Cost of Index Data


graph_by_state_index <- ggplot(cost_of_living_data_by_county, aes(Total.Cost, fill = State)) + 
  geom_histogram(binwidth = 1000)
                                         
print(graph_by_state_index + 
        labs(title = "Average Total Cost Distribution By State"))          

# 


graph_by_state_housing <- ggplot(cost_of_living_data_by_county , aes(Average.Cost.Of.Rent, fill = State)) + 
  geom_histogram(binwidth = 1000) 

print(graph_by_state_housing + 
        labs(title = "Average Yearly Rent Distribution Across The Three States for 2021", x = "Housing Cost Index"))          
#Seperate Data for Washington, California, and New York 

numeric_vals <-cost_of_living_data_by_county[,-c(1:3)]

library(ggcorrplot)
corr <- round(cor(numeric_vals),2)
ggcorrplot(corr,type = "lower",outline.col = "white",lab = TRUE)


ggplot(cost_of_living_data_by_county, 
            aes(x = Average.Cost.Of.Rent, Total.Cost)) +
  geom_point(aes(color = State), size = 3, alpha = 0.5) +
  xlab(" Average Cost of Rent (USD) ") + 
  ylab(" Total cost (USD) ") +
  ggtitle("Scatter Plot of Average Cost of Rent vs Total Cost")

summary(cost_of_living_data_by_county)


# DO NOT REMOVE # 
# for(x in 1:10000){
#   print('i love bella') 
# }

