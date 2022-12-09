# Data From https://worldpopulationreview.com/state-rankings/cost-of-living-index-by-state

summary(cost_of_living_data)

# > summary(cost_of_living_data)
# state             costIndex      groceryCost     housingCost    utilitiesCost    transportationCost    miscCost     
# Length:50          Min.   : 83.3   Min.   : 90.1   Min.   : 66.3   Min.   : 82.30   Min.   : 86.7      Min.   : 90.30  
# Class :character   1st Qu.: 91.3   1st Qu.: 95.6   1st Qu.: 80.0   1st Qu.: 90.83   1st Qu.: 94.9      1st Qu.: 96.62  
# Mode  :character   Median : 99.5   Median : 99.5   Median : 96.7   Median : 96.60   Median : 99.7      Median : 99.45  
# Mean   :104.6     Mean   :102.7   Mean   :109.6   Mean   :101.28   Mean   :102.8      Mean   :102.51  
# 3rd Qu.:111.2   3rd Qu.:107.2   3rd Qu.:118.7   3rd Qu.:104.78   3rd Qu.:108.4      3rd Qu.:108.30  
# Max.   :193.3   Max.   :152.9   Max.   :315.0   Max.   :164.20   Max.   :133.7      Max.   :126.70 


library(ggplot2)
require("ggrepel")

# Graphinh Cost of Index Data
order_by_cost_index <- cost_of_living_data
order_by_cost_index$state <- factor(order_by_cost_index$state, 
                                        levels = order_by_cost_index$state[order(order_by_cost_index$costIndex)])

pos <- order_by_cost_index$costIndex
pos

graph_by_state_index <- ggplot(order_by_cost_index, aes(costIndex, state)) + 
  geom_point(stat = "identity") + 
  geom_label_repel(aes(label = costIndex), size = 2.5)
                                         
print(graph_by_state_index + 
        labs(title = "State vs Cost of Index", y = "State Name", x = "Cost of Living Index"))          

# 
order_by_cost_housing <- cost_of_living_data

order_by_cost_housing$state <- factor(order_by_cost_housing$state, 
                                    levels = order_by_cost_housing$state[order(order_by_cost_housing$housingCost)])


graph_by_state_housing <- ggplot(order_by_cost_housing , aes(housingCost, state)) + 
  geom_point(stat = "identity") + 
  geom_label_repel(aes(label = housingCost), size = 2.5)

print(graph_by_state_housing + 
        labs(title = "State vs Cost of Housing Cost", y = "State Name", x = "Housing Cost Index"))          
#Seperate Data for Washington, California, and New York 



# DO NOT REMOVE # 
# for(x in 1:10000){
#   print('i love bella') 
# }

