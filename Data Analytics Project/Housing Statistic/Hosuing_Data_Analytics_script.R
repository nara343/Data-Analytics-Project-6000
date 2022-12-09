#Looking at the data 

library(ggplot2)
library(forcats)
library(dplyr)
library("ggrepel")


return_subset_by_state <- function(df, state){
  
  subset_df <- df[grep(state, df$Geographic.Area.Name), ]

  return (subset_df)
}

# Graphing the number of vacant homes in an area

# Since there are so many counties that are being looked at let's reduce the search
# by state and the ones that are above the average

#2018

california_2018 <- return_subset_by_state(Housing_data_ACS_2018, "California")

Plot_california_2018 <- california_2018 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In California  2018")



#2019
california_2019 <- return_subset_by_state(Housing_data_ACS_2019, "California")

Plot_california_2019 <- california_2019 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
    geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
    geom_point(stat = "identity") + 
    coord_flip() +
    xlab("County Name") + 
    ylab("Percent of Available Housing Occupied") + 
    labs(title = "Percent of Occupied Housing By County In California  2019")

#2021
california_2021 <- return_subset_by_state(Housing_data_ACS_2021, "California")

Plot_california_2021 <- california_2021 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In California  2021")


#### New York ####

#2018

NY_2018 <- return_subset_by_state(Housing_data_ACS_2018, "New York")

Plot_NY_2018 <- NY_2018 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In New York 2018")

Plot_NY_2018

#2019
NY_2019 <- return_subset_by_state(Housing_data_ACS_2019, "New York")

Plot_NY_2019 <- NY_2019 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In New York 2019")
Plot_NY_2019

#2021
NY_2021 <- return_subset_by_state(Housing_data_ACS_2021, "New York")

Plot_NY_2021 <- NY_2021 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In New York 2021")
Plot_NY_2021


#### Washington ####


#2018

WA_2018 <- return_subset_by_state(Housing_data_ACS_2018, "Washington")

Plot_WA_2018 <- WA_2018 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In Washington 2018")

Plot_WA_2018

#2019
WA_2019 <- return_subset_by_state(Housing_data_ACS_2019, "Washington")

Plot_WA_2019 <- WA_2019 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In Washington 2019")
Plot_WA_2019

#2021
WA_2021 <- return_subset_by_state(Housing_data_ACS_2021, "Washington")

Plot_WA_2021 <- WA_2021 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In Washington 2021")
Plot_WA_2021





