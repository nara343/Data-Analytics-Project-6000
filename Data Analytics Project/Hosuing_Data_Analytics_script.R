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

#2010
california_2010 <- return_subset_by_state(Housing_data_ACS_2010, "California")

california_2010 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name, Percent..HOUSING.OCCUPANCY..Occupied.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, y = Percent..HOUSING.OCCUPANCY..Occupied.housing.units)) +
    geom_label_repel(aes(label = Percent..HOUSING.OCCUPANCY..Occupied.housing.units)) +
    geom_point(stat = "identity") + 
    coord_flip() +
    xlab("County Name") + 
    ylab("Percent of Available Housing Occupied") + 
    labs(title="Percent of Occupied Housing By County In California  2010")

#2011
california_2011 <- return_subset_by_state(Housing_data_ACS_2011, "California")

california_2011 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In California  2011")

#2012
california_2012 <- return_subset_by_state(Housing_data_ACS_2012, "California")

california_2012 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In California  2012")

#2013
california_2013 <- return_subset_by_state(Housing_data_ACS_2013, "California")

california_2013 %>% 
  mutate(Geographic.Area.Name = fct_reorder(Geographic.Area.Name,
                                            Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) %>%
  ggplot(aes(x = Geographic.Area.Name, 
             y = Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units)) +
  
  geom_label_repel(aes(label = round(Estimate..HOUSING.OCCUPANCY..Total.housing.units..Occupied.housing.units/ Estimate..HOUSING.OCCUPANCY..Total.housing.units, 2))) +
  geom_point(stat = "identity") + 
  coord_flip() +
  xlab("County Name") + 
  ylab("Percent of Available Housing Occupied") + 
  labs(title = "Percent of Occupied Housing By County In California  2013")

#2014
california_2014 <- return_subset_by_state(Housing_data_ACS_2014, "California")

california_2014 %>% 
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

#2015
california_2015 <- return_subset_by_state(Housing_data_ACS_2015, "California")

california_2015 %>% 
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

#2016
california_2016 <- return_subset_by_state(Housing_data_ACS_2016, "California")

california_2016 %>% 
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

#2017
california_2017 <- return_subset_by_state(Housing_data_ACS_2017, "California")

california_2017 %>% 
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

#2018

california_2018 <- return_subset_by_state(Housing_data_ACS_2018, "California")

california_2018 %>% 
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



#2019
california_2019 <- return_subset_by_state(Housing_data_ACS_2019, "California")

california_2019 %>% 
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


