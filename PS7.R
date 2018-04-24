library(readr)
March2018 <- read_csv("~/GitHub/PS7/March2018.csv")

library(dplyr)
library(ggplot2)

March2018<- as.tbl(March2018)

#Question 2


March2018<- March2018 %>% 
  separate(DateOccur, into = c("Date", "Time"), sep = " ")


crimes <- March2018 %>% 
  group_by(Description) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))

crimes 

#Most common crime= "LEAVING SCENE OF ACCIDENT"

#number of crimes per day 
crimes <-  mutate(crimes, perDay = (count/31))

#14.96 "LEAVING SCENE OF ACCIDENT" per day 


#Question 3

# Compute the number of crime per day by neighborhood. Which neighborhood
#has the most number of crime?



hood <- March2018 %>% 
  group_by( Neighborhood) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))

hood 

hood <-  mutate(hood, perDay = (count/31))

#Neighborhood 35 has 305 crimes total. Which is 9.83 crimes per day












