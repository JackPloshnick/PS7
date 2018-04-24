library(readr)
March2018 <- read_csv("~/GitHub/PS7/March2018.csv")

library(dplyr)
library(ggplot2)

March2018<- as.tbl(March2018)

# Compute the number of crime per day by the type of crime


March2018<- March2018 %>% 
  separate(DateOccur, into = c("Date", "Time"), sep = " ")


crimes <- March2018 %>% 
  group_by(Description, Date) %>% 
  summarise(count=n()) 

crimes #Gives number of crimes committed each day









