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
  arrange(desc(count))%>%
  mutate(perDay = (count/31))

crimes 

#Most common crime= "LEAVING SCENE OF ACCIDENT"

#14.96 "LEAVING SCENE OF ACCIDENT" per day 


#Question 3

# Compute the number of crime per day by neighborhood. Which neighborhood
#has the most number of crime?



hood <- March2018 %>% 
  group_by( Neighborhood) %>% 
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  mutate( perDay = (count/31))

hood 

#Neighborhood 35 has 305 crimes total. Which is 9.83 crimes per day

## Question 4

#Compute the proportion of crime related to robbery by district. Which
#district has the largest proportion of crime related to robbery?

RobberyData<- March2018 %>% 
  separate(Description, into = c("Type", "Specifics"), sep = ("-")) %>%
  separate(Type, into = c("Type","S"), sep = (" ")) %>%
  group_by(Type, District) %>%
  filter(Type == "ROBBERY") %>%
  summarise(count=n()) %>%
  arrange(desc(count)) 

RobberyData

District <- March2018 %>% 
  group_by( District) %>% 
  summarise(total=n()) %>%
  arrange(desc(total))

District 

RobberyData<- left_join(RobberyData,  District) 
 
RobberyData<- mutate(RobberyData, prop= (RobberyData$count / RobberyData$total)) %>%
  arrange(desc(prop))

RobberyData

#district 5 has the highest robbery proportion, 0.03986711

#Question 5
#Visualize changes of all types of crime over time using ggplot2. Write
#appropriate labels and titles. 


Plottingdata<-  March2018 %>% 

  group_by( Date, District) %>%
  summarise(count=n()) %>%
  arrange(desc(count))%>%
  separate(Date, into = c("Month" , "Day", "Year"), sep = ("/"), convert = T) 


Plottingdata


ggplot(data = Plottingdata, mapping = aes(x = Day, y = count)) + 
  geom_point() +
  
  labs(title = "Crimes by time")+
  ylab("Number of Crimes")+
  theme(plot.title = element_text(hjust = 0.5))

## Question 6



ggplot(data = Plottingdata, mapping = aes(x = Day, y = count)) + 
  geom_point(aes(color = factor( District))) +

  labs(title = "Crimes by time and district", color = "District")+
  ylab("Number of Crimes")+
  theme(plot.title = element_text(hjust = 0.5))




