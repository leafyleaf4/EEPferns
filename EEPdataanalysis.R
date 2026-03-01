#data analysis for EEP project 

library(tidyverse)
library(readxl)

#load data
ferndata<-read_excel("ferndata.xlsx")


#data wrangling
ferndata<- ferndata %>% mutate(fertileratio= fertilefrond/numberfrond)

#visualise data 
#blade length against substrate
ggplot(data=ferndata, aes(x=Substrate, y=bladelength))+
         geom_boxplot()

#ratio of fertile frond to total frond 
ggplot(data=ferndata, aes(x=Substrate, y=fertileratio))+
  geom_point()

#
ggplot(data=ferndata, aes(x=Substrate, y=fertileratio))+
  geom_point()
