#data analysis for EEP project 

#okay PLAN: 
#need to get LME4 sorted - linear mixed effects model but ANOVA??? is it possible? 
## GOT LME4 WORKING YIPEEE


library(tidyverse) 
library(readxl) 
library(lme4) #for fancy modelling 
library(lmerTest) #testing lmer
library(scales) 
library(DHARMa)
library(StatisticalModels)
library(ggeffects)
library(performance)


#load data
ferndata<-read_excel("ferndata.xlsx")

str(ferndata)

#data wrangling
ferndata<- ferndata %>% mutate(fertileratio= fertilefrond/numberfrond, widthlengthratio=bladewidth/bladelength) #create new columns 
ferndata$Substrate<- as.factor(ferndata$Substrate) # convert substrate into a factor 
#visualise data 
#frond length against substrate
ggplot(data=ferndata, aes(x=Substrate, y=frondlength))+
  geom_boxplot()

#blade length against substrate
ggplot(data=ferndata, aes(x=Substrate, y=bladelength))+
         geom_boxplot()

#ratio of fertile frond to total frond 
ggplot(data=ferndata, aes(x=Substrate, y=fertileratio))+
  geom_boxplot()

#blade width 
ggplot(data=ferndata, aes(x=Substrate, y=bladewidth))+
  geom_boxplot()

#stipe lengtt
ggplot(data=ferndata, aes(x=Substrate, y=stipelength))+
  geom_boxplot()

#blade width divided by blade length
ggplot(data=ferndata, aes(x=Substrate, y=widthlengthratio))+
  geom_boxplot()



#cool results looking interesting, anova time: 

#testing models for frond length vs substrate: 

#test 1: simple anova 
test1<- lm(frondlength~Substrate, data=ferndata) #create model 
# Checking normality
par(mfrow = c(1,2))  
hist(test1$residuals)   # Makes histogram of residuals  
plot(test1, which = 2)   # Makes Q-Q plot
par(mfrow = c(1,1))
#normality is fine 

# Checking homoscedasticity 
plot(test1, which = 1)  # Makes residuals VS fitted plot
#kinda violated - run welch f test instead? 

anova(test1)

#very statistically significant, and high F value, but also didnt quite fit homodescadicity thingy as much as i wanted 


#mixed effect model with species as random effect
test2<-lmer(frondlength~Substrate+(1|Species), data=ferndata, REML=TRUE)
anova(test2) 
#why isnt bestie giving me a p value 

rand(test2)
