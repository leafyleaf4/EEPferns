#data analysis for EEP project 

#okay PLAN: 
#need to get LME4 sorted - linear mixed effects model but ANOVA??? is it possible? 
## GOT LME4 WORKING YIPEEE


library(tidyverse) 
library(readxl) 
library(lme4) #for fancy modelling 
library(lmerTest) #testing lmer
library(scales) #not using yet
library(DHARMa) #using this!
library(StatisticalModels) #for r2 glmer however???? it doesnt work :(
library(ggeffects) #not using yet 
library(performance) #not usiung yet 
library(ggthemes) #for pretty grpahs
library(emmeans) # for tukesy test 



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

#### more pretty graphs 
sum_table <- ferndata %>% group_by(Substrate) %>% summarise (mean_frondlength = mean(frondlength), sdev = sd(frondlength), se_length = sd(frondlength)/sqrt(n()))

sum_table

#graph for mean, SEM, and raw data points 
ggplot()+
  geom_point(data =ferndata, aes(x=Substrate , y=frondlength, col=Substrate), shape=21)+
  geom_point(data = ferndata, aes(x=Substrate , y=frondlength, col=Substrate), stat="summary", fun="mean", size=3)+
  geom_errorbar(data=sum_table, aes(x=Substrate, ymin=(mean_frondlength-se_length), ymax=(mean_frondlength+se_length), col=Substrate), width=0.1)+
  scale_x_discrete(labels=c("concrete"="Artificial Rock", "rock"="Natural Rock", "soil"="Soil"))+
  labs(x= "Substrate", y= "Frond Length (mm)")+
  theme_few()+
  theme(legend.position = "none")

#same graph, but showing effect of species 

ggplot()+
  geom_point(data =ferndata, aes(x=Substrate , y=frondlength, col=Species), alpha=0.75)+
  geom_point(data = ferndata, aes(x=Substrate , y=frondlength), stat="summary", fun="mean", size=3)+
  geom_errorbar(data=sum_table, aes(x=Substrate, ymin=(mean_frondlength-se_length), ymax=(mean_frondlength+se_length)), width=0.1)+
  scale_x_discrete(labels=c("concrete"="Artificial Rock", "rock"="Natural Rock", "soil"="Soil"))+
  labs(x= "Substrate", y= "Frond Length (mm)")+
  theme_few()



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
#o okay lower f value and p but p is still rly significant 

rand(test2)
#likelyhood ratio improved model fit

#get r2 
R2GLMER(test2)




#need to test assumptions of model now: 
simulateResiduals(fittedModel = test2, plot=T) #checking assumptions using DHARMA plot 
#slightly grim unfortunately 

#trying again but logging 
ferndata<- ferndata %>% mutate(log_frondlength=log(frondlength), log_bladelength=log(bladelength))

#same again but with logged frondlength USE THIS ONE!!!!!!
test3<-lmer(log_frondlength~Substrate+(1|Species), data=ferndata, REML=TRUE)
anova(test3) 
#p and f value stronger than before 
rand(test3)
#not quite as good fit but still works well
summary(test3)
#look at the intercept and substrate rocks and soil under fixed effects,
#look at estimate, and from that interpret the percent change from each substrate 

#get r2
R2GLMER(test3)


#DHARMA time 
simulateResiduals(fittedModel = test3, plot=T)
#DHARMA LESS BAD :DDDDD
#THIS IS THE HYPE ONE WE WANT TO GO WITH THISSSS!!!! YIPEEE (as long as things dont change when we fix species)


#tukeys test time: 
emmeans(test3, list(pairwise~Substrate), adjust ="tukey")
#from this, need to back-transform means (exponentiate), to get percentage difference between each 






# graph for the distribution of species in different habitats 

#use code i used for PCE report in this too 
species_sum <- ferndata %>% group_by(Substrate) %>%  count(Species)







