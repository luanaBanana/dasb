#LECTURE9 --> exploring how to extend the linear model for forecasting

library(modelr)
library(tidyverse)
studPerf <- read_csv("C:/Users/Luana/HSLU/DASB/dasb/StudentsPerformance.csv",
                     col_types = cols(gender = col_factor(levels = c("female", "male")), 
                                      'race/ethnicity' = col_factor(levels = c("group A", "group B", "group C", "group D", "group E")),
                                      'parental level of education' = col_factor(levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school", "some high school")), 
                                      lunch = col_factor(levels = c("free/reduced", "standard")),
                                      'test preparation course' = col_factor(levels = c("none", "completed")),
                                      'math score' = col_integer(), 
                                      'reading score' = col_integer(),
                                      'writing score' = col_integer()))

studPerf <- as.data.frame(studPerf)
summary(studPerf) # summary


## Replace column names
colnames(studPerf)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(studPerf) <- namesOfColumns

##################### Base transformation ########################################################

View(studPerf)

## Add column total score 
studPerf <- studPerf %>% mutate(Total_Score =  Writing_Score + Math_Score + Reading_Score )

## Ergebnis -> M?nner durchschnittlich h?her
studPerf %>% group_by(Gender) %>% summarise(minMath= min(Math_Score), 
                                            maxMath=max(Math_Score), 
                                            meanMath=mean(Math_Score), 
                                            sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

## Ergebnis -> Eine Gruppe immer höhher.. 
studPerf %>% group_by(`Race`) %>% summarise(minMath= min(Math_Score), 
                                            maxMath=max(Math_Score), 
                                            meanMath=mean(Math_Score), 
                                            sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

studPerf %>% group_by(`Race`) %>% summarise(minReading= min(Reading_Score), 
                                            maxReading=max(Reading_Score), 
                                            meanReading=mean(Reading_Score), 
                                            sdMath = sd(Reading_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

studPerf %>% group_by(`Race`) %>% summarise(minWriting= min(Writing_Score), 
                                            maxWriting=max(Writing_Score), 
                                            meanWriting=mean(Writing_Score), 
                                            sdMath = sd(Writing_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

## Ergebnis -> standard lunch deutlich h?her.. 
studPerf %>% group_by(`Lunch`) %>% summarise(minMath= min(Math_Score), 
                                             maxMath=max(Math_Score), 
                                             meanMath=mean(Math_Score), 
                                             sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                             nr=n()) %>% ungroup()

## Ergebnis -> bei Eltern mit einem degree h?her
studPerf %>% group_by(Parent_Education) %>% summarise(minMath= min(Math_Score), 
                                                      maxMath=max(Math_Score), 
                                                      meanMath=mean(Math_Score), 
                                                      sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                      nr=n()) %>% ungroup()

## Ergebnis -> kein grosser Unterschied zwischen completed und none bei Math
##-> bei reading/writing jedoch schon (was heisst das? A la entweder man versteht es oder nicht?)

studPerf %>% group_by(`Test_Prep`) %>% summarise(minMath= min(Math_Score), 
                                                 maxMath=max(Math_Score), 
                                                 meanMath=mean(Math_Score), 
                                                 sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                 nr=n()) %>% ungroup()

studPerf %>% group_by(Test_Prep) %>% summarise(minReading= min(Reading_Score), 
                                                               maxReading=max(Reading_Score), 
                                                               meanReading=mean(Reading_Score), 
                                                               sdMath = sd(Reading_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                               nr=n()) %>% ungroup()

studPerf %>% group_by(Test_Prep) %>% summarise(minWriting= min(Writing_Score), 
                                                               maxWriting=max(Writing_Score), 
                                                               meanWriting=mean(Writing_Score), 
                                                               sdMath = sd(Writing_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                               nr=n()) %>% ungroup()

##### Zu lim_inf / lim_sup : Der Limes superior und der Limes inferior ist der gr??te und der kleinste H?ufungspunkt einer Folge. 
##### Diese dienen als partiellen Ersatz f?r den Grenzwert, wenn dieser nicht existiert. 
##### Macht aber bei unserem DataSet keinen Sinn weil wir einen Grenzwert haben.

studPerf %>% group_by(`Gender`,`Parent_Education`)  %>% summarise(minMath= min(Math_Score), 
                                                                             maxMath=max(Math_Score), 
                                                                             meanMath=mean(Math_Score), 
                                                                             sdMath = sd(Math_Score), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                                             lim_inf=meanMath-2*sdMath, lim_sup=meanMath+2*sdMath,
                                                                             nr=n()) %>% ungroup()
## Sehr h?bsch: kann noch f?r Reading Writing gemacht werden
ggplot(data=studPerf, mapping=aes(x=Reading_Score,y=Math_Score)) + 
  geom_point(aes(color=Gender)) + 
  facet_wrap(.~Parent_Education) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data=studPerf, mapping=aes(x=Reading_Score,y=Math_Score)) + 
  geom_point(aes(color=Gender)) + 
  facet_wrap(.~Race) + 
  geom_smooth(method = lm, se = FALSE)

## Much better to put Math_Score on the y Axis
ggplot(data=studPerf, mapping=aes(x=Parent_Education,y=Math_Score)) + 
  geom_point(aes(color=Lunch)) + 
  facet_wrap(.~Gender) + 
  geom_smooth(method = lm, se = FALSE)

## Could especially female students perform higher with a completed test course ?
ggplot(data=studPerf, mapping=aes(x=Race,y=Math_Score)) + 
  geom_point(aes(color=Gender)) + 
  facet_wrap(.~Test_Prep) + 
  geom_smooth(method = lm, se = FALSE)



library(gridExtra)

library(nnet) # for using multiclass classification 

mod_lin <- multinom(Math_Score ~ Test_Prep + Gender, data=studPerf)
mod_non_lin <- multinom(Math_Score ~ Test_Prep * Gender , data=studPerf)
mod_non_lin2 <- multinom(Math_Score ~ Test_Prep * Gender + Parent_Education , data=studPerf)

anova(mod_lin,mod_non_lin,mod_non_lin2)


stud_perf_math <- studPerf %>% data_grid(Test_Prep,Gender,Parent_Education) %>% gather_predictions(mod_lin,mod_non_lin,mod_non_lin2)

ggplot(data=studPerf, aes(x=Test_Prep,y=Parent_Education)) + 
  geom_point(aes(color=Gender)) + 
  geom_tile(data = stud_perf_math, aes(fill=pred), alpha = 0.3, show.legend = F) +
  facet_wrap(.~model) + 
  theme_bw(base_size = 15) +
  ggtitle('Model comparisons') +
  coord_fixed(ratio = 0.8) + 
  theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none')


