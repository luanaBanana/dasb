#LECTURE8 --> start understanding and "playing" with models



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
# There are no NA or missing values

## Replace column names
colnames(studPerf)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(studPerf) <- namesOfColumns

## Positive correlation between the scores
cor(studPerf$Math_Score,studPerf$Reading_Score)
cor(studPerf$Math_Score,studPerf$Writing_Score)
cor(studPerf$Writing_Score,studPerf$Reading_Score)

cor(studPerf$Writing_Score,studPerf$totalScore)
cor(studPerf$Math_Score,studPerf$totalScore)
cor(studPerf$Reading_Score,studPerf$totalScore)


lm1 <- lm(Math_Score~Race+Lunch+Parent_Education+Test_Prep,data=studPerf)


lm1
summary(lm1)
par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1))

ggplot() + geom_point(data=studPerf, aes(x=Parent_Education, y=Math_Score)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5) 


ggplot() + geom_point(data=studPerf, aes(x=Race, y=Math_Score)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5) 

