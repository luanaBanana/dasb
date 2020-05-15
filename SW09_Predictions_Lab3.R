#LECTURE9 --> exploring how to extend the linear model for forecasting

library(modelr)
library(tidyverse)
install.packages("hexbin")

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

cor(studPerf$Math_Score,studPerf$Reading_Score)
cor(studPerf$Math_Score,studPerf$Writing_Score)
cor(studPerf$Reading_Score,studPerf$Writing_Score)

## Mit einem Histogram ist das sinnvoller ... eventuell besser für eine Prediction Darstellung?
ggplot(data=studPerf, aes(x=Math_Score,y=Reading_Score)) + geom_point(color="blue")
ggplot(data=studPerf, aes(x=Math_Score)) + geom_histogram(color="blue")
ggplot(data=studPerf, aes(x=Reading_Score)) + geom_histogram(color="red")
##Wieso Log: http://onlinestatbook.com/2/transformations/log.html Bringt bei unserem Beispiel nichts
ggplot(data=studPerf, aes(x=log(Reading_Score))) + geom_histogram(color="red")
ggplot(studPerf, aes(Math_Score,Reading_Score)) +  geom_hex(bins = 50)


ggplot(data=studPerf, aes(x=log(Math_Score),y=log(Reading_Score))) + geom_point(color="red")
ggplot(studPerf, aes(log(Math_Score), Reading_Score)) +  geom_hex(bins = 50)
ggplot(studPerf, aes(Math_Score, log(Reading_Score))) +  geom_hex(bins = 50)
ggplot(studPerf, aes(log(Math_Score), log(Reading_Score))) +  geom_hex(bins = 50)

mod_diam_1 <- lm(Math_Score ~ Parent_Education+Gender, data=studPerf)

