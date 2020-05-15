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



##################### SIM3: Comparison of different models ########################################################

studPerf

ggplot(data=studPerf)+geom_point(aes(x=Math_Score,y=Reading_Score,color=Gender)) + scale_x_discrete()
ggplot(data=studPerf)+geom_point(aes(x=as.factor(Math_Score),y=Reading_Score,color=Gender)) + scale_x_discrete()

ggplot(data=studPerf)+geom_point(aes(x=as.factor(Math_Score),y=Lunch,color=Gender)) + scale_x_discrete()


ggplot(data=studPerf, mapping=aes(x=Math_Score,y=Reading_Score))+
  geom_point(aes(color=as.factor(Gender))) + 
  facet_wrap(~Lunch) + 
  geom_smooth(method = lm, se = FALSE)

summary(sim3)
View(sim3)


# for convenient window arrangement in plotting
library(gridExtra) 

# Create training and test data
set.seed(99)
train.data <- sample_frac(studPerf, 0.7) # select 70% random samples
test.data <- setdiff(studPerf,train.data)

##Check if the test data is alright and representative
p1 <- ggplot() + geom_point(data = train.data, aes(x=Math_Score,y=Reading_Score,color=Gender), color='steelblue3') + 
  geom_point(data = test.data, aes(x=Math_Score,y=Reading_Score,color=Gender), color='darkred', size=2) 
p2 <- ggplot() + geom_point(data = train.data, aes(x=Lunch,y=Reading_Score,color=Gender), color='steelblue3') + 
  geom_point(data = test.data, aes(x=Lunch,y=Reading_Score,color=Gender), color='darkred', size=2) 
grid.arrange(p1, p2, nrow = 1)
##--------------------------------

###Models (but not so good ones)
mod_parz_Gender <- lm(Math_Score ~ Gender,data=train.data)
mod_parz_Parent <- lm(Math_Score ~ Parent_Education,data=train.data)
#------------------

studPred <- studPerf %>% data_grid(Gender, Parent_Education) %>%
  gather_predictions(mod_parz_Gender, mod_parz_Parent)

ggplot(studPerf, aes(Parent_Education, Math_Score, color = Gender, group=1)) +
  geom_point() +
  geom_line(data = studPred, aes(y = pred)) +
  facet_wrap(~ model)

###Analyzing Model Quality ( interaction seems better than summative)

mod_indep <- lm(Math_Score ~ Gender + Parent_Education,data=train.data)
mod_interaction <- lm(Math_Score ~ Gender * Parent_Education + Test_Prep + Lunch + Race,data=train.data)

mod_final <- lm(Math_Score ~ Gender + Parent_Education + Test_Prep + Lunch + Race,data=train.data)
mod_final2 <- lm(Math_Score ~ Gender * Parent_Education * Test_Prep * Lunch + Race,data=train.data)
mod_final3 <- lm(Math_Score ~ Gender * Parent_Education * Test_Prep * Lunch * Race,data=train.data)
mod_final4 <- lm(Math_Score ~ .,data=train.data)

anova(mod_indep,mod_interaction,mod_final,mod_final2,mod_final3,mod_final4)

## Man kann hier noch ein Delta hinzufügen .. um zu vergleichen Siehe Lab2
studPred_final <- studPerf %>% data_grid(Gender, Parent_Education, Lunch, Test_Prep, Race) %>%
  gather_predictions(mod_final)

ggplot(studPerf, aes(Parent_Education, Math_Score, color = Gender)) +
  geom_point() +
  geom_line(data = studPred_final, aes(y = pred)) +
  facet_grid(Test_Prep~ model)

anova(mod_parz_Gender,mod_parz_Parent,mod_indep,mod_interaction,mod_final)

