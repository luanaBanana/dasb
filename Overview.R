## Presentation
library(gridExtra) 
library(modelr)
library(tidyverse)
library(ggplot2)
install.packages("ISLR"); 
library(MASS)
library(ISLR)


## Read data source and modify column names
studPerf <- read_csv("./StudentsPerformance.csv",
                     col_types = cols(gender = col_factor(levels = c("female", "male")), 
                                      'race/ethnicity' = col_factor(levels = c("group A", "group B", "group C", "group D", "group E")),
                                      'parental level of education' = col_factor(levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school", "some high school")), 
                                      lunch = col_factor(levels = c("free/reduced", "standard")),
                                      'test preparation course' = col_factor(levels = c("none", "completed")),
                                      'math score' = col_integer(), 
                                      'reading score' = col_integer(),
                                      'writing score' = col_integer()))

studPerf <- as.data.frame(studPerf)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(studPerf) <- namesOfColumns
studPerf <- as_tibble(studPerf)
studPerf <- mutate(studPerf, totalScore = studPerf$Math_Score+studPerf$Reading_Score+studPerf$Writing_Score)


#### Overview 
View(studPerf)
summary(studPerf)

#### EDA

#Taken from below list students who:
# - took a test preparation course, 
# - had standard lunch,
# - have parents with a higher educational degree
# - are female
# are likely to show up more often in the top scorer postition of the total score, reading score and writing score.

arrange(studPerf, desc(totalScore))
arrange(studPerf, Reading_Score)

arrange(studPerf, desc(Writing_Score))
arrange(studPerf, Writing_Score)

arrange(studPerf, desc(Reading_Score))
arrange(studPerf, Reading_Score)

# A look upon the Math Score shows similar results except that the gender doesn't seem to play as much of a role as in the other scores.

arrange(studPerf, desc(Math_Score))
arrange(studPerf, Math_Score)


# The above statement is underlined by looking at the mean of each score where we can see that female students have higher avarage scores overall
# except for math scores. 
# Interestingly the mean of math score differs less than mean of the writing, reading and total score.
summarise(group_by(studPerf, `Gender`), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())

# Below summaries also underline the statements of having higher scores when eating standard lunch, taking a test prep course 
# and having parents with higher education.
summarise(group_by(studPerf, `Lunch`), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())

summarise(group_by(studPerf, Test_Prep), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())

summarise(group_by(studPerf, Parent_Education), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())
# Taking a look at the avarage scores in dependency to ethnic group there appears to be an even ranking throughout all the categories
# starting from group A having the lowest scores in each category, to B, C, D and ending with group E having the highest scores in each category.

summarise(group_by(studPerf, Race), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())

### Visuals

# Facet_Grid of Parental Education
## Female students generally higher in Reading and Writing
ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Gender)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=Writing_Score,color=Gender)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=totalScore,color=Gender)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Total score")

# Other factors that result in higher scores like lunch

ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Lunch)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

# No apparent visual difference in the race group comparison
ggplot(studPerf, aes(x=Math_Score, y=Writing_Score,color=Race)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green","yellow","black","pink")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")
# Again no apparent visual difference in race group comparison if facet_grid is changed to Race group
ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Gender)) + 
  geom_point() +
  facet_grid(vars(Race)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

# Interestingly, test preparation course does not change the math score as much as the other ones
# We could assume thath this means that math is either understood or not but cannot be learned

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

### Modelling

# Positive correlation between the scores
cor(studPerf$Math_Score,studPerf$Reading_Score)
cor(studPerf$Math_Score,studPerf$Writing_Score)
cor(studPerf$Writing_Score,studPerf$Reading_Score)

cor(studPerf$Writing_Score,studPerf$totalScore)
cor(studPerf$Math_Score,studPerf$totalScore)
cor(studPerf$Reading_Score,studPerf$totalScore)

# Models

# Linear model in graph -> obvious positive correlation
ggplot(data=studPerf, mapping=aes(x=Math_Score,y=Reading_Score))+
  geom_point(aes(color=as.factor(Gender))) + 
  facet_wrap(~Lunch) + 
  geom_smooth(method = lm, se = FALSE)

# Linear Model to predict Math Score with summative factors
math_model <- lm(Math_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)
summary(math_model)
math_model
# Linear Model to predict Reading Score with summative factors
reading_model <- lm(Reading_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)
summary(reading_model)
reading_model
# Linear Model to predict Writing Score with summative factors
writing_model <- lm(Writing_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)
summary(writing_model)
writing_model

# Prepare test data

# Create training and test data
set.seed(99)
train.data <- sample_frac(studPerf, 0.7) # select 70% random samples
test.data <- setdiff(studPerf,train.data)

#Check if the test data is alright and representative -> Looks good
p1 <- ggplot() + geom_point(data = train.data, aes(x=Math_Score,y=Reading_Score,color=Gender), color='steelblue3') + 
  geom_point(data = test.data, aes(x=Math_Score,y=Reading_Score,color=Gender), color='darkred', size=2) 
p2 <- ggplot() + geom_point(data = train.data, aes(x=Lunch,y=Reading_Score,color=Gender), color='steelblue3') + 
  geom_point(data = test.data, aes(x=Lunch,y=Reading_Score,color=Gender), color='darkred', size=2) 
grid.arrange(p1, p2, nrow = 1)

studPred <- studPerf %>% data_grid(Gender, Parent_Education,Race,Lunch,Test_Prep) %>%
  gather_predictions(writing_model, reading_model, math_model)

# Prediciton in black line
ggplot(studPerf, aes(Parent_Education, Math_Score, color = Gender)) +
  geom_point() +
  geom_line(data = studPred, aes(y = pred), color="black") +
  facet_grid(Test_Prep~ model)
