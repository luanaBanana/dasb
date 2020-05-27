library(tidyverse)

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
summary(studPerf) # summary
View(studPerf)

studPerf <- as_tibble(studPerf)

## Finding --> More females top scores in writing / reading, slightly more male topscorers in math.
filter(studPerf, Writing_Score > 99))
filter(studPerf, Math_Score > 90)
filter(studPerf, Reading_Score > 99)

## Finding --> Topscorers in math mostly have parents with a higher education degree, ate standard lunch and are part of ethnics group E 
arrange(studPerf, desc(Math_Score))
arrange(studPerf, Math_Score) #without specifying "desc" it will automatically choose "asc"

## Finding --> Topscorers mostly completed test preparation course, have parents higher education level, are female and ate standard lunch 
arrange(studPerf, desc(Writing_Score))
arrange(studPerf, Writing_Score)

## Finding --> Topscorers mostly completed test preparation course, have parents higher education level, are female and ate standard lunch 
arrange(studPerf, desc(Reading_Score))
arrange(studPerf, Reading_Score)


select(studPerf , -1)
select(studPerf, c(2,3,4))
select(studPerf, c(Gender,5))


## Study the columns
table(studPerf$Race)
table(studPerf$Parent_Education)
table(studPerf$Test_Prep)
table(studPerf$Lunch)
table(studPerf$Gender)


## Add totalScore Column to tibble
studPerf <- mutate(studPerf, totalScore = studPerf$Math_Score+studPerf$Reading_Score+studPerf$Writing_Score)

## Arrange by total score
arrange(studPerf, desc(studPerf$totalScore))

## Finding: Mean of math scores differs less than mean of writing and reading score
summarise(group_by(studPerf, `Gender`), 
          mean_reading = mean(Reading_Score), 
          mean_writing = mean(Writing_Score), 
          mean_math = mean(Math_Score),
          mean_total = mean(totalScore),
          n=n())

arrange(summarise(group_by(studPerf, Lunch),n=n()), desc (n))
