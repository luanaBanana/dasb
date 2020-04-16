library(tidyverse)

studPerf <- read_csv("C:/Users/Luana/HSLU/DASB/dasb/StudentsPerformance.csv")
View(studPerf)

studPerf <- as_tibble(studPerf)

## Finding --> More females top scores in writing / reading, more male topscorers in math.
filter(studPerf, studPerf$`writing score` > 90)
filter(studPerf, studPerf$`math score` > 90)
filter(studPerf, studPerf$`reading score` > 90)

## Finding --> Topscorers mostly completed test preparation course, have parents higher education level and ate standard lunch 
arrange(studPerf, desc(studPerf$`math score`))
arrange(studPerf, studPerf$`math score`) #without specifying "desc" it will automatically choose "asc"

## Finding --> Topscorers mostly completed test preparation course, have parents higher education level and ate standard lunch 
arrange(studPerf, desc(studPerf$`writing score`))
arrange(studPerf, studPerf$`writing score`)

## Finding --> Topscorers mostly completed test preparation course, have parents higher education level and ate standard lunch 
arrange(studPerf, desc(studPerf$`reading score`))
arrange(studPerf, studPerf$`reading score`)


select(studPerf , -1)
select(studPerf, c(2,3,4))
select(studPerf, c(gender,5))


table(studPerf$`race/ethnicity`)
table(studPerf$`parental level of education`)
table(studPerf$`test preparation course`)
table(studPerf$lunch)

## Add totalScore Column to tibble
studPerf <- mutate(studPerf, totalScore = studPerf$`math score`+studPerf$`reading score`+studPerf$`writing score`)

## Arrange by total score
arrange(studPerf, desc(studPerf$totalScore))

## Finding: Mean of math scores differs less than mean of writing and reading score
summarise(group_by(studPerf, `gender`), 
          mean_reading = mean(`reading score`), 
          mean_writing = mean(`writing score`), 
          mean_math = mean(`math score`),
          n=n())

arrange(summarise(group_by(studPerf, `lunch`),n=n()), desc (n))