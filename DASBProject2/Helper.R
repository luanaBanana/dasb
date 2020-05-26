library(tidyverse)
library(modelr)

# Load data ----
studPerf <- read_csv("./StudentsPerformance.csv")
studPerf2 <- read_csv("./StudentsPerformance.csv",
                     col_types = cols(gender = col_factor(levels = c("female", "male")), 
                                      'race/ethnicity' = col_factor(levels = c("group A", "group B", "group C", "group D", "group E")),
                                      'parental level of education' = col_factor(levels = c("bachelor's degree", "some college", "master's degree", "associate's degree", "high school", "some high school")), 
                                      lunch = col_factor(levels = c("free/reduced", "standard")),
                                      'test preparation course' = col_factor(levels = c("none", "completed")),
                                      'math score' = col_integer(), 
                                      'reading score' = col_integer(),
                                      'writing score' = col_integer()))

studPerf2 <- as.data.frame(studPerf2)
summary(studPerf2) # summary
# There are no NA or missing values

## Replace column names
colnames(studPerf2)
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(studPerf2) <- namesOfColumns

get_degree <- function(iparentEducation, ilunch, iprep, igender, irace) {
  input_data <- data.frame(
    Gender = c(igender), Race = c(irace), Lunch = c(ilunch), Parent_Education = c(iparentEducation), Test_Prep = c(iprep)
  )
  
  # Model to predict Math Score
  
  math_model <- lm(Math_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf2)
  
  summary(math_model)
  math_model
  # Model to predict Reading Score
  reading_model <- lm(Reading_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf2)
  summary(reading_model)
  # Model to predict Writing Score
  writing_model <- lm(Writing_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf2)
  
  # Prediction
  output_math <- round(predict(math_model, newdata = input_data), digits = 0)
  output_reading <- round(predict(reading_model, newdata = input_data), digits = 0)
  output_writing <- round(predict(writing_model, newdata = input_data), digits = 0)
  
  #create result
  result <- c(output_math, output_reading, output_writing)
  
  # return the result
  return(result)
}
