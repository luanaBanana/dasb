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

# _______________________________________ predict with Shiny Inputs _________________________________________

# Input Shiny
input_gender <- "female"
input_race <- "group D"
input_lunch <- "standard"
input_education <- "bachelor's degree"
input_prep <- "completed"

input_data <- data.frame(
  Gender = c(input_gender), Race = c(input_race), Lunch = c(input_lunch), Parent_Education = c(input_education), Test_Prep = c(input_prep)
)

# Model to predict Math Score

math_model <- lm(Math_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)

summary(math_model)
math_model
# Model to predict Reading Score
reading_model <- lm(Reading_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)
summary(reading_model)
# Model to predict Writing Score
writing_model <- lm(Writing_Score ~ Race + Lunch + Gender + Parent_Education + Test_Prep,data=studPerf)

# Prediction
output_math <- round(predict(math_model, newdata = input_data), digits = 0)
output_reading <- round(predict(reading_model, newdata = input_data), digits = 0)
output_writing <- round(predict(writing_model, newdata = input_data), digits = 0)

cat("Math Score:", output_math, "Reading Score:", output_reading, "Writing Score:", output_writing)

