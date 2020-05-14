### Prediction model



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

randIndex <- sample(1:dim(studPerf)[1])
#  # In order to split data, create a 2/3 cutpoint and round the number
cutpoint2_3 <- floor(2*dim(studPerf)[1]/3)

# create train data set, which contains the first 2/3 of overall data
trainData <- studPerf[randIndex[1:cutpoint2_3],]
# dim(trainData)
# head(trainData)

# create test data, which contains the left 1/3 of the overall data
testData <- studPerf[randIndex[(cutpoint2_3+1):dim(studPerf)[1]],]
dim(testData)   # check test data set
head(trainData)

#------------------------------------------------------lm model
model <- lm(Math_Score ~ Writing_Score + Gender + Race + Lunch + Parent_Education + Test_Prep,data=trainData)
summary(model)
lmPred <- predict(model,testData,interval = "prediction", level=0.95)
summary(lmPred)
head(lmPred)

# 1. Add predictions 
mydata1 <- cbind(testData, lmPred)
head(mydata1)
# 2. Regression line + confidence intervals
p <- ggplot(mydata1, aes( fit, Math_Score)) +
  geom_point(color="green") +
  stat_smooth(method = lm)

ggplot(mydata1, aes( fit, Math_Score)) +
  geom_point() +
  stat_smooth(method = lm)


# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  xlab("Predicted Scores") + ylab("Test Scores")

