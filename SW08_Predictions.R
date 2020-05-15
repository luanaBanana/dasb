#LECTURE8 --> start understanding and "playing" with models

install.packages("ISLR"); 
library(MASS)
library(ISLR)
library(tidyverse)

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


cor(studPerf$Math_Score,studPerf$Reading_Score,studPerf$Writing_Score)


#strating with the most correlated variable as predictor
lm1 <- lm(Math_Score~Race+Lunch+Parent_Education+Test_Prep,data=studPerf)


lm1
summary(lm1)
par(mfrow=c(2,2)); plot(lm1); par(mfrow=c(1,1))

## Bringt das hier eventuell etwas?
ggplot() + geom_point(data=studPerf, aes(x=Parent_Education, y=Math_Score)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5) 


ggplot() + geom_point(data=studPerf, aes(x=Race, y=Math_Score)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="red", alpha=0.5) 

####CONTINUE HERE

#Let's do some predictions
idx <- c(0.125, 2.2, 5, 10, 15, 22.75, 30, 37.5, 39.95)
pred1 <- predict(lm1,data.frame(lstat=idx), interval="prediction")
pred2 <- predict(lm1,data.frame(lstat=idx), interval="confidence")

pred.frame <-  tibble(idx,pred1[,"fit"],pred1[,"lwr"],pred1[,"upr"],pred2[,"lwr"],pred2[,"upr"])
colnames(pred.frame) <- c("x","y","y_min","y_max","conf_min","conf_max")

ggplot(data=Boston) +  geom_point(data=Boston, aes(x=lstat, y=medv)) + 
  geom_abline(intercept=coef(lm1)[1], slope=coef(lm1)[2], size=2, color="grey", alpha=0.5) + 
  geom_smooth(data=Boston, aes(x=lstat, y=medv)) +
  geom_point(data=pred.frame, aes(x=x, y=y),color="red", size=2) +
  geom_point(data=pred.frame, aes(x=x, y=y_min),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=y_max),color="red", size=4, alpha=0.5, shape =3) +
  geom_point(data=pred.frame, aes(x=x, y=conf_min),color="red", size=4, alpha=0.5, shape =95) +
  geom_point(data=pred.frame, aes(x=x, y=conf_max),color="red", size=4, alpha=0.5, shape =95)

#let's have a look at the variable correlation with medv, in graphical format
ggplot(data=medv.corr) + geom_point(aes(x=col, y=correlation)) + geom_hline(yintercept=0, color="blue", size=2)

#second model, including also "rm"
lm2 <- lm(medv~lstat+rm,data=Boston)

summary(lm2)

#extending further, including also "ptratio"
lm3 <- lm(medv~lstat+rm+ptratio,data=Boston)

summary(lm3)

# what about using ALL the avaialble variables as predictor set (BAD IDEA, in general)
lm.tot <- lm(medv~.,data=Boston)

summary(lm.tot)

#let's prune off the one not significant in the model 
#  --> WE EXPECT this to be the BEST model possible, from the predictive point of view...
lm.red <- lm(medv~.-age-indus,data=Boston)

summary(lm.red)

#let's see what happens when we remove other predictors, starting from the less significant ones
lm.red2 <- lm(medv~.-age-indus-crim-chas,data=Boston)

summary(lm.red2)

#let's cpompare the models obtained, in a structured way
anova(lm1, lm2, lm3, lm.red2, lm.red, lm.tot)



##############   Logistic   ##########################

summary(Default)
View(Default)
library(gridExtra) # for convenient window arrangement in plotting

# Display the distributions of balance and income
p1 <- ggplot(Default, aes(x=income)) + geom_histogram(color="black", fill="white")
p2 <- ggplot(Default, aes(x=balance)) + geom_histogram(color="black", fill="white")
grid.arrange(p1, p2, nrow = 1)
par(mfrow=c(1,1))

# Display the classes in the Default data
p1 <- ggplot(Default, aes(x=balance, y=income)) + geom_point(aes(col=default))
p2 <- ggplot(Default, aes(x=default, y=balance, fill=default)) + geom_boxplot()
p3 <- ggplot(Default, aes(x=default, y=income, fill=default)) + geom_boxplot()
grid.arrange(p1, p2, p3, nrow = 1, widths = c(2, 1, 1))
par(mfrow=c(1,1))


# Create training and test data
set.seed(213)
indices <- sample(1:10000, 250) # select 250 random samples
test.data <- Default[indices,]
table(test.data$default)
training.data <- Default[-indices,]
table(training.data$default)
p1 <- ggplot() + geom_point(data = training.data, aes(x=balance, y=default), color='steelblue3') + 
  geom_point(data = test.data, aes(x=balance, y=default), color='darkred', size=4) 
p2 <- ggplot() + geom_point(data = training.data, aes(x=balance, y=income), color='steelblue3') + 
  geom_point(data = test.data, aes(x=balance, y=income), color='darkred', size=4) 
grid.arrange(p1, p2, nrow = 1)

glm1 <- glm(default~balance, data=training.data, family="binomial")
summary(glm1)

# Making predictions

# "Predicting" the TRAINING data 
pred3 = predict(glm1) # No data set is supplied to the predict() function: the probabilities are computed 
# for the training data that was used to fit the logistic regression model. 
# --> Notice: Without the type option specified in predict we get the linear predictor scale (see next plot)
pred3.df <- data.frame(balance=training.data$balance,prediction=pred3,default=training.data$default) 
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred3.df, aes(x=balance, y=prediction, col=default)) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + ylim(-15,2) # Plot. 

pred.train.probs = predict(glm1, type = "response") 
# With type = "response", we get the response variable scale, i.e., the probabilities.
pred.train.probs.df <- data.frame(balance=training.data$balance,pred.train.probs=pred.train.probs,default=training.data$default) 
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.train.probs.df, aes(x=balance, y=pred.train.probs, col=default)) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) # Plot. 

# Predicting the TEST data PROBABILITIES
pred.test.probs <- predict(glm1, test.data, type = "response")
pred.test.probs.df <- data.frame(balance=test.data$balance,pred.test.probs=pred.test.probs, default=test.data$default)
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.test.probs.df, aes(x=balance, y=pred.test.probs, col=default), size=5) + 
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) + geom_hline(yintercept = 0.5, linetype="dashed") + ylim(0,1)

# Predicting the TEST data CLASSES
pred.test.classes <- rep("No",nrow(test.data)) 
# In order to predict the classes, we must convert the predicted into class labels, Yes or No. We start by converting all to No.
pred.test.classes[pred.test.probs > 0.5] = "Yes"  
# Now we set those to Yes whose proobability is greater than 0.5.
pred.test.classes.df <- data.frame(balance=test.data$balance,pred.test.classes=pred.test.classes)
# make it a data frame for plotting purposes
ggplot() + geom_point(data = pred.test.classes.df, aes(x=balance, y=pred.test.classes, col=test.data$default), size=5)

# Confusion matrix  
table(test.data$default, pred.test.classes)

# Calculating the validation error rate (percentage of incorrectly classified samples) as an estimate of the test error rate
mean(pred.test.classes != test.data$default)

# Predicting probabilities and classes for a balance of 1000 and 2000 Dollars:
new.data <- data.frame(student = c("No", "No","No", "No","No"), balance= c(1000, 2000, 2750, 1900, 1950), income=c(1000, 2000, 0,0, 0)) 
# student and income are arbitrarily set, since they will not be used by predict
predict(glm1, newdata = new.data, type = "response")


# Logistic Regression with >1 PREDICTORS (including qualitative predictors)

# Fitting the model to the training data 
glm2 <- glm(default~., family = "binomial", data = training.data) 
summary(glm2)

glm3 <- glm(default~.-income, family = "binomial", data = training.data) 
summary(glm3)

anova(glm1, glm3, glm2, test = "Chisq")

# Predicting probabilities and classes for a balance of 1000 and 2000 Dollars:
new.data <- data.frame(student = c("No", "No"), balance= c(1000, 2000), income=c(1000, 2000)) # student and income are arbitrarily set, since they will not be used by predict
(pred.glm2a <- predict(glm2, newdata = new.data, type = "response"))

class.glm2a <- rep("No",length(pred.glm2a)) 
class.glm2a[pred.glm2a > 0.5] = "Yes" 
class.glm2a

new.data2 <- data.frame(student = c("No", "Yes"), balance= c(1000, 2000), income=c(1000, 2000)) # student and income are arbitrarily set, since they will not be used by predict
(pred.glm2b <- predict(glm2, newdata = new.data2, type = "response"))

class.glm2b <- rep("No",length(pred.glm2b)) 
class.glm2b[pred.glm2b > 0.5] = "Yes" 
class.glm2b

new.data3 <- data.frame(student = c("Yes", "Yes"), balance= c(1000, 2000), income=c(1000, 2000)) # student and income are arbitrarily set, since they will not be used by predict
(pred.glm2c <- predict(glm2, newdata = new.data3, type = "response"))

class.glm2c <- rep("No",length(pred.glm2c)) 
class.glm2c[pred.glm2c > 0.5] = "Yes" 
class.glm2c


