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

## Ergebnis -> M?nner durchschnittlich h?her
studPerf %>% group_by(gender) %>% summarise(minMath= min(`math score`), 
                                            maxMath=max(`math score`), 
                                            meanMath=mean(`math score`), 
                                            sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

## Ergebnis -> Eine Gruppe deutlich h?her.. 
studPerf %>% group_by(`race/ethnicity`) %>% summarise(minMath= min(`math score`), 
                                            maxMath=max(`math score`), 
                                            meanMath=mean(`math score`), 
                                            sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                            nr=n()) %>% ungroup()

## Ergebnis -> standard lunch deutlich h?her.. 
studPerf %>% group_by(`lunch`) %>% summarise(minMath= min(`math score`), 
                                                      maxMath=max(`math score`), 
                                                      meanMath=mean(`math score`), 
                                                      sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                      nr=n()) %>% ungroup()

## Ergebnis -> bei Eltern mit einem degree h?her
studPerf %>% group_by(`parental level of education`) %>% summarise(minMath= min(`math score`), 
                                             maxMath=max(`math score`), 
                                             meanMath=mean(`math score`), 
                                             sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                             nr=n()) %>% ungroup()

## Ergebnis -> kein grosser Unterschied zwischen completed und none -> bei reading/writing jedoch schon (was heisst das? A la entweder man versteht es oder nicht?)

studPerf %>% group_by(`test preparation course`) %>% summarise(minMath= min(`math score`), 
                                                                   maxMath=max(`math score`), 
                                                                   meanMath=mean(`math score`), 
                                                                   sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                                   nr=n()) %>% ungroup()

studPerf %>% group_by(`test preparation course`) %>% summarise(minReading= min(`reading score`), 
                                                               maxReading=max(`reading score`), 
                                                               meanReading=mean(`reading score`), 
                                                               sdMath = sd(`reading score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                               nr=n()) %>% ungroup()

studPerf %>% group_by(`test preparation course`) %>% summarise(minWriting= min(`writing score`), 
                                                               maxWriting=max(`writing score`), 
                                                               meanWriting=mean(`writing score`), 
                                                               sdMath = sd(`writing score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                               nr=n()) %>% ungroup()

##### Zu lim_inf / lim_sup : Der Limes superior und der Limes inferior ist der gr??te und der kleinste H?ufungspunkt einer Folge. 
##### Diese dienen als partiellen Ersatz f?r den Grenzwert, wenn dieser nicht existiert. 
##### Macht aber bei unserem DataSet keinen Sinn weil wir einen Grenzwert haben.

studPerf %>% group_by(`gender`,`parental level of education`)  %>% summarise(minMath= min(`math score`), 
                                                                        maxMath=max(`math score`), 
                                                                        meanMath=mean(`math score`), 
                                                                        sdMath = sd(`math score`), #Standardabweichung -> durchschnittliche Entfernung aller Antworten zum Mittelwert
                                                                        lim_inf=meanMath-2*sdMath, lim_sup=meanMath+2*sdMath,
                                                                        nr=n()) %>% ungroup()
## Sehr h?bsch: kann noch f?r Reading Writing gemacht werden
ggplot(data=studPerf, mapping=aes(x=studPerf$`reading score`,y=studPerf$`math score`)) + 
  geom_point(aes(color=gender)) + 
  facet_wrap(.~studPerf$`parental level of education`) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data=studPerf, mapping=aes(x=studPerf$`reading score`,y=studPerf$`math score`)) + 
  geom_point(aes(color=gender)) + 
  facet_wrap(.~studPerf$`race/ethnicity`) + 
  geom_smooth(method = lm, se = FALSE)


library(gridExtra)

library(nnet) # for using multiclass classification 

mod_lin <- multinom(
  studPerf$`math score` ~ studPerf$`lunch` + studPerf$`gender` + studPerf$`parental level of education` , data=studPerf)

math_range <- seq_range(studPerf$`math score`, n=100, pretty=TRUE)
writing_range <- seq_range(studPerf$`writing score`, n=100, pretty=TRUE)


stud_perf_math <- studPerf %>% data_grid(math = math_range, writing = writing_range) %>% gather_predictions(mod_lin)

### -------------------------------------------------------------------------
### BIS HIER

mod_lin <- multinom(Index ~ Height + Weight, data=BMI_ds)
mod_non_lin <- multinom(Index ~ poly(Height,2) * Weight, data=BMI_ds) 
mod_non_lin2 <- multinom(Index ~ I(Height^2) * Weight, data=BMI_ds) # real formula BMI = Weight / Height^2

Height_range <- seq_range(BMI_ds$Height, n=150, pretty=TRUE)
Weight_range <- seq_range(BMI_ds$Weight, n=300, pretty=TRUE)

BMI_ds_for <- BMI_ds %>% data_grid(Height = Height_range, Weight = Weight_range) %>%
  gather_predictions(mod_lin, mod_non_lin, mod_non_lin2)

ggplot(data=BMI_ds, aes(x=Height,y=Weight)) + 
  geom_point(aes(color=Index)) + 
  geom_tile(data = BMI_ds_for, aes(fill=pred), alpha = 0.3, show.legend = F) +
  facet_wrap(.~model) + 
  theme_bw(base_size = 15) +
  ggtitle('Model comparisons') +
  coord_fixed(ratio = 0.8) + 
  theme(axis.ticks=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_blank(), axis.title=element_blank(), legend.position = 'none')

# let's use a regression on the actual BMI value
mod_lin_regression <- lm(BMI ~ I(Height^2) * Weight, data=BMI_ds)

predictions <- predict(mod_lin_regression,BMI_ds)

BMI_ds_enriched <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

#let's add an interaction with the gender
mod_lin_regression2 <- lm(BMI ~ I(Height^2) * Weight * Gender, data=BMI_ds)

predictions2 <- predict(mod_lin_regression2,BMI_ds)

BMI_ds_enriched2 <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched2 %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched2, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

anova(mod_lin_regression,mod_lin_regression2)

#let's consider the gender as indipendent factor only
mod_lin_regression3 <- lm(BMI ~ I(Height^2) * Weight + Gender, data=BMI_ds)

predictions3 <- predict(mod_lin_regression3,BMI_ds)

BMI_ds_enriched3 <- BMI_ds %>% mutate(pred = predictions, delta=pred-BMI)

BMI_ds_enriched3 %>% group_by(Gender, Index) %>% summarise(delta=mean(delta), nr=n())

ggplot(data=BMI_ds_enriched3, aes(x=Height,y=Weight, color=delta)) + geom_point(size=3) + facet_grid(Index~Gender)

anova(mod_lin_regression,mod_lin_regression3,mod_lin_regression2)

##################### Function application ########################################################

##change in the variable space by function application
library(ggplot2)
View(diamonds)

cor(diamonds$carat,diamonds$price)

ggplot(data=diamonds, aes(x=carat,y=price)) + geom_point(color="blue")
ggplot(data=diamonds, aes(x=carat)) + geom_histogram(color="blue")
ggplot(data=diamonds, aes(x=price)) + geom_histogram(color="red")
ggplot(data=diamonds, aes(x=log(price))) + geom_histogram(color="red")
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50)

ggplot(data=diamonds, aes(x=log(carat),y=log(price))) + geom_point(color="red")
ggplot(diamonds, aes(log(carat), price)) +  geom_hex(bins = 50)
ggplot(diamonds, aes(carat, log(price))) +  geom_hex(bins = 50)
ggplot(diamonds, aes(log(carat), log(price))) +  geom_hex(bins = 50)

library(tidyverse)
diamonds2 <- diamonds %>% mutate(log_price = log(price), log_carat = log(carat))

mod_diam_1 <- lm(price ~ carat, data=diamonds)
mod_diam_1_resid <- diamonds %>% mutate(pred = predict(mod_diam_1,diamonds), delta = price - pred)
mod_diam_1_resid

#let's remove the intercept : force it to pass from the origin of the axes
mod_diam_1_bis <- lm(price ~ carat - 1, data=diamonds)
mod_diam_1_bis_resid <- diamonds %>% mutate(pred = predict(mod_diam_1_bis,diamonds), delta = price - pred)
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_1_resid,aes(carat,pred), color = "red", size = 1, alpha=0.5)+ 
  geom_line(data=mod_diam_1_bis_resid,aes(carat,pred), color = "orange", size = 1, alpha=0.5)

mod_diam_2 <- lm(log(price) ~ log(carat), data=diamonds)
mod_diam_2_bis <- lm(log_price ~ log_carat, data=diamonds2)
mod_diam_2_resid <- diamonds %>% mutate(pred = predict(mod_diam_2,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_2_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)


#let's analyze the residuals of this model,
ggplot(mod_diam_2_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) 

#let's analyze the residuals of this model, wrt the other variables
ggplot(mod_diam_2_resid, aes(cut, delta_log)) + geom_boxplot()
# ggplot(mod_diam_2_resid, aes(cut, delta_log)) + geom_jitter()
ggplot(mod_diam_2_resid, aes(color, delta_log)) + geom_boxplot()
ggplot(mod_diam_2_resid, aes(clarity, delta_log)) + geom_boxplot()
ggplot(mod_diam_2_resid, aes(depth, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(x, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(y, delta_log)) + geom_point()
ggplot(mod_diam_2_resid, aes(z, delta_log)) + geom_point()

mod_diam_3 <- lm(log(price) ~ log(carat) + cut + color + clarity, data=diamonds)

anova(mod_diam_2,mod_diam_3)

mod_diam_3_resid <- diamonds %>% mutate(pred = predict(mod_diam_3,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_3_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)

#let's analyze the residuals of this model,
ggplot(mod_diam_3_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) + 
  geom_hline(yintercept=0,color="red",alpha=0.5,size=2)

ggplot(mod_diam_3_resid, aes(depth, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(x, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(y, delta_log)) + geom_point()
ggplot(mod_diam_3_resid, aes(z, delta_log)) + geom_point()

#as a verification, let's see how the model with also these variables as predictor performs
mod_diam_4 <- lm(log(price) ~ log(carat) + cut + color + clarity + depth + x +y + z, data=diamonds)

anova(mod_diam_2,mod_diam_3, mod_diam_4)

mod_diam_4_resid <- diamonds %>% mutate(pred = predict(mod_diam_4,diamonds), delta = price - exp(pred)) %>%
  mutate(delta_log = log(price)-pred) # %>% add_residuals(mod_diam_2,"delta_log_2") ## <-- will give the same result
ggplot(diamonds, aes(carat, price)) +  geom_hex(bins = 50) + 
  geom_line(data=mod_diam_4_resid,aes(carat,exp(pred)), color = "red", size = 1) +
  ylim(0,19000) + xlim(0,2.5)

#let's analyze the residuals of this model,
ggplot(mod_diam_4_resid, aes(log(carat), delta_log)) +  geom_hex(bins = 50) + 
  geom_hline(yintercept=0,color="red",alpha=0.5,size=2)
