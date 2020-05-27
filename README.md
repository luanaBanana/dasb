# Data Science Basic FS20 - Group 10
*This file is best viewed as a Markdown file*

## Used Dataset 

[Kaggle URL](https://www.kaggle.com/spscientist/students-performance-in-exams)

[Locally](./StudentsPerformance.csv)

## R Scripts

Each file contains the needed installations, imports and relative path to the datasource in order to be executable on its own.

### Overview

An overview containing EDA, Modelling and Prediction can be found [here](./Overview.R).

More detailed scripts for each topic are described below.

### EDA

EDA can be found in the files: 
- [SW02_EDA.R](./SW02_EDA.R)
- [SW03_EDA.R](./SW03_EDA.R)

### Modelling / Prediction

Modelling and prediction Code can be found within the files 
- [SW08_Predictions.R](./SW08_Predictions.R)
- [SW09_Predictions_Lab1.R](./SW09_Predictions_Lab1.R)
- [SW09_Predictions_Lab2.R](./SW09_Predictions_Lab2.R)
- [SW09_Predictions_Lab3.R](./SW09_Predictions_Lab3.R)
- [SWXX_MorePredictions.R](./SWXX_MorePredictions.R)

### Graphs

Graphs can be found throughout the whole project.

## Shiny App

The Shiny App can be found as an [R Project by itself](./ShinyApp).

Make sure to open it as a separate project.

### Score Prediction

In the first tab "Score Prediction" you can choose the input factors and predict your score dependently. You can try to find the input factors for the maximal score.

### Summary

The second tab "summary" shows the boxplot of the choosable reading, writing and math scores depending on test preparation and gender. Other factors could be chosen here too, we decided to limit this example to "test preparation" and "gender".

### Compare Parental Education

The third and last tab gives the possibility to directly compare factors and their avarage score with the help of a box plot. 
Other factors could be chosen here too, we decided to limit this example to the parental education level.
