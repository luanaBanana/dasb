#
# Student Exam Marks Shiny App
#

library(shiny)
library(ggplot2)
library(tidyverse)

# Load helper file ----
source("./Helper.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Student Exam Marks"),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel("Score Prediction", 
                
                selectInput(inputId = "parentEducation", label = "Choose your parents education", choices =  
                                c("associate's degree", "some college", "high school", "some high school", "bachelor's degree", "master's degree")),
                
                selectInput(inputId = "lunch", label = "Choose your lunch", choices = c("free/reduced", "standard")),
                
                selectInput(inputId = "prep", label = "Did you complete a test preparation cours", choice = c("completed", "none")),
                
                selectInput(inputId = "gender", label = "Choose your gender", choices = c("male", "female")),
                
                selectInput(inputId = "race", label = "Select your race", choices = c("group A", "group B", "group C", "group D", "group E")),
                textOutput("distPlot")
                ), 
            tabPanel("Summary",                 selectInput(inputId = "type", label = "Which summary would you like to see", choices = c("writing", "reading", "maths")),
                plotOutput("marksummary")), 
            
            tabPanel("Parent Education", 
                selectInput(inputId = "parentEducation2", label = "Choose your parents education", choices =  
                                     c("associate's degree", "some college", "high school", "some high school", "bachelor's degree", "master's degree")),
                selectInput(inputId = "parentEducation3", label = "Choose your parents education", choices =  
                                c( "some college", "associate's degree", "high school", "some high school", "bachelor's degree", "master's degree")),
                plotOutput("parenteducation"))
        )
    ),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderText({ 
        result <- get_degree(input$parentEducation, input$lunch, input$prep, input$gender, input$race)
        paste("The average score for you inputparameters is math ", result[1], "reading  ", result[2], "writing", result[3])
    })
    
    output$marksummary<-renderPlot({
        if (input$type == "writing"){
            ggplot(studPerf, aes(studPerf$gender, studPerf$`writing score`, color = studPerf$`test preparation course`)) + 
                geom_boxplot() + 
                ggtitle("Writing scores by Gender Boxplot") + 
                xlab("Gender") + 
                ylab("Writing Scores")
        } else if (input$type == "reading"){
            ggplot(studPerf, aes(studPerf$gender, studPerf$`reading score`, color = studPerf$`test preparation course`)) + 
                geom_boxplot() + 
                ggtitle("Reading scores by Gender Boxplot") + 
                xlab("Gender") + 
                ylab("Reading Scores")
        } else if (input$type == "maths"){
            ggplot(studPerf, aes(studPerf$gender, studPerf$`math score`, color = studPerf$`test preparation course`)) + 
                geom_boxplot() + 
                ggtitle("Math scores by Gender Boxplot") + 
                xlab("Gender") + 
                ylab("Math Scores")
        }
       
    })
    
    output$parenteducation<-renderPlot({
        # filter for parenteducation
        filterStudPerf <- filter(studPerf, `parental level of education` == input$parentEducation2 | `parental level of education` == input$parentEducation3)
        # add a combined score for math, reading and writing
        filterStudPerf$score <- (filterStudPerf$`math score` + filterStudPerf$`reading score` + filterStudPerf$`writing score`) / 3
        # plot the result
        ggplot(filterStudPerf, aes(filterStudPerf$`parental level of education`, filterStudPerf$`score`, color = filterStudPerf$`parental level of education`)) + 
            geom_boxplot() + 
            ggtitle("Scores by parental education Boxplot") + 
            xlab("Parent Education") + 
            ylab("Scores")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
