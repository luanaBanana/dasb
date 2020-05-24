library(ggplot2)

## Read data source and modify column names
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
namesOfColumns <- c("Gender","Race","Parent_Education","Lunch","Test_Prep","Math_Score","Reading_Score","Writing_Score")
colnames(studPerf) <- namesOfColumns
studPerf <- as_tibble(studPerf)
studPerf <- mutate(studPerf, totalScore = studPerf$Math_Score+studPerf$Reading_Score+studPerf$Writing_Score)

View(studPerf)

mathPlot <- ggplot(data = studPerf, mapping = aes(x = Math_Score))
mathPlot + geom_histogram()

readPlot <- ggplot(data = studPerf, mapping = aes(x = Reading_Score))
readPlot + geom_histogram()

writePlot <- ggplot(data = studPerf, mapping = aes(x = Writing_Score))
writePlot + geom_histogram()
  
  
plot <- ggplot(data = studPerf, mapping = aes(x = Gender))  
plot + geom_violin(mapping = aes(y = Math_Score))

plot <- ggplot(data = studPerf, mapping = aes(x =Lunch))  
plot + geom_violin(mapping = aes(y = Math_Score))

plot <- ggplot(data = studPerf, mapping = aes(x = Race))  
plot + geom_violin(mapping = aes(y = Math_Score))

plot <- ggplot(data = studPerf, mapping = aes(x = Parent_Education))  
plot + geom_violin(mapping = aes(y = Math_Score))


ggplot(studPerf, aes(x=Parent_Education, y=Math_Score, color=Gender)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="math score")

ggplot(studPerf, aes(x=Race, y=Math_Score, color=Gender)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="reading score")

ggplot(studPerf, aes(Math_Score)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=Gender)) + 
  xlab("Math Scores") + 
  ggtitle("Math Scores by Gender")

ggplot(studPerf, aes(Reading_Score)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=Gender)) + 
  xlab("Reading Scores") + 
  ggtitle("Reading Scores by Gender")

ggplot(studPerf, aes(Writing_Score)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=Gender)) + 
  xlab("Writing Scores") + 
  ggtitle("Writing Scores by Gender")


ggplot(studPerf, aes(x=Gender, y=Math_Score, color=Lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(Parent_Education)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="math score")

ggplot(studPerf, aes(x=Gender, y=Writing_Score, color=Lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(Parent_Education)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="writing score")

ggplot(studPerf, aes(x=Gender, y=Reading_Score, color=Lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(Parent_Education)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="reading score")


ggplot(studPerf, aes(x=Math_Score, y=Reading_Score)) +
  geom_density2d() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Gender)) +
  geom_density2d() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Test_Prep)) +
  geom_density2d() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Gender)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=Math_Score, y=Reading_Score,color=Test_Prep)) + 
  geom_point() +
  facet_grid(vars(Parent_Education)) +
  scale_color_manual(values=c("blue", "yellow")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")


ggplot(studPerf, aes(Gender, Writing_Score, color = Test_Prep)) + 
  geom_boxplot() + 
  ggtitle("Writing scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Writing Scores")


ggplot(studPerf, aes(Gender, Reading_Score, color = Test_Prep)) + 
  geom_boxplot() + 
  ggtitle("REading scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Reading Scores")

ggplot(studPerf, aes(Gender, Math_Score, color = Test_Prep)) + 
  geom_boxplot() + 
  ggtitle("Math scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Math Scores")


