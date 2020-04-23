library(ggplot2)

View(studPerf)

mathPlot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`math score`))
mathPlot + geom_histogram()

readPlot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`reading score`))
readPlot + geom_histogram()

writePlot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`writing score`))
writePlot + geom_histogram()
  
  
plot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`gender`))  
plot + geom_violin(mapping = aes(y = studPerf$`math score`))

plot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`lunch`))  
plot + geom_violin(mapping = aes(y = studPerf$`math score`))

plot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`race/ethnicity`))  
plot + geom_violin(mapping = aes(y = studPerf$`math score`))

plot <- ggplot(data = studPerf, mapping = aes(x = studPerf$`parental level of education`))  
plot + geom_violin(mapping = aes(y = studPerf$`math score`))


ggplot(studPerf, aes(x=studPerf$`parental level of education`, y=studPerf$`math score`, color=gender)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="math score")

ggplot(studPerf, aes(x=studPerf$`race/ethnicity`, y=studPerf$`math score`, color=gender)) + 
  geom_point() +
  scale_color_manual(values=c("green", "red")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="reading score")

ggplot(studPerf, aes(studPerf$`math score`)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=gender)) + 
  xlab("Math Scores") + 
  ggtitle("Math Scores by Gender")

ggplot(studPerf, aes(studPerf$`reading score`)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=gender)) + 
  xlab("Reading Scores") + 
  ggtitle("Reading Scores by Gender")

ggplot(studPerf, aes(studPerf$`writing score`)) + 
  geom_histogram(binwidth=5, color="gray", aes(fill=gender)) + 
  xlab("Writing Scores") + 
  ggtitle("Writing Scores by Gender")


ggplot(studPerf, aes(x=studPerf$`gender`, y=studPerf$`math score`, color=lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="math score")

ggplot(studPerf, aes(x=studPerf$`gender`, y=studPerf$`writing score`, color=lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="writing score")

ggplot(studPerf, aes(x=studPerf$`gender`, y=studPerf$`reading score`, color=lunch)) + 
  geom_bin2d() +
  facet_wrap(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("green", "red")) +
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="reading score")


ggplot(studPerf, aes(x=studPerf$`math score`, y=studPerf$`reading score`)) +
  geom_density2d() +
  facet_grid(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=studPerf$`math score`, y=studPerf$`reading score`,color=gender)) +
  geom_density2d() +
  facet_grid(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=studPerf$`math score`, y=studPerf$`reading score`,color=studPerf$`test preparation course`)) +
  geom_density2d() +
  facet_grid(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=studPerf$`math score`, y=studPerf$`reading score`,color=gender)) + 
  geom_point() +
  facet_grid(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("red", "green")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")

ggplot(studPerf, aes(x=studPerf$`math score`, y=studPerf$`reading score`,color=studPerf$`test preparation course`)) + 
  geom_point() +
  facet_grid(vars(studPerf$`parental level of education`)) +
  scale_color_manual(values=c("blue", "yellow")) + 
  scale_y_continuous(breaks=c(0,25,50,75,100),
                     labels=c(0,25,50,75,100),
                     name="Reading score")


ggplot(studPerf, aes(studPerf$gender, studPerf$`writing score`, color = studPerf$`test preparation course`)) + 
  geom_boxplot() + 
  ggtitle("Writing scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Writing Scores")


ggplot(studPerf, aes(studPerf$gender, studPerf$`reading score`, color = studPerf$`test preparation course`)) + 
  geom_boxplot() + 
  ggtitle("REading scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Reading Scores")

ggplot(studPerf, aes(studPerf$gender, studPerf$`writing score`, color = studPerf$`test preparation course`)) + 
  geom_boxplot() + 
  ggtitle("REading scores by Gender Boxplot") + 
  xlab("Gender") + 
  ylab("Writing Scores")


