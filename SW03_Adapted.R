library(ggplot2)

View(mpg)

plot <- ggplot(data = mpg, mapping = aes(x = mpg$hwy))
plot + geom_histogram()
  
  
plot <- ggplot(data = mpg, mapping = aes(x = mpg$hwy))  
plot + geom_violin(mapping = aes(y = mpg$displ))


View(diamonds)

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

# From diamonds {ggplot2} documentation:
#color --> diamond colour, from D (best) to J (worst)
# clarity --> a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_bin2d() +
  facet_wrap(~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_density2d() +
  facet_grid(clarity~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")

ggplot(diamonds, aes(x=carat, y=price, color=cut)) + 
  geom_point() +
  facet_grid(clarity~color) +
  scale_color_manual(values=c("red", "yellow", "green", "blue", "violet")) + 
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,17500),
                     labels=c(0,2.5,5,7.5,10,12.5,15,17.5),
                     name="price(thousands of dollars)")


