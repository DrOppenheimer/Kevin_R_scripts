# From: https://r-graph-gallery.com/boxplot.html
# https://r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html



# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

# You miss bimodal with normal boxplot:



# Boxplot basic
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

# Violin basic
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Violin chart") +
  xlab("")

####################################################################
# Alternatively, base R boxplot with Jitter # Doesn't work

# Create data
names <- c(rep("A", 80) , rep("B", 50) , rep("C", 70))
value <- c( rnorm(80 , mean=10 , sd=9) , rnorm(50 , mean=2 , sd=15) , rnorm(70 , mean=30 , sd=10) )
data <- data.frame(names,value)

# Basic boxplot
boxplot(data$value ~ data$names , col=terrain.colors(4) )

# Add data points
mylevels <- levels(data$names)
levelProportions <- summary(data$names)/nrow(data)
for(i in 1:length(mylevels)){
  
  thislevel <- mylevels[i]
  thisvalues <- data[data$names==thislevel, "value"]
  
  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9)) 
  
}




