library(ggplot2, palmergenguins)

ggplot(
    data = penguins,
  mapping = aes(x=species) 
  ) +
  geom_bar() +
  #scale_color_manual(values = c("red","purple","blue"))
  #scale_fill_manual("legend", values = c("A" = "black", "B" = "orange", "C" = "blue"
  #scale_fill_manual("legend", values=c("Adelie"="red","Gentoo"="purple","Chinstrap"="blue"))
  
  
  ggplot(
    data = penguins,
    mapping = aes(x=fct_infreq(species)) 
  ) +
  geom_bar()

ggplot(
  data = penguins,
  mapping = aes(y=fct_infreq(species)) 
) +
  geom_bar()

# histogram
ggplot(
  data = penguins,
  mapping = aes(x=body_mass_g)
  ) +
    geom_histogram()



ggplot(penguins, aes(x=body_mass_g)) + 
  # Hist has black lines and white fill
  geom_histogram(color="black", fill="white") +

  # Histogram with density plot
ggplot(penguins, aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="red") 
  
  
# Hist with mean line to hist
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean(penguins$body_mass_g, na.rm=TRUE)),
              color="purple", linetype="dashed", linewidth=1)

# hist with mean and density plot
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_vline(aes(xintercept=mean(penguins$body_mass_g, na.rm=TRUE)),
             color="purple", linetype="dashed", linewidth=1) +
  geom_density(alpha=.2, fill="#FF6666")


# Change binwidth
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(binwidth = 20)

ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(binwidth = 200)

# change number of bins
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(bins = 10, na.rm = TRUE)


# Exercises pg 19
# 1
ggplot(
  data = penguins,
  mapping = aes(y=species)
) +
  geom_bar()

# 2
ggplot(penguins, aes(x=species)) +
  geom_bar(color="red")

ggplot(penguins, aes(x=species)) +
  geom_bar(fill="red")

#3
# change number of bins
ggplot(penguins, aes(x=body_mass_g)) +
  geom_histogram(bins = 10, na.rm = TRUE)

#4
# hist of carat from diamonds (tidyverse dataset)
ggplot(
  data=diamonds,
  mapping=aes(x=carat)
) +
  geom_histogram()
  
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.10)

# For more histogram fun see
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization





# Boxplot of penguins data (of body mass by species)
ggplot(
  data=penguins,
  mapping=aes(x=species, y=body_mass_g)
) +
  geom_boxplot()
  
# Density of penguin data (of body mass by species)
ggplot(
  data=penguins,
  # mapping=aes(x=body_mass_g) # all together
  mapping=aes(x=body_mass_g, color=species)# split by species for coloring
) +
  geom_density()

# Density with opaque fill
ggplot(
  data=penguins,
  mapping=aes(x=body_mass_g, color=species, fill=species)# split by species for coloring
) +
  geom_density(alpha=0.5)

# bar (of species by island)
ggplot(
  data=penguins,
  mapping=aes(x=island,fill=species)
) +
  geom_bar()


# scaled bar (of species by island)
ggplot(
  data=penguins,
  mapping=aes(x=island,fill=species)
) +
  geom_bar(position="fill")


# Two numerical variables
ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm,y=body_mass_g)
) +
  geom_point()

# three or more variables
ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm,y=body_mass_g)
) +
  geom_point(aes(color=species, shape=island))

ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm,y=body_mass_g)
) +
  geom_point(aes(color=bill_length_mm))

ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm,y=body_mass_g)
) +
  geom_point(aes(color=species)) +
  facet_wrap(~island)

# Exercised on page 27

# 1
mpg[1,]

# 2
ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point()


ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point(aes(color=year))

ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point(aes(color=trans))

ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point(aes(size=year))

ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point(aes(color=year, size=year))

ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ)
) +
  geom_point(aes(color=trans, size=trans))

# 3
ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=displ,linewidth=trans)
) +
  geom_point()

# 4
ggplot(
  data=mpg,
  mapping=aes(x=hwy,y=hwy)
) +
  geom_point()

# 5 
ggplot(
  data=penguins,
  mapping=aes(x=bill_depth_mm,y=bill_length_mm, color=species)
) +
  geom_point()

ggplot(
  data=penguins,
  mapping=aes(x=bill_depth_mm,y=bill_length_mm)
) +
  geom_point(aes(color=species))

ggplot(
  data=penguins,
  mapping=aes(x=bill_depth_mm,y=bill_length_mm)
) +
  geom_point() +
  facet_wrap(~species)

# 6
ggplot(
  data=penguins,
  mapping=aes(
    x=bill_length_mm, y=bill_depth_mm, color=species, shape=species
    )
) +
  geom_point()
labs(color="Species")
# hugh?

# 7
ggplot(penguins,aes(x=island, fill=species)) +
  geom_bar(position="fill")

ggplot(penguins,aes(x=species, fill=island)) +
  geom_bar(position="fill")


  
