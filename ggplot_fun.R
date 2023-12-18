# check for installation of packages -- install and library if they are not there

if ( is.element("tidyerse", installed.packages()[,1]) == FALSE ){
  install.packages("tidyverse")
  library(tidyverse)
}

# This pacakge has data to play with
if ( is.element("palmerpenguins", installed.packages()[,1]) == FALSE ){
  install.packages("palmerpenguins")
  library(palmerpenguins)
}

# this package has color blind friendly color pallette
if ( is.element("ggthemes", installed.packages()[,1]) == FALSE ){
  install.packages("ggthemes")
  library(ggthemes)
}


# Use gg plot
# this version does a single linear model for all of the data
ggplot( 
  data = penguins,
  mapping = aes( x = flipper_length_mm, y = body_mass_g) 
  ) +
  geom_point(aes(color = species, shape=species)) +
  geom_smooth(method=lm) +
  labs(
    title = "This is my title",
    subtitle = "This is my subtitle",
    x = "This is the x axis",
    y = "This is the y axis"
  ) +
  scale_color_colorblind()


# This version does the lm's per species
ggplot( 
  data = penguins,
  mapping = aes( x = flipper_length_mm, y = body_mass_g, color = species, shape=species) 
) +
  geom_point() +
  geom_smooth(method=lm)


# This version displays the equation of the line

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

## subset just the two columns
my_df <- penguins[,c("flipper_length_mm","body_mass_g")]

# rename them x and y
colnames(my_df) <- c("x", "y")

# solve the line with this function
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


# create plot with line and equation
ggplot( 
  data = penguins,
  mapping = aes( x = flipper_length_mm, y = body_mass_g) 
) +
  geom_point(aes(color = species, shape=species)) +
  geom_smooth(method=lm) +
  labs(
    title = "This is my title",
    subtitle = "This is my subtitle",
    x = "This is the x axis",
    y = "This is the y axis"
  ) +
  scale_color_colorblind() +
  geom_text(x=190, y=5500, label=lm_eqn(my_df), parse=TRUE )








df <- data.frame(x = c(1:100))
df$y <- 2 + 3 * df$x + rnorm(100, sd = 40)
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point()
p




p1 <- p + geom_text(x = 25, y = 300, label = lm_eqn(df), parse = TRUE)



