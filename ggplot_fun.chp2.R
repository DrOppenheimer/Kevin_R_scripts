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