library(palmerpenguins, ggplot)

ggplot(penguins, 
       aes(x = flipper_length_mm, y = bill_length_mm)) +
  geom_point(aes(color = species, shape = species)) +
  #scale_color_manual(values = c("darkorange","purple","cyan4")) +
  scale_color_manual(values = c("red","purple","blue")) +
  labs(
    title = "Flipper and bill length",
    subtitle = "Dimensions for penguins at Palmer Station LTER",
    x = "Flipper length (mm)", y = "Bill length (mm)",
    color = "Penguin species", shape = "Penguin species"
  ) +
  theme_minimal()

# Load required library 
library(ggplot2) 
# Sample data 
data <- data.frame( Category = rep(c("A", "B", "C"), each = 50), 
                         Value = c(rnorm(50, 10), rnorm(50, 15), rnorm(50, 12)) ) 
# Creating the boxplot with explicit fill colors 
ggplot(data, aes(x = Category, y = Value, fill = Category)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("red", "blue", "purple")) + 
  labs(fill = "Categories") + theme_minimal()

