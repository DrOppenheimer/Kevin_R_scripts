# this version does a single linear model for all of the data

# global
ggplot( 
  data = penguins,
  mapping = aes( x = flipper_length_mm, y = body_mass_g) 
) +
  geom_point(aes(color = species, shape=species)) +
  geom_smooth(method=lm) 


# local
ggplot( 
  data = penguins,
  mapping = aes( x = flipper_length_mm, y = body_mass_g, color = species, shape=species) 
) +
  geom_point() +
  geom_smooth(method=lm)