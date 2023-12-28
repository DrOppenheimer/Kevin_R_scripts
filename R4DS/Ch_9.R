library(ggplot2)

# see https://mine-cetinkaya-rundel.github.io/r4ds-solutions to cheat

# Exercises pg 125
# 2
ggplot(
  data=mpg,
  mapping=aes(x=displ, y=hwy)
) +
  geom_smooth( aes(color=drv), show.legend = TRUE )

#4.1
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point() +
  geom_smooth(se=FALSE)

#4.2
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point() +
  geom_smooth(aes(group=drv)) # I can't find the group argument in any documentation

#4.3
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy, color=drv)
) +
  geom_point() +
  geom_smooth(aes(group=drv), se=FALSE) # I can't find the group argument in any documentation

# 4.4
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point(aes(color=drv)) +
  geom_smooth(se=FALSE) 


# 4.5
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy,color=drv)
) +
  geom_point(aes(color=drv)) +
  geom_smooth(aes(linetype=drv, color=NULL),se=FALSE) # cheat leaves out the null here

# 4.6
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy,color=drv)
) +
  geom_point(size=10, color="white") + # the order of these two calls to geom_point DOES matter
  geom_point(aes(color=drv))
  


# Facets
sort(unique(mpg$cyl))

# one variable facet
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point() +
  facet_wrap(~cyl)


# two variable facet
sort(unique(mpg$cyl))
sort(unique(mpg$drv))
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point() +
  facet_grid(drv~cyl)


# two variable facet with free scaling
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
) +
  geom_point() +
  facet_grid(drv~cyl, scales="free_y")

# exercises p 128

#2
ggplot(mpg)+
  geom_point(aes(x=drv,y=cyl))


#3
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_grid(drv ~ .)

ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_grid(.~drv)

ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_wrap(~drv)

#6
ggplot(mpg,aes(x=displ))+
  geom_histogram()+
  facet_grid(drv~.)

ggplot(mpg,aes(x=displ))+
  geom_histogram()+
  facet_grid(.~drv)

#7
ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_grid(drv~.)

ggplot(mpg)+
  geom_point(aes(x=displ,y=hwy))+
  facet_wrap(~drv)


# Statistical transformations
ggplot(diamonds, aes(x=cut))+
  geom_bar()

diamonds |>
  count(cut) |>
  ggplot(aes(x=cut, y=n))+
  geom_bar(stat="identity")

ggplot(diamonds,aes(x=cut, y=after_stat(prop), group=1))+
  geom_bar()





