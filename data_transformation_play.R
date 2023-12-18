install.packages("nycflights13")
library(nycflights13)
library(tidyverse)

flights |> # Pipe means (flights, more_args)
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# alt/option + - is shortcut in Rstudio for " <- "
# Fun with dplyr
# The first argument is a dataframe
# The second argument usually specifies the df columns to operate on
# The output is always a new df

# filter() # filter row content
filtered_flights <- filter(flights, dest == "IAH")
# alternatively
filtered_flights <- flights |> filter( dep_delay >= 120 )
# logic shortcut for == | == is %in%
filtered_flights <- flights |> filter(month==1 | month==2)
# is the same as 
filtered_flights <- flights |> filter(month %in% c(1,2))
