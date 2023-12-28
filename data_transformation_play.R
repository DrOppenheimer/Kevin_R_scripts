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

# ROWS
# filter() # filter row content
filtered_flights <- filter(flights, dest == "IAH")
# alternatively
filtered_flights <- flights |> filter( dep_delay >= 120 )
# logic shortcut for == | == is %in%
filtered_flights <- flights |> filter(month==1 | month==2)
# is the same as 
filtered_flights <- flights |> filter(month %in% c(1,2))

# arrange() # change order of rows based on value of columns
arranged_flights <- flights |> arrange(year,month,day,dep_time)
arranged_flights <- flights |> arrange(desc(month))

# distinct() # find unique rows
distinct_flights <- flights |> distinct() # remove duplicate rows
distinct_flights <- flights |> distinct(origin) # retain all distinct origins
distinct_flights <- flights |> distinct(origin,dest) # retain all distinct origin destination pairs
# use .keep_all = TRUE to retain all columns
distinct_flights <- flights |> distinct(origin,dest,.keep_all = TRUE)

# count() # count the number of occurrences, sort=TRUE for descending sort
counted_flights <- flights |> count(origin,dest,sort=TRUE)

# Exercises on page 44
# 1
selected_flights <- flights |> 
  filter(arr_delay >= 120) |>
  filter(dest %in% c("IAH","HOU")) |>
  filter(carrier %in% c("UA","AA","DL")) |>
  filter(month %in% c(7,8,9)) |>
  # ...
  
# 2
arranged_flights <- flights |> arrange(dep_delay)

arranged_flights <- flights |> arrange(dep_time)


# COLUMNS
# mutate() # add new column(s) calculated from existing
mutated_flights <- flights |> mutate( 
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60
  )
# by defult they are added to the right
# this way they are added at the beginning
mutated_flights <- flights |> mutate( 
  gain = dep_delay - arr_delay,
  speed = distance / air_time * 60,
  .before = 1
)

# select() # i.e. select columns
selected_flights <- flights |> select(year,month,day) # select by column name
selected_flights <- flights |> select(year:day) # select range of columns, inclusive
selected_flights <- flights |> select(!year:day) # select range of columns to exclude
selected_flights <- flights |> select(where(is.character)) # select character columns

# rename() # rename columns
renamed_flights <- flights |> rename(new_name = old_name)
# can also do this with select
selected_flights <- flights |> select(new_name = old_name)
# janitor::clean_names() # automates this , somehow

# relocate() # relocate columns - by default to the front, or using .before or .after
relocated_flights <- flights |> relocate(time_hour)



flights |> # Pipe means (flights, more_args)
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )