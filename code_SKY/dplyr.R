library(dplyr)
library("nycflights13")


#show all data frame
data <- tbl_df(mtcars)
data

#filter

filter(flights, month == 1, day == 1)
flights %>%
        filter( dep_delay < 10 )
flights %>%
        filter( hour < 12, arr_delay <= 0 )

#arrange = order
flights %>%
        filter( hour < 8 ) %>%
        arrange( year, month, day )

flights %>%
        arrange( desc(dep_delay))
# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day
select(flights, year:day)
# Select all columns except those from year to
# day (inclusive)
select(flights, -(year:day))

#mutate = compute new variables
d <- flights %>%
             mutate(
                    gain = arr_delay - dep_delay,
                    speed = distance / air_time * 60
                  ) %>%
              filter( gain > 0 ) %>%
              arrange( desc(speed) )

d %>%
  select( year, month, day, dest, gain, speed ) 

#summarize
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))
flights %>%
        filter( dep_delay > 0 ) %>%
        summarise(arr_delay = mean(arr_delay, na.rm = TRUE))

#group_by
flights %>%
        group_by( tailnum ) %>%
        summarise(
                  count = n(),
                  dist = mean(distance, na.rm = TRUE),
                  delay = mean(arr_delay, na.rm = TRUE)
                  ) %>%
        filter( is.finite(delay) ) %>%
        arrange( desc(count) )

flights %>%
        group_by(dest) %>%
        summarise(
                  planes = n_distinct(tailnum),
                  flights = n()
                  ) %>%
        arrange( desc(flights) )

# inner_join
# all rows from x where there are matching
# values in y, and all columns from x and y. If there are multiple matches
# between x and y, all combination of the matches are returned.
destinations <- flights %>%
                group_by(dest) %>%
                summarise(
                planes = n_distinct(tailnum),
                flights = n() ) %>%
                arrange( desc(flights) ) %>%
                rename( faa = dest )

inner_join( destinations, airports,
            by = "faa")


destinations <- flights %>%
                group_by(dest) %>%
                summarise(
                  planes = n_distinct(tailnum),
                  flights = n()) %>%
                arrange( desc(flights) )

inner_join( destinations, airports,
            by = c( "dest" = "faa" ))

