library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
fires <- read.csv('forestfires.csv')
head(fires)
month_order <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                 'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
day_order <- c('sun', 'mon', 'tue', 'wed', 'thu', 'fri',
               'sat')
fires <- fires %>% 
  mutate(month = factor(month, level = month_order),
         day = factor(day, level = day_order))

#during which months are forest fires most common?
fire_month <- fires %>% 
  group_by(month) %>%
  summarize(number_fires = n())
ggplot(data = fire_month) +
  aes(x = factor(month, level = month_order),
      y = number_fires) +
  geom_bar(stat = 'identity') +
  labs(title = 'Forest Fires vs. Months', x = 'Month',
       y = 'Number of Forest Fires')

#on which days of the week are forest fires most common?
fire_day <- fires %>%
  group_by(day) %>%
  summarize(number_fires = n())
ggplot(data = fire_day) +
  aes(x = factor(day, level = day_order), 
      y = number_fires) +
  geom_bar(stat = 'identity') +
  labs(title = 'Forest Fires vs Days', x = 'Day', 
       y = 'Number of Forest Fires')

#visualizing distribution of weather variables by month
create_box = function(xvar, yvar){
  ggplot(data = fires) +
    aes_string(x = xvar, y = yvar) +
    geom_boxplot()
}
weather_vars <- c('FFMC', 'DMC', 'DC', 'ISI', 'temp',
                  'RH', 'wind', 'rain')
map2(c('month'), weather_vars, create_box)

#visualizing distribution of weather variables by day
map2(c('day'), weather_vars, create_box)

#creating scatter plots for weather variables vs. area
fires_area <- fires %>% 
  filter(area > 3 & area < 300)
create_scatter = function(xvar, yvar){
  ggplot(data = fires_area) +
    aes_string(x = xvar, y = yvar) +
    geom_point()
}
map2(weather_vars, c('area'), create_scatter)
