library(dplyr)
library(lintr)
library(styler)
library(tidyr)
library(leaflet)
library(ggplot2)
library(plotly)

data <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)
shooting_occurred <- nrow(data)

live_lost <- sum(data$num_killed)

# most impact means highest sum of killed and injured number 
city_most_impact <- data %>%
  group_by(city) %>%
  summarise(num_hurt = sum(num_killed)+sum(num_injured)) %>%
  top_n(1, num_hurt) %>%
  pull(city)
#insight 1 the most impact state. 
state_most_impact <- data %>%
  group_by(state) %>%
  summarise(num_hurt = sum(num_killed)+sum(num_injured)) %>%
  top_n(1, num_hurt) %>%
  pull(state)
#insight 2, the day when most hurting happened
miserable_day <- data %>%
  group_by(date) %>%
  summarise(num_hurt = sum(num_killed)+sum(num_injured)) %>%
  top_n(1, num_hurt) %>%
  pull(date)

#summary table: total victim in each cities 
summary_table <- data %>%
  group_by(state) %>%
  group_by(city) %>%
  mutate(num_victim = sum(num_killed)+sum(num_injured)) %>%
  select(state, city, num_victim) %>%
  slice(1, num_victim) %>%
  arrange(-num_victim)

#particular incident (baltimore most victim) 
#https://www.baltimoresun.com/news/crime/bs-md-ci-shootings-20180701-story.html
baltimore_most_victim <- data %>%
  filter(city == 'Baltimore') %>%
  mutate('num_victim' = num_killed + num_injured) %>%
  filter(num_victim == max(num_victim))

address <- baltimore_most_victim %>%
  pull(address)

victim <- baltimore_most_victim %>%
  pull(num_victim)

date <- baltimore_most_victim %>%
  pull(date)

injured <- baltimore_most_victim %>%
  pull(num_injured)

killed <- baltimore_most_victim %>%
  pull(num_killed)

#map
marker_size <- data %>%
  mutate(total_victims = num_killed + num_injured) %>%
  mutate(radius = (total_victims / max(total_victims)) * 10)

map <- leaflet(data = marker_size) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
   popup = paste(
     "City:", marker_size$city, "<br>",
     "Deaths:", marker_size$num_killed, "<br>",
     "Injuries:", marker_size$num_injured
    ),
   radius = ~radius,
  )

#plot
months_data <- data%>%
  mutate(months = format(as.Date(date, "%B %d,", "%Y"), "%B"))%>%
  group_by(months) %>%
  summarise(num_victim = sum(num_killed) + sum(num_injured)) %>%
  arrange(match(months,month.name)) %>%
  mutate(month = 1:12)

chart <- ggplot(data = months_data) +
  geom_col(mapping = aes(x = month, y = num_victim)) +
  ggtitle("Total Victims(Deaths and Jnjuries) Due to Mass Shotting of Each Month in 2018") +
  scale_x_continuous(
    breaks = 1:12,
    name = "Month",
    labels = c(substring(months_data$months, 1, 3))
  ) +
  scale_y_continuous(name = "Number of Victims")

interactive_chart = ggplotly(chart)
