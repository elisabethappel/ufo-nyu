# ufo sightings data exploration
# initial analysis of nuforc data

library(tidyverse)
library(lubridate)

# read data
ufo_data <- read_csv("nuforc_all_reports_table.csv")
glimpse(ufo_data)

# summary statistics
summary(ufo_data)

# missing values
missing_summary <- ufo_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  mutate(Percent = round(Missing_Count / nrow(ufo_data) * 100, 2)) %>%
  arrange(desc(Missing_Count))
print(missing_summary)

# parse dates
ufo_data <- ufo_data %>%
  mutate(
    occurred_date = mdy_hm(Occurred),
    year = year(occurred_date),
    month = month(occurred_date, label = TRUE),
    hour = hour(occurred_date)
  )

# top locations
ufo_data %>%
  count(State, sort = TRUE) %>%
  head(10) %>%
  print()

ufo_data %>%
  count(City, State, sort = TRUE) %>%
  head(10) %>%
  print()


# shape distribution
ufo_data %>%
  count(Shape, sort = TRUE) %>%
  head(10) %>%
  print()

# temporal patterns
ufo_data %>%
  filter(!is.na(year)) %>%
  count(year) %>%
  arrange(desc(year)) %>%
  head(10) %>%
  print()

ufo_data %>%
  filter(!is.na(month)) %>%
  count(month) %>%
  print()

ufo_data %>%
  filter(!is.na(hour)) %>%
  count(hour) %>%
  arrange(hour) %>%
  print()

# basic visualizations

# sightings over time
p1 <- ufo_data %>%
  filter(!is.na(year), year >= 2000) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(title = "UFO Sightings Over Time (2000+)",
       x = "Year",
       y = "Number of Sightings") +
  theme_minimal()

ggsave("sightings_over_time.png", p1, width = 10, height = 6)

# top shapes
p2 <- ufo_data %>%
  count(Shape, sort = TRUE) %>%
  head(10) %>%
  mutate(Shape = fct_reorder(Shape, n)) %>%
  ggplot(aes(x = Shape, y = n, fill = Shape)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 UFO Shapes",
       x = "Shape",
       y = "Number of Sightings") +
  theme_minimal()

ggsave("top_shapes.png", p2, width = 8, height = 6)

# sightings by hour
p3 <- ufo_data %>%
  filter(!is.na(hour)) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col(fill = "darkgreen") +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "UFO Sightings by Hour of Day",
       x = "Hour (24-hour format)",
       y = "Number of Sightings") +
  theme_minimal()

ggsave("sightings_by_hour.png", p3, width = 10, height = 6)

# top states map data
p4 <- ufo_data %>%
  count(State, sort = TRUE) %>%
  head(20) %>%
  mutate(State = fct_reorder(State, n)) %>%
  ggplot(aes(x = State, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 20 States for UFO Sightings",
       x = "State",
       y = "Number of Sightings") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("top_states.png", p4, width = 8, height = 8)