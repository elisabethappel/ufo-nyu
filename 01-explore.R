rm(list = ls())

# ufo sightings data exploration
# initial analysis of nuforc data

library(tidyverse)
library(lubridate)
library(fs)
library(janitor)
library(tidycensus)
library(usmap)

source("01-clean.R")

# visualise ---------------------------------------------------------------

# basic visualizations

# sightings over time
p1_nsightings <- ufo_data %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Year",
       y = "Number of sightings") +
  theme_minimal()

ggsave(path("output", "sightings_over_time.png"), p1_nsightings, width = 10, height = 6)

# sightings over time with key events
p1b_nsightings <- ufo_data %>%
  mutate(year_month = floor_date(occurred_date, "month")) %>%
  count(year_month) %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 1) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, color = "gray30", linetype = "solid", linewidth = 0.8) +
  geom_vline(xintercept = as.numeric(ymd("2017-12-01")), linetype = "dashed", color = "red", alpha = 0.6) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype = "dashed", color = "purple", alpha = 0.6) +
  geom_vline(xintercept = as.numeric(ymd("2020-04-01")), linetype = "dashed", color = "orange", alpha = 0.6) +
  annotate("text", x = ymd("2017-12-01"), y = Inf, label = "NYT Article\n(Dec 2017)", 
           hjust = -0.1, vjust = 1.5, size = 3, color = "red") +
  annotate("text", x = ymd("2020-03-01"), y = Inf, label = "COVID Lockdown\n(Mar 2020)", 
           hjust = -0.1, vjust = 3.5, size = 3, color = "purple") +
  annotate("text", x = ymd("2020-04-01"), y = Inf, label = "Pentagon Videos\n(Apr 2020)", 
           hjust = -0.1, vjust = 5.5, size = 3, color = "orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "",
       x = "Year",
       y = "Number of sightings") +
  theme_minimal()

ggsave(path("output", "sightings_over_time_events.png"), p1b_nsightings, width = 12, height = 6)

# top shapes
p2_shapes <- ufo_data %>%
  filter(!is.na(shape)) %>%
  count(shape, sort = TRUE) %>%
  head(10) %>%
  mutate(shape = fct_reorder(shape, n)) %>%
  ggplot(aes(x = shape, y = n, fill = shape)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Shape",
       y = "Number of sightings") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ggsave(path("output", "top_shapes.png"), p2_shapes, width = 8, height = 6)

# sightings by hour
p3_tsightings <- ufo_data %>%
  filter(!is.na(hour)) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col(fill = "darkgreen") +
  scale_x_continuous(breaks = 0:23) +
  labs(x = "Hour",
       y = "Number of sightings") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

ggsave(path("output", "sightings_by_hour.png"), p3_tsightings, width = 10, height = 6)

# top states map data
p4_states <- ufo_data %>%
  filter(!is.na(state)) %>%
  count(state, sort = TRUE) %>%
  head(20) %>%
  mutate(state = fct_reorder(state, n)) %>%
  ggplot(aes(x = state, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "State",
       y = "Number of Sightings") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma)

p6 <- ufo_data %>%
  group_by(state) %>%
  mutate(sightings = n(),
         sightings_per_1000 = (sightings / population) * 1000) %>%
  select(state, sightings_per_1000) %>%
  distinct() %>%
  arrange(desc(sightings_per_1000)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(state, sightings_per_1000), y = sightings_per_1000, fill = sightings_per_1000)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(x = "State",
       y = "Sightings per 1,000 people") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::comma)

ggsave(path("output", "top_states_per_capita.png"), p6, width = 8, height = 8)

# map
state_counts <- ufo_data %>%
  filter(!is.na(state)) %>%
  count(state, sort = TRUE) 

state_counts <- state_counts %>%
  mutate(state = toupper(state))  # usmap expects uppercase state abbreviations
p5 <- plot_usmap(data = state_counts, values = "n", color = "white") +
  scale_fill_stepsn(colors = c("lightblue", "steelblue", "darkblue"), 
                    n.breaks = 6, 
                    name = "Sightings", 
                    labels = scales::comma) +
  theme(legend.position = "right")

ggsave(path("output", "map_overall.png"), p5, width = 8, height = 8)
