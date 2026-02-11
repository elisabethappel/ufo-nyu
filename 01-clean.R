rm(list = ls())

# ufo sightings data exploration
# clean and prep nuforc data

library(tidyverse)
library(lubridate)
library(fs)
library(janitor)
library(tidycensus)

# load ufo data, summarise ------------------------------------------------

# read data
ufo_raw <- read_csv(path("data", "nuforc_all_reports_table.csv")) %>%
  clean_names()

# summary statistics
summary(ufo_raw)

# top locations
ufo_raw %>%
  count(country, sort = TRUE) %>%
  head(10) %>%
  print()

# pull first and last date of observation
first_date <- ufo_raw %>%
  filter(!is.na(occurred)) %>%
  summarise(first_date = min(mdy_hm(occurred))) %>%
  pull(first_date)

last_date <- ufo_raw %>%
  filter(!is.na(occurred)) %>%
  summarise(last_date = max(mdy_hm(occurred))) %>%
  pull(last_date)

# missing values
missing_summary <- ufo_raw %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(percent = round(missing_count / nrow(ufo_raw) * 100, 2)) %>%
  arrange(desc(missing_count))

print(missing_summary)


# load acs pop counts -----------------------------------------------------

# get state population counts
state_pop <- get_acs(
  geography = "state",
  variables = "B01003_001",
  year = 2024,
  survey = "acs5"
)

head(state_pop)

# create state abbreviation lookup for all states and territories with region/division
state_lookup <- tibble(
  state = c(state.name, "District of Columbia", "Puerto Rico", "Guam", 
            "U.S. Virgin Islands", "American Samoa", "Northern Mariana Islands"),
  state_abbr = c(state.abb, "DC", "PR", "GU", "VI", "AS", "MP"),
  region = c(as.character(state.region), "South", "Territory", "Territory", 
             "Territory", "Territory", "Territory"),
  division = c(as.character(state.division), "South Atlantic", "Territory", "Territory",
               "Territory", "Territory", "Territory")
)

state_pop_clean <- state_pop %>%
  select(NAME, GEOID, estimate) %>%
  rename(
    state = NAME,
    fips = GEOID,
    population = estimate
  ) %>%
  tidylog::left_join(state_lookup, by = "state") %>%
  arrange(desc(population)) %>%
  rename(state_long = state)

print(state_pop_clean)


# clean and merge ---------------------------------------------------------

ufo_data <- ufo_raw %>%
  mutate(
    # parse dates
    occurred_date = mdy_hm(occurred),
    year = year(occurred_date),
    month = month(occurred_date, label = TRUE),
    hour = hour(occurred_date),
    day_of_week = wday(occurred_date, label = TRUE),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      month %in% c("Mar", "Apr", "May") ~ "Spring",
      month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      month %in% c("Sep", "Oct", "Nov") ~ "Fall"
    ),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall"))
  ) %>%
  tidylog::left_join(state_pop_clean, by = c("state" = "state_abbr")) %>%
  # apply sample restriction
  filter(
    occurred_date >= ymd_hm("2014-10-27 00:00"),
    occurred_date < ymd_hm("2024-10-26 00:00")
  )

