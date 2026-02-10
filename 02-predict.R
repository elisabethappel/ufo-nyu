# ufo sightings data exploration
# predict sightings based on date, location, shape, etc.
# use complete years for modeling (2014/10 - 2023/09)
library(tidyverse)
library(lubridate)
library(fs)
library(janitor)

# read data
ufo_data <- read_csv(path("data", "nuforc_all_reports_table.csv")) %>%
  clean_names()

# filter to complete years
ufo_data <- ufo_data %>%
  filter(!is.na(occurred)) %>%
  mutate(occurred_date = mdy_hm(occurred)) %>%
  filter(occurred_date >= ymd_hm("2014-10-01 00:00") & occurred_date < ymd_hm("2023-10-01 00:00")) %>%
  mutate(
    year = year(occurred_date),
    month = month(occurred_date, label = TRUE),
    hour = hour(occurred_date),
    day_of_week = wday(occurred_date, label = TRUE),
    year_numeric = as.numeric(year - 2014)
  )

# aggregate data by state and month for modeling
state_month_data <- ufo_data %>%
  group_by(state, year, month) %>%
  summarise(
    sighting_count = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(state), state != "")

# linear regression model: predicting sighting counts
# predictors: state, year, month
model1 <- lm(sighting_count ~ state + year + month, data = state_month_data)
summary(model1)

# more complex model with interactions
model2 <- lm(sighting_count ~ state * year + month, data = state_month_data)
summary(model2)

# city-level analysis (top cities only to avoid sparse data)
top_cities <- ufo_data %>%
  count(city, state) %>%
  filter(!is.na(city), !is.na(state), city != "", state != "") %>%
  slice_max(n, n = 50) %>%
  select(city, state)

city_month_data <- ufo_data %>%
  semi_join(top_cities, by = c("city", "state")) %>%
  group_by(city, state, year, month) %>%
  summarise(
    sighting_count = n(),
    .groups = "drop"
  )

# model with city and state
model3 <- lm(sighting_count ~ city + state + year + month, data = city_month_data)
summary(model3)

# compare models
cat("\n=== Model Comparison ===\n")
cat("Model 1 (State + Year + Month) R-squared:", summary(model1)$r.squared, "\n")
cat("Model 2 (State*Year + Month) R-squared:", summary(model2)$r.squared, "\n")
cat("Model 3 (City + State + Year + Month) R-squared:", summary(model3)$r.squared, "\n")

# Monthly pattern analysis - simpler aggregation
monthly_summary <- ufo_data %>%
  filter(country == "USA" | is.na(country),
         !is.na(state), 
         state != "", 
         state != "-") %>%
  group_by(month, state) %>%
  summarise(
    sightings = n(),
    .groups = "drop"
  )

# Define high activity (top 25%)
monthly_summary$high_activity <- ifelse(
  monthly_summary$sightings > quantile(monthly_summary$sightings, 0.75), 
  1, 
  0
)

# Logistic regression for high activity prediction
prob_model <- glm(high_activity ~ month + state,
                  family = binomial, 
                  data = monthly_summary)

cat("\n=== Logistic Regression: High Activity Prediction ===\n")
summary(prob_model)

# Predict probability for specific month/state combo
cat("\nProbability of high activity in July for California:\n")
pred_ca_july <- predict(
  prob_model, 
  newdata = data.frame(month = factor("Jul", levels = levels(monthly_summary$month)), 
                       state = "CA"), 
  type = "response"
)
print(pred_ca_july)

# Additional: Simple month effects model (for visualization)
cat("\n=== Simple Monthly Pattern Model ===\n")
monthly_aggregated <- ufo_data %>%
  group_by(year, month) %>%
  summarise(sightings = n(), .groups = "drop")

month_model <- lm(sightings ~ month, data = monthly_aggregated)
summary(month_model)

# Get predictions for each month
month_predictions <- data.frame(
  month = factor(month.abb, levels = month.abb)
)
month_predictions$predicted <- predict(month_model, newdata = month_predictions)

# Add confidence intervals
pred_interval <- predict(month_model, newdata = month_predictions, interval = "confidence")
month_predictions$lower <- pred_interval[, "lwr"]
month_predictions$upper <- pred_interval[, "upr"]

cat("\nMonthly predictions:\n")
print(month_predictions)