rm(list = ls())

# ufo sightings data exploration
# monthly and seasonal pattern analysis

library(tidyverse)
library(lubridate)
library(fs)
library(janitor)


# read data ---------------------------------------------------------------
source("01-clean.R")


# monthly patterns --------------------------------------------------------

# aggregate to monthly level
monthly_aggregated <- ufo_data %>%
  group_by(year, month) %>%
  summarise(sightings = n(), .groups = "drop")

# month effects model
month_model <- lm(sightings ~ month, data = monthly_aggregated)
summary(month_model)

# get predictions for each month
month_predictions <- data.frame(
  month = factor(month.abb, levels = month.abb)
)
month_predictions$predicted <- predict(month_model, newdata = month_predictions)

# add confidence intervals
pred_interval <- predict(month_model, newdata = month_predictions, interval = "confidence")
month_predictions$lower <- pred_interval[, "lwr"]
month_predictions$upper <- pred_interval[, "upr"]

print(month_predictions)

# visualize
ggplot(month_predictions, aes(x = month, y = predicted)) +
  geom_col(fill = "lightblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) +
  labs(
    title = "Average monthly UFO sightings",
    subtitle = "2014-2024, with 95% confidence intervals",
    x = "Month",
    y = "Predicted monthly sightings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0)
  )

ggsave(path("output", "monthly_sightings.png"), width = 10, height = 6, dpi = 300)


# trend-adjusted monthly patterns ----------------------------------------

# month + year model
month_year_model <- lm(sightings ~ month + year, data = monthly_aggregated)
summary(month_year_model)

# get predictions for all month-year combinations
month_year_predictions <- expand.grid(
  month = factor(month.abb, levels = month.abb),
  year = unique(monthly_aggregated$year)
)
pred_interval_my <- predict(month_year_model, newdata = month_year_predictions, interval = "confidence")
month_year_predictions$predicted <- pred_interval_my[, "fit"]
month_year_predictions$lower <- pred_interval_my[, "lwr"]
month_year_predictions$upper <- pred_interval_my[, "upr"]

# average across years
month_year_predictions <- month_year_predictions %>%
  group_by(month) %>%
  summarise(
    predicted = mean(predicted),
    lower = mean(lower),
    upper = mean(upper),
    .groups = "drop"
  )

print(month_year_predictions)

# visualize
ggplot(month_year_predictions, aes(x = month, y = predicted)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) +
  labs(
    title = "Average monthly UFO sightings (adjusted)",
    subtitle = "2014-2024, with 95% confidence intervals",
    x = "Month",
    y = "Predicted monthly sightings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0)
  )

ggsave(path("output", "monthly_sightings_adjusted.png"), width = 10, height = 6, dpi = 300)



# seasonal patterns -------------------------------------------------------

# aggregate to seasonal level
seasonal_aggregated <- ufo_data %>%
  group_by(year, season) %>%
  summarise(sightings = n(), .groups = "drop")

# calculate raw seasonal averages
raw_seasonal_avg <- seasonal_aggregated %>%
  group_by(season) %>%
  summarise(raw_average = mean(sightings), .groups = "drop")

# trend-adjusted model
cat("\n=== Seasonal Pattern Model (with Year Trend) ===\n")
season_year_model <- lm(sightings ~ season + year, data = seasonal_aggregated)
summary(season_year_model)

# get trend-adjusted predictions (holding year at 2018)
season_predictions <- data.frame(
  season = factor(c("Winter", "Spring", "Summer", "Fall"), 
                  levels = c("Winter", "Spring", "Summer", "Fall")),
  year = 2018
)
season_predictions$trend_adjusted <- predict(season_year_model, 
                                             newdata = season_predictions)

# combine for comparison
comparison_data <- raw_seasonal_avg %>%
  left_join(
    season_predictions %>% select(season, trend_adjusted),
    by = "season"
  ) %>%
  pivot_longer(
    cols = c(raw_average, trend_adjusted),
    names_to = "method",
    values_to = "sightings"
  )

# visualize comparison
ggplot(comparison_data, aes(x = season, y = sightings, fill = method)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  scale_fill_manual(
    values = c("raw_average" = "lightblue", "trend_adjusted" = "darkblue"),
    labels = c("Raw average", "Trend-adjusted")
  ) +
  labs(
    x = "Season",
    y = "Sightings per season",
    fill = ""
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

ggsave(path("output", "seasonal_comparison.png"), width = 10, height = 6, dpi = 300)

# print statistics
print(raw_seasonal_avg)
print(season_predictions %>% select(season, trend_adjusted))

cat("\nRaw average range:", 
    round(max(raw_seasonal_avg$raw_average) - min(raw_seasonal_avg$raw_average)), "\n")
cat("Trend-adjusted range:", 
    round(max(season_predictions$trend_adjusted) - min(season_predictions$trend_adjusted)), "\n")


# model comparison --------------------------------------------------------

# fit comparison models
season_only <- lm(sightings ~ season, data = seasonal_aggregated)
year_only <- lm(sightings ~ year, data = seasonal_aggregated)

cat("Season only R²:", round(summary(season_only)$r.squared, 3), "\n")
cat("Year only R²:", round(summary(year_only)$r.squared, 3), "\n")
cat("Season + Year R²:", round(summary(season_year_model)$r.squared, 3), "\n")

# test if summer is significantly different
cat("Is Summer significantly different from Winter?\n")
coef_summary <- summary(season_year_model)$coefficients
if ("seasonSummer" %in% rownames(coef_summary)) {
  summer_coef <- coef_summary["seasonSummer", ]
  cat("Summer coefficient:", round(summer_coef["Estimate"], 1), "\n")
  cat("p-value:", format.pval(summer_coef["Pr(>|t|)"], digits = 3), "\n")
  cat("Significant:", ifelse(summer_coef["Pr(>|t|)"] < 0.05, "YES", "NO"), "\n")
}
