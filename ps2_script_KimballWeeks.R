library(tidyquant)
library(tidyverse)

# ---- Part 3: Read in the data ----

# unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get  = "economic.data",
#  from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

# ---- Part 4.1: Clean up ----

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

# ---- Part 4.2: Wrangle ----

unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()


unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))


unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )


unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# ---- Part 4.3: Extremes ----

unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)

# ---- Part 4.4: Extremes excluding 2020 ----

unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)

# ---- Part 5.1: Line plot ----

p_lines <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(
    title = "Monthly Unemployment Rate",
    x     = "Date",
    y     = "Unemployment rate",
    color = "Region"
  ) +
  theme_minimal()

p_lines

# ---- Part 5.2: Faceted line plots ----

p_facet <- ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region",
    x     = "Date",
    y     = "Unemployment rate"
  ) +
  theme_minimal()

p_facet

# ---- Part 5.3: Bar chart of mean rate ----

p_bar <- ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(
    title = "Average Unemployment Rate by Region",
    x     = "Region",
    y     = "Mean unemployment rate"
  ) +
  theme_minimal()

p_bar

# ---- Part 5.4: Your choice ----
# Each state's unemployment rate minus the U.S. rate over time.
# Shows which states run persistently above/below the national rate
# interesting to compare local with larger trends

unemp_gap <- unemp |>
  group_by(date) |>
  mutate(gap = unemployment_rate - unemployment_rate[region == "United States"]) |>
  ungroup() |>
  filter(region != "United States")

ggplot(unemp_gap, aes(x = date, y = gap, color = region)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  labs(
    title = "State Unemployment Rate Minus U.S. Rate",
    x     = "Date",
    y     = "Gap vs. U.S. (percentage points)",
    color = "State"
  ) +
  theme_minimal()



