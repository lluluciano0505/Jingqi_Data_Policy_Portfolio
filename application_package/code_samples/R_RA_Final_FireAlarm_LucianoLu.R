# Application Code Sample (R)
# Candidate: Luciano Lu
# Target role: RA / Hybrid RA & FC
# Project: Philadelphia False Fire Alarm Prediction
#
# Purpose:
# Build a tract- and incident-level logistic regression pipeline to estimate
# the probability that a fire dispatch is a false alarm.
#
# Inputs (relative to repository root):
# - assignments/Final/data/stat360/stat360_fire_incidents.shp
# - assignments/Final/data/LI_BUILDING_FOOTPRINTS.shp (or alarm_ins alternative)
# - assignments/Final/data/BUILDING_CERT_SUMMARY.csv
# - assignments/Final/data/Fire_Dept.csv
#
# Outputs:
# - AUC table for three nested logistic models
# - 5-fold cross-validated ROC estimate for the full model
#
# Contribution note:
# This script is adapted from a course final project and refactored into a
# standalone sample. Update this note to clearly describe your own contribution
# if this work was co-authored.

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(tidycensus)
  library(lubridate)
  library(riem)
  library(caret)
  library(pROC)
})

options(tigris_use_cache = TRUE, tigris_class = "sf", tigris_progress = FALSE)

# ------------------------------
# 1) Paths and utilities
# ------------------------------
project_root <- normalizePath(".")
data_dir <- file.path(project_root, "assignments", "Final", "data")

pick_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) stop("Required file not found. Checked: ", paste(paths, collapse = ", "))
  hit[[1]]
}

fire_path <- file.path(data_dir, "stat360", "stat360_fire_incidents.shp")
cert_path <- file.path(data_dir, "BUILDING_CERT_SUMMARY.csv")
station_path <- file.path(data_dir, "Fire_Dept.csv")

bldg_path <- pick_existing(c(
  file.path(data_dir, "LI_BUILDING_FOOTPRINTS.shp"),
  file.path(data_dir, "alarm_ins", "LI_BUILDING_FOOTPRINTS.shp")
))

stopifnot(file.exists(fire_path), file.exists(cert_path), file.exists(station_path))

# ------------------------------
# 2) Incident label engineering
# ------------------------------
fire <- st_read(fire_path, quiet = TRUE) |>
  filter(!st_is_empty(geometry)) |>
  mutate(incident_code = as.integer(stringr::str_extract(incident_1, "^\\\\d+"))) |>
  filter(incident_code %in% c(1L, 7L)) |>
  mutate(inc_new = if_else(incident_code == 7L, 1L, 0L))

# ------------------------------
# 3) Census socioeconomic features
# ------------------------------
# Requires a Census API key set in your local environment.
philly_census <- get_acs(
  geography = "tract",
  variables = c(
    total_pop = "B01003_001",
    black = "B02001_003",
    ba_degree = "B15003_022",
    total_edu = "B15003_001",
    median_income = "B19013_001",
    labor_force = "B23025_003",
    unemployed = "B23025_005"
  ),
  year = 2023,
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE
) |>
  select(GEOID, variable, estimate, geometry) |>
  pivot_wider(names_from = variable, values_from = estimate) |>
  mutate(
    ba_rate = 100 * ba_degree / total_edu,
    unemployment_rate = 100 * unemployed / labor_force,
    black_share = 100 * black / total_pop
  ) |>
  st_transform(st_crs(fire))

# ------------------------------
# 4) Building certification features
# ------------------------------
bldg_foot <- st_read(bldg_path, quiet = TRUE)
cert_sum <- readr::read_csv(cert_path, show_col_types = FALSE)

bldg_cert <- bldg_foot |>
  mutate(bin = as.character(bin)) |>
  inner_join(
    cert_sum |> mutate(structure_id = as.character(structure_id)),
    by = c("bin" = "structure_id")
  )

bldg_tract <- bldg_cert |>
  st_transform(st_crs(philly_census)) |>
  st_join(philly_census["GEOID"])

tract_alarm <- bldg_tract |>
  st_drop_geometry() |>
  group_by(GEOID) |>
  summarise(
    n_bldg = n(),
    n_active = sum(fire_alarm_status == "Active", na.rm = TRUE),
    n_expired = sum(fire_alarm_status == "Expired", na.rm = TRUE),
    n_deficient = sum(fire_alarm_status == "Deficient", na.rm = TRUE),
    active_rate = n_active / n_bldg,
    .groups = "drop"
  )

philly_census_alarm <- philly_census |>
  left_join(tract_alarm, by = "GEOID")

# ------------------------------
# 5) Weather features
# ------------------------------
get_phl <- function(start, end) {
  riem_measures(
    station = "PHL",
    date_start = start,
    date_end = end,
    report_type = c("routine", "specials", "hfmetar")
  )
}

weather_data <- bind_rows(
  get_phl("2024-01-01", "2024-03-31"),
  get_phl("2024-04-01", "2024-06-30"),
  get_phl("2024-07-01", "2024-09-30"),
  get_phl("2024-10-01", "2024-12-31"),
  get_phl("2025-01-01", "2025-06-30"),
  get_phl("2025-07-01", "2025-10-06")
) |>
  arrange(valid) |>
  distinct(valid, .keep_all = TRUE)

weather_daily <- weather_data |>
  mutate(
    interval60 = floor_date(valid, unit = "hour"),
    Temperature = tmpf,
    Precipitation = if_else(is.na(p01i), 0, p01i),
    Wind_Speed = sknt
  ) |>
  select(interval60, Temperature, Precipitation, Wind_Speed) |>
  distinct() |>
  tidyr::complete(interval60 = seq(min(interval60), max(interval60), by = "hour")) |>
  tidyr::fill(Temperature, Precipitation, Wind_Speed, .direction = "down") |>
  filter(hour(interval60) == 12) |>
  mutate(date = as.Date(interval60)) |>
  group_by(date) |>
  slice(1) |>
  ungroup() |>
  select(date, Temperature, Precipitation, Wind_Speed) |>
  mutate(
    Precipitation = Precipitation + 1e-5,
    dow = wday(date, label = TRUE, week_start = 1),
    is_weekend = as.integer(dow %in% c("Sat", "Sun"))
  )

# ------------------------------
# 6) Fire station proximity features
# ------------------------------
fire_dept <- read_csv(station_path, show_col_types = FALSE)
firesta_sf <- st_as_sf(fire_dept, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
firesta_buf <- st_transform(firesta_sf, st_crs(fire)) |> st_buffer(dist = 200)

# ------------------------------
# 7) Merge all features
# ------------------------------
fire_weather <- fire |>
  mutate(date = as.Date(alarm_date)) |>
  left_join(weather_daily, by = "date")

fire_weather_station <- fire_weather |>
  st_transform(st_crs(firesta_buf))

idx <- st_intersects(fire_weather_station, firesta_buf)
fire_weather_station <- fire_weather_station |>
  mutate(near_station_200m = as.integer(lengths(idx) > 0))

philly_census_alarm <- philly_census_alarm |>
  st_transform(st_crs(fire_weather_station))

fire_model_df <- fire_weather_station |>
  st_join(philly_census_alarm, join = st_within, left = TRUE) |>
  mutate(
    quarter_num = quarter(date),
    quarter = factor(quarter_num, levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
  )

city_hall <- tibble(lat = 39.952583, lon = -75.165222) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

n <- nrow(fire_model_df)
fire_model_df <- fire_model_df |>
  st_transform(st_crs(city_hall)) |>
  mutate(
    dist_cityhall_m = as.numeric(st_distance(geometry, st_geometry(city_hall)[rep(1, n)], by_element = TRUE)),
    dist_cityhall_km = dist_cityhall_m / 1000
  )

# ------------------------------
# 8) Logistic models
# ------------------------------
m1 <- glm(
  inc_new ~ Temperature + Precipitation + Wind_Speed + active_rate,
  data = fire_model_df,
  family = binomial(link = "logit")
)

m2 <- glm(
  inc_new ~ Temperature + Precipitation + Wind_Speed + active_rate +
    median_income + ba_rate + black_share + unemployment_rate,
  data = fire_model_df,
  family = binomial(link = "logit")
)

m3 <- glm(
  inc_new ~ Temperature + Precipitation + Wind_Speed + active_rate +
    median_income + ba_rate + black_share + unemployment_rate +
    is_weekend + quarter + near_station_200m + dist_cityhall_km,
  data = fire_model_df,
  family = binomial(link = "logit")
)

# ------------------------------
# 9) AUC comparison
# ------------------------------
d1 <- model.frame(m1); p1 <- predict(m1, type = "response")
d2 <- model.frame(m2); p2 <- predict(m2, type = "response")
d3 <- model.frame(m3); p3 <- predict(m3, type = "response")

auc_table <- tibble(
  model = c("Model 1", "Model 2", "Model 3"),
  auc = c(
    as.numeric(auc(roc(d1$inc_new, p1))),
    as.numeric(auc(roc(d2$inc_new, p2))),
    as.numeric(auc(roc(d3$inc_new, p3)))
  )
)

print(auc_table)

# ------------------------------
# 10) 5-fold cross validation
# ------------------------------
set.seed(123)

cv_df <- fire_model_df |>
  st_drop_geometry() |>
  mutate(
    y = if_else(inc_new == 1, "FalseAlarm", "RealIncident"),
    y = factor(y, levels = c("FalseAlarm", "RealIncident"))
  ) |>
  select(
    y,
    Temperature, Precipitation, Wind_Speed, active_rate,
    median_income, ba_rate, black_share, unemployment_rate,
    is_weekend, quarter, near_station_200m, dist_cityhall_km
  ) |>
  drop_na()

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

cv_fit <- train(
  y ~ .,
  data = cv_df,
  method = "glm",
  family = binomial(link = "logit"),
  trControl = ctrl,
  metric = "ROC"
)

print(cv_fit)
