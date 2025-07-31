# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load required packages
library(tidyverse)
library(readxl)
library(janitor)
library(tidymodels)
library(vip)

kpler <- read_excel("data/kpler.xlsx") %>%
  clean_names()

cheniere <- kpler %>%
  filter(charterer == "Cheniere" 
         & vessel_engine_type != "SSD" # filtering out these vessels (just two export journeys) because their emissions can't be calculated using our methodology
         & (origin_installation == "Corpus Christi" | origin_installation == "Sabine Pass")
         & departure_date >= as.Date("2018-01-01") 
         & departure_date < as.Date("2025-01-01")) %>%
  mutate(distance_laden = as.numeric(distance_laden),
         distance_ballast = as.numeric(distance_ballast),
         emissions_laden = as.numeric(emissions_laden),
         emissions_ballast = as.numeric(emissions_ballast),
         vessel_engine_type = case_when(grepl("FDE",vessel_engine_type) ~ "TFDE/DFDE",
                                        TRUE ~ vessel_engine_type)) %>%
  mutate(across(contains("distance"), ~ . * 1.852)) 

unique(cheniere$vessel_engine_type)

# What's missing
cheniere %>%
  group_by(destination_country) %>%
  summarize(n = n(), missing_laden = sum(is.na(distance_laden)), missing_ballast = sum(is.na(distance_ballast))) %>%
  arrange(desc(missing_laden)) %>%
  print(n = 50)

# # A tibble: 32 × 4
# destination_country     n missing_laden missing_ballast
# <chr>               <int>         <int>           <int>
# 1 Mexico                 26            12              12
# 2 South Korea            53            12              12
# 3 China                  56            11              11
# 4 Taiwan                113             7               7
# 5 Argentina              14             5               5
# 6 Japan                  25             4               4
# 7 Netherlands            61             3               3
# 8 Brazil                 29             2               2
# 9 France                 59             2               2
# 10 Chile                  12             1               1
# 11 Egypt                   4             1               1
# 12 Greece                  8             1               1
# 13 India                  20             1               1
# 14 Italy                  23             1               1
# 15 Jordan                  1             1               1
# 16 Portugal                9             1               1
# 17 Thailand               26             1               1
# 18 United Kingdom         44             1               1
# 19 Bangladesh              5             0               0
# 20 Belgium                19             0               0
# 21 Colombia                4             0               0
# 22 Croatia                 3             0               0
# 23 Dominican Republic      3             0               0
# 24 Jamaica                 7             0               0
# 25 Kuwait                 13             0               0
# 26 Lithuania               6             0               0
# 27 Pakistan                1             0               0
# 28 Panama                  2             0               0
# 29 Poland                 49             0               0
# 30 Singapore               7             0               0
# 31 Spain                  38             0               0

cheniere <- cheniere %>%
  mutate(across(c(destination_country, origin_installation, vessel_engine_type), as.factor))

# Only use rows with non-missing distance_laden for training
train_data <- cheniere %>% filter(!is.na(distance_laden))

# Split data for cross-validation
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 5)

# Recipe for preprocessing
rec <- recipe(distance_laden ~ destination_country + origin_installation + vessel_engine_type + vessel_build_year, data = train_data) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Model specs
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger",importance = "permutation") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 500, mtry = tune(), learn_rate = tune(), tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Workflows
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rec)

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec)

# Tuning grids
rf_grid <- grid_regular(mtry(range = c(2, 6)), min_n(range = c(2, 10)), levels = 3)
xgb_grid <- grid_regular(mtry(range = c(2, 6)), learn_rate(range = c(0.01, 0.3)), tree_depth(range = c(3, 8)), levels = 2)

# Tune models with 5-fold CV
rf_res <- tune_grid(rf_wf, resamples = cv_folds, grid = rf_grid, metrics = metric_set(rmse, rsq))
xgb_res <- tune_grid(xgb_wf, resamples = cv_folds, grid = xgb_grid, metrics = metric_set(rmse, rsq))

# Compare performance
rf_best <- select_best(rf_res, metric = "rmse")
xgb_best <- select_best(xgb_res, metric = "rmse")
show_best(rf_res, metric ="rmse", n = 1)$mean
# [1] 2885.384
show_best(xgb_res, metric = "rmse", n = 1)$mean
# [1] 2981.238
show_best(rf_res, metric = "rsq", n = 1)$mean
# [1] 0.7974364
show_best(xgb_res, metric = "rsq", n = 1)$mean
# [1] 0.7774441

# Finalize best model (choose lower RMSE)
if (show_best(rf_res, metric ="rmse", n = 1)$mean < show_best(xgb_res, metric = "rmse", n = 1)$mean) {
  final_wf <- finalize_workflow(rf_wf, rf_best)
  model_name <- "Random Forest"
} else {
  final_wf <- finalize_workflow(xgb_wf, xgb_best)
  model_name <- "XGBoost"
}

cat("\nSelected model:", model_name, "\n")
#  Selected model: Random Forest 

# Fit final model to all non-missing data
final_fit <- fit(final_wf, data = train_data)

# Variable importance plot (if available)
if (model_name == "Random Forest") {
  vip::vip(extract_fit_parsnip(final_fit)$fit)
} else {
  vip::vip(extract_fit_parsnip(final_fit)$fit)
}

# Impute missing distance_laden
impute_data <- cheniere %>% filter(is.na(distance_laden))
impute_preds <- predict(final_fit, new_data = impute_data)
cheniere$distance_laden[is.na(cheniere$distance_laden)] <- impute_preds$.pred

# Diagnostics: predicted vs. actual (on training data)
preds <- predict(final_fit, new_data = train_data) %>%
  bind_cols(train_data)

# Plot predicted vs actual
ggplot(preds, aes(x = distance_laden, y = .pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = paste("Predicted vs Actual distance_laden (", model_name, ")"),
       x = "Actual distance_laden", y = "Predicted distance_laden")


#####
# now try for distance_ballast

# Only use rows with non-missing distance_ballast for training
train_data <- cheniere %>% filter(!is.na(distance_ballast))


# Split data for cross-validation
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 5)

# Recipe for preprocessing
rec <- recipe(distance_ballast ~ destination_country + origin_installation + vessel_engine_type + vessel_build_year, data = train_data) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

# Model specs
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger",importance = "permutation") %>%
  set_mode("regression")

xgb_spec <- boost_tree(trees = 500, mtry = tune(), learn_rate = tune(), tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Workflows
rf_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rec)

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec)

# Tuning grids
rf_grid <- grid_regular(mtry(range = c(2, 6)), min_n(range = c(2, 10)), levels = 3)
xgb_grid <- grid_regular(mtry(range = c(2, 6)), learn_rate(range = c(0.01, 0.3)), tree_depth(range = c(3, 8)), levels = 2)

# Tune models with 5-fold CV
rf_res <- tune_grid(rf_wf, resamples = cv_folds, grid = rf_grid, metrics = metric_set(rmse, rsq))
xgb_res <- tune_grid(xgb_wf, resamples = cv_folds, grid = xgb_grid, metrics = metric_set(rmse, rsq))

# Compare performance
rf_best <- select_best(rf_res, metric = "rmse")
xgb_best <- select_best(xgb_res, metric = "rmse")
show_best(rf_res, metric ="rmse", n = 1)$mean
# [1] 7188.082
show_best(xgb_res, metric = "rmse", n = 1)$mean
# [1] 7773.777
show_best(rf_res, metric = "rsq", n = 1)$mean
# [1] 0.1938023
show_best(xgb_res, metric = "rsq", n = 1)$mean
# [1] 0.2516817

# Abandon XGBoost,  metrics are too poor

# Save imputed dataset
write_csv(cheniere, "processed_data/cheniere.csv", na = "")
