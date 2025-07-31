# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)

# load cheniere data
cheniere <- read_csv("processed_data/cheniere.csv")

# some checks on the data
unique(cheniere$origin_installation)
# [1] "Corpus Christi" "Sabine Pass"
unique(cheniere$charterer)
# [1] "Cheniere"

# look for journeys to multiple destinations
destinations <- cheniere %>%
  group_by(voyage_id) %>%
  summarize(destinations = n_distinct(destination_installation, na.rm = TRUE)) %>%
  arrange(desc(destinations))

# remove duplicates based on voyage_id, add number of destinations and year columns
cheniere <- cheniere %>%
  distinct(voyage_id, .keep_all = TRUE) %>%
  inner_join(destinations, by = "voyage_id") %>%
  mutate(year = year(departure_date)) 
nrow(cheniere)
# [1] 751

# calculate LNG boiled off used as fuel (some not combusted > methane slip) and emissions
cheniere_boil_off_emissions <- cheniere %>%
  mutate(
    days_docked_laden = 0.5 + destinations,
    days_docked_ballast = 1,
    days_maneuvering = case_when(
      destinations > 1 ~ 1 + 0.5*(destinations - 1),
      TRUE ~ 1
    ),
    containment_factor = case_when(
      vessel_build_year < 2000 ~ 2,
      vessel_build_year >= 2017 ~ 1,
      TRUE ~ -0.06 * (vessel_build_year - 2000) + 2
    ),
    surface_area_factor = 10 * pi * (vessel_capacity / 4 / pi)^(2 / 3) / 18115.1859151843,
    vessel_engine_type_factor = case_when(
      vessel_engine_type == "Steam" ~ 1,
      vessel_engine_type != "Steam" & vessel_build_year < 1994 ~ 6.7,
      vessel_engine_type != "Steam" & vessel_build_year > 2020 ~ 1,
      TRUE ~ -0.21 * (vessel_build_year - 2000) + 5.2
    ),
    laden_boil_off_underway_total = distance_laden * containment_factor * surface_area_factor * 0.0803101327514633,
    laden_boil_off_underway_generators = case_when(
      grepl("STaGE|DFDE|TFDE", vessel_engine_type) ~ 0,
      vessel_engine_type == "ME-GI" ~ distance_laden * containment_factor * surface_area_factor * 0.019636083856571 * 1.12,
      TRUE ~ distance_laden * containment_factor * surface_area_factor * 0.019636083856571
    ),
    laden_boil_off_underway_propulsion = laden_boil_off_underway_total - laden_boil_off_underway_generators,
    laden_boil_off_maneuvering_total = days_maneuvering * containment_factor * surface_area_factor * 42.6817647058824,
    laden_boil_off_maneuvering_generators = days_maneuvering * containment_factor * surface_area_factor * 16.4876470588235,
    laden_boil_off_maneuvering_gcu = days_maneuvering * containment_factor * surface_area_factor * 26.1941176470588,
    laden_boil_off_docked_total = days_docked_laden * containment_factor * surface_area_factor * 24.9648484848485,
    laden_boil_off_docked_generators = days_docked_laden * containment_factor * surface_area_factor * 19.6587878787879,
    laden_boil_off_docked_gcu = days_docked_laden * containment_factor * surface_area_factor * 5.30606060606061,
    laden_boil_off_total_no_gcu = laden_boil_off_underway_total + laden_boil_off_maneuvering_total + laden_boil_off_docked_total - laden_boil_off_maneuvering_gcu - laden_boil_off_docked_gcu,
    laden_boil_off_total = laden_boil_off_underway_total + laden_boil_off_maneuvering_total + laden_boil_off_docked_total, # this version does not remove boil off going to GCUs
    laden_methane_slip_underway_generators = laden_boil_off_underway_generators * 0.083404937269912,
    laden_methane_slip_underway_propulsion = case_when(
      grepl("TFDE|DFDE|X-DF", vessel_engine_type) ~ laden_boil_off_underway_propulsion * 0.0218575669591883 * vessel_engine_type_factor,
      vessel_engine_type == "STaGE" ~ laden_boil_off_underway_propulsion * 0.0218575669591883 * vessel_engine_type_factor * 0.5 + laden_boil_off_underway_propulsion * 0.00005 * vessel_engine_type_factor * 0.5,
      vessel_engine_type == "Steam" ~ laden_boil_off_underway_propulsion * 0.00005 * vessel_engine_type_factor,
      vessel_engine_type == "ME-GI" ~ laden_boil_off_underway_propulsion * 0.002 * vessel_engine_type_factor
    ),
    laden_methane_slip_maneuvering_generators = laden_boil_off_maneuvering_generators * 0.0820578686360555,
    laden_methane_slip_docked_generators = laden_boil_off_docked_generators * 0.0878626260135031,
  ) %>%
  rowwise() %>%
  mutate(
    laden_methane_emissions = sum(c_across(contains("laden_methane_slip")), na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    laden_co2_emissions = case_when(
      vessel_engine_type == "ME-GI" ~ 1.07 * (laden_boil_off_total - laden_methane_emissions / 0.915767234575348) * 1.04890591912877 / 16.729037729152 * 44.00995,
      TRUE ~ (laden_boil_off_total - laden_methane_emissions / 0.915767234575348) * 1.04890591912877 / 16.729037729152 * 44.00995
    ),
    laden_emissions_co2_equivalent = laden_co2_emissions + (laden_methane_emissions * 82.5),
    ) %>%
    mutate(across(where(is.numeric), ~ na_if(., 0))) # replace calculated zero values with NA

write_csv(cheniere_boil_off_emissions, "processed_data/journey_level_data.csv", na = "")

cheniere_lng_fuel_emissions_year <- cheniere_boil_off_emissions %>%
  group_by(year) %>%
  summarize(
    export_journeys = n(),
    laden_boil_off_total = sum(laden_boil_off_total, na.rm = TRUE),
    laden_boil_off_total_no_gcu = sum(laden_boil_off_total_no_gcu, na.rm = TRUE),
    laden_methane_emissions = sum(laden_methane_emissions, na.rm = TRUE),
    laden_co2_emissions = sum(laden_co2_emissions, na.rm = TRUE),
    laden_emissions_co2_equivalent = sum(laden_emissions_co2_equivalent, na.rm = TRUE),
    laden_emissions_kpler = sum(emissions_laden, na.rm = TRUE),
  ) %>%
  mutate(
    laden_boil_off_dge = laden_boil_off_total_no_gcu * 2204.62 / 6.06, # 2204.62 is the number of pounds in a metric ton, 6.06 is the conversion factor
    laden_credit = laden_boil_off_dge * 0.5,
    laden_dge_co2_emissions = laden_boil_off_dge * 10.21 / 1000, # 1000 is the number of kg in a metric ton, 10.21 is the conversion factor 
    laden_dge_gcu_co2_emissions = laden_boil_off_total * 1.04890591912877 / 16.729037729152 * 44.00995,
    laden_dge_total_co2_emissions = laden_dge_co2_emissions +  laden_dge_gcu_co2_emissions,
    ) %>%
  select(year,export_journeys,laden_boil_off_total,laden_boil_off_total_no_gcu,laden_methane_emissions,laden_co2_emissions,laden_emissions_co2_equivalent,
         laden_boil_off_dge,laden_credit,laden_dge_co2_emissions,laden_dge_gcu_co2_emissions,laden_dge_total_co2_emissions,
         ) %>%
  mutate(across(everything(), ~ na_if(., 0))) # replace any calculated zero values with NA

# export journeys by propulsion type
vessel_propulsion_year <- cheniere %>%
  group_by(year,vessel_engine_type) %>%
  count() %>%
  pivot_wider(names_from = vessel_engine_type, values_from = n) %>%
  clean_names() %>%
  ungroup()

numeric_cols <- setdiff(names(vessel_propulsion_year), "year")

vessel_propulsion_year_percent <- vessel_propulsion_year %>%
  rowwise() %>%
  mutate(across(
    all_of(numeric_cols),
    ~ round(.x / sum(c_across(all_of(numeric_cols))) * 100, 1)
  )) %>%
  ungroup()

write_csv(cheniere_lng_fuel_emissions_year, "processed_data/cheniere_lng_fuel_emissions_year.csv", na = "")
write_csv(vessel_propulsion_year_percent, "processed_data/vessel_propulsion_year_percent.csv", na = "")


