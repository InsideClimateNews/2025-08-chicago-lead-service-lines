# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(tidycensus)

# don't use scientific notation for numbers
options(scipen = 999)

# cache TIGER shapefiles
options(tigris_use_cache = TRUE)

# variables for ACS
v23 <- load_variables(2023, "acs5", cache = TRUE)

# get socioeconomic variables
econ_vars <- c(
  poverty = "B17001_002",
  population_assessed_poverty = "B17001_001", 
  median_household_income = "B19013_001"
)

econ <- get_acs(
  state = "IL",
  county = "Cook",
  geography = "tract",
  variables = econ_vars,
  year = 2023,
  survey = "acs5",
  output = "wide",
  geometry = TRUE,
  cb = FALSE
) %>%
  clean_names() %>%
  mutate(
    pct_poverty = round(100 * poverty_e / population_assessed_poverty_e,1)
  ) %>%
  select(geoid,pct_poverty,median_household_income = median_household_income_e)

# get variables for race and ethnicity
race_ethnicity_vars <- c(
  population_assessed_ethnicity_race = "B03002_001",
  hispanic = "B03002_012",                      # Hispanic (any race)
  white_nonhispanic = "B03002_003",             # White alone, not Hispanic
  black_nonhispanic = "B03002_004",             # Black alone, not Hispanic
  asian_nonhispanic = "B03002_006"              # Asian alone, not Hispanic
)

race_ethnicity <- get_acs(
  state = "IL",
  county = "Cook",
  geography = "tract", 
  variables = race_ethnicity_vars,
  year = 2023,
  survey = "acs5",
  output = "wide"
) %>%
  clean_names() %>%
  mutate(
    pct_hispanic = round(100 * hispanic_e / population_assessed_ethnicity_race_e,1),
    pct_black_nonhispanic = round(100 * black_nonhispanic_e / population_assessed_ethnicity_race_e,1),
    pct_white_nonhispanic = round(100 * white_nonhispanic_e / population_assessed_ethnicity_race_e,1),
    pct_asian_nonhispanic = round(100 * asian_nonhispanic_e / population_assessed_ethnicity_race_e,1),
    pct_minority = round(100 * (population_assessed_ethnicity_race_e - white_nonhispanic_e) / population_assessed_ethnicity_race_e,1)
  ) %>%
  select(geoid,pct_hispanic,pct_black_nonhispanic,pct_white_nonhispanic,pct_asian_nonhispanic,pct_minority)

# get number of households and data for people who speak English "not well" or "not at all"
misc_vars <- c(
  households = "B11001_001",
  population_5_and_older = "B16005_001", # denominator for limited English
  limited_english_1 = "B16005_007",
  limited_english_2 = "B16005_008",
  limited_english_3 = "B16005_012",
  limited_english_4 = "B16005_013",
  limited_english_5 = "B16005_017",
  limited_english_6 = "B16005_018",
  limited_english_7 = "B16005_022",
  limited_english_8 = "B16005_023",
  limited_english_9 = "B16005_029",
  limited_english_10 = "B16005_030",
  limited_english_11 = "B16005_034",
  limited_english_12 = "B16005_035",
  limited_english_13 = "B16005_039",
  limited_english_14 = "B16005_040",
  limited_english_15 = "B16005_044",
  limited_english_16 = "B16005_045"
)

misc <- get_acs(
  geography = "tract",
  variables = misc_vars,
  year = 2023,
  county = "Cook",
  state = "IL",
  cb = FALSE)  %>%
  clean_names() %>%
  select(-moe) %>%
  group_by(geoid) %>%
  pivot_wider(names_from = variable, values_from = estimate)

misc <- misc %>%
  rowwise() %>%
  mutate(limited_or_no_english = sum(c_across(limited_english_1:limited_english_16), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_limited_or_no_english = round(limited_or_no_english/population_5_and_older*100,1)) %>%
  select(geoid,households,pct_limited_or_no_english)

# combine into single sf object
acs <- econ %>%
  left_join(race_ethnicity) %>%
  left_join(misc)

# get sf for Chicago boundary
chicago <- places(state = "IL", cb = FALSE) %>%
  clean_names() %>%
  filter(name == "Chicago")

# function to remove holes from any polygon geometry, so we include the enclaves within the city's perimeter
remove_holes <- function(geom) {
  if (inherits(geom, "POLYGON")) {
    st_polygon(list(geom[[1]]))  # Keep only the exterior ring
  } else if (inherits(geom, "MULTIPOLYGON")) {
    st_multipolygon(lapply(geom, function(poly) list(poly[[1]])))  # Keep only the exterior ring for each polygon
  } else {
    geom  # Return unchanged if not a polygon
  }
}

# remove holes and check with quick maps
chicago_filled <- chicago
chicago_filled$geometry <- st_sfc(lapply(st_geometry(chicago_filled), remove_holes), crs = st_crs(chicago_filled))
ggplot(chicago) + geom_sf()
ggplot(chicago_filled) + geom_sf()

# clip data to Chicago city boundary
chicago_acs <- st_intersection(acs,chicago) %>%
  select(1:10)
chicago_acs_filled <- st_intersection(acs,chicago_filled) %>%
  select(1:10)

# export data
st_write(chicago_acs,"processed_data/interim/chicago_acs.geojson", delete_dsn = TRUE)
write_csv(chicago_acs %>% st_drop_geometry(),"processed_data/interim/chicago_acs.csv", na = "")
st_write(chicago_acs_filled,"processed_data/interim/chicago_acs_filled.geojson", delete_dsn = TRUE)
write_csv(chicago_acs_filled %>% st_drop_geometry(),"processed_data/interim/chicago_acs_filled.csv", na = "")
