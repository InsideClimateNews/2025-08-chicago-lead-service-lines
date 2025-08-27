# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(sf)
library(tidycensus)
library(readxl)
library(tigris)


# don't use scientific notation
options(scipen = 999)

##############################################
# Service line level data

# load inventory data and perform some initial cleaning
service_lines_2025 <- read_excel("data/2025_inventory.xlsx", sheet = 1, skip = 6) %>%
  clean_names() %>%
  filter(!is.na(service_address)) %>%
  mutate(service_address = gsub("^([0-9]+) ([0-9]+)", "\\1-\\2", service_address), # replace gaps in address ranges with hyphen
         street_address = sub(",.*", "", service_address), # remove periods from addresses
         zip = substr(word(service_address,-1),1,5)) %>% # extract 5 digit zip code
  rename_with(~ str_replace(., "serivice", "service")) %>%
  slice(-1)

service_lines_2025 <- service_lines_2025 %>%
  mutate(row = as.character(c(1:nrow(service_lines_2025)))) # index for joins

# load geocoded data
geocoded_auto <- read_csv("processed_data/interim/geocoded1.csv") %>%
  bind_rows(read_csv("processed_data/interim/geocoded2.csv")) %>%
  bind_rows(read_csv("processed_data/interim/geocoded_outside_chi.csv")) %>%
  mutate(geoid = as.character(geoid),
         row = as.character(row))

geocoded_manual <- read_csv("processed_data/interim/geocoded_manual.csv") %>%
  mutate(row = as.character(row),
         matched_address = full_address,
         m_stnum1 = case_when(!is.na(stnum1) ~ stnum1),
         m_stnum2 = case_when(!is.na(stnum1) ~ stnum2),
         m_stdir = case_when(!is.na(stnum1) ~ stdir),
         m_stname = case_when(!is.na(stnum1) ~ stname),
         m_sttype = case_when(!is.na(stnum1) ~ sttype),
         m_zip = case_when(!is.na(stnum1) ~ zip)) %>%
  select(-notes1,-notes2)

# get tracts boundaries for Cook County
cook_tracts <- tracts(state = "IL", county = "Cook", cb = FALSE) %>%
  clean_names() %>%
  select(geoid)

# spatial join of manually geocoded data to tracts for those for which we have lat and long
geocoded_manual_good <- geocoded_manual %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(cook_tracts) %>%
  st_drop_geometry() %>%
  mutate(geocoder = "Manual")

geocoded_manual_bad <- anti_join(geocoded_manual,geocoded_manual_good, by = "row")

geocoded <- bind_rows(geocoded_auto,geocoded_manual_good)

geocoded %>% get_dupes(row)

# join and export for spatial join to community areas in QGIS
service_lines_geocoded <- service_lines_2025 %>%
  select(row,gooseneck_pigtail,pws_owned_service_line_material,customer_side_service_line_material,classification_for_entire_service_line) %>%
  inner_join(geocoded, by = "row")

write_csv(service_lines_geocoded, "processed_data/service_lines_geocoded.csv", na = "")

# files <- list.files("processed_data", pattern = "service_lines", full.names = TRUE)
# service_lines_geocoded <- map_dfr(files, read_csv)
# glimpse(service_lines_geocoded)
# 
# test <- service_lines_geocoded %>%
#   filter(is.na(full_address))

# saving data
n <- nrow(service_lines_geocoded)
half <- ceiling(n / 2)

# write to csv
write_csv(service_lines_geocoded %>% slice(1:half),"processed_data/service_lines1.csv", na = "")
write_csv(service_lines_geocoded %>% slice((half + 1):n),"processed_data/service_lines2.csv", na = "")



##############################################
# Thematic map layers

# get tracts boundaries for Cook County
cook_tracts <- tracts(state = "IL", county = "Cook", cb = FALSE) %>%
  clean_names() %>%
  select(geoid)

# spatial join of manually geocoded data to tracts for those for which we have lat and long
geocoded_manual_good <- geocoded_manual %>%
  filter(!is.na(lat)) %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(cook_tracts) %>%
  st_drop_geometry() %>%
  mutate(geocoder = "Manual")

geocoded_manual_bad <- anti_join(geocoded_manual,geocoded_manual_good, by = "row")

geocoded <- bind_rows(geocoded_auto,geocoded_manual_good)

geocoded %>% get_dupes(row)

# join and export for spatial join to community areas in QGIS
service_lines_geocoded <- service_lines_2025 %>%
  select(row,gooseneck_pigtail,pws_owned_service_line_material,customer_side_service_line_material,classification_for_entire_service_line) %>%
  inner_join(geocoded, by = "row")

write_csv(service_lines_geocoded, "processed_data/service_lines_geocoded.csv", na = "")

# calculations by tract
service_lines_tract_replaced <- service_lines_2025 %>%
  select(row, was_a_lead_service_line_previously_replaced_at_this_location) %>%
  inner_join(service_lines_geocoded, by = "row") %>%
  select(lat,long,geoid,was_a_lead_service_line_previously_replaced_at_this_location) %>%
  group_by(geoid,was_a_lead_service_line_previously_replaced_at_this_location) %>%
  count() %>%
  pivot_wider(names_from = was_a_lead_service_line_previously_replaced_at_this_location, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         total = N + Y,
         pct_replaced = round(Y / total * 100, 1)) %>%
  select(-total)  # so we don't have this twice in the joined data
         
service_lines_tract_material <- service_lines_geocoded %>%
  group_by(geoid,classification_for_entire_service_line) %>%
  count() %>%
  pivot_wider(names_from = classification_for_entire_service_line, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         total = U + L + GRR + NL,
         flag = case_when(total < 25 | is.na(total) ~ TRUE,
                          TRUE ~ FALSE),
         lead_plus_suspected = U + L,
         requires_replacement = U + L + GRR,
         pct_lead = round(L / total * 100, 1),
         pct_grr = round(GRR / total * 100, 1),
         pct_suspected_lead = round(U / total * 100, 1),
         pct_lead_plus_suspected =  round(lead_plus_suspected / total * 100, 1),
         pct_requires_replacement =  round(requires_replacement / total * 100, 1),
         pct_not_lead =  round(NL / total * 100, 1)) 

# load acs tract data
chicago_acs  <- st_read("processed_data/interim/chicago_acs.geojson")
chicago_acs_filled  <- st_read("processed_data/interim/chicago_acs_filled.geojson")

chicago_tracts <- left_join(chicago_acs, service_lines_tract_material, by = "geoid") %>%
  left_join(service_lines_tract_replaced, by = "geoid")
chicago_tracts_filled <- left_join(chicago_acs_filled, service_lines_tract_material, by = "geoid")  %>%
  left_join(service_lines_tract_replaced, by = "geoid")

# write data
st_write(chicago_tracts, "processed_data/chicago_tracts.geojson", delete_dsn = TRUE)
st_write(chicago_tracts_filled, "processed_data/chicago_tracts_filled.geojson", delete_dsn = TRUE)

write_csv(chicago_tracts %>% st_drop_geometry(), "processed_data/chicago_tracts.csv", na = "")
write_csv(chicago_tracts_filled %>% st_drop_geometry(), "processed_data/chicago_tracts_filled.csv", na = "")

# load community areas boundary data
community_areas <- st_read("data/community_areas.geojson") %>%
  mutate(community = str_to_title(community)) %>%
  select(community, area_num_1)

# load data from intersection with community areas and join to replacement data
service_lines_community_areas <- read_csv("processed_data/service_lines_community_areas.csv") %>%
  mutate(row = as.character(row)) %>%
  left_join(service_lines_2025 %>% select(row, was_a_lead_service_line_previously_replaced_at_this_location), by = "row") 

# calculations by community area
service_lines_community_area_replaced <- service_lines_community_areas %>%
  mutate(community = str_to_title(community),
         area_num_1 = as.character(area_num_1)) %>%
  select(row, community, area_num_1, was_a_lead_service_line_previously_replaced_at_this_location) %>%
  group_by(community, area_num_1, was_a_lead_service_line_previously_replaced_at_this_location) %>%
  count() %>%
  pivot_wider(names_from = was_a_lead_service_line_previously_replaced_at_this_location, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         total = N + Y,
         pct_replaced = round(Y / total * 100, 1)) %>%
  select(-total) # so we don't have this twice in the joined data

service_lines_community_area_material <- service_lines_community_areas %>%
  mutate(community = str_to_title(community),
         area_num_1 = as.character(area_num_1)) %>%
  group_by(community,area_num_1,classification_for_entire_service_line) %>%
  count() %>%
  pivot_wider(names_from = classification_for_entire_service_line, values_from = n) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         total = U + L + GRR + NL,
         flag = case_when(total < 25 | is.na(total) ~ TRUE,
                          TRUE ~ FALSE),
         lead_plus_suspected = U + L,
         requires_replacement = U + L + GRR,
         pct_lead = round(L / total * 100, 1),
         pct_grr = round(GRR / total * 100, 1),
         pct_suspected_lead = round(U / total * 100, 1),
         pct_lead_plus_suspected =  round(lead_plus_suspected / total * 100, 1),
         pct_requires_replacement =  round(requires_replacement / total * 100, 1),
         pct_not_lead = round(NL / total * 100, 1))

chicago_community_areas <- left_join(community_areas, service_lines_community_area_material, by = c("community","area_num_1")) %>%
  left_join(service_lines_community_area_replaced, by = c("community","area_num_1")) %>%
  mutate(community = gsub("Loop","The Loop",community),
         community = gsub("Ohare","O'Hare", community),
         community = gsub("Mckinley Park", "McKinley Park",community))

# add ACS data aggregated to Community Areas 
files <- list.files("processed_data/interim/", pattern = "agg_community", full.names = TRUE)
comm_areas_income <- read_csv(files[2]) %>%
  select(community = CCA, median_household_income = estimated_median_income)
comm_areas_poverty <- read_csv(files[3]) %>%
  select(community = CCA, pct_poverty = pct_est_population_assessed_poverty) %>%
  mutate(pct_poverty = round(pct_poverty * 100,1))
comm_areas_race <- read_csv(files[4]) %>%
  select(community = CCA, contains("pct")) %>%
  mutate(across(where(is.numeric), ~ round(.x * 100, 1)))
comm_areas_acs <- inner_join(comm_areas_income,comm_areas_poverty, by = "community") %>%
  inner_join(comm_areas_race, by = "community")
chicago_community_areas <- inner_join(chicago_community_areas,comm_areas_acs, by = "community")

# write data
st_write(chicago_community_areas, "processed_data/chicago_community_areas.geojson", delete_dsn = TRUE)
write_csv(chicago_community_areas %>% st_drop_geometry(), "processed_data/chicago_community_areas.csv")


