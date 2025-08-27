# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(sf)
library(tidycensus)
library(readxl)
library(tidygeocoder)
library(tigris)

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
  mutate(row = c(1:nrow(service_lines_2025))) # index for later joins

# function to handle street number ranges
expand_range <- function(start, end_val) {
  if (is.na(start)) return(NA_integer_)
  start_int <- as.integer(start)
  
  if (!is.na(end_val)) {
    if (str_detect(end_val, "/")) return(start_int + 1L)  # fraction = next number
    if (nchar(end_val) >= nchar(start)) return(as.integer(end_val))
    prefix_len <- nchar(start) - nchar(end_val)
    full_end <- paste0(substr(start, 1, prefix_len), end_val)
    return(as.integer(full_end))
  }
  
  return(start_int)
}

# function to parse street addresses
parse_address <- function(addr) {
  
  # remove extraneous address elements
  addr_clean <- addr %>%
    str_replace_all("\\s+AND\\s+", " & ") %>%
    str_replace("^0+(\\d+)", "\\1") %>%
    str_replace("^(\\d+)\\s*&\\s*(\\d+)", "\\1-\\2") %>%
    str_replace("^\\s*(\\d+)\\s+(REAR|FRONT|LOWER|COACH|GARAGE|SIDE|UPPER|BASEMENT)\\b", "\\1") %>%
    str_remove_all("\\([^\\)]*\\)") %>% # remove (REAR), (FRONT) etc. 
    str_remove("\\s+(UNIT|APT|STE|SUITE|BLDG|FL|REAR|FRONT|LOWER|COACH|THOUSE|LOT)\\b.*$") %>%
    str_remove("\\s+#.*$") %>% # remove everything after #
    str_replace("^(\\d+)-\\d+/\\d+", "\\1") %>%
    str_replace("^(\\d+)/2", "\\1-2") %>%
    str_replace("^(\\d+)\\.5", "\\1") %>%
    str_replace("^(\\d+)[A-Z]\\b", "\\1") %>%
    str_replace("^(\\d+)[ ]?1/2", "\\1-1/2") %>%
    str_replace("(?<=\\d{1,5}-\\d{1,5})\\s+1/2(?=\\s+[NEWS]{1,2}\\b)", "") %>%
    str_trim()
  
  # flag intersections
  is_intersection <- str_detect(addr_clean, "\\s&\\s") |
    str_detect(addr_clean, "^\\d{2,3}(ST|ND|RD|TH)\\b")  # 103RD etc.
  
  # reorder intersections to put E/W street first if applicable
  addr_clean <- ifelse(
    is_intersection,
    {
      parts <- str_split(addr_clean, "\\s&\\s", simplify = TRUE)
      dir1 <- str_extract(parts[,1], "^[NESW]{1,2}")
      dir2 <- str_extract(parts[,2], "^[NESW]{1,2}")
      
      swap_needed <- dir1 %in% c("N", "S") & dir2 %in% c("E", "W")
      reordered <- ifelse(
        swap_needed,
        paste(parts[,2], parts[,1], sep = " & "),
        addr_clean
      )
      reordered
    },
    addr_clean
  )
  
  # regex to extract address elements
  pattern <- "^\\s*(\\d+(?:-\\d+(?:/\\d+)?)?|\\d+/\\d+)?\\s*(N|S|E|W|NE|NW|SE|SW)?\\s+(.*?)(?:\\s+(AVE|ST|RD|DR|BLVD|CT|LN|PL|WAY|TER|CIR|PKWY|TRL|EXPY|HWY|PLZ|RIVER|BYPASS))?$"
  
  m <- str_match(addr_clean, pattern)
  
  stnum_raw <- m[,2]
  stnum1 <- str_extract(stnum_raw, "^\\d+")
  stnum2_short <- str_extract(stnum_raw, "(?<=-)(\\d+|\\d+/\\d+)$")
  
  tibble(
    cleaned_address    = addr_clean,
    is_intersection    = is_intersection,
    stnum1             = as.integer(stnum1),
    stnum2             = map2_int(stnum1, stnum2_short, expand_range),
    stdir              = m[,3],
    stname             = str_trim(m[,4]),
    sttype             = m[,5]
  ) 
  
}

# parse addresses 
addresses <- bind_cols(service_lines_2025 %>% select(street_address,zip,row),parse_address(service_lines_2025$street_address)) 

glimpse(addresses)

# ensure intersections have NA for extracted address elements
addresses <- addresses %>%
  mutate(across(
    starts_with("st") & !all_of("street_address"),
    ~ ifelse(is_intersection, NA, .)
))

# test for cleaned addresses with unexpected characters
anomalies <- addresses %>%
  filter(str_detect(cleaned_address, "[^A-Za-z0-9- &']"))
  
# correct some identified edge cases, including those anomalies
addresses <- addresses %>%
  mutate(zip = case_when(zip == "46325" ~ "60633", # checked for zip codes not in range for Chicago
                         zip == "66041" ~ "60641",
                         zip == "60007" ~ "60660",
                         TRUE ~ zip),
         cleaned_address = case_when(grepl("INDIANA/ DANIEL",cleaned_address) ~ gsub("INDIANA/ ","",cleaned_address),
                                     TRUE ~ cleaned_address),
         cleaned_address = case_when(grepl("ST. LAWRENCE",cleaned_address) ~ gsub("ST. LAWRENCE","ST LAWRENCE",cleaned_address),
                                     TRUE ~ cleaned_address),
         cleaned_address = str_remove(cleaned_address,"\\s*\\(.*$"))

rm(anomalies)

# identify cleaned addresses with more than one zip code
address_dupe_zip <- addresses %>%
  distinct(cleaned_address,zip) %>%
  group_by(cleaned_address) %>%
  count() %>%
  filter(n > 1)

# remove zip for these so geocoder not thrown off by incorrect zips and return full address for use in geocoding
addresses <- addresses %>%
  mutate(zip = case_when(cleaned_address %in% address_dupe_zip$cleaned_address ~ NA_character_,
                         TRUE ~ zip),
         full_address = if_else(
             is.na(zip),
             paste0(cleaned_address, ", CHICAGO IL"),
             paste0(cleaned_address, ", CHICAGO IL ", zip)
           )
  )

rm(address_dupe_zip)

# correct issue with LOWER WACKER DR created by parse address function
addresses <- addresses %>% 
  mutate(cleaned_address = case_when(grepl("LOWER WACKER",street_address) ~ paste(cleaned_address, "LOWER WACKER DR"), # here correcting some problems with address parsing 
                                   TRUE ~ cleaned_address),
       full_address = case_when(grepl("LOWER WACKER",street_address) ~ paste0(cleaned_address, ", CHICAGO IL ", zip),
                                TRUE ~ full_address))

##########
# geocoding

# first try Census Bureau geocoder

# split into batches of 1000 rows
batch_size <- 1000
batches <- split(addresses, ceiling(seq_len(nrow(addresses)) / batch_size))

# geocoding function 
safe_geocode <- safely(function(batch) {
  geocode(
    .tbl = batch,
    address = full_address,
    method = "census",
    full_results = TRUE,
    return_type = "geographies",
    batch_limit = 1000
  )
})

geocoded_addresses <- map2(
  batches,
  seq_along(batches),
  ~{
    message(glue::glue("Processing batch {.y} of {length(batches)}..."))
    res <- safe_geocode(.x)
    if (!is.null(res$error)) {
      message(glue::glue("❌ Error in batch {.y}: {res$error$message}"))
      return(NULL)
    }
    res$result
  }
)

geocoded_addresses <- bind_rows(geocoded_addresses)

# filter for geocoded addresses
geocoded_census <- geocoded_addresses %>%
  filter(match_indicator == "Match") %>%
  mutate(geoid = paste0(state_fips,county_fips,census_tract)) %>%
  select(1:11, lat, long, matched_address, geoid)

# try again row-by-row for failures in hope of fixing failed rows
geocoded_fail <- geocoded_addresses %>%
  filter(match_indicator == "No_Match" | match_indicator == "Tie") %>%
  select(1:11)

geocoded_census_2 <- geocoded_fail %>%
  split(.$row) %>%
  map_dfr(~geocode(
    .tbl = .x,
    address = full_address,
    method = "census",
    return_type = "geographies",
    full_results = TRUE,
    batch_limit = 1
  ))

geocoded_census_2 <- geocoded_census_2 %>%
  filter(!is.na(lat)) %>%
  select(1:11,lat,long,matchedAddress,`geographies.Census Tracts`) %>%
  mutate(geoid= map_chr(`geographies.Census Tracts`, ~ .x$GEOID)) %>%
  select(-`geographies.Census Tracts`) %>%
  clean_names()

geocoded_census <- bind_rows(geocoded_census,geocoded_census_2) %>%
  mutate(geocoder = "Census Bureau")

geocoded_census %>%
  get_dupes(row)
# no dupes

# extract standardized address elements from the matched address
standardized_elements <- geocoded_census %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_census <- inner_join(geocoded_census, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_census_intesrection_flag <- geocoded_census %>%
  filter(is_intersection != m_is_intersection)

geocoded_census_number_flag <- geocoded_census %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

geocoded_census_name_flag <- geocoded_census %>%
  filter(stname != m_stname) 

# write to CSV for review in Open Refine
write_csv(geocoded_census_name_flag, "processed_data/interim/geocoded_census_name_flag.csv", na = "") 

geocoded_census_name_flag <- read_csv("processed_data/interim/geocoded_census_name_flag_edit.csv")

geocoded_census <- geocoded_census %>%
  anti_join(geocoded_census_number_flag, by = "row") %>%
  anti_join(geocoded_census_name_flag, by = "row")

# clean up environment
rm(geocoded_census_2,geocoded_addresses,expand_range,safe_geocode,batch_size,batches,
   geocoded_census_intesection_flag,geocoded_census_name_flag,geocoded_census_number_flag)

################
# checks for whether the Census Bureau geocoded addresses are within Chicago boundary

# load Chicago tracts
tracts_filled <- st_read("processed_data/interim/chicago_acs_filled.geojson") %>%
  select(geoid) %>%
  st_transform("EPSG:4269")

norwood_park <- tracts_filled %>%
  filter(geoid == "17031810400") # this is definitely included in the inventory, even though it's not part of Chicago

tracts <- st_read("processed_data/interim/chicago_acs.geojson") %>%
  select(geoid) %>%
  st_transform("EPSG:4269") %>%
  bind_rows(norwood_park)

# geocoded addresses in Census tracts outside of Chicago plus Norwood Park
geocoded_census_outside_chi <- geocoded_census %>%
  filter(!geoid %in% tracts$geoid)

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

# remove holes
chicago$geometry <- st_sfc(lapply(st_geometry(chicago), remove_holes), crs = st_crs(chicago))

geocoded_census_inside_chi <- geocoded_census %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(chicago) %>%
  st_drop_geometry()

geocoded_census_outside_chi_2 <- anti_join(geocoded_census,geocoded_census_inside_chi, by = "row")

geocoded_census_outside_chi <- bind_rows(geocoded_census_outside_chi,geocoded_census_outside_chi_2) %>%
  unique()

# write to CSV to examine in QGIS
write_csv(geocoded_census_outside_chi,"processed_data/interim/geocoded_census_outside_chi.csv", na = "")

rm(geocoded_census_outside_chi_2,geocoded_census_inside_chi)

# exclude some addresses which don't seem to have matched correctly on inspection in QGIS
geocoded_census_outside_chi  <- geocoded_census_outside_chi %>%
  filter(row != 437242 & row != 437243 & row != 437244 & row != 437245 & row != 437246 & row != 437247  & row != 243244 & row !=354352 & row != 219145 & row != 415368)

geocoded_census <- anti_join(geocoded_census,geocoded_census_outside_chi, by = "row")

geocoded_fail <- anti_join(addresses,geocoded_census, by = "row") %>%
  select(1:11)

# try Bing geocoder for these
geocoded_bing <- geocode(geocoded_fail,
                         address = full_address,
                         method = "bing",
                         full_results = TRUE) 

geocoded_bing <- geocoded_bing %>%
  filter(entityType == "Address" | entityType == "RoadIntersection") %>%
  select(1:11,lat,long,matched_address = bing_address.formattedAddress) %>%
  mutate(geocoder = "Bing",
         matched_address = str_to_upper(matched_address), # change format to match census
         matched_address = gsub(" IL ", " IL, ", matched_address)) 


# extract standardized address elements from the matched address
standardized_elements <- geocoded_bing %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_bing <- inner_join(geocoded_bing, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_bing_intersection_flag <- geocoded_bing %>%
  filter(is_intersection != m_is_intersection)

geocoded_bing_number_flag <- geocoded_bing %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

geocoded_bing_name_flag <- geocoded_bing %>%
  filter(stname != m_stname) 

# write these to CSV for manual inspection/cleaning in Open Refine
write_csv(geocoded_bing_name_flag,"processed_data/interim/geocoded_bing_name_flag.csv", na = "")

# these are ok
geocoded_bing_name_flag_starred <- read_csv("processed_data/interim/geocoded_bing_name_flag_starred.csv") %>%
  mutate(zip = as.character(zip),
         m_zip = as.character(m_zip)) 

# these are not
geocoded_bing_name_flag_edit <- read_csv("processed_data/interim/geocoded_bing_name_flag_edit.csv") %>%
  mutate(zip = as.character(zip)) %>%
  select(1:11)

geocoded_bing_name_flag_retry <- geocode(geocoded_bing_name_flag_edit,
                               address = full_address,
                               method = "bing",
                               full_results = TRUE
)

geocoded_bing_name_flag_retry  <- geocoded_bing_name_flag_retry  %>%
  filter(entityType == "Address" | entityType == "RoadIntersection") %>%
  select(1:11,lat,long,matched_address = bing_address.formattedAddress) %>%
  mutate(geocoder = "Bing",
         matched_address = str_to_upper(matched_address),
         matched_address = gsub(" IL ", " IL, ", matched_address))

geocoded_bing_name_flag_retry  <- geocoded_bing_name_flag_retry %>%
  filter(grepl("FRONT", matched_address)) # selecting those that are now geocoded

# extract standardized address elements from the matched address
standardized_elements <- geocoded_bing_name_flag_retry  %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

geocoded_bing_name_flag_retry  <- inner_join(geocoded_bing_name_flag_retry, standardized_elements, by = "row")

# clean the data of flagged elements         
geocoded_bing <- geocoded_bing %>%
  anti_join(geocoded_bing_intersection_flag, by = "row") %>%
  anti_join(geocoded_bing_number_flag, by = "row") %>%
  anti_join(geocoded_bing_name_flag, by = "row") 

# add back the checked and corrected ones
geocoded_bing <- geocoded_bing %>%
  bind_rows(geocoded_bing_name_flag_starred) %>%  
  bind_rows(geocoded_bing_name_flag_retry)
  
# add tract geoid by intersection, will remove those not in Chicago or Norwood Park
geocoded_bing <- geocoded_bing %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(tracts) %>%
  st_drop_geometry()

geocoded_bing <- geocoded_bing %>%
  mutate(geocoder = "Bing")

# remove the Bing geocoded rows from geocoded_fail and geocoded_census_outside_chi
geocoded_fail <- anti_join(geocoded_fail,geocoded_bing, by = "row")

geocoded_census_outside_chi <- anti_join(geocoded_census_outside_chi,geocoded_bing, by = "row")

# clean up environment
rm(geocoded_bing_intersection_flag,geocoded_bing_number_flag,geocoded_bing_name_flag,geocoded_bing_name_flag_edit,geocoded_bing_name_flag_retry,geocoded_bing_name_flag_starred)

# Now try HERE geocoder
geocoded_here <- geocode(geocoded_fail,
                         address = full_address,
                         method = "here",
                         full_results = TRUE) 

geocoded_here <- geocoded_here %>%
  filter(resultType == "houseNumber" | resultType == "intersection") %>%
  select(1:11,lat,long,matched_address = here_address.label) %>%
  mutate(geocoder = "HERE",
         matched_address = str_to_upper(matched_address),
         matched_address = gsub(", UNITED STATES", "", matched_address))

# extract standardized address elements from the matched address
standardized_elements <- geocoded_here %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_here <- inner_join(geocoded_here, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_here_intersection_flag <- geocoded_here %>%
  filter(is_intersection != m_is_intersection)

geocoded_here_number_flag <- geocoded_here %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

geocoded_here_name_flag <- geocoded_here %>%
  filter(stname != m_stname)

# write these to CSV for manual inspection/cleaning in Open Refine
write_csv(geocoded_here_name_flag,"processed_data/interim/geocoded_here_name_flag.csv", na = "")

geocoded_here_name_flag_edit <- read_csv("processed_data/interim/geocoded_here_name_flag_edit.csv")

# clean the data of flagged elements         
geocoded_here <- geocoded_here %>%
  anti_join(geocoded_here_intersection_flag, by = "row") %>%
  anti_join(geocoded_here_number_flag, by = "row") %>%
  anti_join(geocoded_here_name_flag_edit, by = "row") 

# add tract geoid by intersection, will remove those not in Chicago or Norwood Park
geocoded_here <- geocoded_here %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(tracts) %>%
  st_drop_geometry()

# remove the HERE geocoded rows from geocoded_fail and geocoded_census_outside_chi
geocoded_fail <- anti_join(geocoded_fail,geocoded_here, by = "row")

geocoded_census_outside_chi <- anti_join(geocoded_census_outside_chi,geocoded_here, by = "row")

# clean environment
rm(geocoded_here_intersection_flag,geocoded_here_number_flag,geocoded_here_name_flag,geocoded_here_name_flag_edit,geocoded_here_name_flag_retry,geocoded_here_name_flag_starred)

# Now try Geocodio geocoder
geocoded_geocodio <- geocode(geocoded_fail,
                             address = full_address,
                             method = "geocodio",
                             full_results = TRUE,
) 

geocoded_geocodio <- geocoded_geocodio %>%
  filter(accuracy_type == "rooftop" | accuracy_type == "intersection") %>%
  select(1:11,lat,long,matched_address = formatted_address) %>%
  mutate(matched_address = str_to_upper(matched_address),
         matched_address = gsub(" AND ", " & ",matched_address))

# extract standardized address elements from the matched address
standardized_elements <- geocoded_geocodio %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_geocodio <- inner_join(geocoded_geocodio, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_geocodio_intersection_flag <- geocoded_geocodio %>%
  filter(is_intersection != m_is_intersection)

geocoded_geocodio_number_flag <- geocoded_geocodio %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

geocoded_geocodio_name_flag <- geocoded_geocodio %>%
  filter(stname != m_stname)

# no flags

# add tract geoid by intersection, will remove those not in Chicago or Norwood Park
geocoded_geocodio <- geocoded_geocodio %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(tracts) %>%
  st_drop_geometry()

geocoded_geocodio <- geocoded_geocodio %>%
  filter(row != 218921) %>%
  mutate(geocoder = "Geocodio")

# remove the Geocodio geocoded rows from geocoded_fail and geocoded_census_outside_chi
geocoded_fail <- anti_join(geocoded_fail,geocoded_geocodio, by = "row")

geocoded_census_outside_chi <- anti_join(geocoded_census_outside_chi,geocoded_geocodio, by = "row")

# clean environment
rm(geocoded_geocodio_intersection_flag,geocoded_geocodio_name_flag, geocoded_geocodio_number_flag)

# now try ArcGIS geocoder 
geocoded_arcgis <- geocode(
  .tbl = geocoded_fail,
  method = "arcgis",
  address = full_address,
  full_results = TRUE
) 

geocoded_arcgis <- geocoded_arcgis %>%
  select(1:11,lat,long,score,matched_address = arcgis_address) %>%
  mutate(matched_address = str_to_upper(matched_address),
         matched_address = gsub(" ILLINOIS", " IL",matched_address))

# extract standardized address elements from the matched address
standardized_elements <- geocoded_arcgis %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_arcgis <- inner_join(geocoded_arcgis, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_arcgis_intersection_flag <- geocoded_arcgis %>%
  filter(is_intersection != m_is_intersection)

geocoded_arcgis_number_flag <- geocoded_arcgis %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

# geocoded_arcgis_name_flag <- geocoded_arcgis %>%
#   filter(stname != m_stname) # few of these

# write these to CSV for manual inspection/cleaning in Open Refine
write_csv(geocoded_arcgis,"processed_data/interim/geocoded_arcgis.csv", na = "")

geocoded_arcgis_flagged_edit <- read_csv("processed_data/interim/geocoded_arcgis_flagged_edit.csv")

geocoded_arcgis_flag_retry <- read_csv("processed_data/interim/geocoded_arcgis_flag_retry.csv") %>%
  select(1:11)

# try ArcGIS geocoder again on these with edits in Open Refine
geocoded_arcgis_flag_retry <- geocode(
  .tbl = geocoded_arcgis_flag_retry,
  method = "arcgis",
  address = full_address,
  full_results = TRUE
) 

geocoded_arcgis_flag_retry <- geocoded_arcgis_flag_retry %>%
  select(1:11,lat,long,score,matched_address = arcgis_address) %>%
  mutate(matched_address = str_to_upper(matched_address),
         matched_address = gsub(" ILLINOIS", " IL",matched_address))

# extract standardized address elements from the matched address
standardized_elements <- geocoded_arcgis_flag_retry %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

geocoded_arcgis_flag_retry  <- inner_join(geocoded_arcgis_flag_retry, standardized_elements, by = "row") %>%
  mutate(m_zip = as.character(m_zip))

geocoded_arcgis_flag_retry <- geocoded_arcgis_flag_retry %>%
  mutate(zip = as.character(zip))

# clean the data of flagged elements         
geocoded_arcgis <- geocoded_arcgis %>%
  anti_join(geocoded_arcgis_intersection_flag, by = "row") %>%
  anti_join(geocoded_arcgis_number_flag, by = "row") %>%
  anti_join(geocoded_arcgis_flagged_edit, by = "row")

# add back the regeocoded addresses
geocoded_arcgis <- geocoded_arcgis %>%
  bind_rows(geocoded_arcgis_flag_retry)

# add tract geoid by intersection, will remove those not in Chicago or Norwood Park
geocoded_arcgis <- geocoded_arcgis %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(tracts) %>%
  st_drop_geometry()

geocoded_arcgis <- geocoded_arcgis %>%
  mutate(geocoder = "ArcGIS")


# remove the ArcGIS geocoded rows from geocoded_fail and geocoded_census_outside_chi
geocoded_fail <- anti_join(geocoded_fail,geocoded_arcgis, by = "row")

geocoded_census_outside_chi <- anti_join(geocoded_census_outside_chi,geocoded_arcgis, by = "row")

# clean environment
rm(geocoded_arcgis_flag_retry, geocoded_arcgis_intersection_flag,geocoded_arcgis_number_flag,geocoded_arcgis_name_flag,geocoded_arcgis_name_flag_edit,geocoded_arcgis_name_flag_retry,geocoded_arcgis_name_flag_starred)

# We're now into diminishing returns, so lets try again with Geocodio and now allow joins to any tract in Cook County
cook_tracts <- tracts(state = "IL", county = "Cook", cb = FALSE) %>%
  clean_names() %>%
  select(geoid)

# Now try Geocodio geocoder
geocoded_geocodio_repeat <- geocode(geocoded_fail,
                             address = full_address,
                             method = "geocodio",
                             full_results = TRUE,
) 

geocoded_geocodio_repeat <- geocoded_geocodio_repeat %>%
  filter(accuracy_type == "rooftop" | accuracy_type == "intersection") %>%
  select(1:11,lat,long,matched_address = formatted_address) %>%
  mutate(matched_address = str_to_upper(matched_address),
         matched_address = gsub(" AND ", " & ",matched_address))

# extract standardized address elements from the matched address
standardized_elements <- geocoded_geocodio_repeat %>%
  mutate(matched_address = gsub(", IL ",", IL, ", matched_address)) %>%
  separate(matched_address, into = c("m_street","m_city","m_state","m_zip"), sep = ",", remove = FALSE) %>%
  select(starts_with("m_"),is_intersection,row,matched_address) %>%
  mutate(m_zip = substr(m_zip,1,6))

standardized_elements <- bind_cols(standardized_elements %>% select(-is_intersection), parse_address(standardized_elements$m_street)) %>%
  rename(m_stnum1 = stnum1,
         m_stnum2 =  stnum2,
         m_stdir = stdir,
         m_stname = stname,
         m_sttype = sttype) %>%
  select(-is_intersection) %>%
  mutate(m_is_intersection = case_when(grepl("&",matched_address) ~ TRUE,
                                       TRUE ~ FALSE)) %>%
  select(row,m_is_intersection,,m_street,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip) %>%
  arrange(row)

standardized_elements %>%
  get_dupes(row)
# no dupes

geocoded_geocodio_repeat <- inner_join(geocoded_geocodio_repeat, standardized_elements, by = "row")

# look for mismatching numbers, names, or flag as intersection
geocoded_geocodio_repeat_intersection_flag <- geocoded_geocodio_repeat %>%
  filter(is_intersection != m_is_intersection)

geocoded_geocodio_repeat_number_flag <- geocoded_geocodio_repeat %>%
  filter(stnum1 != m_stnum1 & stnum1 != m_stnum2 & stnum2 != m_stnum1 & stnum2 != m_stnum2)

geocoded_geocodio_repeat_name_flag <- geocoded_geocodio_repeat_intersection_flag %>%
  filter(stname != m_stname)

# no flags

# add tract geoid by intersection
geocoded_geocodio_repeat <- geocoded_geocodio_repeat %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(cook_tracts) %>%
  st_drop_geometry()

geocoded_geocodio_repeat <- geocoded_geocodio_repeat %>%
  select(1:22,geoid)

geocoded_geocodio_outside_chi <- geocoded_geocodio_repeat %>%
  filter(!geoid %in% tracts$geoid)

geocoded_geocodio_inside_chi <- geocoded_geocodio_repeat %>%
  st_as_sf(coords = c("long","lat"),
           crs = st_crs("EPSG:4269"),
           remove = FALSE) %>%
  st_intersection(chicago) %>%
  st_drop_geometry()

geocoded_geocodio_outside_chi_2 <- anti_join(geocoded_geocodio_repeat,geocoded_geocodio_inside_chi, by = "row")

geocoded_geocodio_outside_chi <- bind_rows(geocoded_geocodio_outside_chi,geocoded_geocodio_outside_chi_2) %>%
  unique() %>%
  mutate(geocoder == "Geocodio")

rm(geocoded_geocodio_outside_chi_2,geocoded_geocodio_inside_chi)

# write to CSV to examine in QGIS
write_csv(geocoded_geocodio_outside_chi,"processed_data/interim/geocoded_geocodio_outside_chi.csv", na = "")

# remove any already present in the census bureau data
geocoded_geocodio_outside_chi <- anti_join(geocoded_geocodio_outside_chi, geocoded_census_outside_chi, by = "row") %>%
  
# combine outside_chi
geocoded_outside_chi <- bind_rows(geocoded_geocodio_outside_chi,geocoded_census_outside_chi)

# clean environment
rm(geocoded_geocodio_outside_chi_2,geocoded_census_outside_chi,geocoded_census_outside_chi_2,geocoded_census_inside_chi,geocoded_geocodio_inside_chi,geocoded_geocodio_repeat,geocoded_geocodio_repeat_intersection_flag,geocoded_geocodio_repeat_name_flag,
   geocoded_geocodio_repeat_number_flag,standardized_elements)

geocoded_fail <- anti_join(geocoded_fail,geocoded_outside_chi, by = "row")

# combine all the geocoded data
geocoded <- bind_rows(geocoded_census,geocoded_bing,geocoded_here,geocoded_arcgis,geocoded_geocodio) %>%
  arrange(geocoder,row) %>%
  select(row,full_address,is_intersection,stnum1,stnum2,stdir,stname,sttype,zip,geocoder,lat,long,geoid,matched_address,m_is_intersection,m_stnum1,m_stnum2,m_stdir,m_stname,m_sttype,m_zip)

geocoded %>%
  get_dupes(row)
# no dupes

geocoded_outside_chi <- geocoded_outside_chi %>%
  arrange(geocoder,row) %>%
  select(row,full_address,is_intersection,stnum1,stnum2,stdir,stname,sttype,zip,geocoder,lat,long,geoid,matched_address,m_is_intersection,m_stnum1,m_stnum2,m_stdir,m_stname,m_zip)

geocoded_fail <- geocoded_fail  %>%
  arrange(row) %>%
  select(row,full_address,is_intersection,stnum1,stnum2,stdir,stname,sttype,zip)

n <- nrow(geocoded)
half <- ceiling(n / 2)

geocoded %>%
  filter(stnum1 != stnum2 & is_intersection == FALSE) %>%
  nrow()

geocoded %>%
  filter(m_stnum1 != m_stnum2 & is_intersection == FALSE) %>%
  nrow()

# write to csv
write_csv(geocoded %>% slice(1:half),"processed_data/interim/geocoded1.csv", na = "")
write_csv(geocoded %>% slice((half + 1):n),"processed_data/interim/geocoded2.csv", na = "")
write_csv(geocoded_outside_chi,"processed_data/interim/geocoded_outside_chi.csv", na = "")
write_csv(geocoded_fail,"processed_data/interim/geocoded_fail.csv", na = "")
