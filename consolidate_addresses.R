# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(data.table)
library(janitor)

files <- list.files("processed_data",pattern = "service_lines", full.names = TRUE)
service_lines_geocoded <- map_dfr(files,read_csv)

# convert to data.table
dt <- as.data.table(service_lines_geocoded)

# remove rows with missing address numbers
dt <- dt[!is.na(stnum1) & !is.na(stnum2)]

# remove leading zeros after dash in full address
dt[, full_address := sub("-(0+)([0-9]+)", "-\\2", full_address)]

# function to expand shorthand numeric ranges for computation
expand_range_vec <- function(addr) {
  num <- str_extract(addr, "^\\d+(-\\d+)?")
  low <- as.integer(str_extract(num, "^\\d+"))
  high <- str_extract(num, "(?<=-)\\d+")
  high[is.na(high)] <- low[is.na(high)]
  
  # Expand shorthand
  idx <- nchar(high) < nchar(low)
  high[idx] <- as.character(as.integer(paste0(substr(low[idx], 1, nchar(low[idx]) - nchar(high[idx])), high[idx])))
  
  # Return a character vector (same length as addr)
  paste0(low, "-", high, str_replace(addr, "^\\d+(-\\d+)?", ""))
}

# apply to full_address
dt[, full_address := expand_range_vec(full_address)]

# parse ranges
dt[, c("start","end") := tstrsplit(gsub(" .*", "", full_address), "-", type.convert=TRUE)]
dt[is.na(end), end := start]
dt[, street := sub("^\\d+-?\\d*\\s*", "", full_address)]
dt[, side := ifelse(start %% 2 == 0, "even", "odd")]


collapse_overlaps <- function(dt) {
  dt <- copy(dt)
  dt[, id := row]  # original row ids
  
  # ensure numeric ranges
  dt[is.na(end), end := start]
  dt[, start := pmin(start, end, na.rm = TRUE)]
  dt[, end   := pmax(start, end, na.rm = TRUE)]
  
  # order
  setorder(dt, street, side, start, end)
  
  # assign groups
  dt[, grp := {
    grp <- integer(.N)
    cur_grp <- 1L
    cur_end <- -Inf
    for (i in seq_len(.N)) {
      if (start[i] <= cur_end) {
        # same group, extend range if needed
        cur_end <- max(cur_end, end[i], na.rm = TRUE)
      } else {
        # new group
        cur_grp <- cur_grp + 1L
        cur_end <- end[i]
      }
      grp[i] <- cur_grp
    }
    grp
  }, by = .(street, side)]
  
  # collapse each group
  collapsed <- dt[, .(
    group_start = min(start, na.rm = TRUE),
    group_end   = max(end, na.rm = TRUE),
    group_address = paste0(min(start, na.rm = TRUE), "-", max(end, na.rm = TRUE), " ", street[1]),
    member_ids = list(id),
    member_addresses = list(full_address)
  ), by = .(street, side, grp)]
  
  collapsed[]
}

potential_overlaps <- collapse_overlaps(dt)

# keep only groups with more than 1 unique address
overlaps <- potential_overlaps[
  lengths(member_ids) > 1 & sapply(member_addresses, function(x) length(unique(x)) > 1)
]

# reshape to unique combinations of revised full address and row
overlapping_addresses <- overlaps %>%
  as_tibble() %>%
  select(full_address = group_address,
         row = member_ids) %>%
  unnest(row)

# check for duplcates by row
overlapping_addresses %>%
  get_dupes(row)
# no dupes
  
########################
# replace these full addresses in the service line inventory
service_lines_replace <- service_lines_geocoded %>%
  filter(row %in% overlapping_addresses$row) %>%
  select(-full_address) %>%
  inner_join(overlapping_addresses, by = "row")

service_lines_keep <- anti_join(service_lines_geocoded,service_lines_replace, by = "row")

# bind back to dataframe with cleaned addresses
service_lines_geocoded <- bind_rows(service_lines_keep,service_lines_replace) %>%
  arrange(row) %>%
  mutate(stnum = str_extract(full_address, "^\\d+(-\\d+)?"),
         stnum1 = as.integer(str_extract(stnum, "^\\d+")),
         stnum2 = as.integer(str_extract(stnum, "(?<=-)\\d+"))) %>%
  select(-stnum)

# save data
n <- nrow(service_lines_geocoded)
half <- ceiling(n / 2)

# write to csv
write_csv(service_lines_geocoded %>% slice(1:half),"processed_data/service_lines1.csv", na = "")
write_csv(service_lines_geocoded %>% slice((half + 1):n),"processed_data/service_lines2.csv", na = "")

