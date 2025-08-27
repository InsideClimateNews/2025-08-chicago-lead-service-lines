# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(sf)

##########
# load data

chicago_tracts_filled <-  st_read("processed_data/chicago_tracts_filled.geojson") 
chicago_community_areas <- st_read("processed_data/chicago_community_areas.geojson") 


###################
# tracts

####
# pct_requires_replacement

# assign NA values fpr tracts with fewer than 25 service lines
chicago_tracts_filled <- chicago_tracts_filled %>%
  mutate(
    pct_requires_replacement = case_when(total >= 25 ~ pct_requires_replacement,
                                         TRUE ~ NA)
  )

quantile(chicago_tracts_filled$pct_requires_replacement, c(.01,.05,.2,.4,.6,.8,.95,.99), type=1, na.rm = TRUE)

min(chicago_tracts_filled$pct_requires_replacement, na.rm = TRUE)
max(chicago_tracts_filled$pct_requires_replacement, na.rm = TRUE)


# breaks
breaks <- c(7.0,32.5,56.5,77.5,89.3,93.8,96.1,97.3)

p1 <- ggplot(chicago_tracts_filled) +
  geom_sf(aes(fill = pct_requires_replacement), color = "white", linewidth = 0.2) +
  scale_fill_fermenter(
    direction = 1,
    palette = "Reds",
    breaks = breaks,
    na.value = "grey90",
    name = ""
  ) +
  theme_void() +
  labs(
    title = "Percent requires replacement"
  )

p1

####
#poverty

quantile(chicago_tracts_filled$pct_poverty, c(.01,.05,.2,.4,.6,.8,.95,.99), type=1, na.rm = TRUE)

# breaks
breaks <- c(1.8,3.7,7.2,12.2,18.4,28,42.5,56.3)

min(chicago_tracts_filled$pct_poverty, na.rm = TRUE)
max(chicago_tracts_filled$pct_poverty, na.rm = TRUE)

p2 <- ggplot(chicago_tracts_filled) +
  geom_sf(aes(fill = pct_poverty), color = "white", linewidth = 0.2) +
  scale_fill_fermenter(
    direction = 1,
    palette = "Blues",
    breaks = breaks,
    na.value = "grey90",
    name = ""
  ) +
  theme_void() +
  labs(
    title = "Percent in poverty"
  )

p2

# nonwhite

chicago_tracts_filled <- chicago_tracts_filled %>%
  mutate(pct_minority_edit = case_when(pct_minority == 100 ~ 100.1,
                                       TRUE ~ pct_minority))

quantile(chicago_tracts_filled$pct_minority,c(.01,.05,.2,.4,.6,.8,.95), type = 1, na.rm = TRUE)

# breaks
breaks <- c(15.0,21.0,37.1,63.2,89.2,98.3,100,100.1)

p3 <- ggplot(chicago_tracts_filled) +
  geom_sf(aes(fill = pct_minority_edit), color = "white", linewidth = 0.2) +
  scale_fill_fermenter(
    direction = 1,
    palette = "Purples",
    breaks = breaks,
    limits = c(0, 100.1), # hack to create a separate bin for the 100% nonwhite tracts
    na.value = "grey90",
    name = ""
  ) +
  theme_void() +
  labs(
    title = "Percent nonwhite"
  )

p3

# generate panel with all three maps
p1+p2+p3
