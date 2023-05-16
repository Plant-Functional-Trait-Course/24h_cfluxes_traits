# This script is to prepare several dataset to test the SEM.
# The main issue will be to decide how to group the data based on the time of the day.
# Because of a lack of replicate, we have to group the data and cannot use the hourly dataset.


# package and data --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dataDownloader)




get_file(
  node = "fcbw4",
  file = "PFTC6_24h_cflux_allsites_2022.csv",
  path = "data",
  remote_path = "c_flux_data"
)

flux <- read_csv("data/PFTC6_24h_cflux_allsites_2022.csv", col_types = "ffdddTtdf")

# day quarters ------------------------------------------------------------

# Here we will just do simple arbitrary division of the day in 4 quarters: 00:00 - 05:59 / 06:00 - 11:59 / 12:00 - 17:59 / 18:00 - 24:00
# It does not really make sense from an ecological point of view, but it is logical and simple

flux_quarters <- flux %>% 
  mutate(
    # time = hms(time),
    quarter = case_when(
      # time %in% c(hms("00:00:01"): hms("05:59:59")) ~ "first"
      
      time < hms("06:00:00") ~ "first",
      time < hms("12:00:00") ~ "second",
      time < hms("18:00:00") ~ "third",
      TRUE ~ "fourth"
    )
  )


# sun angle ---------------------------------------------------------------

# we can try to use the sun angle from there https://www.timeanddate.com/sun/norway/bergen
# on 30/07/22 the sunrise was at 05:11, solar noon at 13:45 and sunset at 22:16

flux_angle <- flux %>% 
  mutate(
    quarter = case_when(
      time < hms("05:11:00") ~ "night",
      time < hms("13:45:00") ~ "morning",
      time < hms("22:16:00") ~ "afternoon",
      TRUE ~ "night"
    )
  )


#List of turfs

metaturf <- metaturf %>% filter(turfID %in% flux$turfID)

#Import community data

community_incline <- read.csv("raw_data/turf_community_Incline.csv", sep = ";")
community_3d <- read.csv("raw_data/turf_community_Three-D.csv", sep = ";")

community_data <- rbind(community_incline, community_3d)

as_tibble(community_data)

#Finding data from the turfs that we are interested in

community_data <- community_data %>% filter(turfID %in% metaturf$turfID)

#Finding the most current plant community data

community_data <- community_data %>% group_by(turfID) %>% filter(year == max(year, na.rm=TRUE))

#Import trait data

library(dataDownloader)
# get_file(node = "fcbw4",
#          file = "PFTC6_leaf_area_2022.csv",
#          path = "clean_data",
#          remote_path = "raw_data/trait_raw_data")
# 
# get_file(node = "fcbw4",
#          file = "PFTC6_clean_leaf_traits_2022.csv",
#          path = "clean_data",
#          remote_path = "trait_data")
# 
# get_file(node = "pk4bg",
#          file = "THREE-D_Cover_2019_2020.csv",
#          path = "clean_data",
#          remote_path = "Vegetation")

# I don't know how to import data from an sqlite file in R
# These data are from https://osf.io/6tu4p 

#Read in data ----
## Traits ----
leaf.area <- read.csv("clean_data/PFTC6_leaf_area_2022.csv") %>%
  select(ID, leaf_area)

# traits = read.csv("clean_data/PFTC6_clean_leaf_traits_2022.csv") %>%
#   separate(plotID, into = c("origin", "trt", "destination"), sep = " ", remove = FALSE) %>%
#   left_join(leaf.area) %>%
#   mutate(sla = leaf_area/wet_mass_g)

traits = read.csv("clean_data/PFTC6_clean_leaf_traits_2022.csv", header = TRUE) %>%
  mutate(sla = leaf_area_total_cm2/wet_mass_total_g)


#Finding trait data from the turfs that we are interested in
#some plots are missing turfIDs

traits <- traits %>% filter(turfID %in% metaturf$turfID)

#Calculating CWM




































































