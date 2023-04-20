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






































































