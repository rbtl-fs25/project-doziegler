library(googlesheets4)
library(dplyr)

data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1qxXpkAtzvy3RioQg1zvR0rJye1f4GZJ-MTt93_jXdFw/edit?usp=sharing")

# Columns of type list need to be character as otherwhise when writing to csv -> NA
sapply(data_raw, class)

data_raw_clean <- data_raw %>%
  mutate(across(where(is.list), as.character))

write_csv(data_raw_clean,"data/raw/repaircafe_data_raw.csv")
