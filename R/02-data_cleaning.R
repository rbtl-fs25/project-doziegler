library(tidyverse)

raw_data <- read.csv("data/raw/repaircafe_data_raw.csv") %>% slice(10:n())

#delete some unnececessary rows
data <- raw_data %>% 
  select(
    -18,
    -19,
    -21,
    -36,
    -42,
    -43
)

# Move stuff around because of google forms ->sheets mess
data <- data %>%
  select(
    1:17,
    28:29,
    35,
    30,
    36,
    31:32,
    34,
    33,
    37,
    18,
    23:27,
    everything()
  )



# Renaming the col names
names(data)[1] <- "time"
names(data)[2] <- "gender"
names(data)[3] <- "age"
names(data)[4] <- "uni"
names(data)[5] <- "monthly_food_expenses"
names(data)[6] <- "stable_income"
names(data)[7] <- "sustainability_importance"
names(data)[8] <- "phone_screen_breaks"
names(data)[9] <- "likelihood_of_repair"
names(data)[10] <- "max_price_of_repair"
names(data)[11] <- "skill_electronics"
names(data)[12] <- "skill_metal"
names(data)[13] <- "skill_wood"
names(data)[14] <- "skill_textile"
names(data)[15] <- "skill_assembly"
names(data)[16] <- "skill_filigrane"
names(data)[17] <- "most_difficult_repair"
names(data)[18] <- "bi_phone"
names(data)[19] <- "bi_tablet"
names(data)[20] <- "bi_computer"
names(data)[21] <- "bi_household_electronics"
names(data)[22] <- "bi_other_electronics"
names(data)[23] <- "bi_bike"
names(data)[24] <- "bi_furniture"
names(data)[25] <- "bi_clothes"
names(data)[26] <- "bi_shoes"
names(data)[27] <- "bi_other_items"
names(data)[28] <- "repair_considered"
names(data)[29] <- "rno_difficult"
names(data)[30] <- "rno_time"
names(data)[31] <- "rno_expensive"
names(data)[32] <- "rno_motivation"
names(data)[33] <- "rno_other"
names(data)[34] <- "eth_rc_usage_likelihood"
names(data)[35] <- "eth_rc"
names(data)[36] <- "eth_rc_help"
names(data)[37] <- "eth_rc_learn"

sapply(data, class)
data <- data %>% mutate(ID = row_number()) %>% relocate(ID,.before = 1)

d <- data

# UFF Reordering and Renaming finally finished, let's begin with actual work

d <- d %>% mutate(stable_income = stable_income == "Yes")
d <- d %>% mutate(monthly_food_expenses= case_when(
  monthly_food_expenses == ">200" ~ 150,
  monthly_food_expenses == "200-400" ~ 300,
  monthly_food_expenses == "400-600" ~ 500,
  monthly_food_expenses == "600-900" ~ 750,
  monthly_food_expenses == "900-1100" ~ 1000,
  monthly_food_expenses == "900-1100" ~ 1250,
)) 
d <- d %>% 
  mutate(
      max_price_of_repair = str_remove(max_price_of_repair, "%") %>%   # remove %
      str_trim() %>%                                 # remove any extra spaces
      as.numeric()                                   # convert to number
  )
d <- d %>%
  mutate(across(
    19:28,
    ~ case_when(
      .x == "more than 5" ~ 6,
      TRUE ~ as.numeric(.x)
    )
  ))

write_csv(d,"data/processed/repaircafe_data_processed.csv")


