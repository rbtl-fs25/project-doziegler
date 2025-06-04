library(tidyverse)
library(ggplot2)
library(gt)
library(knitr)
data <- read_csv("data/processed/repaircafe_data_processed.csv")
data <- data %>% mutate(skill_avg = rowMeans(select(., 12:17), na.rm = TRUE))
data <- data %>% mutate(discarded_items = rowSums(select(., 19:28), na.rm = TRUE))

cor.test(data$likelihood_of_repair,data$skill_avg)

df_summary <- data %>% count(eth_rc) %>% mutate(rc_percent = round(n / sum(n) * 100))

# Max price for repair table
price_summary_tbl <- data %>% summarise(
  
  mean = mean(max_price_of_repair),
  std = sd(max_price_of_repair),
  min = min(max_price_of_repair),
  max = max(max_price_of_repair),
  count = n(),
)
price_summary_tbl %>% gt()

#Nested pie plot ETH REpair cafe


# Summarise by combination of the two variables
df_summary <- data %>%
  count(eth_rc, eth_rc_help) %>%
  mutate(
    eth_rc = factor(eth_rc, levels = c("Yes", "Maybe", "No")),
    eth_rc_help = factor(eth_rc_help, levels = c("Yes", "Maybe", "No"))
  )

library(ggplot2)
library(dplyr)

# Assume df_summary has: eth_rc, eth_rc_help, percent, n

df_summary <- df_summary %>%
  arrange(desc(eth_rc)) %>%
  mutate(
    cumulative = cumsum(percent),
    midpoint = cumulative - percent / 2,
    label = paste0(eth_rc, "\n(n = ", n, ")")  # Label shows eth_rc and count
  )

df_summary <- data %>% count(eth_rc_help) %>% mutate(rc_percent = round(n / sum(n) * 100))

# Add label position
df_summary <- df_summary %>%
  arrange(eth_rc, eth_rc_help) %>%
  mutate(
    percent = n / sum(n),
    cumulative = cumsum(percent),
    midpoint = cumulative - percent / 2,
    label = paste0("Volunteer: ", eth_rc_help, "\n(n = ", n, ")")
  )

data %>% summarize(eth_rc_help) 

ggplot(df_summary, aes(x = "", y = percent, fill = eth_rc)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(y = midpoint, label = label, angle = angle, hjust = hjust),
    size = 4
  ) +
  theme_void() +
  labs(
    title = "Repair Café Use & Volunteering",
    fill = "Would you use the Repair Café?"
  )


# Plot
ggplot(df_summary, aes(x = "", y = percent, fill = eth_rc)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0("Volunteering: ",eth_rc_help)), 
            position = position_stack(vjust = 0.5))+
  theme_void() + labs(fill="Would you use the ETH Repair cafe?")
            



# why you didn't repair
norepair_long <-data %>% select(rno_difficult:rno_other) %>% pivot_longer(everything(),
                                                                          names_to = "reason",
  
                                                                                                                                                  values_to = "value",values_drop_na = TRUE)
norepair_long %>%
  filter(value == TRUE) %>%
  ggplot(aes(x = reason)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Category", y = "Count of TRUE", title = "Count of TRUE per Category")

# Skill Summary table

#convert to long format
long_data <- data %>% 
  select(12:17) %>%                  # Keep only the 6 response columns
  pivot_longer(cols = everything(), # Turn them into two columns
               names_to = "variable",
               values_to = "value")
#summary table
summary_table <- long_data %>%
  group_by(variable) %>%
  summarise(
    count  = n(),
    mean   = mean(value, na.rm = TRUE),
    sd     = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min    = min(value, na.rm = TRUE),
    max    = max(value, na.rm = TRUE),
    .groups = 'drop'
  )
#rename category names
summary_table <- summary_table %>% mutate(variable=factor(variable,
                                         levels = c("skill_assembly","skill_electronics","skill_filigrane","skill_metal","skill_textile","skill_wood"),
                                         labels = c("Assembly","Electronics","Filigrane work","Metalworking","Textile","Woodworking")
))


summary_table %>% gt() %>% 
  tab_header(title = "Repair skills assesment in various categories",
             subtitle =  "Linear Scale from 1-5") %>% 
  fmt_number(columns = count:max, decimals = 2) %>% 
  cols_label(variable = "Skill category")


# WHY didn't you repair??
data %>% summarise(too_difficult = mean(rno_difficult,na.rm = TRUE),
                   time = mean(rno_time,na.rm = TRUE),
                   price = mean(rno_expensive,na.rm = TRUE),
                   motivation = mean(rno_motivation,na.rm = TRUE),
                   other = mean(rno_other,na.rm = TRUE)
                   )

# How much would you pay in percent of new price? 
data %>%
  summarise(mean = mean(max_price_of_repair),
            std = sd(max_price_of_repair),
            max = max(max_price_of_repair),
            min = min(max_price_of_repair))

#Discarded Items and repair considered:
discarded_items <- data %>% select(19:29) %>% pivot_longer(cols = 1:10, # Turn them into two columns
                                                           names_to = "object",
                                                           values_to = "amount")
discarded_items <- discarded_items %>% 
  mutate(object = factor(object,
                         levels = c("bi_clothes","bi_other_items","bi_shoes","bi_other_electronics",
                                    "bi_household_electronics","bi_furniture","bi_computer","bi_tablet",
                                    "bi_phone","bi_bike"),
                         labels = c("Clothes","Other Items","Shoes","Other Electronics","Household Electronics",
                                                                         "Furniture","Computer","Tablet","Phone","Bike")))
discarded_items %>%
  group_by(object,repair_considered) %>%
  summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(object, total), y = total,fill = repair_considered)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "# of discarded items",
       title = "Total discarded items that could have been repaired",
       fill = "Was repairing considered?")

#Likelihood of ETH Repaircafe usage:
data %>%
  summarise(mean = mean(eth_rc_usage_likelihood),
            std = sd(eth_rc_usage_likelihood))
  
ggplot(data = df_summary, aes(
  x = "",
  y = n,
  fill = eth_rc
)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(rc_percent, "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(fill = "ETH RC Usage")


# help other students and willingness to learn table

