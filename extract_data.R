# Load libraries ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(readxl)
library(writexl)
library(data.table)



# Load the data files -----------------------------------------------------

time <- read.csv("dunnhumby_data/time/time.csv")

transactions_data <- dir("dunnhumby_data/transactions", full.names = T) %>% map_df(read_csv)


# Joining time and transaction data
transactions_data_merged <- time %>%
  left_join(transactions_data, by = c("shop_week" = "SHOP_WEEK"))

transactions_data %>% distinct(CUST_CODE) %>% nrow()




# DATA PRE-PROCESSING -----------------------------------------------------

# Data types ----
transactions_data_merged %>% str()



# Review other columns ----
other_columns <- transactions_data_merged %>%
  select(BASKET_SIZE:STORE_REGION)

# Large, medium, small, NA
basket_size <- other_columns %>%
  select(BASKET_SIZE) %>%
  distinct()

# LA, MM, UM, XX, NA
basket_price_sensitivity <- other_columns %>%
  select(BASKET_PRICE_SENSITIVITY) %>%
  distinct()

# Full Shop, Top Up, Small Shop, XX, NA
basket_type <- other_columns %>%
  select(BASKET_TYPE) %>%
  distinct()

# See how basket size and type overlap
# Basket size seems to refer to the $ value of the items
basket_size_type <- other_columns %>%
  select(BASKET_SIZE, BASKET_TYPE) %>%
  distinct()

# Mixed, Fresh, Grocery, Nonfood, XX, NA
basket_dominant_mission <- other_columns %>%
  select(BASKET_DOMINANT_MISSION) %>%
  distinct()

# 761 store codes
store_code <- other_columns %>%
  select(STORE_CODE) %>%
  distinct()

# XLS, LS, MS, SS, NA
store_format <- other_columns %>%
  select(STORE_FORMAT) %>%
  distinct()

# 4 regions (N, S, E, W) with 3 stores in each (12 total) + NA
store_region <- other_columns %>%
  select(STORE_REGION) %>%
  distinct()



# Find columns with NAs ----
columns_with_nas <- transactions_data_merged %>%
  summarise_all(~ sum(is.na(.)))




# DATA CLEANUP ------------------------------------------------------------

# Make date columns as a date type ---
dates_clean_up <- transactions_data_merged %>%
  mutate(date_from = as.Date(as.character(date_from),format="%Y%m%d"),
         date_to = as.Date(as.character(date_to),format="%Y%m%d"),
         SHOP_DATE = as.Date(as.character(SHOP_DATE),format="%Y%m%d"),
         shop_week = tsibble::yearweek(SHOP_DATE, week_start = 1) - 8)


# Total spend calculation ---
total_spend <- dates_clean_up %>%
  mutate(TOTAL_SPEND = QUANTITY * SPEND)


# Remove all rows missing CUST_CODE ---
remove_missing_customer_codes_rows <- total_spend %>%
  filter(!is.na(CUST_CODE))


# Find columns with NAs ---
# Column Seg 2 has missing values, but not relevant for this analysis
columns_with_nas_after_removing_cust_codes <- remove_missing_customer_codes_rows %>%
  summarise_all(~ sum(is.na(.)))




# Final table -------------------------------------------------------------

# Selected relevant variables and additional variables that wouldn't change per purchase date to analyse further 
variables_selected <- remove_missing_customer_codes_rows %>%
  select(shop_week:SPEND, TOTAL_SPEND, CUST_CODE, BASKET_SIZE, BASKET_TYPE, BASKET_PRICE_SENSITIVITY,
         BASKET_DOMINANT_MISSION, STORE_FORMAT, STORE_REGION)


# Group data to find the total basket spend per customer by date and hour (in case they returned same day)
transactions_data_final <- variables_selected %>%
  group_by(CUST_CODE, SHOP_DATE, SHOP_HOUR) %>%
  summarise(TOTAL_BASKET_ITEMS = sum(QUANTITY),
            TOTAL_BASKET_SPEND = sum(TOTAL_SPEND)) %>%
  left_join(variables_selected, by = c("CUST_CODE", "SHOP_DATE", "SHOP_HOUR")) %>%
  slice(1) %>%
  ungroup() %>%
  select(shop_week, date_from, date_to, SHOP_DATE, SHOP_HOUR, CUST_CODE, TOTAL_BASKET_SPEND, 
         TOTAL_BASKET_ITEMS, BASKET_SIZE, BASKET_TYPE, BASKET_PRICE_SENSITIVITY, 
         BASKET_DOMINANT_MISSION, STORE_FORMAT, STORE_REGION)
  

# Find any duplicate mistakes
transactions_data_final %>% get_dupes(CUST_CODE, SHOP_DATE, SHOP_HOUR)




# Save final table to analyse ---------------------------------------------

saveRDS(transactions_data_final, "data/transactions_data_final.RDS")

