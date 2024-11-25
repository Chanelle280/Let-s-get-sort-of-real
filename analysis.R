
# Load libraries ----------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(readxl)
library(writexl)
library(data.table)
library(plotly)



# Load the data file  ------------------------------------------------------

transactions_data_final <- readRDS("data/transactions_data_final.RDS")

transactions_data_final %>% distinct(CUST_CODE) %>% nrow()




# Step 1 ------------------------------------------------------------------
# Calculate the consistency of each customer

# Calculate the time difference between purchase dates
# Add a column to find the number of times a customer shopped
customer_purchase_dates <- transactions_data_final %>%
  group_by(CUST_CODE) %>%
  arrange(SHOP_DATE) %>%
  mutate(ROW_NUMBER = row_number(),
         DAYS_BETWEEN_PURCHASES = as.numeric(difftime(SHOP_DATE,lag(SHOP_DATE), units='days')),
         WEEKS_BETWEEN_PURCHASES = as.numeric(difftime(SHOP_DATE,lag(SHOP_DATE), units='weeks'))) %>%
  ungroup() 

customer_purchase_dates %>% distinct(CUST_CODE) %>% nrow()


# Plot the distribution of the time differences
# The data is not normally distributed
distribution_of_days_between <- customer_purchase_dates %>% 
  ggplot(aes(x = DAYS_BETWEEN_PURCHASES)) +
  geom_histogram(binwidth = 1, boundary = 0, fill = "#2F8979") +
  scale_x_continuous(breaks = seq(0, 200, 7), 
                     expand = c(0, 0), 
                     limits = c(0, 200)) +
  scale_y_continuous(label = scales::comma) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_minimal() +
  labs(x = "Number of days between purchases",
       y = "Count of transactions")

ggplotly(distribution_of_days_between) %>%
  config(displayModeBar = F) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  

# Calculate the interquartile range per customer ---
# Include customers with at least 6 transactions because there won't be enough data
customer_consistency <- customer_purchase_dates %>%
  group_by(CUST_CODE) %>%
  filter(any(ROW_NUMBER >= 6)) %>%
  summarise(IQR_DAY = IQR(DAYS_BETWEEN_PURCHASES, na.rm = TRUE),
            IQR_WEEK = IQR(WEEKS_BETWEEN_PURCHASES, na.rm = TRUE)) %>%
  ungroup()

customer_consistency %>% 
  ggplot(aes(x = IQR_DAY)) +
  geom_boxplot(outlier.colour = "#FF0000",
               outlier.alpha = 0.1,
               staplewidth = 0.1) +
  theme_minimal() +
  labs(x = "IQR Days") 


# Find the quartiles ---
# I've decided that anyone who shops more than 8 days after purchasing is inconsistent
quantile(customer_purchase_dates$DAYS_BETWEEN_PURCHASES, probs = c(0.25, 0.5, 0.75, 0.9, 0.95, 1), na.rm = TRUE)


customer_consistency_final <- customer_consistency %>%
  mutate(consistency = ifelse(IQR_DAY > 24, "Inconsistent", "Consistent"))
  



# Step 2 ------------------------------------------------------------------
# Calculate the value of each customer

# Individual Customer lifetime value ---
# https://www.qualtrics.com/en-gb/experience-management/customer/calculate-clv/#:~:text=The%20formula%20for%20calculating%20CLV,them%20to%20stay%20with%20you.

customer_lifetime_value <- customer_purchase_dates %>%
  filter(date_from >= "2008-01-01") %>%
  group_by(CUST_CODE) %>%
  summarise(total_revenue_2008 = sum(TOTAL_BASKET_SPEND)) %>%
  mutate(lifetime = 5,
         cost_to_serve_per_year = 50,
         lifetime_value = (total_revenue_2008 * lifetime) - (lifetime * cost_to_serve_per_year)) 




# Step 3 ------------------------------------------------------------------
# Determine the target customer segment

# CLV ---
customer_segment_by_clv <- customer_consistency_final %>%
  left_join(customer_lifetime_value, by = "CUST_CODE") %>%
  filter(!is.na(lifetime_value)) %>%
  mutate(`Consistency final` = case_when(
    IQR_DAY > 24 & lifetime_value > 0 ~ "Inconsistent (IQR > 24 days)", 
    IQR_DAY > 24 & lifetime_value <= 0 ~ "Inconsistent & negative LTV",
    IQR_DAY <= 24 & lifetime_value > 0 ~ "Consistent (IQR <= 24 days)", 
    IQR_DAY <= 24 & lifetime_value <= 0 ~ "Consistent & negative LTV",
    TRUE ~ "OTHER")) 

customer_segment_by_clv %>% count(`Consistency final`) %>% adorn_totals()

customer_segment <- customer_segment_by_clv %>%
  ggplot(aes(x = IQR_DAY, 
             y = lifetime_value,
             color = `Consistency final`)) +
  geom_point(alpha = 0.2) +
  labs(y = "Customer lifetime value",
       x = "IQR days to visit") +
  scale_y_continuous(label = scales::comma) +
  theme_minimal()

ggplotly(customer_segment) %>%
  config(displayModeBar = F) %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))




# Step 4 ------------------------------------------------------------------
# Calculate ROI

set.seed(88)

# Select targeted customers
targeted_customers <- customer_segment_by_clv %>%
  filter(`Consistency final` == "Inconsistent (IQR > 24 days)") 

# Find the average spend for target customers and the incremental spend
setup_for_roi <- customer_purchase_dates %>%
  filter(CUST_CODE %in% targeted_customers$CUST_CODE,
         date_from >= "2008-01-01") %>%
  group_by(CUST_CODE) %>%
  summarise(average_basket_spend = mean(TOTAL_BASKET_SPEND)) %>%
  mutate(incremental_spend = 1.05 * average_basket_spend,
         cost = 6) 

# Select 10% randomly
random_selection <- setup_for_roi %>%
  sample_frac(0.1)
  
# ROI calculation
roi <- random_selection %>%
  summarise(total_selected = n(),
            total_revenue = sum(incremental_spend), 
            total_cost = sum(cost) + (nrow(setup_for_roi) - total_selected),
            roi = total_revenue/total_cost)


# ROI calculation
roi_alt <- random_selection %>%
  summarise(total_selected = n(),
            total_revenue = sum(incremental_spend), 
            total_cost = sum(cost) - (nrow(random_selection)),
            roi = total_revenue/total_cost)
