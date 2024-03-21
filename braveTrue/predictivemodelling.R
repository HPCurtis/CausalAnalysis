library(dplyr)
library(tidyr)
library(tibble)
library(brms)
library(ggplot2)
library(tidymodels)
library(rsample)



# Load and clean the data
transactions <- as_tibble(read.csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/customer_transactions.csv"))

# Calculate net value for each customer
profitable <- transactions %>%
  select(customer_id) %>%
  mutate(net_value = rowSums(transactions[, -1]))  # Drop the customer_id column and sum the rests

customer_features <- as.tibble(read.csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/customer_features.csv")) %>%
  left_join(profitable, by = "customer_id")

set.seed(123)  # Set seed for reproducibility

# Assuming 'train' is your data frame containing 'income' and 'net_value' columns
# Create income quantiles using quantile breaks
customer_features <- customer_features %>%
  mutate(income_quantile = cut(income, breaks = quantile(income, probs = seq(0, 1, by = 0.05)),include.lowest = TRUE ) )

set.seed(13)

# Split the data into training and testing sets
split <- initial_split(customer_features, prop = 0.7, strata = NULL)

# Extract the training and testing sets
train <- training(split)
test <- testing(split)

# Simple options 
mean(train)

quantile_avg <- train %>% group_by(income_quantile) %>%
  summarise(avg_net_value = mean(net_value, na.rm = FALSE)) 
  
  
  ggplot(quantile_avg, aes(x = income_quantile, y = avg_net_value))  +
  geom_bar(stat = "identity", aes(fill = income_quantile)) +
    theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1)) +
    theme(legend.position = "none") 
