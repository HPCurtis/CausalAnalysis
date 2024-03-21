library(tidyr)
library(dplyr)
library(ggplot2)
library(brms)
library(mgcv)

options(mc.cores = parallel::detectCores())
url = "https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/ak91.csv"

data <- as_tibble(read.csv(url))
group_data <- data %>%
  group_by(year_of_birth, quarter_of_birth) %>%
  summarize(log_wage = mean(log_wage),
            years_of_schooling = mean(years_of_schooling)) %>%
  ungroup() %>%
  mutate(time_of_birth = year_of_birth + (quarter_of_birth - 1) / 4)

ggplot(group_data, aes(x = time_of_birth, y = years_of_schooling)) +
  geom_line(color = "blue", size = 1, alpha = 0.5) +  # Add a line plot
  geom_point(aes(color = factor(quarter_of_birth))) +  # Add scatterplot with different shapes and colors
  geom_text(aes(label = quarter_of_birth), size = 4, vjust = -0.5) +  # Add numbers to each point
  scale_color_manual(values = c("red", "green", "blue", "purple")) +  # Specify colors
  labs(title = "Years of Education by Quarter of Birth (first stage)",
       x = "Year of Birth",
       y = "Years of Schooling") +
  theme_minimal() +
  theme(legend.position = "none")  +
  scale_x_continuous(breaks = seq(min(group_data$time_of_birth), 40, by = 2)) 


factor_data <- data %>%
  mutate(q1 = as.integer(quarter_of_birth == 1),
         q2 = as.integer(quarter_of_birth == 2),
         q3 = as.integer(quarter_of_birth == 3),
         q4 = as.integer(quarter_of_birth == 4))

# Fit OLS regression model in R
first_stage <- lm(years_of_schooling ~ as.factor(year_of_birth) + as.factor(state_of_birth) + q4, data = factor_data)
mod_sum <- summary(first_stage)
# Extract p-values from the summary
p_values <- mod_sum$coefficients[, "Pr(>|t|)"][-1]


cat("q4 parameter estimate:, ", first_stage$coefficients["q4"])
cat("q4 p-value:, ",  p_values[length(p_values)])


ggplot(group_data, aes(x = time_of_birth, y = log_wage)) +
  geom_line(color = "blue", size = 1, alpha = 0.5) +  # Add a line plot
  geom_point(aes(color = factor(quarter_of_birth))) + 
  geom_text(aes(label = quarter_of_birth), size = 4, vjust = -0.5) +  # Add numbers to each point
  scale_color_manual(values = c("red", "green", "blue", "purple")) +  # Set shape of points
  labs(title = "Average Weekly Wage by Quarter of Birth (reduced form)",
       x = "Year of Birth",
       y = "Log Weekly Earnings") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(min(group_data$time_of_birth), 40, by = 2)) 


# Fit the linear regression model
reduced_form <- lm(log_wage ~ as.factor(year_of_birth) + as.factor(state_of_birth) + q4, data = factor_data)

# Get the coefficient estimate for q4
q4_coef <- coef(reduced_form)["q4"]

# Get the p-value for q4
q4_pvalue <- summary(reduced_form)$coefficients["q4", "Pr(>|t|)"]

# Print the results
cat("q4 parameter estimate:", q4_coef, "\n")
cat("q4 p-value:", q4_pvalue, "\n")  

coef(reduced_form)["q4"] / first_stage$coefficients["q4"]


fit <- brm(log_wage ~ as.factor(year_of_birth) + as.factor(state_of_birth) + q4,
    data = factor_data, chains = 2, cores = 2)

lin <-lm(log_wage ~ year_of_birth, data = group_data)
spline <- gam(log_wage ~ s(year_of_birth), data = group_data)
AIC(lin, spline)


summary(gam(log_wage ~ s(time_of_birth), data = group_data))
