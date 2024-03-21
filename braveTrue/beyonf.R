library(dplyr)
library(tidyr)
library(tibble)
library(brms)
library(ggplot2)
library(tidymodels)
library(rsample)
library(tidybayes)
rstan::rstan_options(autowrite=TRUE)

data <- as.tibble(read.csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/collections_email.csv"))

data$email_jittered <- jitter(data$email, amount = 0.01)

# Create scatterplot using ggplot2
ggplot(data, aes(x = email_jittered, y = payments)) +
  geom_point(color = "blue",alpha = 0.3) +
  labs(x = "Email", y = "Payments") +
  ggtitle("Scatterplot of Email vs Payments")

fit  <- brm(data = data, payments ~ email, chains = 2, cores = 2)
fit2 <- brm(data = data, payments ~ email + credit_limit + risk_score, chains = 2, cores = 2)

summary(fit)
summary(fit2)

fit_resid <- residuals(fit, summary = TRUE)
fit2_resid <- residuals(fit2, summary = TRUE)

data %>% add_residual_draws(fit) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()
