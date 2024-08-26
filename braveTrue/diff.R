# Load library.
library(brms)
library(rstan)
library(loo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidybayes)
library(broom)

# Parallelise cores
options(mc.cores=parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load data
# from https://matheusfacure.github.io/python-causality-handbook/13-Difference-in-Differences.html
data <- read.csv("https://raw.githubusercontent.com/matheusfacure/python-causality-handbook/master/causal-inference-for-the-brave-and-true/data/billboard_impact.csv")

# just for memory.
data["JorM"] <- ifelse(data$jul == 1, "July", "May")
data["PoaorFL"] <- ifelse(data$jul == 1, "FL", "POA")


# Plot data 

# iaotgram for poa_before
ggplot() +
  geom_histogram( aes(x =data[data$poa == 1 & data$jul == 0, "deposits"])) +
  xlab("Porto Alegre before treament deposits")

ggplot() +
  geom_histogram( aes(x =data[data$poa == 1 & data$jul == 1, "deposits"])) +
  xlab("Porto Alegre after treament deposits")

ggplot() +
  geom_histogram( aes(x =data[data$poa == 0 & data$jul == 0, "deposits"])) +
  xlab("Florianopolis before treament deposits")

ggplot() +
  geom_histogram( aes(x =data[data$poa == 0 & data$jul == 1, "deposits"])) +
  xlab("Florianopolis, after treament deposits")
  

# Calculate mean deposits for poa=1 and jul=0
poa_before <- mean(data[data$poa == 1 & data$jul == 0, "deposits"])

# Calculate mean deposits for poa=1 and jul=1
poa_after <- mean(data[data$poa == 1 & data$jul == 1, "deposits"])

# Calculate the difference in means
difference <- poa_after - poa_before

# Calculate mean deposits for poa=0 and jul=1
fl_after <- mean(data[data$poa == 0 & data$jul == 1, "deposits"])

poa_after - fl_after

fl_before <- mean(data[data$poa == 0 & data$jul == 0, "deposits"])
diff_in_diff <- (poa_after-poa_before)-(fl_after-fl_before)
diff_in_diff

# Bayesian models
fit <- brm(deposits ~ poa*jul, data=data, chain  = 2, core = 2, warmup = 2000,
           iter = 20000,
           control = list(adapt_delta = 0.9),
           save_pars = save_pars(all = TRUE))

fit <- add_criterion(fit, c("loo"))

# Plot MCMC Chains and posteriors
plot(fit)


# Fit the hurdle models.
fit_lnhurdle <- brm(deposits ~ poa*jul, family =  hurdle_lognormal(), data = data, chain = 2, core = 2, warmup = 2000,
                    iter = 20000, save_pars = save_pars(all = TRUE) )

fit_lnhurdle <- add_criterion(fit_lnhurdle, c("loo"))
plot(fit_lnhurdle)

# hurdle model whilst modelling the zeros values.
fit_lnhurdlehu <- brm(bf(deposits ~ poa * jul, hu ~ poa * jul),
                          family = hurdle_lognormal(),
                          data = data,
                          chains = 2,
                          cores = 2,
                          iter = 20000,
                          control = list(adapt_delta = 0.9),
                          save_pars = save_pars(all = TRUE))

# Call once model above has been fit
library(bridgesampling)

fit_lnhurdlehu <- add_criterion(fit_lnhurdlehu, c("loo"))
plot(fit_lnhurdlehu)

# summary result outputs of coefficients
summary(fit)
summary(fit_lnhurdle)
summary(fit_lnhurdlehu)

hurdle_diff_diff <- tidy_draws(fit_lnhurdle)$`b_poa:jul`
# return diff_diff on origanl scale 
mean(exp(hurdle_diff_diff))

hurdlehu_intercept <- tidy_draws(fit_lnhurdlehu)$b_hu_Intercept
hurdlehu_diff_diff <-  tidy_draws(fit_lnhurdlehu)$`b_hu_poa:jul`

mean(exp(hurdlehu_diff_diff))

# Calculate percentage from logit scale
# Is the incremental impact when you go from May to July and from Florianopolis to POA
# and this decreases the probability of seeing a 0 in deposits by 27%
# percentage points, on average
mean(plogis(hurdlehu_intercept + hurdlehu_diff_diff) - plogis(hurdlehu_diff_diff))


# Posterior predictive checks.
pp_check(fit)
pp_check(fit_lnhurdle)
pp_check(fit_lnhurdlehu)

## PP check for mean
pp_check(fit,
         type = "stat",
         ndraws = 1000,
         newdata = data,
         stat = "mean")

pp_check(fit_lnhurdle ,
         type = "stat",
         ndraws = 1000,
         newdata = data,
         stat = "mean")

pp_check(fit_lnhurdlehu ,
         type = "stat",
         ndraws = 1000,
         newdata = data,
         stat = "mean")

# Compare model remvmbe this not useful to causal inquisition but does 
# show the point that normal model is poor for the data analysed.
loo_compare(fit, fit_lnhurdlehu)

margLogLik_n <- bridge_sampler(fit, silent = TRUE)

margLogLik_ln_hu <- bridge_sampler(fit_lnhurdlehu, silent = TRUE)
bayes_factor(margLogLik_n, margLogLik_ln_hu)









# --- ueefult future code not relevant to current analysis.
hurdle_gaussian <- 
  # Create a custom family that is logit if y = 0, normal/gaussian if not
  custom_family("hurdle_gaussian", 
                dpars = c("mu", "sigma", "hu"),
                links = c("identity", "log", "logit"),
                lb = c(NA, 0, NA),
                type = "real")


# Stan code
stan_funs <- "
  real hurdle_gaussian_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_lpmf(1 | hu); 
    } else { 
      return bernoulli_lpmf(0 | hu) +  
             normal_lpdf(y | mu, sigma); 
    } 
  }
"

# Prepare Stan code for use in brm()
stanvars <- stanvar(scode = stan_funs, block = "functions")

fitnhurdlen <-  brm(deposits ~ poa*jul, family = hurdle_gaussian,
    stanvars = stanvars, data=data, chain = 2, core = 2)

posterior_predict_hurdle_gaussian <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  sigma <- brms::get_dpar(prep, "sigma", i = i)
  theta <- brms::get_dpar(prep, "hu", i = i)
  
  hu <- runif(prep$ndraws, 0, 1)
  ifelse(hu < theta, 0, rnorm(prep$ndraws, mu,sigma))
}

pp_check(fit_lnhurdle)






# Generate code to run stan model with log_likelihood
hurdle_n_sm <- '
functions {
  real hurdle_gaussian_lpdf(real y, real mu, real sigma, real hu) { 
    if (y == 0) { 
      return bernoulli_lpmf(1 | hu); 
    } else { 
      return bernoulli_lpmf(0 | hu) +  
             normal_lpdf(y | mu, sigma); 
    } 
  }

}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}
parameters {
  vector[Kc] b;  // regression coefficients
  real Intercept;  // temporary intercept for centered predictors
  real<lower=0> sigma;  // dispersion parameter
  real hu;  // hurdle probability
}
transformed parameters {
  real lprior = 0;  // prior contributions to the log posterior
  lprior += student_t_lpdf(Intercept | 3, 165.5, 109);
  lprior += student_t_lpdf(sigma | 3, 0, 109)
    - 1 * student_t_lccdf(0 | 3, 0, 109);
  lprior += beta_lpdf(hu | 1, 1);
  
  // initialize linear predictor term
    vector[N] mu = rep_vector(0.0, N);
    mu += Intercept + Xc * b;
}
model {
  // likelihood including constants
  if (!prior_only) {
    
    for (n in 1:N) {
      target += hurdle_gaussian_lpdf(Y[n] | mu[n], sigma, hu);
    }
  }
  // priors including constants
  target += lprior;
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept - dot_product(means_X, b);
  
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = hurdle_gaussian_lpdf(Y[n] | mu[n], sigma, hu);
  }
}
'
X <- model.matrix(~  poa*jul, data)
stan_data <- list(Y = data$deposits, X = X, N = nrow(data), K = ncol(X), Kc = ncol(X) -1, prior_only = 0)
model <- stan_model(model_code = hurdle_n_sm)

fit_nhurdle <- sampling(model, data = stan_data, chain = 1, core = 2)
summary(fit_nhurdle)

x <- extract(fit_nhurdle)
mean(x$Intercept)


log_lik_1 <- extract_log_lik(fit_nhurdle, merge_chains = FALSE)
r_eff <- relative_eff(exp(log_lik_1), cores = 2)

# preferably use more than 2 cores (as many cores as possible)
# will use value of 'mc.cores' option if cores is not specified
loo_hn <- loo(log_lik_1, r_eff = r_eff, cores = 4)
loo_n <- loo(fit)
loo_hln <- loo(fit_lnhurdle)

loo_compare(loo_n, loo_hln, loo_hn)
loo
