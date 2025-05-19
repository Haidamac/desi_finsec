library(brms)
library(ggplot2)
library(bayesplot)
library(ggdist)
library(bridgesampling)

# Read data from CSV files
desi_finsec_data <- read.csv("desi_finsec_output.csv")

# Bayesian panel model (with random intercept by country)
bayes_panel_model <- brm(
  financial_security_ratio ~ value + (1 | geo),   
  data = desi_finsec_data,
  family = gaussian(),
  prior = c(
    prior(normal(0, 5), class = "b"),         
    prior(normal(0, 5), class = "Intercept"),
    prior(exponential(1), class = "sd")       
  ),
  chains = 4, iter = 4000, warmup = 2000, seed = 123,
  control = list(adapt_delta = 0.95),
  save_pars = save_pars(all = TRUE)
)

summary(bayes_panel_model)

# Model diagnostics
plot(bayes_panel_model)
