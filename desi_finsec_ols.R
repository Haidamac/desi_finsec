library(lmtest)     # For diagnostic tests
library(sandwich)   # For robust standard errors
library(car)        # For additional diagnostic tools
library(MASS)       # For robustbase and rlm
library(ggplot2)    # For visualization
library(broom)      # For neat presentation of results

# Read data from CSV files
desi_finsec_data <- read.csv("desi_finsec_output.csv")

#-----------------
# 1. Classical linear regression (OLS)
#-----------------
model_ols <- lm(financial_security_ratio ~ value, data = desi_finsec_data)

# Output of results
summary_ols <- summary(model_ols)
print(summary_ols)

# Model diagnostics
par(mfrow = c(2, 2))
plot(model_ols)  # Стандартні діагностичні графіки
par(mfrow = c(1, 1))

# Test for normality of residual distribution
shapiro_test <- shapiro.test(residuals(model_ols))
print(shapiro_test)

# Heteroscedasticity test
bp_test <- bptest(model_ols)
print(bp_test)

# Emissions testing and impact observations
influence_measures <- influence.measures(model_ols)
influential_obs <- which(influence_measures$is.inf[, "cook.d"] == TRUE)
if(length(influential_obs) > 0) {
  cat("Influential observations identified: ", influential_obs, "\n")
}

#-----------------
# 2. Robust linear regression with adjusted standard errors
#-----------------
# Using robust standard errors
coeftest_robust <- coeftest(model_ols, vcov = vcovHC(model_ols, type = "HC1"))
print(coeftest_robust)

#-----------------
# 3. Robust regression (M-estimation)
#-----------------
# Robust assessment to reduce the impact of emissions
model_robust <- rlm(financial_security_ratio ~ value, data = desi_finsec_data)
summary_robust <- summary(model_robust)
print(summary_robust)

#-----------------
# 4. Classical regression with additional control variables
#-----------------
if(all(c("GDP_per_capita", "year") %in% names(desi_finsec_data))) {
  model_ols_extended <- lm(financial_security_ratio ~ value + GDP_per_capita + factor(year), 
                           data = desi_finsec_data)
  summary_extended <- summary(model_ols_extended)
  print(summary_extended)
  
  # Model comparison using ANOVA
  anova_test <- anova(model_ols, model_ols_extended)
  print(anova_test)
}

#-----------------
# 5. Regression with a polynomial term to check for nonlinearity
#-----------------
model_poly <- lm(financial_security_ratio ~ value + I(value^2), data = desi_finsec_data)
summary_poly <- summary(model_poly)
print(summary_poly)

# Checking whether adding a quadratic term significantly improves the model
anova_poly <- anova(model_ols, model_poly)
print(anova_poly)

# If the quadratic term is significant, we visualize a nonlinear relationship
if(summary_poly$coefficients[3, 4] < 0.05) {
  # Creating a sequence of values for a predictor variable
  value_range <- seq(min(desi_finsec_data$value), max(desi_finsec_data$value), length.out = 100)
  
  # Predicting values ​​for a nonlinear model
  pred_data <- data.frame(value = value_range)
  pred_data$predicted <- predict(model_poly, newdata = pred_data)
  
  # Visualization
  ggplot() +
    geom_point(data = desi_finsec_data, aes(x = value, y = financial_security_ratio), alpha = 0.5) +
    geom_line(data = pred_data, aes(x = value, y = predicted), color = "blue", size = 1) +
    labs(title = "The non-linear relationship between digitalization and financial security",
         x = "DESI",
         y = "Financial security ratio") +
    theme_minimal()
}

#-----------------
# 6. Quantile regression (at different levels of the distribution of the dependent variable)
#-----------------
if(require(quantreg)) {
  # Quantile Regression at different quantiles (0.25, 0.5, 0.75)
  model_quant25 <- rq(financial_security_ratio ~ value, data = desi_finsec_data, tau = 0.25)
  model_quant50 <- rq(financial_security_ratio ~ value, data = desi_finsec_data, tau = 0.50)
  model_quant75 <- rq(financial_security_ratio ~ value, data = desi_finsec_data, tau = 0.75)
  
  # Output of results
  print("Quantile Regression (25-й percentile):")
  print(summary(model_quant25))
  print("Quantile Regression (median):")
  print(summary(model_quant50))
  print("Quantile Regression (75-й percentile):")
  print(summary(model_quant75))
  
  # Visualization of quantile regression results
  plot(summary(rq(financial_security_ratio ~ value, data = desi_finsec_data, 
                  tau = seq(0.1, 0.9, by = 0.1))))
}

#-----------------
# 7. Generalized linear models (GLM) - if there is a suspicion of a non-Gaussian distribution of the dependent variable
#-----------------
# Checking the distribution of the dependent variable
hist(desi_finsec_data$financial_security_ratio, breaks = 30, 
     main = "Distribution of the financial security indicator",
     xlab = "Value", col = "lightblue")

if(all(desi_finsec_data$financial_security_ratio > 0)) {
  model_glm_gamma <- glm(financial_security_ratio ~ value, 
                         data = desi_finsec_data, 
                         family = Gamma(link = "inverse"))
  summary_glm_gamma <- summary(model_glm_gamma)
  print(summary_glm_gamma)
}

#-----------------
# 8. Creating a summary table for comparing models
#-----------------
# Function for obtaining standardized coefficients
get_standardized_coef <- function(model) {
  sd_x <- sd(model$model[, 2])  
  sd_y <- sd(model$model[, 1])  
  coef <- coef(model)[2]        
  std_coef <- coef * (sd_x / sd_y)
  return(std_coef)
}

models_comparison <- data.frame(
  Model = c("OLS", "Robust Regression", "Polynomial", "Panel FE", "Panel RE", "Bayesian"),
  Coefficient = c(coef(model_ols)[2], 
                  coef(model_robust)[2], 
                  coef(model_poly)[2],
                  -0.010009,  # coeff. FE model
                  -0.028344,  # coeff. RE model
                  -0.03),     # coeff. Bayesian model
  Std_Error = c(summary(model_ols)$coefficients[2, 2],
                summary(model_robust)$coefficients[2, 2],
                summary(model_poly)$coefficients[2, 2],
                0.012730,    # St.Err. FE model
                0.009189,    # St.Err. RE model
                0.01),       # St.Err. Bayesian model
  p_value = c(summary(model_ols)$coefficients[2, 4],
              NA,  
              summary(model_poly)$coefficients[2, 4],
              0.43275,      # p-value FE model
              0.002038,     # p-value RE model
              NA),          
  R_squared = c(summary(model_ols)$r.squared,
                NA,  
                summary(model_poly)$r.squared,
                0.0033122,   # R² FE model
                0.042954,    # R² RE model
                NA)          
)

print(models_comparison)

write.csv(models_comparison, "models_comparison.csv", row.names = FALSE)

#-----------------
# 9. Graphical comparison of models
#-----------------
pred_ols <- predict(model_ols, newdata = desi_finsec_data)
pred_robust <- predict(model_robust, newdata = desi_finsec_data)
pred_poly <- predict(model_poly, newdata = desi_finsec_data)

ggplot(desi_finsec_data, aes(x = value, y = financial_security_ratio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", formula = y ~ x) +
  geom_line(aes(y = pred_robust), color = "red") +
  geom_line(aes(y = pred_poly), color = "green") +
  labs(x = "DESI",
       y = "Financial Security Ratio",
       caption = "Blue line: OLS, Red line: Robust regression, Green line: Polynomial regression") 
# + theme_minimal()