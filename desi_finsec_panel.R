library(ggplot2)
library(plm)
library(lmtest)
library(carData)

# Read data from CSV files
desi_finsec_data <- read.csv("desi_finsec_output.csv")

# Data preparation for panel analysis
pdata <- pdata.frame(desi_finsec_data, index = c("geo", "TIME_PERIOD"))

# Fixed effects model
model_fe <- plm(financial_security_ratio ~ value, data = pdata, model = "within")

# Random effects model
model_re <- plm(financial_security_ratio ~ value, data = pdata, model = "random")

# Test for the presence of individual effects (Pooling vs. Panel)
pooltest <- plmtest(model_re, type = "bp")
print(pooltest)

# Summary of results
summary(model_fe)
summary(model_re)

# Hausman test
phtest(model_fe, model_re)

# Test for twoways effects
model_twoways <- plm(financial_security_ratio ~ value, data = pdata, 
                     model = "within", effect = "twoways")
pFtest(model_twoways, model_fe)

# Test for time-fixed effects
model_individual <- plm(financial_security_ratio ~ value, data = pdata, model = "within", effect = "individual")
model_two_way <- plm(financial_security_ratio ~ value, data = pdata, model = "within", effect = "twoways")
pFtest(model_two_way, model_individual)

# Test for cross-sectional dependence
pcdtest(model_re, test = "cd")

# Driscoll-Kraay standard errors
summary(model_re, vcov = vcovDC)

# Serial correlation test
pbgtest(model_re)

# Test for heteroskedasticity
bptest(financial_security_ratio ~ value, data = pdata, studentize = TRUE)

# Normality test for residuals
shapiro.test(residuals(model_re))

# Additional model diagnostics
# VIF test not needed here as we only have one predictor
# But would be important with multiple predictors
# vif(model_re)
# If heteroskedasticity is present, use robust standard errors
coeftest(model_re, vcov = vcovHC(model_re, type = "HC0"))

# Graphic representation
ggplot(pdata, aes(x = TIME_PERIOD, y = financial_security_ratio, group = geo, color = geo)) +
  geom_line() +
  labs(title = "Temporal dynamics of finsec per capita",
       x = "Year",
       y = "finsec per capita") +
  theme_minimal() +
  theme(legend.position = "none")
