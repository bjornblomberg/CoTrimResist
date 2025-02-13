# Crude risk ratios and 95%CI 

# Binomial regression with log-link
# Data used in Figure 2 - data to be added in /r_data/forest.xlsx

rt |>
  filter(!is.na(v0esbl)) |> 
  group_by(treat, v0esbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 


# Crude relative risks inn ITT analysis (complete cases)
model <- glm(v0esbl ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v1esbl ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v2esbl ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v3esbl ~ tx, data = rt, family = binomial (link = "log"))

model <- glm(v0mrsa ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v1mrsa ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v2mrsa ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v3mrsa ~ tx, data = rt, family = binomial (link = "log"))

model <- glm(v0pnsp ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v1pnsp ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v2pnsp ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v3pnsp ~ tx, data = rt, family = binomial (link = "log"))

# Crude relative risks in PPA analysis 
model <- glm(v0esbl ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v1esbl ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v2esbl ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v3esbl ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))

model <- glm(v0mrsa ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v1mrsa ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v2mrsa ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v3mrsa ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))

model <- glm(v0pnsp ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v1pnsp ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v2pnsp ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))
model <- glm(v3pnsp ~ tx, data = subset(rt, compliance75_123 == 1), family = binomial (link = "log"))

# ITT - carriage of staphylococci and pneumococci overall
model <- glm(v0staph ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v1staph ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v2staph ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v3staph ~ tx, data = rt, family = binomial (link = "log"))

model <- glm(v0spn ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v1spn ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v2spn ~ tx, data = rt, family = binomial (link = "log"))
model <- glm(v3spn ~ tx, data = rt, family = binomial (link = "log"))

# Crude relative risks inn Sensitivity analysis n=193 without recent exposure to healthcare and antibiotics
model <- glm(v0esbl ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v1esbl ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v2esbl ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v3esbl ~ tx, data = sens, family = binomial (link = "log"))

model <- glm(v0mrsa ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v1mrsa ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v2mrsa ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v3mrsa ~ tx, data = sens, family = binomial (link = "log"))

model <- glm(v0pnsp ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v1pnsp ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v2pnsp ~ tx, data = sens, family = binomial (link = "log"))
model <- glm(v3pnsp ~ tx, data = sens, family = binomial (link = "log"))

# Get output from model
summary(model)

coef_est <- coef(model) # Extract coefficient estimates
se <- summary(model)$coefficients[, "Std. Error"] # Extract standard errors
rr <- exp(coef_est) # Calculate relative risks
ci_low <- exp(coef_est - 1.96 * se) # Calculate confidence intervals for relative risks
ci_high <- exp(coef_est + 1.96 * se)
print(data.frame(mean = rr, lower = ci_low, upper = ci_high)) # Create a data frame to store results
