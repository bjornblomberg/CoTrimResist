# Manuscript - consequtive analyses in the text
library(tidyverse)

# Section on Participants and follow-up

# 50% compliance
table(rt$compliance50_1)
table(!is.na(rt$compliance50_1))
proportions(table(rt$compliance50_1))

table(rt$compliance50_12)
table(!is.na(rt$compliance50_2))
proportions(table(rt$compliance50_12))
393/442

table(rt$compliance50_123)
table(!is.na(rt$compliance50_3))
proportions(table(rt$compliance50_123))
333/398
# 75% compliance
table(rt$compliance75_1)
table(!is.na(rt$compliance75_1))
proportions(table(rt$compliance75_1))
470/513

table(rt$compliance75_12)
table(!is.na(rt$compliance75_2))
proportions(table(rt$compliance75_2))
327/442

table(rt$compliance75_123)
table(!is.na(rt$compliance75_3))
proportions(table(rt$compliance75_123))

# ESBL-E carriage at baseline

table(rt$antibiotics4w)
rt |> 
  filter(!is.na(v0esbl)) |> 
  group_by(antibiotics4w, v0esbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(antibiotics4w) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0esbl)) |> 
  group_by(hosp1y, v0esbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(hosp1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0esbl)) |> 
  group_by(opd1y, v0esbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(opd1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

model <- glm(v0esbl ~ antibiotics4w, data = rt, family = binomial (link = "log"))
model <- glm(v0esbl ~ hosp1y, data = rt, family = binomial (link = "log"))
model <- glm(v0esbl ~ opd1y, data = rt, family = binomial (link = "log"))

# S. aureus carriage at baseline

rt |> 
  filter(!is.na(v0staph)) |> 
  group_by(antibiotics4w, v0staph)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(antibiotics4w) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0staph)) |> 
  group_by(hosp1y, v0staph)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(hosp1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0staph)) |> 
  group_by(opd1y, v0staph)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(opd1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

model <- glm(v0staph ~ antibiotics4w, data = rt, family = binomial (link = "log"))
model <- glm(v0staph ~ hosp1y, data = rt, family = binomial (link = "log"))
model <- glm(v0staph ~ opd1y, data = rt, family = binomial (link = "log"))

# MRSA carriage at baseline

rt |> 
  filter(!is.na(v0mrsa)) |> 
  group_by(antibiotics4w, v0mrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(antibiotics4w) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0mrsa)) |> 
  group_by(hosp1y, v0mrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(hosp1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(v0mrsa)) |> 
  group_by(opd1y, v0mrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(opd1y) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

model <- glm(v0mrsa ~ antibiotics4w, data = rt, family = binomial (link = "log"))
model <- glm(v0mrsa ~ hosp1y, data = rt, family = binomial (link = "log"))
model <- glm(v0mrsa ~ opd1y, data = rt, family = binomial (link = "log"))

# Explorative analyses
# Newly acquired ESBL during the study period 
rt$newesbl <- NA
rt$newesbl[rt$v0esbl == 0] <- 0
rt$newesbl[rt$v1esbl == 0] <- 0
rt$newesbl[rt$v2esbl == 0] <- 0
rt$newesbl[rt$v3esbl == 0] <- 0
rt$newesbl[rt$v1esbl == 1] <- 1
rt$newesbl[rt$v2esbl == 1] <- 1
rt$newesbl[rt$v3esbl == 1] <- 1
rt$newesbl[rt$v0esbl == 1] <- NA

table(rt$newesbl, useNA = "ifany")
table(rt$newesbl, rt$v0esbl, useNA = "ifany")

rt |> 
  filter(!is.na(newesbl)) |> 
  group_by(abstudy, newesbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(abstudy) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(newesbl)) |> 
  group_by(hosp, newesbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(hosp) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(newesbl)) |> 
  group_by(opdvisit, newesbl)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(opdvisit) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

model <- glm(newesbl ~ abstudy, data = rt, family = binomial (link = "log"))
model <- glm(newesbl ~ hosp, data = rt, family = binomial (link = "log"))
model <- glm(newesbl ~ opdvisit, data = rt, family = binomial (link = "log"))

# Newly acquired MRSA during the study period 
rt$newmrsa <- NA
rt$newmrsa[rt$v0mrsa == 0] <- 0
rt$newmrsa[rt$v1mrsa == 0] <- 0
rt$newmrsa[rt$v2mrsa == 0] <- 0
rt$newmrsa[rt$v3mrsa == 0] <- 0
rt$newmrsa[rt$v1mrsa == 1] <- 1
rt$newmrsa[rt$v2mrsa == 1] <- 1
rt$newmrsa[rt$v3mrsa == 1] <- 1
rt$newmrsa[rt$v0mrsa == 1] <- NA

table(rt$newmrsa, useNA = "ifany")
table(rt$newmrsa, rt$v0mrsa, useNA = "ifany")

rt |> 
  filter(!is.na(newmrsa)) |> 
  group_by(abstudy, newmrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(abstudy) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(newmrsa)) |> 
  group_by(hosp, newmrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(hosp) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

rt |> 
  filter(!is.na(newmrsa)) |> 
  group_by(opdvisit, newmrsa)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(opdvisit) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

model <- glm(newmrsa ~ abstudy, data = rt, family = binomial (link = "log"))
model <- glm(newmrsa ~ hosp, data = rt, family = binomial (link = "log"))
model <- glm(newmrsa ~ opdvisit, data = rt, family = binomial (link = "log"))

# Get output from model
summary(model)

coef_est <- coef(model) # Extract coefficient estimates
se <- summary(model)$coefficients[, "Std. Error"] # Extract standard errors
rr <- exp(coef_est) # Calculate relative risks
ci_low <- exp(coef_est - 1.96 * se) # Calculate confidence intervals for relative risks
ci_high <- exp(coef_est + 1.96 * se)
print(data.frame(mean = rr, lower = ci_low, upper = ci_high)) # Create a data frame to store results
