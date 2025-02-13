# T4 - safety
library(tidyverse)
library(epitools)

# Relative risks
model <- glm(treat ~ death, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ hosp_combi, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ hosp_death, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ opdvisit, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ sae, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ malaria, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ pneumonia, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ meningitis, data = rt, family = binomial (link = "log"))

# Get output from model
coef_est <- coef(model) # Extract coefficient estimates
se <- summary(model)$coefficients[, "Std. Error"] # Extract standard errors
rr <- exp(coef_est) # Calculate relative risks
ci_low <- exp(coef_est - 1.96 * se) # Calculate confidence intervals for relative risks
ci_high <- exp(coef_est + 1.96 * se)
print(data.frame(mean = rr, lower = ci_low, upper = ci_high)) # Create a data frame to store results

# Death
rt |>
  group_by(treat, death)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Hospitalization
rt |>
  group_by(treat, hosp_combi)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 


# Composite outcome - death and/or hospitalization
rt |>
  group_by(treat, hosp_death)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# OPD visit
rt |>
  group_by(treat, opdvisit)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Adverse events
rt |>
  group_by(treat, sae)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Malaria
rt |>
  group_by(treat, malaria)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Pneumonia
rt |>
  group_by(treat, pneumonia)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Meningitis
rt |>
  group_by(treat, meningitis)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Old -----------------------------------------------
# Deaths
rt$death <- rt$Death
rt$death[is.na(rt$Death)] <- 0

table(rt$treat, !is.na(rt$death))
table(rt$treat, rt$death)
proportions(table(rt$treat, rt$death), margin = 1)
epitab(rt$treat, rt$death, method = "riskratio")


# Hospitalization - Joel's file

table(rt$treat, !is.na(rt$admission))
table(rt$treat, rt$admission, useNA = "ifany")
proportions(table(rt$treat, rt$admission), margin = 1)
epitab(rt$treat, rt$admission, method = "riskratio")

# Hospitalization or death - composite
rt$hosp_death <- rt$admission
rt$hosp_death[rt$death == 1] <- 1 

table(rt$treat, !is.na(rt$hosp_death))
table(rt$treat, rt$hosp_death, useNA = "ifany")
proportions(table(rt$treat, rt$hosp_death), margin = 1)
epitab(rt$treat, rt$hosp_death, method = "riskratio")


# Hospitalization from REDCap
table(rt$hosp_1, rt$treat, useNA = "ifany") # 2 weeks
table(rt$hospit_2, rt$treat, useNA = "ifany") # 6 months
table(rt$hospit_3, rt$treat, useNA = "ifany") # 1 year

table(rt$hosp, rt$treat, useNA = "ifany") # any time
table(rt$hosp, rt$admission, useNA = "ifany") # any time

# Hospitalization - combined

table(rt$treat, !is.na(rt$hosp_combi))
table(rt$treat, rt$hosp_combi)
proportions(table(rt$treat, rt$hosp_combi), margin = 1)
epitab(rt$treat, rt$hosp_combi, method = "riskratio")

# OPD visit - combined

table(rt$treat, !is.na(rt$opdvisit))
table(rt$treat, rt$opdvisit)
proportions(table(rt$treat, rt$opdvisit), margin = 1)
epitab(rt$treat, rt$opdvisit, method = "riskratio")


# SAE
rt$sae <- rt$SAE
rt$sae[is.na(rt$SAE)] <- 0

table(rt$treat, !is.na(rt$sae))
table(rt$treat, rt$sae)
proportions(table(rt$treat, rt$sae), margin = 1)
epitab(rt$treat, rt$sae, method = "riskratio")


# Reported malaria treatment at OPD or hospital

table(rt$treat, !is.na(rt$malaria))
table(rt$treat, rt$malaria)
proportions(table(rt$treat, rt$malaria), margin = 1)
epitab(rt$treat, rt$malaria, method = "riskratio")

# Reported malaria treatment at OPD - only

table(rt$treat, !is.na(rt$malaria_opd))
table(rt$treat, rt$malaria_opd)
proportions(table(rt$treat, rt$malaria_opd), margin = 1)
epitab(rt$treat, rt$malaria_opd, method = "riskratio")

# Reported malaria treatment at hospital - only

table(rt$treat, !is.na(rt$malaria_hosp))
table(rt$treat, rt$malaria_hosp)
proportions(table(rt$treat, rt$malaria_hosp), margin = 1)
epitab(rt$treat, rt$malaria_hosp, method = "riskratio")


# Reported pneumonia (opd or hospital adm)

table(rt$treat, !is.na(rt$pneumonia))
table(rt$treat, rt$pneumonia)
proportions(table(rt$treat, rt$pneumonia), margin = 1)
epitab(rt$treat, rt$pneumonia, method = "riskratio")

# Reported meningitis

table(rt$treat, !is.na(rt$meningitis))
table(rt$treat, rt$meningitis)
proportions(table(rt$treat, rt$meningitis), margin = 1)
epitab(rt$treat, rt$meningitis, method = "riskratio")
