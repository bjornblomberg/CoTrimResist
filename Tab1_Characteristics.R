# T1 - characteristics
library(tidyverse)
library(epitools)

# Age
table(is.na(rt$age))
median(rt$age)
rt$age_o35 <- 0
rt$age_o35[rt$age > 35] <- 1

rt |>
  group_by(treat, age_o35)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Female sex
rt$female <- NA
rt$female[rt$sex == 0] <- 1
rt$female[rt$sex == 1] <- 0

rt |>
  group_by(treat, female)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Education
rt$edu_pp <- 0
rt$edu_pp[rt$edu == "Secondary"] <- 1
rt$edu_pp[rt$edu == "Post-secondary"] <- 1
table(rt$edu, rt$edu_pp, useNA = "ifany")

rt |>
  group_by(treat, edu_pp)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

# Clinical stage > 1
rt |>
  group_by(treat, stage1)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$stage1, rt$treat)
prop.table(table(rt$stage1, rt$treat), margin = 2)
epitab(rt$treat, rt$stage1, method = "riskratio")

# CD4 < 500
rt |>
  group_by(treat, cd4u500)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$cd4u500, rt$treat)
prop.table(table(rt$cd4u500, rt$treat), margin = 2)
epitab(rt$treat, rt$cd4u500, method = "riskratio")


# Underweight BMI < 18.5
rt |>
  group_by(treat, underweight)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$underweight, rt$treat, useNA = "ifany")
prop.table(table(rt$underweight, rt$treat), margin = 2)
epitab(rt$treat, rt$underweight, method = "riskratio")

# Overweight BMI >30
rt |>
  group_by(treat, obese)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$obese, rt$treat, useNA = "ifany")
prop.table(table(rt$obese, rt$treat), margin = 2)
epitab(rt$treat, rt$obese, method = "riskratio")

# Antibiotic use 
rt |>
  group_by(treat, antibiotics4w)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$antibiotics4w, rt$treat, useNA = "ifany")
prop.table(table(rt$antibiotics4w, rt$treat), margin = 2)
epitab(rt$treat, rt$antibiotics4w, method = "riskratio")

# Hospitalization last year
rt |>
  group_by(treat, hosp1y)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$hosp1y, rt$treat, useNA = "ifany")
prop.table(table(rt$hosp1y, rt$treat), margin = 2)
epitab(rt$treat, rt$hosp1y, method = "riskratio")


# OPD last year
rt |>
  group_by(treat, opd1y)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$opd1y, rt$treat, useNA = "ifany")
prop.table(table(rt$opd1y, rt$treat), margin = 2)
epitab(rt$treat, rt$opd1y, method = "riskratio")
chisq.test(rt$treat, rt$opd1y)

# Surgery ever
rt |>
  group_by(treat, surgery_ever)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$surgery_ever, rt$treat, useNA = "ifany")
prop.table(table(rt$surgery_ever, rt$treat), margin = 2)
epitab(rt$treat, rt$surgery_ever, method = "riskratio")
chisq.test(rt$treat, rt$surgery_ever)

# Surgery last year
rt |>
  group_by(treat, surgery1y)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$surgery1y, rt$treat, useNA = "ifany")
prop.table(table(rt$surgery1y, rt$treat), margin = 2)
epitab(rt$treat, rt$surgery1y, method = "riskratio")

# Injection last year
rt |>
  group_by(treat, injection1y)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$injection1y, rt$treat, useNA = "ifany")
prop.table(table(rt$injection1y, rt$treat), margin = 2)
epitab(rt$treat, rt$injection1y, method = "riskratio")

# District of residence

rt$district <- factor(rt$district, levels = c("Temeke", "Kinondoni", "Ilala", 
                                                "Ubungu", "Kigamboni", "Other"))
levels(rt$district) 
table(rt$district)

rt |>
  group_by(treat, district)  |> 
  summarise(n = n(), .groups = 'drop') |> 
  group_by(treat) |> 
  mutate(total = sum(n), percentage = n / total * 100) 

table(rt$district, rt$treat, useNA = "ifany")
prop.table(table(rt$district, rt$treat), margin = 2)
epitab(rt$treat, rt$district, method = "riskratio")

# risk ratios by binomial regression
model <- glm(treat ~ age_o35, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ female, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ edu_pp, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ stage1, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ cd4u500, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ underweight, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ obese, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ antibiotics4w, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ hosp1y, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ opd1y, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ surgery_ever, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ surgery1y, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ injection1y, data = rt, family = binomial (link = "log"))
model <- glm(treat ~ district, data = rt, family = binomial (link = "log"))

# Get output from model
coef_est <- coef(model) # Extract coefficient estimates
se <- summary(model)$coefficients[, "Std. Error"] # Extract standard errors
rr <- exp(coef_est) # Calculate relative risks
ci_low <- exp(coef_est - 1.96 * se) # Calculate confidence intervals for relative risks
ci_high <- exp(coef_est + 1.96 * se)
print(data.frame(mean = rr, lower = ci_low, upper = ci_high)) # Create a data frame to store results



