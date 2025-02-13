# GEE models (used for results in Fig 2, )

# GEE models with binomial family, log-link, unstructured correlation structure
# Chose scripts 1-4 below to create models for
#     1. Baseline-adjusted analysis
#     2. Change over time analysis - COTRIMOXAZOLE group 
#     3. Change over time analysis - PLACEBO group 
#     4. Analysis NOT adjusted for baseline 

library(geepack)
library(tidyverse)

# Chose dateframe
# ITT analyses
gee_long <- gee_long_esbl # choose ESBL
gee_long <- gee_long_mrsa # choose MRSA
gee_long <- gee_long_pnsp # choose PNSP
#ITT analysis - carriage of S aureus and pneumocci (regardless of resistance)
gee_long <- gee_long_staph # choose S aureus overall
gee_long <- gee_long_pneumo # choose Pneumococci overall
# Sensitivity analyses
gee_long <- gee_long_esbl_adh # choose ESBL - adherence
gee_long <- gee_long_mrsa_adh # choose MRSA - adherence
gee_long <- gee_long_pnsp_adh # choose PNSP - adherence
gee_long <- gee_long_esbl_hc # choose ESBL - healthcare exposure
gee_long <- gee_long_mrsa_hc # choose MRSA - healthcare exposure
gee_long <- gee_long_pnsp_hc # choose PNSP - healthcare exposure

# Create dataframe with variable for post-baseline observations (aftertr==1)
gee_long$aftertr <- 0
gee_long$aftertr[gee_long$time > 0] <- 1
table(gee_long$time, gee_long$aftertr)

# 1. Baseline-adjusted analysis (GEE model) ------------------------------------
model <- geeglm(carriage ~  tx * aftertr, family = binomial(link = "log"), data = gee_long, id = rct_id, corstr = "unstructured")

# 2. Change over time - COTRIMOXAZOLE group (GEE model) ------------------------
gee_long_sxt <- gee_long |> 
  filter(tx == 1)
model <- geeglm(carriage ~ time, family = binomial(link = "log"), data = gee_long_sxt, id = rct_id, corstr = "unstructured")

# 3. Change over time - PLACEBO group (GEE model)-------------------------------
gee_long_plc <- gee_long |> 
  filter(tx == 0) 
model <- geeglm(carriage ~ time, family = binomial(link = "log"),  data = gee_long_plc, id = rct_id, corstr = "unstructured")

# 4. Analysis NOT adjusted for baseline ----------------------------------------
# Create dataframe omitting baseline observations
gee_long <- gee_long |> 
  filter(time > 0)
table(gee_long$time)
model <- geeglm(carriage ~  tx, family = binomial(link = "log"), data = gee_long, id = rct_id, corstr = "unstructured") 


# --------VIEW RESULTS ---------------------------------------------------------
summary(model) 

# Display relative risks with 95% confidence intervals
coef_log <- coef(model) # Extract coefficients (log scale)
RR <- exp(coef_log) # Exponentiate coefficients to get relative risks
se <- sqrt(diag(summary(model)$cov.scaled)) # Extract standard errors
lower_log <- coef_log - 1.96 * se # Calculate 95% CI on log scale
upper_log <- coef_log + 1.96 * se
RR_CI_lower <- exp(lower_log) # Exponentiate to get CI on relative risk scale
RR_CI_upper <- exp(upper_log)
results <- data.frame(            # Combine results into a data frame
  Coefficients = coef_log,        # Log-coefficients (log-relative risk)
  RR = RR,                        # Relative Risk
  CI_Lower = RR_CI_lower,         # 95% CI lower bound for RR
  CI_Upper = RR_CI_upper)         # 95% CI upper bound for RR
print(results) # View the results
