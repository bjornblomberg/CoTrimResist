# Prepare data for GEE analysis
library(tidyverse)


# ESBL prepare data --------------------------------
gee_esbl <- rt |> 
  select(rct_id, tx, v0esbl, v1esbl, v2esbl, v3esbl)

gee_long_esbl <- gee_esbl %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v" ("visit"))
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_esbl$time)
gee_long_esbl$time[gee_long_esbl$time == "v0esbl"] <- 0
gee_long_esbl$time[gee_long_esbl$time == "v1esbl"] <- 1
gee_long_esbl$time[gee_long_esbl$time == "v2esbl"] <- 2
gee_long_esbl$time[gee_long_esbl$time == "v3esbl"] <- 3
str(gee_long_esbl$time)
gee_long_esbl$time <- as.numeric(gee_long_esbl$time)

table(gee_long_esbl$time, useNA = "ifany")
table(gee_long_esbl$carriage, useNA = "ifany")
table(gee_long_esbl$time, gee_long_esbl$carriage, useNA = "ifany")
rm(gee_esbl)

# ESBL sensitivity analysis 75% adherence n= 260 prepare data --------------------------------
gee_esbl_adh <- sens75 |> 
  select(rct_id, tx, v0esbl, v1esbl, v2esbl, v3esbl)

gee_long_esbl_adh <- gee_esbl_adh |>  # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v" ("visit"))
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_esbl_adh$time)
gee_long_esbl_adh$time[gee_long_esbl_adh$time == "v0esbl"] <- 0
gee_long_esbl_adh$time[gee_long_esbl_adh$time == "v1esbl"] <- 1
gee_long_esbl_adh$time[gee_long_esbl_adh$time == "v2esbl"] <- 2
gee_long_esbl_adh$time[gee_long_esbl_adh$time == "v3esbl"] <- 3
str(gee_long_esbl_adh$time)
gee_long_esbl_adh$time <- as.numeric(gee_long_esbl_adh$time)

table(gee_long_esbl_adh$time, useNA = "ifany")
table(gee_long_esbl_adh$carriage, useNA = "ifany")
table(gee_long_esbl_adh$time, gee_long_esbl_adh$carriage, useNA = "ifany")
rm(gee_esbl_adh)

# ESBL sensitivity analysis no prior healthcare association n= 193 - prepare data --------------------------------
gee_esbl_hc <- sens |> 
  select(rct_id, tx, v0esbl, v1esbl, v2esbl, v3esbl)

gee_long_esbl_hc <- gee_esbl_hc |>  # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v" ("visit"))
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_esbl_hc$time)
gee_long_esbl_hc$time[gee_long_esbl_hc$time == "v0esbl"] <- 0
gee_long_esbl_hc$time[gee_long_esbl_hc$time == "v1esbl"] <- 1
gee_long_esbl_hc$time[gee_long_esbl_hc$time == "v2esbl"] <- 2
gee_long_esbl_hc$time[gee_long_esbl_hc$time == "v3esbl"] <- 3
str(gee_long_esbl_hc$time)
gee_long_esbl_hc$time <- as.numeric(gee_long_esbl_hc$time)

table(gee_long_esbl_hc$time, useNA = "ifany")
table(gee_long_esbl_hc$carriage, useNA = "ifany")
table(gee_long_esbl_hc$time, gee_long_esbl_hc$carriage, useNA = "ifany")
rm(gee_esbl_hc)



# MRSA prepare data --------------------------------
gee_mrsa <- rt |> 
  select(rct_id, tx, v0mrsa, v1mrsa, v2mrsa, v3mrsa)

gee_long_mrsa <- gee_mrsa %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_mrsa$time)
gee_long_mrsa$time[gee_long_mrsa$time == "v0mrsa"] <- 0
gee_long_mrsa$time[gee_long_mrsa$time == "v1mrsa"] <- 1
gee_long_mrsa$time[gee_long_mrsa$time == "v2mrsa"] <- 2
gee_long_mrsa$time[gee_long_mrsa$time == "v3mrsa"] <- 3
str(gee_long_mrsa$time)
gee_long_mrsa$time <- as.numeric(gee_long_mrsa$time)

table(gee_long_mrsa$time, useNA = "ifany")
table(gee_long_mrsa$carriage, useNA = "ifany")
table(gee_long_mrsa$time, gee_long_mrsa$carriage, useNA = "ifany")
rm(gee_mrsa)

# MRSA prepare data - sensitivity analysis 75% adherence n= 260 --------------------------------
gee_mrsa_adh <- sens75 |> 
  select(rct_id, tx, v0mrsa, v1mrsa, v2mrsa, v3mrsa)

gee_long_mrsa_adh <- gee_mrsa_adh %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_mrsa_adh$time)
gee_long_mrsa_adh$time[gee_long_mrsa_adh$time == "v0mrsa"] <- 0
gee_long_mrsa_adh$time[gee_long_mrsa_adh$time == "v1mrsa"] <- 1
gee_long_mrsa_adh$time[gee_long_mrsa_adh$time == "v2mrsa"] <- 2
gee_long_mrsa_adh$time[gee_long_mrsa_adh$time == "v3mrsa"] <- 3
str(gee_long_mrsa_adh$time)
gee_long_mrsa_adh$time <- as.numeric(gee_long_mrsa_adh$time)

table(gee_long_mrsa_adh$time, useNA = "ifany")
table(gee_long_mrsa_adh$carriage, useNA = "ifany")
table(gee_long_mrsa_adh$time, gee_long_mrsa_adh$carriage, useNA = "ifany")
rm(gee_mrsa_adh)

# MRSA prepare data - sensitivity analysis no prior healthcare association n= 193 --------------------------------
gee_mrsa_hc <- sens |> 
  select(rct_id, tx, v0mrsa, v1mrsa, v2mrsa, v3mrsa)

gee_long_mrsa_hc <- gee_mrsa_hc %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_mrsa_hc$time)
gee_long_mrsa_hc$time[gee_long_mrsa_hc$time == "v0mrsa"] <- 0
gee_long_mrsa_hc$time[gee_long_mrsa_hc$time == "v1mrsa"] <- 1
gee_long_mrsa_hc$time[gee_long_mrsa_hc$time == "v2mrsa"] <- 2
gee_long_mrsa_hc$time[gee_long_mrsa_hc$time == "v3mrsa"] <- 3
str(gee_long_mrsa_hc$time)
gee_long_mrsa_hc$time <- as.numeric(gee_long_mrsa_hc$time)

table(gee_long_mrsa_hc$time, useNA = "ifany")
table(gee_long_mrsa_hc$carriage, useNA = "ifany")
table(gee_long_mrsa_hc$time, gee_long_mrsa_hc$carriage, useNA = "ifany")
rm(gee_mrsa_hc)


# PNSP prepare data --------------------------------
gee_pnsp <- rt |> 
  select(rct_id, tx, v0pnsp, v1pnsp, v2pnsp, v3pnsp)

gee_long_pnsp <- gee_pnsp %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_pnsp$time)
gee_long_pnsp$time[gee_long_pnsp$time == "v0pnsp"] <- 0
gee_long_pnsp$time[gee_long_pnsp$time == "v1pnsp"] <- 1
gee_long_pnsp$time[gee_long_pnsp$time == "v2pnsp"] <- 2
gee_long_pnsp$time[gee_long_pnsp$time == "v3pnsp"] <- 3
str(gee_long_pnsp$time)
gee_long_pnsp$time <- as.numeric(gee_long_pnsp$time)

table(gee_long_pnsp$time, useNA = "ifany")
table(gee_long_pnsp$carriage, useNA = "ifany")
table(gee_long_pnsp$time, gee_long_pnsp$carriage, useNA = "ifany")
rm(gee_pnsp)

# PNSP prepare data - sensitivity analysis 75% adherence n= 260 --------------------------------
gee_pnsp_adh <- sens75 |> 
  select(rct_id, tx, v0pnsp, v1pnsp, v2pnsp, v3pnsp)

gee_long_pnsp_adh <- gee_pnsp_adh %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_pnsp_adh$time)
gee_long_pnsp_adh$time[gee_long_pnsp_adh$time == "v0pnsp"] <- 0
gee_long_pnsp_adh$time[gee_long_pnsp_adh$time == "v1pnsp"] <- 1
gee_long_pnsp_adh$time[gee_long_pnsp_adh$time == "v2pnsp"] <- 2
gee_long_pnsp_adh$time[gee_long_pnsp_adh$time == "v3pnsp"] <- 3
str(gee_long_pnsp_adh$time)
gee_long_pnsp_adh$time <- as.numeric(gee_long_pnsp_adh$time)

table(gee_long_pnsp_adh$time, useNA = "ifany")
table(gee_long_pnsp_adh$carriage, useNA = "ifany")
table(gee_long_pnsp_adh$time, gee_long_pnsp_adh$carriage, useNA = "ifany")
rm(gee_pnsp_adh)

# PNSP prepare data - sensitivity analysis no prior healthcare association n= 193 --------------------------------
gee_pnsp_hc <- sens |> 
  select(rct_id, tx, v0pnsp, v1pnsp, v2pnsp, v3pnsp)

gee_long_pnsp_hc <- gee_pnsp_hc %>% # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_pnsp_hc$time)
gee_long_pnsp_hc$time[gee_long_pnsp_hc$time == "v0pnsp"] <- 0
gee_long_pnsp_hc$time[gee_long_pnsp_hc$time == "v1pnsp"] <- 1
gee_long_pnsp_hc$time[gee_long_pnsp_hc$time == "v2pnsp"] <- 2
gee_long_pnsp_hc$time[gee_long_pnsp_hc$time == "v3pnsp"] <- 3
str(gee_long_pnsp_hc$time)
gee_long_pnsp_hc$time <- as.numeric(gee_long_pnsp_hc$time)

table(gee_long_pnsp_hc$time, useNA = "ifany")
table(gee_long_pnsp_hc$carriage, useNA = "ifany")
table(gee_long_pnsp_hc$time, gee_long_pnsp_hc$carriage, useNA = "ifany")
rm(gee_pnsp_hc)


# S. aureus - prepare data --------------------------------
gee_staph <- rt |> 
  select(rct_id, tx, v0staph, v1staph, v2staph, v3staph)

gee_long_staph <- gee_staph |>  # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_staph$time)
gee_long_staph$time[gee_long_staph$time == "v0staph"] <- 0
gee_long_staph$time[gee_long_staph$time == "v1staph"] <- 1
gee_long_staph$time[gee_long_staph$time == "v2staph"] <- 2
gee_long_staph$time[gee_long_staph$time == "v3staph"] <- 3
str(gee_long_staph$time)
gee_long_staph$time <- as.numeric(gee_long_staph$time)

table(gee_long_staph$time, useNA = "ifany")
table(gee_long_staph$carriage, useNA = "ifany")
table(gee_long_staph$time, gee_long_staph$carriage, useNA = "ifany")
rm(gee_staph)

# Penumococci - prepare data --------------------------------
gee_pneumo <- rt |> 
  select(rct_id, tx, v0spn, v1spn, v2spn, v3spn)

gee_long_pneumo <- gee_pneumo |>  # Convert wide to long format
  pivot_longer(
    cols = starts_with("v"),  # Columns to pivot (the ones starting with "v")
    names_to = "time",            # Name of the new "time" column
    values_to = "carriage")           # Name of the new "carriage" column

unique(gee_long_pneumo$time)
gee_long_pneumo$time[gee_long_pneumo$time == "v0spn"] <- 0
gee_long_pneumo$time[gee_long_pneumo$time == "v1spn"] <- 1
gee_long_pneumo$time[gee_long_pneumo$time == "v2spn"] <- 2
gee_long_pneumo$time[gee_long_pneumo$time == "v3spn"] <- 3
str(gee_long_pneumo$time)
gee_long_pneumo$time <- as.numeric(gee_long_pneumo$time)

table(gee_long_pneumo$time, useNA = "ifany")
table(gee_long_pneumo$carriage, useNA = "ifany")
table(gee_long_pneumo$time, gee_long_pneumo$carriage, useNA = "ifany")
rm(gee_pneumo)

# Export for analysis in Stata -------------------
library(haven)
write_dta(gee_long_esbl, path = "stata/esbl.dta")
write_dta(gee_long_esbl_adh, path = "stata/esbl_adh.dta")
write_dta(gee_long_esbl_hc, path = "stata/esbl_hc.dta")

write_dta(gee_long_mrsa, path = "stata/mrsa.dta")
write_dta(gee_long_mrsa_adh, path = "stata/mrsa_adh.dta")
write_dta(gee_long_mrsa_hc, path = "stata/mrsa_hc.dta")

write_dta(gee_long_pnsp, path = "stata/pnsp.dta")
write_dta(gee_long_pnsp_adh, path = "stata/pnsp_adh.dta")
write_dta(gee_long_pnsp_hc, path = "stata/pnsp_hc.dta")

write_dta(gee_long_staph, path = "stata/staph.dta")
write_dta(gee_long_pneumo, path = "stata/pneumo.dta")
# library(writexl)
# write_xlsx(gee_long_esbl, path = "stata/long_esbl.xlsx")

