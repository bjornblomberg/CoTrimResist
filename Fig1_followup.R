# Follow-up

# screening sc-database
table(sc$sex, useNA = "ifany")
table(sc$u18, useNA = "ifany")
table(sc$cd4u350, useNA = "ifany")
table(sc$u18, sc$cd4u350, useNA = "ifany")


# --- Available samples for analysis -------------------------------
# Rectal swabs
table(rt$v0swab_r, rt$specrectalb_2, useNA = "ifany")
table(rt$v0swab_r, rt$v0esbl, useNA = "ifany")
table(rt$v1swab_r, rt$rectalswab, useNA = "ifany")
table(rt$v1swab_r, rt$v1esbl, useNA = "ifany")
table(rt$v2swab_r, rt$rectal_swab_6, useNA = "ifany")
table(rt$v2swab_r, rt$v2esbl, useNA = "ifany")
table(rt$v3swab_r, rt$rectal_swab, useNA = "ifany")
table(rt$v3swab_r, rt$v3esbl, useNA = "ifany")

# baseline
table(rt$specrectalb_2, rt$treat, useNA = "ifany")
table(!is.na(rt$specrectalb_2), rt$treat, useNA = "ifany")
table(rt$v0swab_r, rt$treat, useNA = "ifany")
table(rt$v0esbl, rt$treat, useNA = "ifany")
table(!is.na(rt$v0esbl), rt$treat, useNA = "ifany")

# 2 weeks
table(rt$rectalswab, rt$treat, useNA = "ifany")
table(!is.na(rt$rectalswab), rt$treat, useNA = "ifany")
table(rt$v1esbl, rt$treat, useNA = "ifany")
table(!is.na(rt$v1esbl), rt$treat, useNA = "ifany")

# 24 weeks
table(rt$rectal_swab_6, rt$treat, useNA = "ifany")
table(!is.na(rt$rectal_swab_6), rt$treat, useNA = "ifany")
table(rt$v2esbl, rt$treat, useNA = "ifany")
table(!is.na(rt$v2esbl), rt$treat, useNA = "ifany")

# 48 weeks
table(rt$rectal_swab, rt$treat, useNA = "ifany")
table(!is.na(rt$rectal_swab), rt$treat, useNA = "ifany")
table(rt$v3esbl, rt$treat, useNA = "ifany")
table(!is.na(rt$v3esbl), rt$treat, useNA = "ifany")

# Nasopharyngeal
table(rt$v0swab_n, rt$specnoseb, useNA = "ifany")
table(rt$v1swab_n, rt$nasopharyngeal_swab, useNA = "ifany")
table(rt$v2swab_n, rt$nasopharyngeal_swab_6, useNA = "ifany")
table(rt$v3swab_n, rt$naso_swab_3, useNA = "ifany")


# baseline
table(rt$specnoseb, rt$treat, useNA = "ifany")
table(!is.na(rt$specnoseb), rt$treat, useNA = "ifany")
table(rt$v0swab_n, rt$treat, useNA = "ifany")
table(!is.na(rt$v0swab_n), rt$treat, useNA = "ifany")


# 2 weeks
table(rt$nasopharyngeal_swab, rt$treat, useNA = "ifany")
table(!is.na(rt$nasopharyngeal_swab), rt$treat, useNA = "ifany")
table(rt$v1staphaureus, rt$treat, useNA = "ifany")
table(!is.na(rt$v1staphaureus), rt$treat, useNA = "ifany")
table(rt$v1staph, rt$treat, useNA = "ifany")
table(!is.na(rt$v1staph), rt$treat, useNA = "ifany")
table(rt$v1spn, rt$treat, useNA = "ifany")
table(!is.na(rt$v1spn), rt$treat, useNA = "ifany")

# 24 weeks
table(rt$nasopharyngeal_swab_6, rt$treat, useNA = "ifany")
table(!is.na(rt$nasopharyngeal_swab_6), rt$treat, useNA = "ifany")

table(rt$v2staph, rt$treat, useNA = "ifany")
table(!is.na(rt$v2staph), rt$treat, useNA = "ifany")
table(rt$v2spn, rt$treat, useNA = "ifany")
table(!is.na(rt$v2spn), rt$treat, useNA = "ifany")

# 48 weeks
table(rt$naso_swab_3, rt$treat, useNA = "ifany")
table(!is.na(rt$naso_swab_3), rt$treat, useNA = "ifany")
table(rt$v3staph, rt$treat, useNA = "ifany")
table(!is.na(rt$v3staph), rt$treat, useNA = "ifany")
table(rt$v3spn, rt$treat, useNA = "ifany")
table(!is.na(rt$v3spn), rt$treat, useNA = "ifany")

# ------------------------------------------------------------------
# baseline
table(sc$baseline_visit_informed_consent_study_enrolment_complete)
table(rt$baseline_visit_informed_consent_study_enrolment_complete)
table(rt$consent)
table(rt$signed)
table(rt$consent, rt$signed, useNA = "ifany")
table(is.na(rt$rctserialnumber))

# # Follow-up 2 weeks
table(rt$fup1, rt$treat, useNA = "ifany")
table(rt$v1death, rt$treat, useNA = "ifany")
table(rt$Death_period, rt$treat, useNA = "ifany")
table(rt$Death_period, rt$fup1, useNA = "ifany")
table(rt$Death_period, rt$fup2, useNA = "ifany")
table(rt$Death_period, rt$fup3, useNA = "ifany")

table(rt$LTF, rt$treat, useNA = "ifany")

table(rt$fup1, rt$v1esbl, useNA = "ifany")
table(rt$fup1, rt$v1staph, useNA = "ifany")
table(rt$fup1, rt$v1spn, useNA = "ifany")
table(rt$Death_period, rt$treat, useNA = "ifany")
table(rt$fup1, useNA = "ifany")
table(rt$fup1, rt$week_2_visit_complete, useNA = "ifany")
table(rt$fup1, is.na(rt$date_visit_1), useNA = "ifany")

table(rt$fup1, rt$compliance_1, useNA = "ifany")
proportions(table(rt$fup1, rt$compliance_1, useNA = "ifany"))
table(rt$compliance50_1[rt$fup1 == 1])
proportions(table(rt$compliance50_1[rt$fup1 == 1]))

table(rt$fup1, rt$rectalswab, useNA = "ifany")
table(rt$fup1, rt$nasopharyngeal_swab, useNA = "ifany")
table(rt$rectalswab, rt$nasopharyngeal_swab, useNA = "ifany")
table(rt$fup1, rt$death_1, useNA = "ifany")

# 2 months

table(rt$stat_1, rt$treat, useNA = "ifany")


# 6 months
table(rt$fup2, useNA = "ifany")
table(rt$fup2, rt$treat, useNA = "ifany")
table(rt$v2death, rt$treat, useNA = "ifany")

# 1 year
table(rt$fup3, useNA = "ifany")
table(rt$fup3, rt$treat, useNA = "ifany")
table(rt$v3death, rt$treat, useNA = "ifany")


# Completion of data form
table(rt$complete_study, useNA = "ifany")
table(rt$complete_study_date, useNA = "ifany")
table(rt$complete_study, !is.na(rt$complete_study_date), useNA = "ifany")

table(!is.na(rt$withdraw_date), useNA = "ifany")
table(rt$withdraw_date, useNA = "ifany")
table(rt$complete_study, !is.na(rt$withdraw_date), useNA = "ifany")

table(rt$withdraw_reason, useNA = "ifany")
table(rt$withdraw_reason, !is.na(rt$withdraw_date), useNA = "ifany")

table(rt$study_comments, useNA = "ifany")

# Table - follow-up - flow-chart
# Follow-up 
table(rt$fup1)
table(rt$fup2)
table(rt$fup3)
prop.table(table(rt$fup1))
prop.table(table(rt$fup2))
prop.table(table(rt$fup3))

# Naso-swabs
table(rt$v0swab_n)
table(rt$v1swab_n)
table(rt$v2swab_n)
table(rt$v3swab_n)
prop.table(table(rt$v0swab_n))
prop.table(table(rt$v1swab_n))
prop.table(table(rt$v2swab_n))
prop.table(table(rt$v3swab_n))

# Rectal swabs
table(rt$v0swab_r)
table(rt$v1swab_r)
table(rt$v2swab_r)
table(rt$v3swab_r)
prop.table(table(rt$v0swab_r))
prop.table(table(rt$v1swab_r))
prop.table(table(rt$v2swab_r))
prop.table(table(rt$v3swab_r))

# Rectal swabs available for analysis
table(!is.na(rt$v0esbl))
table(!is.na(rt$v1esbl))
table(!is.na(rt$v2esbl))
table(!is.na(rt$v3esbl))
prop.table(table(!is.na(rt$v0esbl)))
prop.table(table(!is.na(rt$v1esbl)))
prop.table(table(!is.na(rt$v2esbl)))
prop.table(table(!is.na(rt$v3esbl)))

# compliance
# compliance > 50%
table(rt$compliance50_1, useNA = "ifany")
table(rt$compliance50_2, useNA = "ifany")
table(rt$compliance50_3, useNA = "ifany")
prop.table(table(rt$compliance50_1, useNA = "ifany"))
prop.table(table(rt$compliance50_2, useNA = "ifany"))
prop.table(table(rt$compliance50_3, useNA = "ifany"))

# compliance > 75%
table(rt$compliance75_1, useNA = "ifany")
table(rt$compliance75_2, useNA = "ifany")
table(rt$compliance75_3, useNA = "ifany")
prop.table(table(rt$compliance75_1, useNA = "ifany"))
prop.table(table(rt$compliance75_2, useNA = "ifany"))
prop.table(table(rt$compliance75_3, useNA = "ifany"))

# Crosschecking compliance
table(rt$compliance50_1, rt$compliance50_2, useNA = "ifany")
table(rt$compliance50_1, rt$compliance50_3, useNA = "ifany")
table(rt$compliance50_2, rt$compliance50_3, useNA = "ifany")

table(rt$compliance75_1, rt$compliance75_2, useNA = "ifany")
table(rt$compliance75_1, rt$compliance75_3, useNA = "ifany")
table(rt$compliance75_2, rt$compliance75_3, useNA = "ifany")

# Lost to follow-up
# From Joel's overview
table(rt$LTF)
table(rt$Death)
table(rt$Withdrawal)
table(rt$MovedClinic)
table(rt$LTF, rt$Death)
table(rt$LTF, rt$Withdrawal)
table(rt$LTF, rt$MovedClinic)

rt$lost <- "0"
rt$lost[rt$LTF == 1] <- "Lost"
rt$lost[rt$Withdrawal == 1] <- "Withdrawal"
rt$lost[!is.na(rt$withdraw_reason)] <- "Withdrawal"
rt$lost[rt$MovedClinic == 1] <- "Moved_clinic"
rt$lost[rt$Death == 1] <- "Died"

rt$lost_1 <- rt$lost
rt$lost_1[rt$fup1 == 1] <- "0"
table(rt$lost_1, rt$fup1, useNA = "ifany")

rt$lost_2 <- rt$lost
rt$lost_2[rt$fup2 == 1] <- "0"
table(rt$lost_2, rt$fup2, useNA = "ifany")

rt$lost_3 <- rt$lost
rt$lost_3[rt$fup3 == 1] <- "0"
table(rt$lost_3, rt$fup3, useNA = "ifany")
table(rt$lost, rt$fup3, useNA = "ifany")

table(rt$lost, useNA = "ifany")
table(rt$lost, rt$fup1, useNA = "ifany")
table(rt$lost, rt$fup2, useNA = "ifany")
table(rt$lost, rt$fup3, useNA = "ifany")




table(rt$death_1, rt$Death)
table(rt$death_2, rt$Death)
table(rt$death_3, rt$Death)

table(rt$withdraw_reason, rt$Withdrawal, useNA = "ifany")

table(rt$withdrawal_combined)
table(rt$withdrawal_combined, rt$death)




