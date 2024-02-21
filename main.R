#INSTALLING PACKAGES, does not work with function count! 
#install.packages("dplyr")
library("dplyr")
#install.packages("gtsummary")
library("gtsummary")

#TABLE1 Local audit filters
auditfilter1 <- merged.data %>% 
  select(VK_sap_less90, VK_gcs_less9_ej_intubTE, VK_iss_15_ej_iva, VK_iss_15_ej_TE, 
         VK_ej_trombrof_TBI_72h, VK_mer_30min_DT, VK_leverskada, VK_mjaltskada, 
         VK_hlr_thorak, VK_mass_transf, res_survival)

auditfilter1$VK_sap_less90 <- ifelse(auditfilter1$VK_sap_less90 == "Ja", "Yes", 
                                     ifelse(auditfilter1$VK_sap_less90 == "nej", NA,
                                            ifelse(auditfilter1$VK_sap_less90 == "Nej", NA, NA)))

auditfilter1$VK_gcs_less9_ej_intubTE <- ifelse(auditfilter1$VK_gcs_less9_ej_intubTE == "Ja", "Yes",
                                               ifelse(auditfilter1$VK_gcs_less9_ej_intubTE == "nej", NA,
                                                      ifelse(auditfilter1$VK_gcs_less9_ej_intubTE == "Nej", NA, NA)))                            

auditfilter1$VK_iss_15_ej_iva <- ifelse(auditfilter1$VK_iss_15_ej_iva == "Ja", "Yes",
                                        ifelse(auditfilter1$VK_iss_15_ej_iva == "nej", NA,
                                               ifelse(auditfilter1$VK_iss_15_ej_iva == "Nej", NA, NA)))     

auditfilter1$VK_iss_15_ej_TE <- ifelse(auditfilter1$VK_iss_15_ej_TE == "Ja", "Yes",
                                       ifelse(auditfilter1$VK_iss_15_ej_TE == "nej", NA,
                                              ifelse(auditfilter1$VK_iss_15_ej_TE == "Nej", NA, 
                                                     ifelse(auditfilter1$VK_iss_15_ej_TE == "nn", NA,NA))))     

auditfilter1$VK_ej_trombrof_TBI_72h <- ifelse(auditfilter1$VK_ej_trombrof_TBI_72h == "Ja", "Yes",
                                              ifelse(auditfilter1$VK_ej_trombrof_TBI_72hE == "nej", NA,
                                                     ifelse(auditfilter1$VK_ej_trombrof_TBI_72h == "Nej", NA, NA)))     

auditfilter1$VK_mer_30min_DT <- ifelse(auditfilter1$VK_mer_30min_DT == "Ja", "Yes",
                                       ifelse(auditfilter1$VK_mer_30min_DT == "ja", "Yes",
                                              ifelse(auditfilter1$VK_mer_30min_DT == "Nej", NA, NA)))     

auditfilter1$VK_hlr_thorak <- ifelse(auditfilter1$VK_hlr_thorak == "Ja", "Yes", 
                                     ifelse(auditfilter1$VK_hlr_thorak == "Nej", NA, NA))


auditfilter1$VK_mass_transf <- ifelse(auditfilter1$VK_mass_transf == "Ja", "Yes", 
                                      ifelse(auditfilter1$VK_mass_transf == "Nej", NA, NA))

auditfilter1$res_survival <- ifelse(auditfilter1$res_survival == 1, "Yes", 
                                    ifelse(auditfilter1$res_survival == 2, NA, 
                                           ifelse(auditfilter1$res_survival == 999, NA,  NA)))

auditfilter1$LiverorSpleen <- ifelse(auditfilter1$VK_leverskada == "Ja", "Yes", 
                                     ifelse(auditfilter1$VK_mjaltskada == "Ja", "Yes", NA))

auditfilter2 <- auditfilter1 %>% 
  select(VK_sap_less90, VK_gcs_less9_ej_intubTE, VK_iss_15_ej_iva, VK_iss_15_ej_TE, 
         VK_ej_trombrof_TBI_72h, VK_mer_30min_DT, 
         VK_hlr_thorak, VK_mass_transf, res_survival, LiverorSpleen)

auditfilter2 %>%
  tbl_summary(
    label = list(
      VK_sap_less90 = "Systolic blood pressure under 90",
      VK_gcs_less9_ej_intubTE = "Glasgow Coma Scale (GCS) less than 9 and not intubated",
      VK_iss_15_ej_iva = "Injury Severity Score (ISS) more than 15 but not admitted to the ICU",
      VK_iss_15_ej_TE = "Injury Severity Score (ISS) more than 15 and no trauma team activation",
      VK_ej_trombrof_TBI_72h = "No anticoagulation treatment within 72 h after traumatic brain injury time to acute intervention more than 60 min",
      VK_mer_30min_DT = "Time to computed tomography (CT) more than 30 min",
      VK_hlr_thorak = "Cardiopulmonary resuscitation with thoracotomy",
      VK_mass_transf = "Massive transfusion",
      res_survival = "Death within 30 days after trauma",
      LiverorSpleen = "Liver or spleen injury"
    ),
    type = list(VK_mer_30min_DT ~ "dichotomous", 
                LiverorSpleen ~ "dichotomous", 
                VK_sap_less90 ~ "dichotomous",
                VK_gcs_less9_ej_intubTE ~ "dichotomous",
                VK_iss_15_ej_iva ~ "dichotomous",
                VK_iss_15_ej_TE ~ "dichotomous",
                VK_ej_trombrof_TBI_72h ~ "dichotomous",
                VK_hlr_thorak ~ "dichotomous",
                VK_mass_transf ~ "dichotomous",
                res_survival ~ "dichotomous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n}"
    ),
    missing = "no"
  ) %>%
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 1. Local audit filters</div>") %>%
  modify_footnote(stat_0 ~ "The total number of unique patients flagged was ___. The sum of the number of patient cases flagged exceeds the number of unique flagged because several filters could flag the same patient.") %>%
  modify_header(update = list(label = "**Audit filter**")) %>%
  modify_header(stat_0 ~ "**Number of patient cases flagged between 2017-2022**") %>%
  print()

#FLOWCHART: included/excluded
#install.packages("Gmisc")
library(Gmisc, quietly = TRUE)
library(glue)
#install.packages("htmlTable")
library(htmlTable)
library(grid)
library(magrittr)

org_cohort <- boxGrob(glue("Total patient cases in trauma quality database",
                           "n = {pop}",
                           pop = txtInt(5000),
                           .sep = "\n"))
eligible <- boxGrob(glue("Eligible",
                         "n = {pop}",
                         pop = txtInt(3500),
                         .sep = "\n"))
included <- boxGrob(glue("Included",
                         "n = {incl}",
                         incl = txtInt(3000),
                         .sep = "\n"))
excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - Patients < 15 years: {age}",
                         " - Dead on arrival: {doa}",
                         tot = 30,
                         age = 12,
                         doa = 30 - 12,
                         .sep = "\n"),
                    just = "left")

grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included)
horiz <- spreadHorizontal(excluded = excluded)  # Spread horizontally does not work?

# Move excluded box
excluded <- moveBox(excluded,
                    x = 0.8,
                    y = 0.3)

# Connect boxes vertically
for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}

# Connect excluded box horizontally
connectGrob(vert$included, excluded, type = "L")

# Print boxes
vert
excluded


##CLEANING DATA
subdat <- merged.data %>%
  select(ofi, pt_Gender, pt_age_yrs,  ed_gcs_sum, ed_sbp_value, ed_rr_value, 
         res_survival, pre_intubated, ed_intubated, iva_vardtillfallen_n, dt_ed_first_ct, ISS,
         Deceased, DeceasedDate, DateTime_ArrivalAtHospital, FirstTraumaDT_NotDone,
         host_care_level, hosp_vent_days, dt_ed_emerg_proc, pt_asa_preinjury, iva_dagar_n)

#Converting subdat$ofi to logical so subset can be used 
subdat$ofi <- ifelse(subdat$ofi == "Yes", TRUE, FALSE)

#Removing pt_yrs < 15
adult <- subset(subdat, subset = (pt_age_yrs > 14))

#Deceased on arrival 
alive <- subset(adult, subset = (DeceasedDate > DateTime_ArrivalAtHospital))

#Counting number of TRUE and FALSE 
alive %>% count(ofi)

iva <- subset(alive, subset = (host_care_level == 5))

#Only showing the subset with TRUE for ofi in patients over 14 yrs old                
ofi <- subset(iva, subset = (ofi == TRUE | ofi == FALSE))


#DEFINING VARIABLES FOR TABLE 1 
#Gender
ofi$Sex <- ifelse(ofi$pt_Gender == 1, "Male", 
                  ifelse(ofi$pt_Gender == 2, "Female", 
                         ifelse(ofi$pt_Gender == 999, NA,NA)))

#Age
#ofi$Age <- ifelse(ofi$pt_age_yrs %in% 15:19, "Older adolescents (15-19)",
#                  ifelse(ofi$pt_age_yrs %in% 20:24, "Young adults (20-24)",
#                         ifelse(ofi$pt_age_yrs %in% 25:59, "Adults (25-59)",
#                                ifelse(ofi$pt_age_yrs %in% 60:100, "Older adults (60-100)", NA))))

ofi$Age <- ofi$pt_age_yrs

#Intubation 
ofi$Intubation1 <- ifelse(ofi$pre_intubated == 1, "Intubation",
                          ifelse(ofi$pre_intubated == 2, "No intubation",  
                                 ifelse(ofi$pre_intubated == 999, NA,
                                        ifelse(ofi$ed_intubated == 1, "Intubation",
                                               ifelse(ofi$ed_intubated == 2, "No intubation",  
                                                      ifelse(ofi$ed_intubated == 999, NA, NA))))))

#Intubation combined with ventilator days 
ofi$Intubation <- ifelse(ofi$Intubation1 == "No intubation", "No intubation",
                         ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days %in% 1:3, "Intubation 1-3 days",
                                ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days %in% 4:7, "Intubation 4-7 days",
                                       ifelse(ofi$Intubation1 == "Intubation" & ofi$hosp_vent_days > 7, "Intubation > 7 days", "Missing"))))

#Respiratory rate 
#ofi$RespiratoryRate <- ifelse(ofi$ed_rr_value %in% 10:29, "10-29",
#                              ifelse(ofi$ed_rr_value %in% 6:9, "6-9",
#                                     ifelse(ofi$ed_rr_value %in% 1:5, "1-5",
#                                            ifelse(ofi$ed_rr_value == 0, "0",
#                                                   ifelse(ofi$ed_rr_value > 29, "> 29", "Missing")))))

ofi$RespiratoryRate <- ofi$ed_rr_value

#Systolic blood pressure 
#ofi$SystolicBloodPressure <- ifelse(ofi$ed_sbp_value > 89, "> 89",
#                              ifelse(ofi$ed_sbp_value %in% 76:89, "76-89",
#                                     ifelse(ofi$ed_sbp_value %in% 50:75, "50-75",
#                                            ifelse(ofi$ed_sbp_value %in% 1:49, "1-49",
#                                                   ifelse(ofi$ed_sbp_value == 0, "0", "Missing")))))
ofi$SystolicBloodPressure <- ofi$ed_sbp_value

#Glasgow Coma Scale
#ofi$GlasgowComaScale <- ifelse(ofi$ed_gcs_sum %in% 13:15, "Mild 13-15",
#                               ifelse(ofi$ed_gcs_sum %in% 9:12, "Moderate 9-12",
#                               ifelse(ofi$ed_gcs_sum %in% 3:8, "Servere 3-8", "Missing")))

ofi$GlasgowComaScale <- ifelse(ofi$ed_gcs_sum == 99, NA,
                               ifelse(ofi$ed_gcs_sum == 999, NA,
                                      ifelse(ofi$ed_gcs_sum == 3, 3,
                                             ifelse(ofi$ed_gcs_sum == 4, 4,
                                                    ifelse(ofi$ed_gcs_sum == 5, 5,
                                                           ifelse(ofi$ed_gcs_sum == 6, 6,
                                                                  ifelse(ofi$ed_gcs_sum == 7, 7,
                                                                         ifelse(ofi$ed_gcs_sum == 8, 8,
                                                                                ifelse(ofi$ed_gcs_sum == 9, 9,
                                                                                       ifelse(ofi$ed_gcs_sum == 10, 10,
                                                                                              ifelse(ofi$ed_gcs_sum == 11, 11,
                                                                                                     ifelse(ofi$ed_gcs_sum == 12, 12,
                                                                                                            ifelse(ofi$ed_gcs_sum == 13, 13,
                                                                                                                   ifelse(ofi$ed_gcs_sum == 14, 14,
                                                                                                                          ifelse(ofi$ed_gcs_sum == 15, 15, NA)))))))))))))))



#Working hours: arrived between 8 am and 5 pm 
ofi$hour <- format(ofi$DateTime_ArrivalAtHospital, "%H")
class(ofi$hour)
ofi$WorkingHoursTF <- ifelse(ofi$hour == "08" | ofi$hour == "09" | ofi$hour == "10" | ofi$hour == "11" | ofi$hour == "12" | ofi$hour == "13" | ofi$hour == "14" | ofi$hour == "15" | ofi$hour == "16", TRUE, FALSE)
ofi$WorkingHours <- ifelse(ofi$WorkingHoursTF == TRUE, "Yes", 
                           ifelse(ofi$WorkingHoursTF == FALSE, "No", NA))

#Weekend: arrived on Saturday or Sunday 
ofi$Weekdays <- weekdays(ofi$DateTime_ArrivalAtHospital)
ofi$WeekendTF <- ifelse(ofi$Weekdays == "Saturday" | ofi$Weekdays == "Sunday", TRUE, FALSE)
ofi$Weekend <- ifelse(ofi$WeekendTF == TRUE, "Yes",
                      ifelse(ofi$WeekendTF == FALSE, "No", NA))

#Duty shift
ofi$Jour <- ifelse(ofi$Weekend == "Yes", 1,
                   ifelse(ofi$WorkingHours == "No", 1, 0))

#Injury Severity Score
#ofi$ISS <- ifelse(ofi$ISS < 9, "Mild < 9",
#                               ifelse(ofi$ISS %in% 9:15, "Moderate 9-15",
#                                      ifelse(ofi$ISS %in% 16:25, "Severe 16-25",
#                                             ifelse(ofi$ISS > 25, "Profound > 25", "Missing"))))

#Time to first CT
#ofi$TimeFCT <- ifelse(ofi$FirstTraumaDT_NotDone == 1, "No CT",
#                      ifelse(ofi$dt_ed_first_ct %in% 0:30, "0-30",
#                      ifelse(ofi$dt_ed_first_ct %in% 31:60, "31-60",
#                             ifelse(ofi$dt_ed_first_ct > 60, "> 60", "Missing"))))
ofi$TimeFCT <- ofi$dt_ed_first_ct

#Time to first intervention 
ofi$TimeFInt <- ofi$dt_ed_emerg_proc

#Pt ASA preinjury
ofi$ASApreinjury <- ifelse(ofi$pt_asa_preinjury == 1 | ofi$pt_asa_preinjury == 2, "ASA 1-2",
                           ifelse(ofi$pt_asa_preinjury %in% 3:7, "ASA 3-7",
                                  ifelse(ofi$pt_asa_preinjury == 999, NA, NA)))

#Days in the ICU 
#ofi$DaysinICU <- ofi$iva_dagar_n

#Survival after 30 days 
ofi$Survival <- ifelse(ofi$res_survival == 1, "Dead",
                       ifelse(ofi$res_survival == 2, "Alive",
                              ifelse(ofi$res_survival == 999, NA, NA)))


#OFI 
ofi$OpportunityForImprovement <- ifelse(ofi$ofi == TRUE, "Opportunity for improvement",
                                        ifelse(ofi$ofi == FALSE, "No opportunity for improvement", NA))

ofi$OpportunityForImprovement1 <- ifelse(ofi$OpportunityForImprovement == "Opportunity for improvement", 1,
                                         ifelse(ofi$OpportunityForImprovement == "No opportunity for improvement", 0, NA))


#TABLE 2: Sample characteristics
#Creating new table with defined data 
table1 <- ofi %>% 
  select(Sex, Age, Intubation, RespiratoryRate, SystolicBloodPressure, GlasgowComaScale, ISS, Jour, TimeFCT, TimeFInt, 
         ASApreinjury, Survival, OpportunityForImprovement)

table2 <- table1 %>%
  #mutate(Age = factor(Age, levels = c("Older adolescents (15-19)", "Young adults (20-24)", "Adults (25-59)", "Older adults (60-100)"))) %>%
  #mutate(Intubation = factor(Intubation, levels = c("None", "Pre-hospital", "Emergency department"))) %>%
  mutate(Intubation = factor(Intubation, levels = c("No intubation", "Intubation 1-3 days", "Intubation 4-7 days", "Intubation > 7 days"))) %>%
  #mutate(RespiratoryRate = factor(RespiratoryRate, levels = c(">29", "10-29", "6-9", "1-5", "0"))) %>%
  #mutate(ISS = factor(ISS, levels = c("Mild < 9", "Moderate 9-15", "Severe 16-25", "Profound > 25"))) %>%
  #mutate(GlasgowComaScale = factor(GlasgowComaScale, levels = c("Mild 13-15", "Moderate 9-12", "Severe 3-8"))) %>%
  #mutate(SystolicBloodPressure = factor(SystolicBloodPressure, levels = c(">89", "76-89", "50-75", "1-49", "0"))) %>%
  #mutate(TimeFCT = factor(TimeFCT, levels = c(">60", "31-60", "0-30", "No CT"))) %>%
  tbl_summary(by= OpportunityForImprovement,
              type = list(Jour ~ "dichotomous"),
              label =list(RespiratoryRate = "Respiratory rate",
                          SystolicBloodPressure = "Systolic blood pressure",
                          ISS = "Injury Severity Score",
                          GlasgowComaScale = "Glasgow Coma Scale",
                          TimeFCT = "Time to first CT", 
                          TimeFInt = "Time to first intervention", 
                          ASApreinjury = "ASA preinjury"),
              #DaysinICU = "Days in the ICU"),
              statistic = list(
                all_continuous() ~ "{mean} ({sd})",
                all_categorical() ~ "{n} ({p}%)"
              ),
              missing = "no",
              missing_text = "Missing",
              digits = all_continuous() ~ 2
  )  %>%
  modify_table_styling( 
    columns = label,
    rows = label == "Jour",
    footnote = "Arrival at the hospital on Saturday or Sunday, or arrival at the hospital before 8 am or after 5 pm") %>%
  bold_labels() %>% 
  add_overall(last = TRUE) %>% 
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 2. Sample Characteristics</div>") %>% 
  modify_footnote(
  ) %>%
  print()

#TABLE 3: Adjusted and unadjusted logistic regression
# Data Preparation
tablereg <- ofi %>% 
  select(Sex, Age, Intubation, RespiratoryRate, SystolicBloodPressure, GlasgowComaScale, ISS, Jour, TimeFCT, TimeFInt, 
         ASApreinjury, Survival, OpportunityForImprovement1)

tablereg$Intubation <- fct_relevel(tablereg$Intubation, "No intubation", "Intubation 1-3 days", "Intubation 4-7 days", "Intubation > 7 days")

# Unadjusted Table
table3a <- tbl_uvregression(data = tablereg,
                            method = glm,
                            y = OpportunityForImprovement1,
                            method.args = list(family = binomial),
                            label = list(
                              RespiratoryRate = "Respiratory rate",
                              SystolicBloodPressure = "Systolic blood pressure",
                              GlasgowComaScale = "Glasgow Coma Scale",
                              TimeFInt = "Time to first intervention",
                              ASApreinjury = "ASA preinjury",
                              TimeFCT = "Time to first CT"
                            )) %>%
  bold_labels() %>%
  bold_p(t = 0.05)

print(table3a)

# Adjusted Table
adjusted_table <- glm(OpportunityForImprovement1 ~ Sex + Age + Intubation + RespiratoryRate + SystolicBloodPressure +  ISS + GlasgowComaScale + Jour + TimeFCT + TimeFInt + ASApreinjury + Survival, family = binomial, data = tablereg) 

table3b <- tbl_regression(adjusted_table,
                          label = list(RespiratoryRate = "Respiratory rate",
                                       SystolicBloodPressure = "Systolic blood pressure",
                                       GlasgowComaScale = "Glasgow Coma Scale",
                                       TimeFInt = "Time to first intervention",
                                       ASApreinjury = "ASA preinjury",
                                       TimeFCT = "Time to first CT")) %>%
  bold_labels() %>%
  bold_p() %>%
  bold_p(t = 0.05)

print(table3b)

# Merging Tables
table3_merge <- tbl_merge(tbls = list(table3a, table3b),
                          tab_spanner = c("**Unadjusted**", "**Adjusted**")) %>%
  #modify_caption("**Table 3.** Unadjusted and adjusted logistic regression analyses of associations between patient level factors and opportunities for improvement")
  modify_caption("<div style='text-align: left; font-weight: bold; color: black'>Table 3. Unadjusted and adjusted logistic regression analyses of associations between patient level factors and opportunities for improvement</div>")

print(table3_merge)

