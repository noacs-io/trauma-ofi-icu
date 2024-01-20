## INTRUCTIONS 
## This is your project's main script file and together with
## manuscript.Rmd it provides and entry point for you and other people
## coming to the project. The code in this file should give an outline
## of the different steps conducted in your study, from importing data
## to producing results.

## This file should be relatively short, and most of the heavy
## lifting should be done by specialised functions. These functions
## live in the folder functions/ and you create a new function using
## create_function().

## Source all functions (if you tick the box "Source on save" in
## RStudio functions will be automatically sourced when you save
## them). They all need to be sourced however when you compile your
## manuscript file or run this file as a job, as that happens in a
## clean R session.



## install.packages(devtools)
## devtools::install_github("martingerdin/noacsr")
## devtools::install_github("martingerdin/rofi")

## Load packages
library(dotenv)
library(noacsr)
library(rofi)
noacsr::source_all_functions()

## Import data
data <- import_data(test = TRUE)

## Merge data
merged.data <- merge_data(data, test = TRUE)

## Add outcome variable ofi
## merged.data$ofi <- create_ofi(merged.data)
## The function "create_ofi" creates OFI variable which is either yes or no. 
## Yes: the case was flagged and reviewed in a meeting and the consensus was that there were OFI, or that the patient died and the death was determined as preventable or potentially preventable. 
## No: the consensus was that there were no OFI or the nurses in the initial review did not send the case for review bc everything was ok.
## NA: the case was never selected for review or the patient died but whether the death was preventable is registered as unknown.
## Produces a list of Yes, No or NA for OFI or not. 




##16/1: 
Gender <- merged.data$Gender
Age <- merged.data$pt_age_yrs
GCS <- merged.data$pre_gcs_sum
RR <- merged.data$ed_rr_value
SBP <- merged.data$ed_sbp_value
ISS <- merged.data$ISS
Intubated <- merged.data$ed_intubated
OFI <- merged.data$ofi

  
## Gender, survival after 30 days, Glasgow Coma Scale (GCS), respiratory rate, 
## systolic blood pressure, working hours, weekend, 
## time from arrival at the hospital until first computed tomography (CT) and if the patient was intubated
## age, ISS

table <- data.frame(Gender, 
           Age, 
           merged.data$uppfolj_30_dgr, 
           GCS, 
           RR, 
           SBP, 
           merged.data$DateTime_Of_Trauma,
           merged.data$dt_ed_first_ct, 
           Intubated, 
           ISS,
           merged.data$IVA_efter_TE,
           merged.data$ofi)

tbl_summary(table)

table %>%
  tbl_summary(by = Gender) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Table 1. Sample Characteristics**") %>%
  bold_labels()


## Övrigt

##Vilken för survival after 30 days, GCS, working hours, weekend??

OFI
merged.data$ofi
## Counting NAs in merged.data$ofi
my.na <- is.na(merged.data$ofi)
sum(my.na)
##5282 NAs dock inkluderar både "Yes" och "No"
notna.ofi <- !is.na(merged.data$ofi)
sum(notna.ofi)
## 6582 OFIs identified in the entire merged.data dataset
length(merged.data$ofi)
##11 864 elements of OFI
sum(merged.data$ofi == "NA") ##Funkar EJ


GENDER
sum(merged.data$Gender == "M" & merged.data$pt_age_yrs == 18)
sum(merged.data$Gender == "K")

PT YEARS
sum(merged.data$Gender == "M" & merged.data$pt_age_yrs == "> 15 & < 20")
sum(merged.data$Gender == "M" & 14< merged.data$pt_age_yrs <20)
##Behöver ändras
hist(merged.data$pt_age_yrs, breaks = 7)

GCS
sum(merged.data$pre_gcs_sum == 15)

RR
sum(merged.data$Gender == "M" & merged.data$ed_rr_value > 15)

SBP
sum(merged.data$ed_sbp_value > 15)

DATA KOPPLAT TILL IVA
merged.data$iva_vardtillfallen_n
na.iva_vardtillfallen_n <- is.na(merged.data$iva_vardtillfallen_n)
sum(na.iva_vardtillfallen_n)
## 11847 NAs in vardtillfallen 
notna.iva_vardtillfallen_n <- !is.na(merged.data$iva_vardtillfallen_n)
sum(notna.iva_vardtillfallen_n)
## 17 patients who have multiple IVA vardtillfällen

merged.data$IVA_efter_TE
notna.ivaefterte <- !is.na(merged.data$ofi)
sum(notna.ivaefterte)
## 6582 patienter gick till IVA efter TE som är trauma? samma som OFIs totalt 

merged.data$IVA_eft_avd
notna.ivaeftavd <- !is.na(merged.data$IVA_eft_avd)
sum(notna.ivaeftavd)
## 17 IVA efter avdelning





