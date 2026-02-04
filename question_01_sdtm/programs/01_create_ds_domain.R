# Program Name:  01_create_ds_domain.R
# Author:        Ashwini Yermal Shanbhogue
# Date:          2026-01-28
# Purpose:       Create DS SDTM domain from pharmaverseraw::ds_raw data
# Input:         pharmaverseraw::ds_raw
# Output:        DS.RDS, DS.xlsx, DS.XPT
# Notes:

# Install and load required packages
# install.packages(c("sdtm.oak", "pharmaverseraw", "pharmaversesdtm", "dplyr", "purrr", "readr", "tidyr", "lubridate", "styler", "haven", "logrx", "logrxaddin", "writexl"))

library("sdtm.oak")
library("pharmaverseraw")
library("pharmaversesdtm")
library("purrr")
library("readr")
library("dplyr")
library("tidyr")
library("lubridate")
library("writexl")
library("haven")
library("logrx")
library("logrxaddin")
library("styler")

# Read ds_raw data
ds_raw <- pharmaverseraw::ds_raw

# Create new date class column containing date value from IT.DSSTDAT to be used for sorting
ds_raw <- ds_raw %>% mutate(DSSTDAT = lubridate::mdy(IT.DSSTDAT))

# Since the collected values for the NCOMPLT codelist in the Controlled Terminology file available on Pharmaverse GitHub were not matching the
# IT.DSDECOD column in the ds_raw data and since I wanted to avoid any errors/ warnings appearing if this program were to be run using that CT file,
# I am creating study_ct programmatically. Also missing from the GitHub CT file are the PROTMLST and OTHEVENT codelists, which I am also adding
# to study_ct programmatically. Since the OTHEVENT codelist is extensible, I have added the collected terms, 'Final Lab Visit' and
# 'Final Retrieval Visit', which appear in the OTHERSP column of ds_raw data, to it.
study_ct <-
  data.frame(
    stringsAsFactors = FALSE,
    codelist_code = c(
      "C66727", "C66727",
      "C66727", "C66727", "C66727", "C66727", "C66727", "C66727",
      "C66727", "C66727", "C114118", "C114118", "C114118", "C114118",
      "C114118", "C114118", "C114118", "C114118", "C114118", "C114118",
      "C114118", "C114118", "C114118", "C150811", "C150811", "C150811",
      "C150811", "C150811", "C150811", "C74558", "C74558", "C74558"
    ),
    term_code = c(
      "C41331", "C25250",
      "C28554", "C48226", "C48227", "C48250", "C142185", "C49628",
      "C49632", "C49634", "C186210", "C176358", "C132447", "C164344", "C161417",
      "C161418", "C202445", "C202444", "C16735", "C186211", "C176357", "C114209", "C186212", "C186208",
      "C204692", "C142742", "C170610", "", "", "C74590", "C150824", "C74588"
    ),
    term_value = c(
      "ADVERSE EVENT",
      "COMPLETED", "DEATH", "LACK OF EFFICACY", "LOST TO FOLLOW-UP", "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
      "SCREEN FAILURE", "STUDY TERMINATED BY SPONSOR", "WITHDRAWAL BY SUBJECT", "DECLINED TO CONTINUE INTO NEXT TRIAL ELEMENT",
      "DECLINED TO CONTINUE INTO SURVIVAL FOLLOW-UP", "ELIGIBILITY CRITERIA MET", "ELIGIBILITY CRITERIA NOT MET",
      "ENTERED INTO TRIAL", "INFORMED ASSENT OBTAINED", "INFORMED CONSENT DECLINED FOR PROTOCOL-SPECIFIED ACTIVITY",
      "INFORMED CONSENT OBTAINED FOR PROTOCOL-SPECIFIED ACTIVITY", "INFORMED CONSENT OBTAINED", "OPTED TO CONTINUE INTO NEXT TRIAL ELEMENT",
      "OPTED TO CONTINUE INTO SURVIVAL FOLLOW-UP", "RANDOMIZED", "RE-RANDOMIZED", "SITE TRANSFER", "SUBJECT ENTERED ADDITIONAL RESEARCH STUDY",
      "TREATMENT UNBLINDED", "WITHDRAWAL OF CONSENT FROM PROTOCOL-SPECIFIED ACTIVITY", "FINAL LAB VISIT", "FINAL RETRIEVAL VISIT",
      "DISPOSITION EVENT", "OTHER EVENT", "PROTOCOL MILESTONE"
    ),
    collected_value = c(
      "Adverse Event",
      "Completed", "Death", "Lack of Efficacy", "Lost to Follow-Up",
      "Physician Decision", "Protocol Violation",
      "Screen Failure", "Study Terminated by Sponsor",
      "Withdrawal by Subject", "", "", "", "", "", "", "", "", "", "", "", "Randomized", "", "", "", "", "",
      "Final Lab Visit", "Final Retrieval Visit", "Disposition Event", "Other Event", "Protocol Milestone"
    ),
    term_preferred_term = c(
      "AE", "Completed", "Died",
      NA, NA, NA, "Violation",
      "Failure to Meet Inclusion/Exclusion Criteria", NA, "Dropout", "", "", "", "", "", "", "", "", "",
      "", "", "", "", "", "", "", "", "", "", "Protocol Disposition Event", "Non-Protocol Disposition", "Event	Protocol Milestone"
    ),
    term_synonyms = c(
      "ADVERSE EVENT",
      "COMPLETE", "Death", NA, NA, NA, NA, NA, NA,
      "Discontinued Participation", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""
    )
  )

# Read DM data
dm <- pharmaversesdtm::dm

# Obtain snapshot of raw data
# str(ds_raw) # Display structure of ds_raw
# unique_values_list <- map(ds_raw, unique) # Apply unique function to all variables in ds_raw using map()
# print(unique_values_list) # Display resulting list

# Start creating DS variables
# VISIT and VISITNUM
# Create dummy Trial Visits (TV) domain
tv <- data.frame(
  stringsAsFactors = FALSE,
  STUDY = c(
    "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01",
    "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01", "CDISCPILOT01"
  ),
  domain = c("TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV", "TV"),
  VISITNUM = c("1.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "201.0"),
  VISIT = c(
    "SCREENING 1", "BASELINE", "WEEK 2", "WEEK 4", "AMBUL ECG REMOVAL", "WEEK 6", "WEEK 8", "WEEK 12",
    "WEEK 16", "WEEK 20", "WEEK 24", "WEEK 26", "RETRIEVAL"
  )
)

# Create VISIT variable from INSTANCE variable of ds_raw
ds_raw <- ds_raw %>% mutate(VISIT = toupper(INSTANCE))

# Join TV with ds_raw on VISIT variable to get the VISITNUMs from TV
ds_raw <- ds_raw %>%
  left_join(tv %>% select(STUDY, VISITNUM, VISIT),
    by = c("STUDY", "VISIT")
  )

# If VISITNUM is missing, make VISIT also missing to reflect how it would appear in an actual trial
ds_raw <- ds_raw %>%
  mutate(
    VISIT = if_else(is.na(VISITNUM), NA_character_, VISIT)
  )

# Sort by PATNUM and date form of date subject completed/ discontinued from study, group by PATNUM and
# create missing VISITNUMs for unscheduled visits as 'VISITNUM of the previous visit.1'
ds_raw <- ds_raw %>%
  arrange(PATNUM, DSSTDAT) %>%
  mutate(VISITNUM = as.numeric(VISITNUM)) %>%
  group_by(PATNUM) %>%
  mutate(
    base_visit = VISITNUM,
  ) %>%
  fill(base_visit, .direction = "down") %>%
  group_by(PATNUM, base_visit) %>%
  mutate(
    VISITNUM = if_else(
      is.na(VISITNUM),
      base_visit + (row_number() - 1) * 0.1,
      VISITNUM
    )
  ) %>%
  dplyr::ungroup() %>%
  select(-base_visit)

# These two edge cases did not have a Screening 1 visit before Baseline visit as expected
# but do have an unscheduled visit before the Baseline visit. Since unscheduled VISITNUM cannot be
# created with reference to the VISITNUM above it, VISITNUM is assigned as 1.1
ds_raw <- ds_raw %>%
  mutate(
    VISITNUM = if_else((is.na(VISIT) & is.na(VISITNUM) & PATNUM %in% c("703-1197", "708-1236")), 1.1, VISITNUM)
  )

# Create missing/ unscheduled VISITs as the word UNSCHEDULED followed by the unscheduled VISITNUM
ds_raw <- ds_raw %>%
  mutate(
    VISIT = if_else(is.na(VISIT), paste0("UNSCHEDULED ", VISITNUM), VISIT)
  )

# Split ds_raw dataset into two based on values in IT.DSDECOD to allow DSDECOD variable
# to be created using the right codelists
ds_rand_pre <- ds_raw %>% filter(IT.DSDECOD == "Randomized")
ds_rest_pre <- ds_raw %>% filter(IT.DSDECOD != "Randomized" | is.na(IT.DSDECOD))

# Create oak ID variables, so that SDTM and raw data variables are linked properly in both datasets
ds_rand_pre <- ds_rand_pre %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_rand_pre"
  )

ds_rest_pre <- ds_rest_pre %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_rest_pre"
  )

# Continue to create separately using the two datasets, rest of DS variables
# Map Topic variable DSTERM
ds_rand <- ds_rand_pre %>%
  # Map IT.DSTERM if OTHERSP is null
  {
    assign_no_ct(
      raw_dat = condition_add(ds_rand_pre, is.na(OTHERSP)),
      raw_var = "IT.DSTERM",
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    )
  } %>%
  # Map OTHERSP if OTHERSP is NOT null
  {
    assign_no_ct(
      raw_dat = condition_add(ds_rand_pre, !is.na(OTHERSP)),
      raw_var = "OTHERSP",
      tgt_dat = .,
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    )
  }

ds_rest <- ds_rest_pre %>%
  # Map IT.DSTERM if OTHERSP is null
  {
    assign_no_ct(
      raw_dat = condition_add(ds_rest_pre, is.na(OTHERSP)),
      raw_var = "IT.DSTERM",
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    )
  } %>%
  # Map OTHERSP if OTHERSP is NOT null
  {
    assign_no_ct(
      raw_dat = condition_add(ds_rest_pre, !is.na(OTHERSP)),
      raw_var = "OTHERSP",
      tgt_dat = .,
      tgt_var = "DSTERM",
      id_vars = oak_id_vars()
    )
  }

# Map rest of the variables
# DSCAT
ds_rand <- ds_rand %>%
  # DSCAT is PROTOCOL MILESTONE if IT.DSDECOD equals Randomized
  {
    hardcode_ct(
      raw_dat = ds_rand_pre,
      raw_var = "IT.DSDECOD",
      tgt_dat = .,
      tgt_var = "DSCAT",
      tgt_val = "PROTOCOL MILESTONE",
      ct_spec = study_ct,
      ct_clst = "C74558",
      id_vars = oak_id_vars()
    )
  }

ds_rest <- ds_rest %>%
  # DSCAT is DISPOSITION EVENT if IT.DSDECOD does NOT equal Randomized
  {
    hardcode_ct(
      raw_dat = condition_add(ds_rest_pre, is.na(OTHERSP)),
      raw_var = "IT.DSDECOD",
      tgt_dat = .,
      tgt_var = "DSCAT",
      tgt_val = "DISPOSITION EVENT",
      ct_spec = study_ct,
      ct_clst = "C74558",
      id_vars = oak_id_vars()
    )
  } %>%
  # DSCAT is OTHER EVENT if OTHERSP is NOT null
  {
    hardcode_ct(
      raw_dat = condition_add(ds_rest_pre, !is.na(OTHERSP)),
      raw_var = "OTHERSP",
      tgt_dat = .,
      tgt_var = "DSCAT",
      tgt_val = "OTHER EVENT",
      ct_spec = study_ct,
      ct_clst = "C74558",
      id_vars = oak_id_vars()
    )
  }

# DSDECOD
ds_rand <- ds_rand %>%
  # Map IT.DSDECOD if OTHERSP is null
  # Use PROTMLST codelist to map 'Randomized'
  {
    assign_ct(
      raw_dat = ds_rand_pre,
      raw_var = "IT.DSDECOD",
      tgt_dat = .,
      tgt_var = "DSDECOD",
      ct_spec = study_ct,
      ct_clst = "C114118",
      id_vars = oak_id_vars()
    )
  }

ds_rest <- ds_rest %>%
  # Map IT.DSDECOD if OTHERSP is null
  # Use NCOMPLT codelist to map all terms other than 'Randomized' from IT.DSDECOD
  {
    assign_ct(
      raw_dat = condition_add(ds_rest_pre, is.na(OTHERSP)),
      raw_var = "IT.DSDECOD",
      tgt_dat = .,
      tgt_var = "DSDECOD",
      ct_spec = study_ct,
      ct_clst = "C66727",
      id_vars = oak_id_vars()
    )
  } %>%
  # Map OTHERSP if OTHERSP is NOT null
  # Use OTHEVENT codelist
  {
    assign_ct(
      raw_dat = condition_add(ds_rest_pre, !is.na(OTHERSP)),
      raw_var = "OTHERSP",
      tgt_dat = .,
      tgt_var = "DSDECOD",
      ct_spec = study_ct,
      ct_clst = "C150811",
      id_vars = oak_id_vars()
    )
  }

# DSDTC and # DSSTDTC
ds_rand <- ds_rand %>%
  {
    assign_datetime(
      raw_dat = ds_rand_pre,
      raw_var = c("DSDTCOL", "DSTMCOL"),
      tgt_dat = .,
      tgt_var = "DSDTC",
      raw_fmt = c("mm-dd-yyyy", "H:M"),
      id_vars = oak_id_vars(),
    )
  } %>%
  {
    assign_datetime(
      raw_dat = ds_rand_pre,
      raw_var = "IT.DSSTDAT",
      tgt_dat = .,
      tgt_var = "DSSTDTC",
      raw_fmt = c("mm-dd-yyyy"),
      id_vars = oak_id_vars(),
    )
  }

ds_rest <- ds_rest %>%
  {
    assign_datetime(
      raw_dat = ds_rest_pre,
      raw_var = c("DSDTCOL", "DSTMCOL"),
      tgt_dat = .,
      tgt_var = "DSDTC",
      raw_fmt = c("mm-dd-yyyy", "H:M"),
      id_vars = oak_id_vars(),
    )
  } %>%
  {
    assign_datetime(
      raw_dat = ds_rest_pre,
      raw_var = "IT.DSSTDAT",
      tgt_dat = .,
      tgt_var = "DSSTDTC",
      raw_fmt = c("mm-dd-yyyy"),
      id_vars = oak_id_vars(),
    )
  }

# Stack and sort ds_rand and ds_rest datasets created from the two raw datasets created from splitting the original ds_raw
ds <- bind_rows(ds_rand, ds_rest) %>% arrange(patient_number, DSSTDTC)

# Create SDTM derived variables
ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01", "-", ds_raw$PATNUM),
    VISIT = ds_raw$VISIT,
    VISITNUM = ds_raw$VISITNUM,
  ) %>%
  derive_seq(
    tgt_dat = .,
    tgt_var = "DSSEQ",
    rec_vars = c("STUDYID", "DOMAIN", "USUBJID", "DSSTDTC", "DSCAT", "DSDECOD"),
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY",
    merge_key = "USUBJID"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )

# Save created dataframe
# RDS object
# saveRDS(ds, file = "question_01_sdtm/output/DS.RDS")
# XLSX file
# write_xlsx(ds, "question_01_sdtm/output/DS.xlsx", col_names = TRUE, format_headers = TRUE)
# XPT file for Pinnacle validation
# write_xpt(ds, "question_01_sdtm/output/DS.xpt")
