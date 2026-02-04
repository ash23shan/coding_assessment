# Program Name:  01_create_adsl.R
# Author:        Ashwini Yermal Shanbhogue
# Date:          2026-01-30
# Purpose:       Create ADSL from pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex, pharmaversesdtm::ds, and pharmaversesdtm::ae
# Input:         pharmaversesdtm::dm, pharmaversesdtm::vs, pharmaversesdtm::ex, pharmaversesdtm::ds, pharmaversesdtm::ae
# Output:        ADSL.RDS, ADSL.xlsx
# Notes:

# Install and load required packages
# install.packages(c("admiral", "pharmaversesdtm", "dplyr", "lubridate", "stringr", "purrr", "styler", "writexl", "logrx", "logrxaddin"))

library("admiral")
library("dplyr", warn.conflicts = FALSE)
library("pharmaversesdtm")
library("lubridate")
library("stringr")
library("purrr")
library("logrx")
library("logrxaddin")
library("writexl")
library("styler")

# Read SDTM data
dm <- pharmaversesdtm::dm
ds <- pharmaversesdtm::ds
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ae <- pharmaversesdtm::ae

# Assign required DM variables to adsl object
adsl <- dm %>% select(-DOMAIN)

# Obtain snapshot of current adsl
# str(adsl) # Display structure of current adsl
# unique_values_list <- map(adsl, unique) # Apply unique function to all variables in current adsl using map()
# print(unique_values_list) # Display resulting list

# Create ADSL variables
# AGEGR9 and AGEGR9N
agegr9_lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  is.na(AGE),          "Missing",        4,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  !is.na(AGE),             ">50",        3
)

adsl <- adsl %>% derive_vars_cat(definition = agegr9_lookup)

# TRTSDTM and TRTSTMF
# Create EXSTDTM and EXENDTM variables as pre-processing steps and impute missing time as 00:00:00 in EXSTDTM
ex_dtm <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    time_imputation = "00:00:00"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# Format EXSTDTM to ISO8601 datetime format
lubridate::format_ISO8601(ex_dtm$EXSTDTM)

# Merge dataset created in pre-processing step to create TRTSDTM as datetime of first exposure and the associated imputation flag, TRTSTMF
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_dtm,
    # Filter for valid dose as defined in specifications
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# ITTFL
# Set to Y if ARM is not missing, else N
adsl <- adsl %>% mutate(ITTFL = if_else(!is.na(ARM), "Y", "N"))

# LSTAVLDT
# Derive TRTEDT to use in LSTAVLDT calculation
adsl_trtedt <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_dtm,
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTEDTM))

# Use derive_vars_extreme_event to find the last date across domains
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      # vital signs event
      event(
        dataset_name = "vs",
        condition = !is.na(VSDTC) & (!is.na(VSSTRESN) | !is.na(VSSTRESC)) & (nchar(VSDTC) == 10),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(VSDTC))
      ),
      # ae event
      event(
        dataset_name = "ae",
        condition = !is.na(AESTDTC) & (nchar(AESTDTC) == 10),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(AESTDTC))
      ),
      # ds event
      event(
        dataset_name = "ds",
        condition = !is.na(DSSTDTC) & (nchar(DSSTDTC) == 10),
        set_values_to = exprs(LSTAVLDT = convert_dtc_to_dt(DSSTDTC))
      ),
      # ex/ treatment date event
      event(
        dataset_name = "adsl_trtedt",
        condition = !is.na(TRTEDT),
        set_values_to = exprs(LSTAVLDT = TRTEDT)
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl_trtedt = adsl_trtedt),
    order = exprs(LSTAVLDT),
    mode = "last",
    check_type = "warning",
    new_vars = exprs(LSTAVLDT)
  )

# Ad hoc QC
# Create test datasets using conditions that were used to create events for derive_vars_extreme_event function
# vs_test <- vs %>% filter(!is.na(VSDTC) & (!is.na(VSSTRESN) | !is.na(VSSTRESC)) & (nchar(VSDTC) == 10)) %>% group_by(USUBJID) %>% summarize(last_date_v = max(convert_dtc_to_dt(VSDTC), na.rm = TRUE))
# ds_test <- ds %>% filter(!is.na(DSSTDTC) & (nchar(DSSTDTC) == 10)) %>% group_by(USUBJID) %>% summarize(last_date_d = max(convert_dtc_to_dt(DSSTDTC), na.rm = TRUE))
# ae_test <- ae %>% filter(!is.na(AESTDTC) & (nchar(AESTDTC) == 10)) %>% group_by(USUBJID) %>% summarize(last_date_a = max(convert_dtc_to_dt(AESTDTC), na.rm = TRUE))
# adsl_test <- adsl_trtedt %>% filter(!is.na(TRTEDT)) %>% group_by(USUBJID) %>% summarize(last_date_ad = max(TRTEDT, na.rm = TRUE))

# Merge test datasets and view for visual QC
# all <- list(adsl_test,ae_test,ds_test,vs_test)
# test <- all %>% reduce(full_join, by = 'USUBJID')

# Save created dataframe
# RDS object
# saveRDS(adsl, file = "question_02_adam/output/ADSL.RDS")
# XLSX file
# write_xlsx(adsl, "question_02_adam/output/ADSL.xlsx", col_names = TRUE, format_headers = TRUE)
