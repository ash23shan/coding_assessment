# Program Name:  01_create_ae_summary_table.R
# Author:        Ashwini Yermal Shanbhogue
# Date:          2026-01-31
# Purpose:       Create AE summary table from pharmaverseadam::adae and pharmaverseadam::adsl
# Input:         pharmaverseadam::adae, pharmaverseadam::adsl
# Output:        Summary Table of TEAEs.html
# Notes:

# Install and load required packages
# install.packages(c("pharmaverseadam", "dplyr", "gtsummary", "flextable", "logrx", "logrxaddin"))

library("pharmaverseadam")
library("dplyr")
library("gtsummary")
library("flextable")
library("logrx")
library("logrxaddin")

# Read ADSL and ADAE data
adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

# Filter for safety population with treatment emergent AEs and select required columns
ae <- adae %>%
  filter(
    SAFFL == "Y",
    TRTEMFL == "Y"
  ) %>%
  select(STUDYID, USUBJID, AETERM, AESOC, SAFFL, TRTEMFL)

# Create hierarchical table and sort by descending frequency
tbl <- adae %>%
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = list(..ard_hierarchical_overall.. = "Treatment Emergent AEs")
  ) %>%
  add_overall(
    last = TRUE, col_label = "**Total** \nN = {N}"
  ) %>%
  sort_hierarchical()

# View table
tbl

# Create flextable and save as html
tbl %>%
  as_flex_table() %>%
  flextable::set_table_properties("autofit") %>%
  save_as_html(tbl, path = "question_03_tlg/output/Summary Table of TEAEs.html")
