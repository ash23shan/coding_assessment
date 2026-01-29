# Program Name:  01_create_ds_domain.R
# Author:        Ashwini Yermal Shanbhogue
# Date:          2026-01-28
# Purpose:       Create DS SDTM domain from pharmaverseraw::ds_raw data
# Input:         pharmaverseraw::ds_raw
# Output:        
# Notes:         

# Install and load required packages
install.packages(c("sdtm.oak", "pharmaverseraw", "purrr", "readr", "styler"))

library("sdtm.oak")
library("pharmaverseraw")
library("purrr")
library("readr")

# Read raw data and CT file
ds_raw <- pharmaverseraw::ds_raw
sdtm_ct <- read_csv("question_01_sdtm/metadata/sdtm_ct.csv", col_types = cols(
  codelist_code = col_character(),
  term_code = col_character(),
  term_value = col_character(),
  collected_value = col_character(),
  term_preferred_term = col_character(),
  term_synonyms = col_character()
))

# Obtain snapshot of raw data
str(ds_raw) # Display structure of ds_raw
unique_values_list <- map(ds_raw, unique) # Apply unique function to all variables in ds_raw using map() 
print(unique_values_list) # Display resulting list

