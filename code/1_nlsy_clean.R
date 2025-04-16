# ===================================================================
# NLSY97 Data Cleaning Script
# ===================================================================
# Author: Luzia Bruckamp
# Date: 16/04/2025
# Purpose: This script cleans the NLSY97 dataset for analysis of education and fertility patterns
# 
# Input: nlsy97.csv - Raw NLSY97 data
# Output: cleaned_data_nlsy.csv - Processed dataset with education and fertility variables
#
# R version: R 4.2.2
# Required packages: tidyverse, lubridate
# ===================================================================
#
# IMPORTANT: Before running this script, change the directory path below
# to the folder where you've saved both this script and the nlsy97.csv file
# ===================================================================

# Set working directory - EDIT THIS LINE to match your file location
wd <- '/Users/luzia/Documents/blog'  # <-- CHANGE THIS PATH
setwd(wd)

# Verify data file exists
if (!file.exists("nlsy97.csv")) {
  stop("nlsy97.csv not found in ", getwd(), 
       "\nPlease check that:\n",
       "1. The data file is in the directory specified above\n",
       "2. You've correctly modified the path in the 'wd' variable")
}


cat("Found data file. Installing packages if needed and beginning data cleaning...\n")

# Ensure required packages are installed
required_packages <- c("tidyverse", "lubridate")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load required packages
library(tidyverse)
library(lubridate)

############## load data ###################

# rename a few variables and filter for active cases
# (sampling_weight is 0 if the respondent did not actually
# participate in this round)
data <- read.csv('nlsy97.csv') %>%
  rename(ID = R0000100,
         sampling_weight = U4958700,
         gross_family_income = U4949700,
         interview_month = U4953401,
         interview_year = U4953402) %>%
  filter(sampling_weight != 0)

#### Clean the education dates ####

# Get variable names starting with E511
# these document enrollment in education in each month
vars <- grep("^E511", names(data), value = TRUE)

# Function to parse dates from variable names
# The months are coded in the last 4 digits of the variable name
# the first of these digits gives the decade (1990, 2000, 2010, 2020)
# the second digit gives the year in the decade
# the last two digits give the month
parse_date <- function(varname) {
  # varname example: 'E5111701'
  code <- substr(varname, 5, 8)
  decade <- as.numeric(substr(code, 1, 1))
  year_in_decade <- as.numeric(substr(code, 2, 2))
  month <- as.numeric(substr(code, 3, 4))
  
  # Calculate the year based on the decade code
  year <- ifelse(decade == 1, 1990 + year_in_decade,
                 ifelse(decade == 2, 2000 + year_in_decade,
                        ifelse(decade == 3, 2010 + year_in_decade,
                               ifelse(decade == 4, 2020 + year_in_decade, NA))))
  
  # Create a date object
  date <- as.Date(paste(year, month, "01", sep = "-"))
  return(date)
}

# Create a data frame mapping variable names to dates
vars_dates <- data.frame(
  varname = vars,
  date = sapply(vars, parse_date),
  stringsAsFactors = FALSE
)

# Pivot data to long format
data_long <- data %>%
  select(ID, all_of(vars)) %>%
  pivot_longer(
    cols = -ID,
    names_to = "varname",
    values_to = "value"
  )

# Join with vars_dates to get the corresponding dates
data_long <- data_long %>%
  left_join(vars_dates, by = "varname")

# Filter for enrollment codes where:
# 2 = 2-year college
# 3 = 4-year college 
# 4 = Graduate program
data_long_filtered <- data_long %>%
  filter(value %in% c(2, 3, 4))

# Compute statistics for each ID and enrollment code
stats_all_codes <- data_long_filtered %>%
  group_by(ID, value) %>%
  summarise(
    first_date = min(date),
    last_date = max(date),
    total_count = n(),
    .groups = 'drop'
  )

# Pivot wider to get separate columns for each code
stats_wide <- stats_all_codes %>%
  pivot_wider(
    id_cols = ID,
    names_from = value,
    values_from = c(first_date, last_date, total_count),
    names_glue = "{.value}_{value}"
  )

# Ensure date columns remain as Date objects
date_columns <- grep("^(first_date|last_date)_", names(stats_wide), value = TRUE)
stats_wide <- stats_wide %>%
  mutate(across(all_of(date_columns), as.Date, origin = "1970-01-01"))

# Merge the computed statistics back into the original data
data_final <- data %>%
  left_join(stats_wide, by = "ID")

#### Clean the fertility information ####

# Create child indices and variable names
# The children from 1 to 15 are coded with 32 to 46
# in the variable names
child_indices <- 1:15
child_nums <- 32:46

child_vars <- data.frame(
  child_index = child_indices,
  child_num = child_nums,
  var_month = paste0('U494', child_nums, '00'),
  var_year = paste0('U494', child_nums, '01'),
  stringsAsFactors = FALSE
)

# Check that variables exist in data
vars_to_check <- c(child_vars$var_month, child_vars$var_year)
missing_vars <- vars_to_check[!vars_to_check %in% names(data)]
if(length(missing_vars) > 0) {
  warning('The following variables are missing in the data:', paste(missing_vars, collapse = ', '))
}

# Proceed only with variables that exist in the data
existing_vars <- vars_to_check[vars_to_check %in% names(data)]

# Reshape data into long format
data_long <- data %>%
  select(ID, all_of(existing_vars)) %>%
  pivot_longer(
    cols = -ID,
    names_to = c('child_num', 'suffix'),
    names_pattern = 'U494(\\d{2})(\\d{2})',
    values_to = 'value'
  ) %>%
  mutate(
    child_num = as.numeric(child_num),
    child_index = child_num - 31,
    suffix = case_when(
      suffix == '00' ~ 'month',
      suffix == '01' ~ 'year',
      TRUE ~ NA_character_
    )
  ) %>%
  select(ID, child_index, suffix, value)

# Pivot wider to get 'month' and 'year' columns
data_long <- data_long %>%
  pivot_wider(
    names_from = 'suffix',
    values_from = 'value'
  ) %>%
  mutate(
    month = as.numeric(month),
    year = as.numeric(year),
    date_of_birth = as.Date(paste(year, month, '01', sep = '-'), format = '%Y-%m-%d')
  )

# Spread date_of_birth to wide format
data_wide <- data_long %>%
  select(ID, child_index, date_of_birth) %>%
  pivot_wider(
    names_from = 'child_index',
    values_from = 'date_of_birth',
    names_prefix = 'date_of_birth_'
  )

# Calculate total number of children
data_wide <- data_wide %>%
  mutate(
    total_children = rowSums(!is.na(select(., starts_with('date_of_birth_'))))
  )

# Merge back to original data
data_final <- data_final %>%
  left_join(data_wide, by = 'ID')

#### Clean up final dataset by renaming and selecting ####

data_final <- data_final %>%
  rename(sex = R0536300,
         dob_month = R0536401,
         dob_year = R0536402,
         race = R1482600,
         cohab_1_month = Z9073000,
         cohab_1_year = Z9073001,
         num_cohabs = Z9122900,
         marry_1_month = Z9073200,
         marry_1_year = Z9073201,
         marriage_ended = Z9073400,
         marry_end_month = Z9073500,
         marry_end_year = Z9073501,
         num_marry = Z9123000)

# Select the generated and renamed variables
data_clean <- data_final %>%
  select(
    # ID variable
    ID,
    
    # Renamed variables
    sex, dob_month, dob_year, race,
    cohab_1_month, cohab_1_year, num_cohabs,
    marry_1_month, marry_1_year, marriage_ended,
    marry_end_month, marry_end_year, num_marry,
    
    # Generated college enrollment variables
    starts_with("first_date_"), starts_with("last_date_"), starts_with("total_count_"),
    
    # Generated fertility variables
    starts_with("date_of_birth_"), total_children,
    gross_family_income, sampling_weight, interview_month, interview_year
  ) %>%
  mutate(
    highest_education = case_when(
      total_count_4 > 0 ~ "Graduate Program",
      total_count_3 > 0 ~ "4-year College",
      total_count_2 > 0 ~ "2-year College",
      TRUE ~ "No College"
    ),
    highest_education = factor(
      highest_education,
      levels = c("No College", "2-year College", "4-year College", "Graduate Program")
    ),
    last_date_highest_edu = case_when(
      highest_education == "Graduate Program" ~ last_date_4,
      highest_education == "4-year College" ~ last_date_3,
      highest_education == "2-year College" ~ last_date_2,
      TRUE ~ NA_Date_
    ),
    sex = factor(sex, levels = c(1, 2), labels = c("Men", "Women")),
    dob_month = as.numeric(dob_month),
    dob_year = as.numeric(dob_year),
    respondent_dob = as.Date(paste(dob_year, dob_month, "01", sep = "-"), format = "%Y-%m-%d")
  ) %>%
  mutate(across(starts_with('total_count_'), ~ replace_na(.x, 0))) %>%
  # Sum the total education counts
  mutate(total_edu = total_count_2 + total_count_3 + total_count_4)



data_clean <- data_clean %>%
  # Ensure date variables are numeric and handle missing values
  mutate(
    marry_1_month = as.numeric(marry_1_month),
    marry_1_year = as.numeric(marry_1_year),
    cohab_1_month = as.numeric(cohab_1_month),
    cohab_1_year = as.numeric(cohab_1_year),
    marry_1_year = ifelse(marry_1_year > 0, marry_1_year, NA),
    marry_1_month = ifelse(marry_1_month > 0, marry_1_month, NA),
    cohab_1_year = ifelse(cohab_1_year > 0, cohab_1_year, NA),
    cohab_1_month = ifelse(cohab_1_month > 0, cohab_1_month, NA)
  ) %>%
  # Create date objects
  mutate(
    date_of_first_marriage = make_date(year = marry_1_year, month = marry_1_month, day = 1),
    date_of_first_cohabitation = make_date(year = cohab_1_year, month = cohab_1_month, day = 1)
  ) %>%
  # Calculate age at first marriage and cohabitation (integer years)
  mutate(
    age_at_first_marriage = floor(as.numeric(difftime(date_of_first_marriage, respondent_dob, units = "days")) / 365.25),
    age_at_first_cohabitation = floor(as.numeric(difftime(date_of_first_cohabitation, respondent_dob, units = "days")) / 365.25)
  ) %>%
  # Calculate age at each childbirth (integer years)
  mutate(across(
    starts_with("date_of_birth_"),
    ~ floor(as.numeric(difftime(.x, respondent_dob, units = "days")) / 365.25),
    .names = "age_at_birth_{str_remove(.col, 'date_of_birth_')}"
  )) %>%
  # Calculate age at first and last birth (integer years)
  rowwise() %>%
  mutate(
    age_at_first_birth = min(c_across(starts_with("age_at_birth_")), na.rm = TRUE),
    age_at_first_birth = ifelse(is.infinite(age_at_first_birth), NA, age_at_first_birth),
    age_at_last_birth = max(c_across(starts_with("age_at_birth_")), na.rm = TRUE),
    age_at_last_birth = ifelse(is.infinite(age_at_last_birth), NA, age_at_last_birth)
  ) %>%
  ungroup() %>%
  filter(is.na(age_at_first_birth) | age_at_first_birth >= 14)

#### Export cleaned data as csv for use in analysis script ####

write.csv(data_clean, file = "cleaned_data_nlsy.csv")
