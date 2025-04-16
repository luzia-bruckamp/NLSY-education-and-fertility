# ===================================================================
# NLSY97 Analysis Script
# ===================================================================
# Author: Luzia Bruckamp
# Date: 16/04/2025
# Purpose: This script analyses the cleaned NLSY97 data to uncover education and fertility patterns
# 
# Input: cleaned_data_nlsy.csv - Processed dataset with education and fertility variables
# Output: Tables with summary statistics and 6 figures from the corresponding blog post
#
# R version: R 4.2.2
# Required packages: tidyverse, lubridate
# ===================================================================
#
# IMPORTANT: Before running this script, change the directory path below
# to the folder that you were using in the previous script and where
# the cleaned dataset will have been saved.
# ===================================================================

# Set working directory - EDIT THIS LINE to match your file location
wd <- '/Users/luzia/Documents/blog'  # <-- CHANGE THIS PATH
setwd(wd)

# Verify data file exists
if (!file.exists("cleaned_data_nlsy.csv")) {
  stop("cleaned_data_nlsy.csv not found in ", getwd(), 
       "\nPlease check that:\n",
       "1. The data file is in the directory specified above\n",
       "2. You've correctly modified the path in the 'wd' variable")
}

cat("Found data file. Installing packages if needed and beginning data analysis...\n")

# Ensure required packages are installed
required_packages <- c("tidyverse", "lubridate", "RColorBrewer")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# load relevant packages
library(tidyverse)
library(lubridate)
library(RColorBrewer)

############## load data ###################

# Load data and convert sampling_weight to actual weight value
data <- read.csv('cleaned_data_nlsy.csv') %>%
  mutate(weight = sampling_weight / 100)

#### Figures 1 and 2 ####

# do some descriptives
# mean_cohab = average number of cohabitations
# mean_marry = average number of marriages
# mean_edu = average total number of months spent in education
# mafc = mean age at first cohabitation
# mafm = mean age at first marriage
# mafb = mean age at first birth
data_all <- data %>%
  group_by(sex, highest_education) %>%
  summarise(
    # Count weighted number of respondents
    num_respondents = sum(weight),
    # Weighted means for each measure
    mean_cohab = weighted.mean(num_cohabs, w = weight, na.rm = TRUE),
    mean_marry = weighted.mean(num_marry, w = weight, na.rm = TRUE),
    mean_edu = weighted.mean(total_edu, w = weight, na.rm = TRUE),
    mafc = round(weighted.mean(age_at_first_cohabitation, w = weight, na.rm = TRUE), digits = 1),
    mafm = round(weighted.mean(age_at_first_marriage, w = weight, na.rm = TRUE), digits = 1),
    mafb = round(weighted.mean(age_at_birth_1, w = weight, na.rm = TRUE), digits = 1),
    num_children_all = round(weighted.mean(total_children, w = weight, na.rm = TRUE), digits = 2)
  ) %>%
  group_by(sex) %>%
  mutate(
    total_respondents = sum(num_respondents),
    percent_education = num_respondents/total_respondents,
    highest_education = factor(
      highest_education,
      levels = c("No College", "2-year College", "4-year College", "Graduate Program"))
  )


# ===================================================================
# Figure 1: Distribution of highest education by gender
# This figure shows the educational distribution that is roughly
# representative of the American population born 1980-1984
# ===================================================================
ggplot(data_all, aes(x = highest_education, y = percent_education, fill = as.factor(sex))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +  # Manually set colors: blue for men, red for women
  labs(
    title = "Distribution of highest education",
    y = "Share of respondents",
    x = "Highest education"
  ) +
  theme_minimal() +
  facet_wrap(~sex) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )


# Pivot data in order to plot all three life events together
data_long <- data_all %>%
  pivot_longer(
    cols = c(mafc, mafm, mafb), 
    names_to = "event", 
    values_to = "mean_age"
  ) %>%
  mutate(
    event = factor(
      event, 
      levels = c("mafc", "mafm", "mafb"),  # Ensure correct order
      labels = c("First Cohabitation", "First Marriage", "First Birth")
    )
  )

# ===================================================================
# Figure 2: Mean age at first cohabitation, first marriage, and first birth
# This figure shows the mean age at different life events by gender
# and highest education, giving us a sense of how much the age shifts
# up with more education
# ===================================================================
ggplot(data_long, aes(x = highest_education, y = mean_age, group = sex, colour = sex)) +
  geom_line(size = 1.5) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("#00BFC4", "#F8766D")) +  # Teal for men, red for women
  labs(
    title = "Average age at life events",
    y = "Age",
    x = "Highest Education",
    colour = ""
  ) +
  scale_y_continuous(limits = c(20, 30)) +  # Set y-axis limits
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12)  # Adjust facet label size
  ) +
  facet_wrap(~event, ncol = 3)  # Create 3 side-by-side facets

#### Figure 3 ####

# Function to calculate ASFR with simple handling of missing interview dates
calculate_asfr_simple <- function(data, default_end_date = "2022-10-31") {
  # Define standard age groups and their ranges
  age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")
  
  # Helper function to assign ages to age groups
  assign_age_group <- function(age) {
    case_when(
      age >= 15 & age < 20 ~ "15-19",
      age >= 20 & age < 25 ~ "20-24",
      age >= 25 & age < 30 ~ "25-29",
      age >= 30 & age < 35 ~ "30-34",
      age >= 35 & age < 40 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      TRUE ~ NA_character_
    )
  }
  
  # Convert default end date to Date object
  default_end <- ymd(default_end_date)
  
  # Preprocess data to create interview dates
  data_processed <- data %>%
    mutate(
      # Convert DOB to Date object
      dob = ymd(respondent_dob),
      
      # Create interview date when available (assume 15th of month)
      # For missing dates (coded as -5), use the default end date
      interview_date = case_when(
        interview_month > 0 & interview_year > 0 ~ ymd(paste0(interview_year, "-", interview_month, "-15")),
        TRUE ~ default_end
      )
    )
  
  # Calculate exposure for each person at each age
  exposure_data <- data_processed %>%
    # We need ID, sex, weight, highest_education, DOB, and interview date
    select(ID, sex, weight, highest_education, dob, interview_date) %>%
    # Create a row for each person and each possible age (15-44)
    crossing(age = 15:44) %>%
    # Calculate exposure time at each age
    mutate(
      # Date when they enter this age
      date_enter_age = dob + years(age),
      # Date when they exit this age
      date_exit_age = dob + years(age + 1) - days(1),
      
      # Adjust for observation period (start of survey or interview date)
      date_enter_observed = pmax(date_enter_age, ymd("1997-01-01")),
      date_exit_observed = pmin(date_exit_age, interview_date),
      
      # Calculate exposure time in years (capped at 1 year maximum per age)
      exposure_years = as.numeric(interval(date_enter_observed, date_exit_observed) / years(1)),
      # Ensure non-negative exposure and cap at 1 year per age
      exposure_years = pmin(1, pmax(0, exposure_years)),
      
      # Assign age group
      age_group = assign_age_group(age)
    ) %>%
    # Remove ages with no exposure
    filter(exposure_years > 0)
  
  # Calculate weighted total exposure by group
  exposure_summary <- exposure_data %>%
    group_by(sex, highest_education, age_group) %>%
    summarise(
      exposure_years_weighted = sum(weight * exposure_years, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Process birth data to match age groups
  birth_data <- data_processed %>%
    select(ID, sex, weight, highest_education, starts_with("age_at_birth_")) %>%
    pivot_longer(
      cols = starts_with("age_at_birth_"), 
      names_to = "birth_variable", 
      values_to = "age_at_birth"
    ) %>%
    filter(!is.na(age_at_birth) & age_at_birth >= 15 & age_at_birth <= 44) %>%
    # Assign each birth to an age group
    mutate(
      age_group = assign_age_group(age_at_birth)
    ) %>%
    # Calculate weighted births by group
    group_by(sex, highest_education, age_group) %>%
    summarise(
      num_births_weighted = sum(weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join birth data with exposure data and calculate ASFR
  asfr_data <- full_join(birth_data, exposure_summary, by = c("sex", "highest_education", "age_group")) %>%
    mutate(
      # Replace NA with 0 for births
      num_births_weighted = coalesce(num_births_weighted, 0),
      # Calculate ASFR per 1000 person-years
      asfr = (num_births_weighted / exposure_years_weighted) * 1000,
      # Factor highest_education for consistent ordering
      highest_education = factor(
        highest_education,
        levels = c("No College", "2-year College", "4-year College", "Graduate Program")
      ),
      # Factor age_group for consistent ordering
      age_group = factor(age_group, levels = age_groups)
    ) %>%
    # Ensure all combinations are present
    complete(sex, highest_education, age_group, fill = list(num_births_weighted = 0, asfr = 0))
  
  return(asfr_data)
}

# Run the function to calculate ASFR for the data
asfr_data <- calculate_asfr_simple(data)

# ===================================================================
# Figure 3: Age-specific fertility rates (ASFR)
# This figure shows the ASFR curves by gender and education.
# ASFR is the fraction of the exposed population giving birth at
# a certain age.
# ===================================================================
ggplot(asfr_data, 
       aes(x = age_group, 
           y = asfr, 
           colour = highest_education,
           group = highest_education)) +  # This fixes the issue!
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +  # Adding points makes it easier to see each age group value
  labs(
    title = 'Age-specific fertility rates',
    x = "Age Group",
    y = "Births per 1,000 Person-Years",
    colour = "Education"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~sex, ncol = 2) +  # free_y allows different scales for each sex
  scale_colour_brewer(palette = "RdYlBu")

#### Figures 4-6 ####

# add time between education completion and 
# first birth/first marriage

# Ensure the variables are in date format
data <- data %>%
  mutate(
    last_date_highest_edu = as.Date(last_date_highest_edu),  # Convert to date
    date_of_birth_1 = as.Date(date_of_birth_1),              # Convert to date
    months_diff = interval(last_date_highest_edu, date_of_birth_1) %/% months(1), # Difference in months
    year_diff = months_diff/12
  )

# ===================================================================
# Figure 4: Probability density of first birth relative to education completion
# Shows the likelihood of a first birth relative to when the highest
# education was completed.
# ===================================================================
ggplot(
  data %>% filter(highest_education != "No College"),
  aes(x = year_diff, colour = sex, weight = weight)
) +
  # Add weighted density lines
  geom_density(size = 1.2, adjust = 1) +
  # Vertical line at education completion
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black", size = 0.8) +
  # Labels
  labs(
    title = "Timing of First Birth Relative to Education Completion",
    x = "Years since highest education completed",
    y = "Density",
    colour = "",
    size = "Weight"
  ) +
  # Theme settings
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Set axis limits
  scale_x_continuous(limits = c(-10, 10)) +
  # Manual color specification (switched for men and women)
  scale_colour_manual(
    values = c("Women" = "#F8766D", "Men" = "#00BFC4")
  )

# ===================================================================
# Figure 5: Probability density of first birth relative to education completion
# Shows the same as figure 4 but separately for each education level.
# ===================================================================
ggplot(
  data %>% filter(highest_education != "No College"),
  aes(x = year_diff, colour = sex, weight = weight)
) +
  # Add weighted density lines
  geom_density(size = 1.2, adjust = 1) +
  # Vertical line at education completion
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black", size = 0.8) +
  # Labels
  labs(
    title = "Timing of First Birth Relative to Education Completion",
    x = "Years since highest education completed",
    y = "Density",
    colour = "",
    size = "Weight"
  ) +
  # Theme settings
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Set axis limits
  scale_x_continuous(limits = c(-10, 10)) +
  # Manual color specification (switched for men and women)
  scale_colour_manual(
    values = c("Women" = "#F8766D", "Men" = "#00BFC4")
  ) +
  facet_wrap(~highest_education, ncol = 2)

#### do the same for first marriage

# Ensure the variables are in date format
data <- data %>%
  mutate(
    date_of_first_marriage = as.Date(date_of_first_marriage),              # Convert to date
    months_diff_marry = interval(last_date_highest_edu, date_of_first_marriage) %/% months(1), # Difference in months
    year_diff_marry = months_diff_marry/12
  )


# ===================================================================
# Figure 6: Probability density of first marriage relative to education completion
# Shows the same as figure 5 but for first marriages instead of first
# births.
# ===================================================================
# Plot weighted density separately for each education level
ggplot(
  data %>% filter(highest_education != "No College"),
  aes(x = year_diff_marry, colour = sex, weight = weight)
) +
  # Add weighted density lines
  geom_density(size = 1.2, adjust = 1) +
  # Vertical line at education completion
  geom_vline(xintercept = 0, linetype = "dashed", colour = "black", size = 0.8) +
  # Labels
  labs(
    title = "Timing of First Marriage Relative to Education Completion",
    x = "Years since highest education completed",
    y = "Density",
    colour = "",
    size = "Weight"
  ) +
  # Theme settings
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  # Set axis limits
  scale_x_continuous(limits = c(-10, 10)) +
  # Manual color specification (switched for men and women)
  scale_colour_manual(
    values = c("Women" = "#F8766D", "Men" = "#00BFC4")
  ) +
  facet_wrap(~highest_education, ncol = 2)


