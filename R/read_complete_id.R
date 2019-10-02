# Read the full data and save the complete ids
library(tidyverse)
Sys.setlocale("LC_ALL", "Hebrew")


# Set new column names ----------------------------------------------------

new_col_names <- readxl::read_excel("data/new_col_names.xlsx")

# read the file from original source (online surveying tool) ----
flu_data_raw <- read_csv("data/survey_raw_data.csv", 
                         skip = 1,
                         col_names = new_col_names$new_name) %>% 
  select(-contains("skip")) %>% 
  filter(saridr::notin(id, c(91:93, 106))) # exclusing 4 responses which are researcher's tests


# save the file locally for future use ----
# Run only once:
# write_csv(flu_data, "data/survey_raw_data.csv")


# Complete ID (to send to the online sample provider) ---------------------

complete_ids <- flu_data_raw %>% 
  filter(Status == "Complete") %>% 
  select(userID)


# Double check agains duplicates from previous experiments ----------------

previous_experiments <- readxl::read_excel("data/previous_experiments.xlsx")

sum(previous_experiments$Panel4AlluserID %in% complete_ids$userID)


# Glimpse and see updated names -------------------------------------------

glimpse(flu_data_raw)
names(flu_data_raw)

# Check action values for recoding ----

recode_action_values <- flu_data_raw %>% 
  select(starts_with("action")) %>% 
  gather(action_type, value) %>% 
  distinct(value)

## Run only once
# write_excel_csv(recode_action_values, "c:/temp/action_values.csv")

# Read recoding
action_recoding <- readxl::read_excel("data/action_values_recoded.xlsx")

# Combine recoding into file, and omit old action variables
flu_action_combined <- flu_data_raw %>% 
  select(id, starts_with("action")) %>% 
  gather(variable, action_original_value, -id) %>% 
  filter(!is.na(action_original_value)) %>% 
  left_join(action_recoding, by = "action_original_value") %>% 
  select(-variable, -action_original_value) %>% 
  arrange(id)


# these were omitted to a surveygizmo bug, which made them see all treatments instead of a randomized one
response_bug <- flu_action_combined %>% 
  count(id) %>% 
  arrange(desc(n)) %>%
  filter(n>1) %>% 
  pull(id)

flu_data <- flu_data_raw %>% 
  filter(saridr::notin(id, response_bug)) %>% 
  left_join(flu_action_combined, by = "id") %>% 
  select(-action_control:-action_benefit) %>% 
  mutate(branch = ifelse(branch == "Inventory", "Stock", branch)) %>% 
  rename(treatment = branch) %>% 
  mutate(certainty = 
           recode_factor(certainty,
                         "כלל לא בטוח" = 1,
                         "בטוח במידה מועטה" = 2,
                         "בטוח במידה בינונית" = 3,
                         "בטוח במידה רבה" = 4,
                         "בטוח במידה רבה מאוד" = 5
  )) %>% 
  mutate(last_year_vaccination = 
           recode_factor(last_year_vaccine, 
                         "כן" = 1,
                         "לא" = 0)) %>% 
  mutate(five_years_vaccinations = 
           recode_factor(five_years_vaccinations,
                         "אף פעם" = 0,
                         "פעם אחת" = 1,
                         "פעמיים" = 2,
                         "3 פעמים" = 3,
                         "4 פעמים" = 4,
                         "5 פעמים" = 5
                         ))


# Read and add demographic data -------------------------------------------

demographic <- readxl::read_excel("data/demographic_data.xlsx", skip = 1,
                                  col_names = c("userID",
                                                "gender",
                                                "age",
                                                "education",
                                                "religion",
                                                "religiousness",
                                                "income",
                                                "family_status"),
                                  col_types = c("text", "text", "numeric", "text", "text", "text", "text", "text")) %>% 
  mutate(gender = recode_factor(gender,
                                "זכר" = "Male",
                                "נקבה" = "Female")) %>% 
  mutate(education = recode_factor(education, 
                                   "אקדמאית - תואר שני ומעלה" = "Academic - Graduate",
                                   "אקדמאית - תואר ראשון" = "Academic - Undergraduate",
                                   "על תיכונית" = "Vocational",
                                   "תיכונית חלקית" = "Highschool",
                                   "תיכונית מלאה" = "Highschool",
                                   "יסודית" = "Elementary")) %>% 
  mutate(income = recode_factor(income,
                                "הרבה מעל הממוצע" = "Above",
                                "מעט מעל הממוצע" = "Slightly above",
                                "כמו הממוצע" = "Average",
                                "מעט מתחת לממוצע" = "Slightly under",
                                "הרבה מתחת לממוצע" = "Under") %>% 
           na_if(., "איני מעוניין להשיב") %>% 
           na_if(., "NA") %>% 
           forcats::fct_explicit_na(.)) %>% 
  mutate(family_status = recode_factor(family_status,
                                       "אלמן" = "Widowed",
                                       "גרוש" = "Divorced",
                                       "נשוי" = "Married",
                                       "פרוד" = "Divorced",
                                       "רווק" = "Single")) %>% 
  mutate(religiousness = recode_factor(religiousness,
                                       "חרדי" = "Ultra-orthodox",
                                       "דתי" = "Religious",
                                       "מסורתי" = "Traditional",
                                       "חילוני" = "Secular"))

# Export final file after all recoding has been applied -------------------

flu_data_final <- flu_data %>% 
  left_join(demographic, by = "userID")

write_excel_csv(flu_data_final, "data/flu_data_final.csv")
