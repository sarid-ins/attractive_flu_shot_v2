# Read the full data and save the complete ids
library(tidyverse)
Sys.setlocale("LC_ALL", "Hebrew")
flu_data <- read_csv("https://app.sarid-ins.com/reportsview/?key=471756-10219089-5d8ac2599e14891fa224b951348f884e")

complete_ids <- flu_data %>% 
  filter(Status == "Complete") %>% 
  select(userID)

#write_excel_csv(complete_ids, "data/complete_ids.csv")