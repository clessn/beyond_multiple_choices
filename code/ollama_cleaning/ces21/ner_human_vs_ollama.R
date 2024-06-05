library(dplyr)
library(stringr)

data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean.rds")

data <- data %>%
  mutate(issue_comparison_llama3 = case_when(
    is.na(issue_category_human) & is.na(issue_category_llama3) ~ 1,
    is.na(issue_category_human) | is.na(issue_category_llama3) ~ 0,
    str_to_lower(issue_category_human) == str_to_lower(issue_category_llama3) ~ 1,
    TRUE ~ 0
  ))

data <- data %>%
  mutate(issue_comparison_gpt4 = case_when(
    is.na(issue_category_human) & is.na(issue_category_gpt4) ~ 1,
    is.na(issue_category_human) | is.na(issue_category_gpt4) ~ 0,
    str_to_lower(issue_category_human) == str_to_lower(issue_category_gpt4) ~ 1,
    TRUE ~ 0
  ))

table(data$issue_comparison_llama3, useNA = "ifany")
table(data$issue_comparison_gpt4, useNA = "ifany")