# Load Necessary Libraries
library(tm)
library(dplyr)
library(stringr)

# Load and Prepare the Data

data <- readRDS("data/ollama_cleaning_process/clean/ces2021_ollama_clean_ner.rds")

data$open_ended_issue <- as.character(data$open_ended_issue)

# Create Issue Dictionaries
dictionary <- read.csv("data/dict/dictionnaire_merge_v1.csv", sep = ";") |> 
    filter(language == "en")

issue_dictionaries <- list(
  "Law and Crime" = dictionary$item[dictionary$category == "Loi et crime"],
  "Culture and Nationalism" = dictionary$item[dictionary$category == "Culture et nationalisme"],
  "Public Lands and Agriculture" = dictionary$item[dictionary$category == "Terres publiques et agriculture"],
  "Governments and Governance" = dictionary$item[dictionary$category == "Gouvernements et gouvernance"],
  "Immigration" = dictionary$item[dictionary$category == "Immigration"],
  "Rights, Liberties, Minorities, and Discrimination" = dictionary$item[dictionary$category == "Droits, libertés, minorités et discrimination"],
  "Health and Social Services" = dictionary$item[dictionary$category == "Santé et services sociaux"],
  "Economy and Employment" = dictionary$item[dictionary$category == "Économie et travail"],
  "Education" = dictionary$item[dictionary$category == "Éducation"],
  "Environment and Energy" = dictionary$item[dictionary$category == "Environnement et énergie"],
  "International Affairs and Defense" = dictionary$item[dictionary$category == "Affaires internationales et défense"],
  "Technology" = dictionary$item[dictionary$category == "Technologie"]
)

# Preprocess Text Data
clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  return(text)
}

data$cleaned_text <- sapply(data$open_ended_issue, clean_text)

# Classify Responses Based on Keywords
classify_response <- function(response, dictionaries) {
  issue_count <- sapply(dictionaries, function(keywords) {
    sum(sapply(keywords, function(keyword) grepl(keyword, response)))
  })
  if (all(issue_count == 0)) {
    return("Other")
  } else {
    return(names(which.max(issue_count)))
  }
}

data$issue_category_dict <- sapply(data$cleaned_text, classify_response, dictionaries = issue_dictionaries)

# Compare Classified Issues with Human-Annotated Categories
data <- data %>%
  mutate(issue_comparison_dict = case_when(
    is.na(issue_category_human) & is.na(issue_category_dict) ~ 1,
    is.na(issue_category_human) | is.na(issue_category_dict) ~ 0,
    str_to_lower(issue_category_human) == str_to_lower(issue_category_dict) ~ 1,
    TRUE ~ 0
  ))

table(data$issue_comparison_dict, useNA = "ifany")

# Print the summary to review
print(data)

# Save the Cleaned Data

saveRDS(data, "data/ollama_cleaning_process/clean/ces2021_ollama_clean_ner_dict.rds")
