# Load necessary libraries
library(dplyr)
library(knitr)

# Load the dataset
data <- readRDS("data/ollama_cleaning_process/clean/ces2021_ollama_clean_ner_dict.rds")

# Function to calculate F Score
calculate_f_score <- function(data, human_col, model_col) {
  results <- data %>%
    group_by(!!sym(human_col)) %>%
    summarise(
      TP = sum(!!sym(human_col) == !!sym(model_col)),
      FN = sum(!!sym(human_col) != !!sym(model_col) & !!sym(human_col) != "NA"),
      FP = sum(!!sym(model_col) != !!sym(human_col) & !!sym(model_col) != "NA")
    ) %>%
    mutate(
      Precision = TP / (TP + FP),
      Recall = TP / (TP + FN),
      F_Score = 2 * (Precision * Recall) / (Precision + Recall)
    ) %>%
    mutate(across(Precision:F_Score, round, 2)) %>%
    filter(!is.na(!!sym(human_col))) %>%
    mutate(F_Score = ifelse(is.nan(F_Score), NA, F_Score))  # Replace NaN with NA for mean calculation
  
  return(results %>% select(!!sym(human_col), F_Score))
}

# Calculate F Scores for each model
f_score_llama3 <- calculate_f_score(data, "issue_category_human", "issue_category_llama3")
f_score_phi3 <- calculate_f_score(data, "issue_category_human", "issue_category_phi3")
f_score_mistral <- calculate_f_score(data, "issue_category_human", "issue_category_mistral")
f_score_gpt4 <- calculate_f_score(data, "issue_category_human", "issue_category_gpt4")
f_score_dict <- calculate_f_score(data, "issue_category_human", "issue_category_dict")

# Combine F Scores into one table
f_scores_combined <- f_score_llama3 %>%
  rename(F_Score_llama3 = F_Score) %>%
  left_join(f_score_phi3 %>% rename(F_Score_phi3 = F_Score), by = "issue_category_human") %>%
  left_join(f_score_mistral %>% rename(F_Score_mistral = F_Score), by = "issue_category_human") %>%
  left_join(f_score_gpt4 %>% rename(F_Score_gpt4 = F_Score), by = "issue_category_human") %>%
  left_join(f_score_dict %>% rename(F_Score_dict = F_Score), by = "issue_category_human")

human_count <- data  |> 
  group_by(issue_category_human) |>
  summarise(count = n())


f_scores_combined |> 
  left_join(human_count, by = c("issue_category_human" = "issue_category_human"))


# Print the LaTeX table
print(latex_table)

# Load necessary libraries
library(dplyr)
library(knitr)

# Load the dataset
data <- readRDS("data/ollama_cleaning_process/clean/ces2021_ollama_clean_ner_dict.rds")

# Function to calculate F Score
calculate_f_score <- function(data, human_col, model_col) {
  results <- data %>%
    group_by(!!sym(human_col)) %>%
    summarise(
      TP = sum(!!sym(human_col) == !!sym(model_col)),
      FN = sum(!!sym(human_col) != !!sym(model_col) & !!sym(human_col) != "NA"),
      FP = sum(!!sym(model_col) != !!sym(human_col) & !!sym(model_col) != "NA")
    ) %>%
    mutate(
      Precision = TP / (TP + FP),
      Recall = TP / (TP + FN),
      F_Score = 2 * (Precision * Recall) / (Precision + Recall)
    ) %>%
    mutate(across(Precision:F_Score, round, 2)) %>%
    filter(!is.na(!!sym(human_col))) %>%
    mutate(F_Score = ifelse(is.nan(F_Score), NA, F_Score))  # Replace NaN with NA for mean calculation
  
  return(results %>% select(!!sym(human_col), F_Score))
}

# Calculate F Scores for each model
f_score_llama3 <- calculate_f_score(data, "issue_category_human", "issue_category_llama3")
f_score_phi3 <- calculate_f_score(data, "issue_category_human", "issue_category_phi3")
f_score_mistral <- calculate_f_score(data, "issue_category_human", "issue_category_mistral")
f_score_gpt4 <- calculate_f_score(data, "issue_category_human", "issue_category_gpt4")
f_score_dict <- calculate_f_score(data, "issue_category_human", "issue_category_dict")

# Combine F Scores into one table
f_scores_combined <- f_score_llama3 %>%
  rename(F_Score_llama3 = F_Score) %>%
  left_join(f_score_phi3 %>% rename(F_Score_phi3 = F_Score), by = "issue_category_human") %>%
  left_join(f_score_mistral %>% rename(F_Score_mistral = F_Score), by = "issue_category_human") %>%
  left_join(f_score_gpt4 %>% rename(F_Score_gpt4 = F_Score), by = "issue_category_human") %>%
  left_join(f_score_dict %>% rename(F_Score_dict = F_Score), by = "issue_category_human")

# Add count of issues
human_count <- data  |> 
  group_by(issue_category_human) |>
  summarise(count = n())

f_scores_combined <- f_scores_combined |> 
  left_join(human_count, by = c("issue_category_human" = "issue_category_human"))

# Calculate weighted mean F-scores
weighted_means <- f_scores_combined %>%
  summarise(
    F_Score_llama3 = round(sum(F_Score_llama3 * count, na.rm = TRUE) / sum(count, na.rm = TRUE), 2),
    F_Score_phi3 = round(sum(F_Score_phi3 * count, na.rm = TRUE) / sum(count, na.rm = TRUE), 2),
    F_Score_mistral = round(sum(F_Score_mistral * count, na.rm = TRUE) / sum(count, na.rm = TRUE), 2),
    F_Score_gpt4 = round(sum(F_Score_gpt4 * count, na.rm = TRUE) / sum(count, na.rm = TRUE), 2),
    F_Score_dict = round(sum(F_Score_dict * count, na.rm = TRUE) / sum(count, na.rm = TRUE), 2),
    count = sum(count, na.rm = TRUE)
  )

# Create a new row with weighted means
new_row <- c("Weighted Mean", weighted_means$F_Score_llama3, weighted_means$F_Score_phi3, 
             weighted_means$F_Score_mistral, weighted_means$F_Score_gpt4, weighted_means$F_Score_dict, weighted_means$count)

# Add the new row to the dataframe
f_scores_combined <- rbind(f_scores_combined, new_row) |> 
  select(-count)

# Generate the LaTeX table
latex_table <- kable(f_scores_combined, format = "markdown", booktabs = TRUE, 
                     col.names = c("Issue Category", "Llama3", "Phi3", "Mistral", "GPT-4", "Dict"))

# Print the LaTeX table
print(latex_table)
