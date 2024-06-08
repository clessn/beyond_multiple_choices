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

# Calculate mean F-scores
mean_f_scores <- f_scores_combined %>%
  summarise(across(starts_with("F_Score"), ~ round(mean(.x, na.rm = TRUE), 2))) %>%
  mutate(issue_category_human = "Mean F-score") %>%
  select(issue_category_human, everything())

# Append mean F-scores to the combined data frame
f_scores_combined <- f_scores_combined %>%
  bind_rows(mean_f_scores)

# Generate the LaTeX table
latex_table <- kable(f_scores_combined, format = "markdown", booktabs = TRUE, 
                     col.names = c("Issue Category", "Llama3", "Phi3:mini", "Mistral", "GPT-4", "Dictionary"))

# Print the LaTeX table
print(latex_table)

caption = "F Scores for each model by issue category.",