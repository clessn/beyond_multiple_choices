library(dplyr)
library(caret)

data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean_ner_dict.rds")

# Ensure both columns are factors
data$issue_category_llama3 <- factor(data$issue_category_llama3)
data$issue_category_human <- factor(data$issue_category_human)

# Ensure both factors have the same levels
all_levels <- union(levels(data$issue_category_llama3), levels(data$issue_category_human))
data$issue_category_llama3 <- factor(data$issue_category_llama3, levels = all_levels)
data$issue_category_human <- factor(data$issue_category_human, levels = all_levels)

# Create the confusion matrix
confusion_matrix <- confusionMatrix(data = data$issue_category_llama3, reference = data$issue_category_human)

# Print the byClass values to check where NA values may be
print(confusion_matrix$byClass)

# Exclude classes with NA values for F1 score
valid_classes <- !is.na(confusion_matrix$byClass[,'F1'])
valid_f_scores <- confusion_matrix$byClass[valid_classes, 'F1']

# Calculate the mean F1 score for valid classes
mean_f_score <- mean(valid_f_scores, na.rm = TRUE)
print(mean_f_score)
