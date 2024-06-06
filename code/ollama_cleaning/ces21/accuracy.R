# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean_ner_dict.rds")

# Function to calculate accuracy
calculate_accuracy <- function(data, human_col, model_col) {
  correct_predictions <- sum(data[[human_col]] == data[[model_col]], na.rm = TRUE)
  total_predictions <- nrow(data)
  accuracy <- (correct_predictions / total_predictions) * 100
  print(paste("Accuracy for", model_col, ":", accuracy))
  return(accuracy)
}

# Calculate accuracy for each model
accuracy_llama3 <- calculate_accuracy(data, "issue_category_human", "issue_category_llama3")
accuracy_phi3 <- calculate_accuracy(data, "issue_category_human", "issue_category_phi3")
accuracy_mistral <- calculate_accuracy(data, "issue_category_human", "issue_category_mistral")
accuracy_gpt4 <- calculate_accuracy(data, "issue_category_human", "issue_category_gpt4")
accuracy_dict <- calculate_accuracy(data, "issue_category_human", "issue_category_dict")

# Combine accuracy into a data frame
accuracy_data <- data.frame(
  Model = c("Human (Ground Truth)", "Llama3", "Phi3", "Mistral", "GPT-4", "Dictionary"),
  Accuracy = c(100, accuracy_llama3, accuracy_phi3, accuracy_mistral, accuracy_gpt4, accuracy_dict)
)

# Sort the data frame by accuracy
accuracy_data <- accuracy_data %>%
  arrange(desc(Accuracy))

# Create the ggplot graph
ggplot(accuracy_data, aes(x = reorder(Model, -Accuracy), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "#585858") +
  labs(x = "Model",
       y = "Accuracy (%)") +
  clessnize::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 ggsave("pub_beyond_multiple_choices/graphs/accuracy.png",
       width = 8, height = 6, dpi = 300)
