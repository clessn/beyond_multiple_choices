# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean_ner_dict.rds")

# Calculate the distribution of issue_category_human
issue_distribution <- data %>%
  group_by(issue_category_human) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Define new labels for the issue categories
new_labels <- c(
  "Economy and Employment" = "Economy and\nEmployment",
  "Health and Social Services" = "Health and\nSocial Services",
  "Governments and Governance" = "Governments and\nGovernance",
  "Environment and Energy" = "Environment and\nEnergy",
  "Rights, Liberties, Minorities, and Discrimination" = "Rights, Liberties,\nMinorities,\nand Discrimination",
  "NA" = "NA",
  "Education" = "Education",
  "Culture and Nationalism" = "Culture and\nNationalism",
  "Immigration" = "Immigration",
  "Law and Crime" = "Law and Crime"
)


# Create the bar graph
ggplot(issue_distribution, aes(x = reorder(issue_category_human, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#585858") +
  labs(x = "Issue Category",
       y = "Count") +
  clessnize::theme_clean_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1)) +
  scale_x_discrete(labels = new_labels)

ggsave("pub_beyond_multiple_choices/graphs/issue_distribution.png",
       width = 8, height = 6, dpi = 300)
