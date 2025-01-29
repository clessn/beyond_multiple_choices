library(dplyr)
library(stringr)
library(purrr)

# Load data
data <- readRDS("data/ollama_cleaning_process/clean/ces2021_clean.rds")

# Define issue categories
issues <- c("Law and Crime", "Culture and Nationalism", "Public Lands and Agriculture", 
           "Governments and Governance", "Immigration", "Rights, Liberties, Minorities, and Discrimination", 
           "Health and Social Services", "Economy and Employment", "Education", 
           "Environment and Energy", "International Affairs and Defense", "Technology")

issues_string <- sprintf("'%s'", paste(issues, collapse = "', '"))

# Create a function to generate prompt
create_prompt <- function(response) {
  sprintf("In this survey question, respondents had to name their most important issue. Please read the answer and determine to which of the following %d categories it belongs: %s. Use your judgement and only output a single issue category. The answer your need to categorize is: %s.",
          length(issues), issues_string, response)
}

# Function to detect issue from LLM response
detect_issue <- function(answer, issues) {
  detected <- issues[str_detect(tolower(answer), tolower(issues))]
  if(length(detected) > 0) return(detected[1])
  return(NA_character_)
}

# Function to handle LLM calls with retry logic
process_with_llm <- function(prompt, model, max_attempts = 10) {
  for(attempt in 1:max_attempts) {
    if(model == "gpt-4") {
      response <- openai::create_chat_completion(
        model = "gpt-4-turbo",
        messages = list(
          list("role" = "system", "content" = "Your role is to categorize open-ended survey responses into given categories"),
          list("role" = "user", "content" = prompt)
        )
      )$choices$message.content
    } else {
      response <- clellm::ollama_prompt(prompt = prompt, model = model, print_result = FALSE)
    }
    
    detected <- detect_issue(response, issues)
    if(!is.na(detected)) return(detected)
  }
  return(NA_character_)
}

# Process responses with different models
models <- c("mistral", "llama3", "phi3:mini", "gpt-4")

results <- models %>%
  set_names() %>%
  map(function(model) {
    responses <- data$open_ended_issue %>%
      map_chr(create_prompt) %>%
      map_chr(function(prompt) {
        result <- process_with_llm(prompt, model)
        cat(sprintf("Processing with %s: %s -> %s\n", model, substr(prompt, 1, 50), result))
        result
      })
    
    # Add results to data frame
    column_name <- paste0("issue_category_", gsub(":", "", model))
    data[[column_name]] <- responses
    responses
  })

# Save results
saveRDS(data, "data/ollama_cleaning_process/clean/ces2021_ollama_clean.rds")

# Print summary tables
walk(names(results), ~{
  cat("\nResults for", .x, ":\n")
  print(table(results[[.x]], useNA = "ifany"))
})
