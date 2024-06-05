library(dplyr)
library(stringr)

data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_clean.rds")

# Define the issue categories
issues <- c("Law and Crime", "Culture and Nationalism", "Public Lands and Agriculture", "Governments and Governance", 
            "Immigration", "Rights, Liberties, Minorities, and Discrimination", "Health and Social Services", 
            "Economy and Employment", "Education", "Environment and Energy", "International Affairs and Defense", "Technology")

issues_string <- paste0("'", paste(issues, collapse = "', '"), "'")

# Prompting mistral

data$issue_category_mistral <- NA

for (i in 1:nrow(data)) {
    prompt <- paste0("In this survey question, respondents had to name their most important issue. Please read the answer and determine to which of the following ",length(issues) ," categories it belongs: ",issues_string,". Use your judgement and only output a single issue category. The answer your need to categorize is: ", data$open_ended_issue[i], ".")

    detected_issue <- NA
    attempt <- 1
    
    while (is.na(detected_issue) && attempt <= 10) {
        # Get the answer from the LLM
        answer <- clellm::ollama_prompt(prompt = prompt, model = "mistral", print_result = FALSE)
        
        # Check if the answer matches any of the predefined categories
        for (issue in issues) {
            if (str_detect(answer, fixed(issue, ignore_case = TRUE))) {
                detected_issue <- issue
                break
            }
        }
        
        attempt <- attempt + 1
    }
    
    # Assign the detected issue to the issue_category column
    data$issue_category_mistral[i] <- detected_issue

    print(paste0(i, " was ", data$open_ended_issue[i], " and is now marked as ", detected_issue, "."))
}

table(data$issue_category_mistral, useNA = "ifany")

# Prompting llama3

data$issue_category_llama3 <- NA

for (i in 1:nrow(data)) {
    prompt <- paste0("In this survey question, respondents had to name their most important issue. Please read the answer and determine to which of the following ",length(issues) ," categories it belongs: ",issues_string,". Use your judgement and only output a single issue category. The answer your need to categorize is: ", data$open_ended_issue[i], ".")

    detected_issue <- NA
    attempt <- 1
    
    while (is.na(detected_issue) && attempt <= 10) {
        # Get the answer from the LLM
        answer <- clellm::ollama_prompt(prompt = prompt, model = "llama3", print_result = FALSE)
        
        # Check if the answer matches any of the predefined categories
        for (issue in issues) {
            if (str_detect(answer, fixed(issue, ignore_case = TRUE))) {
                detected_issue <- issue
                break
            }
        }
        
        attempt <- attempt + 1
    }
    
    # Assign the detected issue to the issue_category column
    data$issue_category_llama3[i] <- detected_issue

    print(paste0(i, " was ", data$open_ended_issue[i], " and is now marked as ", detected_issue, "."))
}

table(data$issue_category_llama3, useNA = "ifany")

# Prompting phi3

data$issue_category_phi3 <- NA

for (i in 1:nrow(data)) {
    prompt <- paste0("In this survey question, respondents had to name their most important issue. Please read the answer and determine to which of the following ",length(issues) ," categories it belongs: ",issues_string,". Use your judgement and only output a single issue category. The answer your need to categorize is: ", data$open_ended_issue[i], ".")

    detected_issue <- NA
    attempt <- 1
    
    while (is.na(detected_issue) && attempt <= 10) {
        # Get the answer from the LLM
        answer <- clellm::ollama_prompt(prompt = prompt, model = "phi3:mini", print_result = FALSE)
        
        # Check if the answer matches any of the predefined categories
        for (issue in issues) {
            if (str_detect(answer, fixed(issue, ignore_case = TRUE))) {
                detected_issue <- issue
                break
            }
        }
        
        attempt <- attempt + 1
    }
    
    # Assign the detected issue to the issue_category column
    data$issue_category_phi3[i] <- detected_issue

    print(paste0(i, " was ", data$open_ended_issue[i], " and is now marked as ", detected_issue, "."))
}

table(data$issue_category_phi3, useNA = "ifany")

# Prompting GPT-4

data$issue_category_gpt4 <- NA

for (i in 1:nrow(data)) {
    system <- "Your role is to categorize open-ended survey responses into given categories"
    
    prompt <- paste0("In this survey question, respondents had to name their most important issue. Please read the answer and determine to which of the following ",length(issues) ," categories it belongs: ",issues_string,". Use your judgement and only output a single issue category. The answer your need to categorize is: ", data$open_ended_issue[i], ".")

    detected_issue <- NA
    attempt <- 1
    
    while (is.na(detected_issue) && attempt <= 10) {
        # Get the answer from the LLM
        chat_prompt <- openai::create_chat_completion(
        model = "gpt-4-turbo",
        messages = list(
            list("role" = "system",
                 "content" = system
            ),
            list(
                "role" = "user",
                "content" = prompt)
            )
        )   

        answer <- chat_prompt$choices$message.content
        
        # Check if the answer matches any of the predefined categories
        for (issue in issues) {
            if (str_detect(answer, fixed(issue, ignore_case = TRUE))) {
                detected_issue <- issue
                break
            }
        }
        
        attempt <- attempt + 1
    }
    
    # Assign the detected issue to the issue_category column
    data$issue_category_gpt4[i] <- detected_issue

    print(paste0(i, " was ", data$open_ended_issue[i], " and is now marked as ", detected_issue, "."))
}

table(data$issue_category_gpt4, useNA = "ifany")

saveRDS(data, "_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean.rds")