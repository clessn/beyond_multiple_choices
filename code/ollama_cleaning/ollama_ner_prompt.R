library(dplyr)
library(stringr)

data <- readRDS("data/ollama_cleaning_process/clean/ces2021_ollama_clean.rds")

# Function to extract named entities from GPT output
extract_entities <- function(gpt_output) {
  political_parties <- str_match(gpt_output, "- Political Parties: (.+)")[,2]
  politicians <- str_match(gpt_output, "- Politicians: (.+)")[,2]
  ministries <- str_match(gpt_output, "- Ministries: (.+)")[,2]
  organizations <- str_match(gpt_output, "- Organizations: (.+)")[,2]
  
  list(
    political_parties = political_parties,
    politicians = politicians,
    ministries = ministries,
    organizations = organizations
  )
}

# NER LLAMA3

data$ner_llama3 <- NA

for (i in 1:nrow(data)) {
  prompt <- paste0(
    "You are an AI trained to identify named entities in text. ",
    "In the context of a Canadian election survey, respondents were asked, ",
    "\"What is the most important issue to you in this election?\" ",
    "Your task is to extract the named entities mentioned in each response, ",
    "categorizing them into appropriate types such as politicians, political parties, ministries, and organizations.\n\n",
  
    "Here are some example responses:\n\n",
  
    "1. \"I support the policies of Justin Trudeau.\"\n",
    "2. \"The Liberal Party's stance on healthcare is important to me.\"\n",
    "3. \"I believe the Ministry of Health needs to focus on mental health services.\"\n",
    "4. \"Organizations like Greenpeace are crucial for climate change advocacy.\"\n",
    "5. \"Andrew Scheer's approach to the economy resonates with me.\"\n\n",
  
    "For each response, identify and categorize the named entities as follows:\n\n",
  
    "Response 1:\n",
    "- Politicians: Justin Trudeau\n\n",
  
    "Response 2:\n",
    "- Political Parties: Liberal Party\n\n",
  
    "Response 3:\n",
    "- Ministries: Ministry of Health\n\n",
  
    "Response 4:\n",
    "- Organizations: Greenpeace\n\n",
  
    "Response 5:\n",
    "- Politicians: Andrew Scheer\n\n",
  
    "Now, extract the named entities for the following response:\n\n",
  
    "Response: \"", data$open_ended_issue[i], "\"\n\n",
  
    "Response 1:"
  )

  detected_issue <- NA
  attempt <- 1

  while (is.na(detected_issue) && attempt <= 10) {
    # Get the answer from the LLM
    answer <- clellm::ollama_prompt(prompt = prompt, model = "llama3", print_result = FALSE)
    
    # Extract named entities
    entities <- extract_entities(answer)
    
    # Determine detected issue based on extracted entities
    if (!is.null(entities$political_parties) && !is.na(entities$political_parties)) {
      detected_issue <- entities$political_parties
    } else if (!is.null(entities$politicians) && !is.na(entities$politicians)) {
      detected_issue <- entities$politicians
    } else if (!is.null(entities$ministries) && !is.na(entities$ministries)) {
      detected_issue <- entities$ministries
    } else if (!is.null(entities$organizations) && !is.na(entities$organizations)) {
      detected_issue <- entities$organizations
    }
    
    attempt <- attempt + 1
  }

  # Assign the detected issue to the issue_category column
  data$ner_llama3[i] <- detected_issue

  print(paste0(i, " was \"", data$open_ended_issue[i], "\" and is now marked as \"", ifelse(is.na(detected_issue), "NA", detected_issue), "\"."))
}

saveRDS(data, "_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_ollama_clean_ner.rds")

# NER GPT4

data$ner_gpt4 <- NA

for (i in 186:nrow(data)) {
  
  system <- "You are a Name Entity Recognition (NER) model."
  
  prompt <- paste0(
    "You are an AI trained to identify named entities in text. ",
    "In the context of a Canadian election survey, respondents were asked, ",
    "\"What is the most important issue to you in this election?\" ",
    "Your task is to extract the named entities mentioned in each response, ",
    "categorizing them into appropriate types such as politicians, political parties, ministries, and organizations.\n\n",
  
    "Here are some example responses:\n\n",
  
    "1. \"I support the policies of Justin Trudeau.\"\n",
    "2. \"The Liberal Party's stance on healthcare is important to me.\"\n",
    "3. \"I believe the Ministry of Health needs to focus on mental health services.\"\n",
    "4. \"Organizations like Greenpeace are crucial for climate change advocacy.\"\n",
    "5. \"Andrew Scheer's approach to the economy resonates with me.\"\n\n",
  
    "For each response, identify and categorize the named entities as follows:\n\n",
  
    "Response 1:\n",
    "- Politicians: Justin Trudeau\n\n",
  
    "Response 2:\n",
    "- Political Parties: Liberal Party\n\n",
  
    "Response 3:\n",
    "- Ministries: Ministry of Health\n\n",
  
    "Response 4:\n",
    "- Organizations: Greenpeace\n\n",
  
    "Response 5:\n",
    "- Politicians: Andrew Scheer\n\n",
  
    "Now, extract the named entities for the following response:\n\n",
  
    "Response: \"", data$open_ended_issue[i], "\"\n\n",
  
    "Response 1:"
  )

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
    entities <- extract_entities(answer)
    
    # Determine detected issue based on extracted entities
    if (!is.null(entities$political_parties) && !is.na(entities$political_parties)) {
      detected_issue <- entities$political_parties
    } else if (!is.null(entities$politicians) && !is.na(entities$politicians)) {
      detected_issue <- entities$politicians
    } else if (!is.null(entities$ministries) && !is.na(entities$ministries)) {
      detected_issue <- entities$ministries
    } else if (!is.null(entities$organizations) && !is.na(entities$organizations)) {
      detected_issue <- entities$organizations
    }
    
    attempt <- attempt + 1
  }

  # Assign the detected issue to the issue_category column
  data$ner_gpt4[i] <- detected_issue

  print(paste0(i, " was \"", data$open_ended_issue[i], "\" and is now marked as \"", ifelse(is.na(detected_issue), "NA", detected_issue), "\"."))
}

saveRDS(data, "data/ollama_cleaning_process/clean/ces2021_ollama_clean_ner.rds")

table(data$ner_llama3, useNA = "ifany")
table(data$ner_gpt4, useNA = "ifany")
