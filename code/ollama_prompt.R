library(dplyr)

data <- readRDS("_SharedFolder_beyong_multiple_choices/data/data_open_questions.rds") |> 
    select(starts_with("best"))

for (i in 1:nrow(data)) {

    if (data$best_cad_book[i] == "dk") {
        print(paste0(i, " is already marked as 'dk'."))
        data$best_cad_book[i] <- data$best_cad_book[i]
        next
    }

    prompt <- paste0("In this survey question, respondents had to name the best book in their opinion. Does this survey answer mean 'I don't know'? Answer with 1 if yes and with 0 if the respondent named a book. The respondent's answer is: ", data$best_cad_book[i], ". Answer only with 1 if it means 'I don't know' and with 0 if it's a book.")

    answer <- clellm::ollama_prompt(prompt = prompt, model = "phi3:mini", print_result = FALSE)

    num_answer <- as.numeric(gsub("[^0-9]", "", answer))

    if (num_answer == 1) {
        print(paste0(i, " was ", data$best_cad_book[i], " and is now marked as 'dk'."))
        data$best_cad_book[i] <- "dk"
    } else {
        print(paste0(i, " was ", data$best_cad_book[i], ". It was something else so it is not marked as 'dk'."))
    }

}

saveRDS(data, "_SharedFolder_beyong_multiple_choices/data/01_data_remove_dk.rds")

