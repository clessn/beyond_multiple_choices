library(dplyr)

data <- readRDS("_SharedFolder_beyong_multiple_choices/data/data_open_questions.rds")

for (i in 1:nrow(data_raw)) {

    if (data$know_french_media_personality[i] == "dk") {
        print(paste0(i, " is already marked as 'dk'."))
        data$know_french_media_personality[i] <- data$know_french_media_personality[i]
        next
    }

    prompt <- paste0("In this survey answer, respondents had to name a famous person. Does this survey answer mean 'I don't know'? Answer with 1 if yes and with 0 if the respondent named someone. The answer is: ", data$know_french_media_personality[i], ". Answer only with 1 if it means 'I don't know' and with 0 if it's someone's name.")

    answer <- clellm::ollama_prompt(prompt = prompt, model = "phi3:mini", print_result = FALSE)

    num_answer <- as.numeric(gsub("[^0-9]", "", answer))

    if (num_answer == 1) {
        print(paste0(i, " was ", data$know_french_media_personality[i], " and is now marked as 'dk'."))
        data$know_french_media_personality[i] <- "dk"
    } else {
        print(paste0(i, " was ", data$know_french_media_personality[i], ". It was something else so it is not marked as 'dk'."))
    }

}
