library(dplyr)

variables <- names(readRDS("_SharedFolder_beyong_multiple_choices/data/selected_variables_full_open.rds"))

data_raw <- haven::read_sav("_SharedFolder_beyong_multiple_choices/data/data_raw.sav") |> 
    filter(code == "complete")

data_clean <- data.frame(id = 1:nrow(data_raw))

attributes(data_raw$know_fre_1_1) # media personality
table(data_raw$know_fre_1_1, useNA = "ifany")
sum(table(data_raw$know_fre_1_TEXT))
data_clean$know_french_media_personality <- NA
data_clean$know_french_media_personality[data_raw$know_fre_1_1 == 1] <- "dk"
index_know_french_media_personality <- data_raw$know_fre_1_TEXT != ""
data_clean$know_french_media_personality[index_know_french_media_personality] <- data_raw$know_fre_1_TEXT[index_know_french_media_personality]
table(data_clean$know_french_media_personality, useNA = "ifany")

attributes(data_raw$know_fre_2_1) # A singer
table(data_raw$know_fre_2_1, useNA = "ifany")
sum(table(data_raw$know_fre_2_TEXT))
data_clean$know_french_singer <- NA
data_clean$know_french_singer[data_raw$know_fre_2_1 == 1] <- "dk"
index_know_french_singer <- data_raw$know_fre_2_TEXT != ""
data_clean$know_french_singer[index_know_french_singer] <- data_raw$know_fre_2_TEXT[index_know_french_singer]
table(data_clean$know_french_singer, useNA = "ifany")

attributes(data_raw$know_fre_3_1) # An actor
table(data_raw$know_fre_3_1, useNA = "ifany")
sum(table(data_raw$know_fre_3_TEXT))
data_clean$know_french_actor <- NA
data_clean$know_french_actor[data_raw$know_fre_3_1 == 1] <- "dk"
index_know_french_actor <- data_raw$know_fre_3_TEXT != ""
data_clean$know_french_actor[index_know_french_actor] <- data_raw$know_fre_3_TEXT[index_know_french_actor]
table(data_clean$know_french_actor, useNA = "ifany")

attributes(data_raw$know_fre_4_1) # A writer
table(data_raw$know_fre_4_1, useNA = "ifany")
sum(table(data_raw$know_fre_4_TEXT))
data_clean$know_french_writer <- NA
data_clean$know_french_writer[data_raw$know_fre_4_1 == 1] <- "dk"
index_know_french_writer <- data_raw$know_fre_4_TEXT != ""
data_clean$know_french_writer[index_know_french_writer] <- data_raw$know_fre_4_TEXT[index_know_french_writer]
table(data_clean$know_french_writer, useNA = "ifany")

attributes(data_raw$know_eng_1_1) # media personality
table(data_raw$know_eng_1_1, useNA = "ifany")
sum(table(data_raw$know_eng_1_TEXT))
data_clean$know_english_media_personality <- NA
data_clean$know_english_media_personality[data_raw$know_eng_1_1 == 1] <- "dk"
index_know_english_media_personality <- data_raw$know_eng_1_TEXT != ""
data_clean$know_english_media_personality[index_know_english_media_personality] <- data_raw$know_eng_1_TEXT[index_know_english_media_personality]
table(data_clean$know_english_media_personality, useNA = "ifany")

attributes(data_raw$know_eng_2_1) # A singer
table(data_raw$know_eng_2_1, useNA = "ifany")
sum(table(data_raw$know_eng_2_TEXT))
data_clean$know_english_singer <- NA
data_clean$know_english_singer[data_raw$know_eng_2_1 == 1] <- "dk"
index_know_english_singer <- data_raw$know_eng_2_TEXT != ""
data_clean$know_english_singer[index_know_english_singer] <- data_raw$know_eng_2_TEXT[index_know_english_singer]
table(data_clean$know_english_singer, useNA = "ifany")

attributes(data_raw$know_eng_3_1) # An actor
table(data_raw$know_eng_3_1, useNA = "ifany")
sum(table(data_raw$know_eng_3_TEXT))
data_clean$know_english_actor <- NA
data_clean$know_english_actor[data_raw$know_eng_3_1 == 1] <- "dk"
index_know_english_actor <- data_raw$know_eng_3_TEXT != ""
data_clean$know_english_actor[index_know_english_actor] <- data_raw$know_eng_3_TEXT[index_know_english_actor]
table(data_clean$know_english_actor, useNA = "ifany")

attributes(data_raw$know_eng_4_1) # A writer
table(data_raw$know_eng_4_1, useNA = "ifany")
sum(table(data_raw$know_eng_4_TEXT))
data_clean$know_english_writer <- NA
data_clean$know_english_writer[data_raw$know_eng_4_1 == 1] <- "dk"
index_know_english_writer <- data_raw$know_eng_4_TEXT != ""
data_clean$know_english_writer[index_know_english_writer] <- data_raw$know_eng_4_TEXT[index_know_english_writer]
table(data_clean$know_english_writer, useNA = "ifany")

attributes(data_raw$best_cad_1_1) # book
table(data_raw$best_cad_1_1, useNA = "ifany")
sum(table(data_raw$best_cad_1_TEXT))
data_clean$best_cad_book <- NA
data_clean$best_cad_book[data_raw$best_cad_1_1 == 1] <- "dk"
index_best_cad_book <- data_raw$best_cad_1_TEXT != ""
data_clean$best_cad_book[index_best_cad_book] <- data_raw$best_cad_1_TEXT[index_best_cad_book]
table(data_clean$best_cad_book, useNA = "ifany")

attributes(data_raw$best_cad_2_1) # music album
table(data_raw$best_cad_2_1, useNA = "ifany")
sum(table(data_raw$best_cad_2_TEXT))
data_clean$best_cad_music_album <- NA
data_clean$best_cad_music_album[data_raw$best_cad_2_1 == 1] <- "dk"
index_best_cad_music_album <- data_raw$best_cad_2_TEXT != ""
data_clean$best_cad_music_album[index_best_cad_music_album] <- data_raw$best_cad_2_TEXT[index_best_cad_music_album]
table(data_clean$best_cad_music_album, useNA = "ifany")

attributes(data_raw$best_cad_3_1) # movie
table(data_raw$best_cad_3_1, useNA = "ifany")
sum(table(data_raw$best_cad_3_TEXT))
data_clean$best_cad_movie <- NA
data_clean$best_cad_movie[data_raw$best_cad_3_1 == 1] <- "dk"
index_best_cad_movie <- data_raw$best_cad_3_TEXT != ""
data_clean$best_cad_movie[index_best_cad_movie] <- data_raw$best_cad_3_TEXT[index_best_cad_movie]
table(data_clean$best_cad_movie, useNA = "ifany")

attributes(data_raw$best_cad_4_1) # newspaper
table(data_raw$best_cad_4_1, useNA = "ifany")
sum(table(data_raw$best_cad_4_TEXT))
data_clean$best_cad_newspaper <- NA
data_clean$best_cad_newspaper[data_raw$best_cad_4_1 == 1] <- "dk"
index_best_cad_newspaper <- data_raw$best_cad_4_TEXT != ""
data_clean$best_cad_newspaper[index_best_cad_newspaper] <- data_raw$best_cad_4_TEXT[index_best_cad_newspaper]
table(data_clean$best_cad_newspaper, useNA = "ifany")

# Save data

saveRDS(data_clean, "_SharedFolder_beyong_multiple_choices/data/data_open_questions.rds")
