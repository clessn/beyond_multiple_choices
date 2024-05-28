library(dplyr)

variables <- names(readRDS("_SharedFolder_beyong_multiple_choices/data/selected_variables.rds"))

data_raw <- haven::read_sav("_SharedFolder_beyong_multiple_choices/data/data_raw.sav") |> 
    filter(code == "complete")

data_clean <- data.frame(id = 1:nrow(data_raw))

# Occupation variable cleaning

attributes(data_raw$occupation_1)
table(data_raw$occupation_1)
attributes(data_raw$occupation_2)
attributes(data_raw$occupation_3)
attributes(data_raw$occupation_4)
attributes(data_raw$occupation_5)
attributes(data_raw$occupation_6)
attributes(data_raw$occupation_7)
attributes(data_raw$occupation_8)
attributes(data_raw$occupation_9)
attributes(data_raw$occupation_12)
attributes(data_raw$occupation_13)
attributes(data_raw$occupation_10)
data_clean$occupation <- NA
data_clean$occupation_to_clean <- NA
data_clean$occupation[data_raw$occupation_1 == 1] <- "self_employed"
data_clean$occupation[data_raw$occupation_2 == 1] <- "salaried_work"
data_clean$occupation[data_raw$occupation_3 == 1] <- "retired"
data_clean$occupation[data_raw$occupation_4 == 1] <- "unemployed"
data_clean$occupation[data_raw$occupation_5 == 1] <- "student"
data_clean$occupation[data_raw$occupation_6 == 1] <- "caregiver"
data_clean$occupation[data_raw$occupation_7 == 1] <- "parent_at_home"
data_clean$occupation[data_raw$occupation_8 == 1] <- "unemployed_handicap"
data_clean$occupation[data_raw$occupation_9 == 1] <- "multiple_jobs_self_employed"
data_clean$occupation[data_raw$occupation_12 == 1] <- "multiple_jobs_salaried_work"
data_clean$occupation[data_raw$occupation_13 == 1] <- "social_assistance"
index_occupation <- data_raw$occupation_10 == 1 & data_raw$occupation_10_TEXT != ""
data_clean$occupation[index_occupation] <- data_raw$occupation_10_TEXT[index_occupation]

data_clean$occupation_to_clean[is.na(data_raw$occupation_10)] <- 0
data_clean$occupation_to_clean[index_occupation] <- 1


table(data_clean$occupation)
table(data_clean$occupation_to_clean)

# work field variable cleaning

attributes(data_raw$travail_domaine_1)
attributes(data_raw$travail_domaine_2)
attributes(data_raw$travail_domaine_3)
attributes(data_raw$travail_domaine_4)
attributes(data_raw$travail_domaine_5)
attributes(data_raw$travail_domaine_6)
attributes(data_raw$travail_domaine_7)
attributes(data_raw$travail_domaine_8)
attributes(data_raw$travail_domaine_9)
attributes(data_raw$travail_domaine_10)
attributes(data_raw$travail_domaine_11)

data_clean$work_field <- NA
data_clean$work_field_to_clean <- NA

data_clean$work_field[data_raw$travail_domaine_1 == 1] <- "management"
data_clean$work_field[data_raw$travail_domaine_2 == 1] <- "finance_business"
data_clean$work_field[data_raw$travail_domaine_3 == 1] <- "natural_sciences"
data_clean$work_field[data_raw$travail_domaine_4 == 1] <- "health"
data_clean$work_field[data_raw$travail_domaine_5 == 1] <- "education_law_government"
data_clean$work_field[data_raw$travail_domaine_6 == 1] <- "arts_sports"
data_clean$work_field[data_raw$travail_domaine_7 == 1] <- "sales"
data_clean$work_field[data_raw$travail_domaine_8 == 1] <- "trades_transport"
data_clean$work_field[data_raw$travail_domaine_9 == 1] <- "agriculture"
data_clean$work_field[data_raw$travail_domaine_10 == 1] <- "manufacturing"
index_field <- data_raw$travail_domaine_11 == 1 & data_raw$travail_domaine_11_TEXT != ""
data_clean$work_field[index_field] <- data_raw$travail_domaine_11_TEXT[index_field]

data_clean$work_field_to_clean[is.na(data_raw$travail_domaine_11)] <- 0
data_clean$work_field_to_clean[index_field] <- 1

table(data_clean$work_field)
table(data_clean$work_field_to_clean)

# Religion variable cleaning

data_clean$religion <- NA
data_clean$religion_to_clean <- NA

attributes(data_raw$religion)
data_clean$religion[data_raw$religion == 1] <- "none"
data_clean$religion[data_raw$religion == 2] <- "agnostic"
data_clean$religion[data_raw$religion == 3] <- "buddhism"
data_clean$religion[data_raw$religion == 4] <- "hindu"
data_clean$religion[data_raw$religion == 5] <- "judaism"
data_clean$religion[data_raw$religion == 6] <- "islam"
data_clean$religion[data_raw$religion == 7] <- "sikhism"
data_clean$religion[data_raw$religion == 8] <- "catholicism"
data_clean$religion[data_raw$religion == 9] <- "protestantism"
data_clean$religion[data_raw$religion == 10] <- "orthodox"
index_religion <- data_raw$religion == 11 & data_raw$religion_11_TEXT != ""
data_clean$religion[index_religion] <- data_raw$religion_11_TEXT[index_religion]

data_clean$religion_to_clean[data_raw$religion != 11] <- 0
data_clean$religion_to_clean[index_religion] <- 1

table(data_clean$religion)
table(data_clean$religion_to_clean)

# Housing variable cleaning

data_clean$habitation <- NA
data_clean$habitation_to_clean <- NA

attributes(data_raw$habitation)
data_clean$habitation[data_raw$habitation == 1] <- "appartment"
data_clean$habitation[data_raw$habitation == 2] <- "loft"
data_clean$habitation[data_raw$habitation == 3] <- "condo"
data_clean$habitation[data_raw$habitation == 4] <- "high_rise"
data_clean$habitation[data_raw$habitation == 5] <- "house"
data_clean$habitation[data_raw$habitation == 6] <- "town_house"
data_clean$habitation[data_raw$habitation == 7] <- "duplex"
data_clean$habitation[data_raw$habitation == 8] <- "coop"
data_clean$habitation[data_raw$habitation == 9] <- "social_housing"
data_clean$habitation[data_raw$habitation == 10] <- "mobile_home"
index_habitation <- data_raw$habitation == 11 & data_raw$habitation_11_TEXT != ""
data_clean$habitation[index_habitation] <- data_raw$habitation_11_TEXT[index_habitation]

data_clean$habitation_to_clean[data_raw$habitation != 11] <- 0
data_clean$habitation_to_clean[index_habitation] <- 1

table(data_clean$habitation)
table(data_clean$habitation_to_clean)

# Style variable cleaning

data_clean$style <- NA
data_clean$style_to_clean <- NA

attributes(data_raw$style)
data_clean$style[data_raw$style == 1] <- "hippie"
data_clean$style[data_raw$style == 2] <- "elegant"
data_clean$style[data_raw$style == 3] <- "classical"
data_clean$style[data_raw$style == 4] <- "casual"
data_clean$style[data_raw$style == 5] <- "formal"
data_clean$style[data_raw$style == 6] <- "punk"
data_clean$style[data_raw$style == 7] <- "rock"
data_clean$style[data_raw$style == 8] <- "sporty"

index_style <- data_raw$style == 9 & data_raw$style_9_TEXT != ""
data_clean$style[index_style] <- data_raw$style_9_TEXT[index_style]

data_clean$style_to_clean[data_raw$style != 9] <- 0
data_clean$style_to_clean[index_style] <- 1

table(data_clean$style)
table(data_clean$style_to_clean)

# Consultation variable cleaning

data_clean$consultation <- NA
data_clean$consultation_to_clean <- NA

attributes(data_raw$consult_who_1)
attributes(data_raw$consult_who_2)
attributes(data_raw$consult_who_3)
attributes(data_raw$consult_who_4)
attributes(data_raw$consult_who_5)
attributes(data_raw$consult_who_6)
attributes(data_raw$consult_who_7)
data_clean$consultation[data_raw$consult_who_1 == 1] <- "family_doctor"
data_clean$consultation[data_raw$consult_who_2 == 1] <- "psychiatrist"
data_clean$consultation[data_raw$consult_who_3 == 1] <- "psychologist"
data_clean$consultation[data_raw$consult_who_4 == 1] <- "nurse"
data_clean$consultation[data_raw$consult_who_5 == 1] <- "social_worker"
data_clean$consultation[data_raw$consult_who_7 == 1] <- "not consulted"

index_consultation <- data_raw$consult_who_6 == 1 & data_raw$consult_who_6_TEXT != ""
data_clean$consultation[index_consultation] <- data_raw$consult_who_6_TEXT[index_consultation]

data_clean$consultation_to_clean[is.na(data_raw$consult_who_6)] <- 0
data_clean$consultation_to_clean[index_consultation] <- 1

table(data_clean$consultation)
table(data_clean$consultation_to_clean)

# Save cleaned data

saveRDS(data_clean, "_SharedFolder_beyong_multiple_choices/data/data_beyond_multiple_choices.rds")


