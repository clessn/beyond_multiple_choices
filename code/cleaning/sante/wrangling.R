library(dplyr)

data <- haven::read_sav("_SharedFolder_beyong_multiple_choices/data/data_raw.sav") |> 
    filter(code == "complete")

data_text_prelim <- data  |> 
    select(ends_with("TEXT"))

data_text <- data_text_prelim  |>
    select(-taille_1_TEXT, -taille_2_TEXT, -poids_1_TEXT, -poids_2_TEXT) |> 
    select(-starts_with("know")) |>
    select(-starts_with("best"))

data_semi_open <- data |>
    select(starts_with("occupation"), starts_with("travail_domaine"), starts_with("religion"), starts_with("habitation"), starts_with("style"), starts_with("consult_who"))

data_full_open <- data |>
    select(starts_with("know"), starts_with("best"))

saveRDS(data_semi_open, "_SharedFolder_beyong_multiple_choices/data/selected_variables_semi_open.rds")

saveRDS(data_full_open, "_SharedFolder_beyong_multiple_choices/data/selected_variables_full_open.rds")
