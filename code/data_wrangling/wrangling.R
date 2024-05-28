library(dplyr)

data <- haven::read_sav("_SharedFolder_beyong_multiple_choices/data/data_raw.sav") |> 
    filter(code == "complete")

data_text_prelim <- data  |> 
    select(ends_with("TEXT"))

data_text <- data_text_prelim  |>
    select(-taille_1_TEXT, -taille_2_TEXT, -poids_1_TEXT, -poids_2_TEXT) |> 
    select(-starts_with("know")) |>
    select(-starts_with("best"))

data_bmc <- data |>
    select(starts_with("occupation"), starts_with("travail_domaine"), starts_with("religion"), starts_with("habitation"), starts_with("style"), starts_with("consult_who"))

saveRDS(data_bmc, "_SharedFolder_beyong_multiple_choices/data/selected_variables.rds")
