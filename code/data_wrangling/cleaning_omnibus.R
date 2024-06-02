library(dplyr)


## Get a vector of the ids with open answers about MI issue ----------------
ids <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/surveys.rds") %>% 
  filter(source_id %in% c("february", "march", "april",
                          "may", "pilote1", "pilote2")) %>% ## source_id with open answers about MI issue
  pull(., id)

Data <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/surveys.rds")

## ! Respondents to keep pilote 1 ----------------------------------------

## in this survey, some respondents were removed in the cleaning.
clean_survey <- sondr::read_any_csv("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/Pilote1_clean.csv")
respondents_to_keep_pilote1 <- clean_survey$id ## vector containing the rows to keep

# remove unwanted df from environment
rm(clean_survey)

## ! Respondents to remove from pilote 2 -------------------------------------
all_respondents_df <- sondr::read_any_csv("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/datagotchi_pilot2_2022.csv")
## this df contains all 1970 respondents of this survey, but with the RCI cleaned.

#### for the RCI
####### if the respondent has answered NA to 4 or less parties,
######### this means that the NAs should become 0.5, the default position of the slider in Qualtrics
######### (programming error in Qualtrics)
raw1 <- readRDS("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/Pilote2.rds")
raw1$nas <- rowSums(is.na(raw1 %>% select(starts_with("potGrowth"))))
table(raw1$nas)
# 0 means the respondent answered for the 5 parties (nothing to do)
# 1,2,3 or 4 means the respondent answered the question but skipped some parties
##### (which means the respondent didnt change the party from the default 5 position in Qualtrics)
# 5 means the respondent didnt answer the question

### Remove respondents who didnt answer potGrowth question
respondents_to_remove_pilote2 <- which(raw1$nas==5)

## remove unwanted dataframes
rm(all_respondents_df, raw1)

## Merge open answers ------------------------------------------------------
table(Data$source_id)

rm_open_answers <- function(source_id, idvar_in_survey = "QUEST", idvar_in_open = "{ID de fiche}"){
  survey_ids <- haven::read_sav(paste0("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/omnibus/", source_id, "/", source_id, ".Sav"))[[idvar_in_survey]]
  open_answers <- readxl::read_excel(paste0("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/omnibus", source_id, "/", source_id, "_open.xlsx"))[[idvar_in_open]]
  output <- open_answers[!(open_answers %in% survey_ids)]
  return(output)
}

rm_open_answers(source_id = "february",
                 idvar_in_survey = "QUEST",
                 idvar_in_open = "{ID de fiche}")

rm_open_answers(source_id = "march")

## February ----------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/february/february_open.xlsx")
feb_answers <- open$O_C7[!(open$`{ID de fiche}` %in% rm_open_answers("february"))]

## March -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/march/march_open.xlsx")
march_answers <- open$O_C6[!(open$`{ID de fiche}` %in% rm_open_answers("march"))]

## April -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/april/april_open.xlsx")
april_answers <- open$O_C6[!(open$QUEST %in% rm_open_answers("april", idvar_in_open = "QUEST"))]

## May -------------------------------------------------------------------

open <- readxl::read_excel("_SharedFolder_memoire-pot-growth/data/lake/omnibus/may/may_open.xlsx")
may_answers <- open$O_C6[!(open$QUEST %in% rm_open_answers("may", idvar_in_open = "QUEST"))]

## Pilote1 -----------------------------------------------------------------

pilote1_answers <- haven::read_sav("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot1_2022/ULA12-BASE-1500.sav")$Q83O[respondents_to_keep_pilote1]

## Pilote2 -----------------------------------------------------------------

pilote2_answers_fr <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv")$FR_openFutur[-respondents_to_remove_pilote2][-c(1:2)]
pilote2_answers_fr[is.na(pilote2_answers_fr)] <- NA
pilote2_answers_en <- sondr::read_any_csv("_SharedFolder_memoire-pot-growth/data/lake/datagotchi_pilot2_2022/datagotchi_pilot2_2022.csv")$EN_openPersonally[-respondents_to_remove_pilote2][-c(1:2)]
pilote2_answers_en[is.na(pilote2_answers_en)] <- NA
pilote2_answers <- coalesce(pilote2_answers_fr, pilote2_answers_en)

rm(pilote2_answers_fr, pilote2_answers_en)

# Merge it all ------------------------------------------------------------

open_answers <- c(feb_answers, march_answers, april_answers, may_answers, pilote1_answers, pilote2_answers)
names(open_answers) <- c(Data$id[Data$source_id == "february"],
                         Data$id[Data$source_id == "march"],
                         Data$id[Data$source_id == "april"],
                         Data$id[Data$source_id == "may"],
                         Data$id[Data$source_id == "pilote1"],
                         Data$id[Data$source_id == "pilote2"])