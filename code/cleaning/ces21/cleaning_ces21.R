library(dplyr)

# Variables\ to select
# cps21_imp_iss
# cps21_genderid 
# cps21_province
# cps21_education
# cps21_religion
# cps21_age

data <- sondr::read_any_csv("_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/raw/ces2021.csv")  

data_raw <- data |> 
  select(cps21_imp_iss, cps21_genderid, cps21_province, cps21_education, cps21_religion, cps21_income_cat, cps21_age, cps21_language_1) |> 
    filter(cps21_language_1 == 1)

data_clean <- data.frame(id = 1:nrow(data_raw))

# clean gender

table(data_raw$cps21_genderid)
data_clean$ses_female <- NA
data_clean$ses_female[data_raw$cps21_genderid == "A woman"] <- 1
data_clean$ses_female[data_raw$cps21_genderid != "A woman"] <- 0
table(data_clean$ses_female)

# clean province

table(data_raw$cps21_province)
data_clean$ses_province <- NA
data_clean$ses_province[data_raw$cps21_province == "Alberta"] <- "ab"
data_clean$ses_province[data_raw$cps21_province == "British Columbia"] <- "bc"
data_clean$ses_province[data_raw$cps21_province == "Manitoba"] <- "mb"
data_clean$ses_province[data_raw$cps21_province == "New Brunswick"] <- "nb"
data_clean$ses_province[data_raw$cps21_province == "Newfoundland and Labrador"] <- "nl"
data_clean$ses_province[data_raw$cps21_province == "Nova Scotia"] <- "ns"
data_clean$ses_province[data_raw$cps21_province == "Ontario"] <- "on"
data_clean$ses_province[data_raw$cps21_province == "Prince Edward Island"] <- "pe"
data_clean$ses_province[data_raw$cps21_province == "Quebec"] <- "qc"
data_clean$ses_province[data_raw$cps21_province == "Saskatchewan"] <- "sk"
data_clean$ses_province[data_raw$cps21_province == "Northwest Territories"] <- "nt"
data_clean$ses_province[data_raw$cps21_province == "Nunavut"] <- "nu"
data_clean$ses_province[data_raw$cps21_province == "Yukon"] <- "yt"
data_clean$ses_province <- as.factor(data_clean$ses_province)
table(data_clean$ses_province)

# clean education

table(data_raw$cps21_education)
data_clean$ses_education <- NA
data_clean$ses_education[data_raw$cps21_education == "No schooling "] <- "no_school"
data_clean$ses_education[data_raw$cps21_education == "Some elementary school"] <- "some_elem"
data_clean$ses_education[data_raw$cps21_education == "Completed elementary school"] <- "elem"
data_clean$ses_education[data_raw$cps21_education == "Some secondary/ high school"] <- "some_hs"
data_clean$ses_education[data_raw$cps21_education == "Completed secondary/ high school"] <- "hs"
data_clean$ses_education[data_raw$cps21_education == "Some technical, community college, CEGEP, College Classique"] <- "some_post_sec"
data_clean$ses_education[data_raw$cps21_education == "Completed technical, community college, CEGEP, College Classique"] <- "post_sec"
data_clean$ses_education[data_raw$cps21_education == "Some university"] <- "some_uni"
data_clean$ses_education[data_raw$cps21_education == "Bachelor's degree"] <- "undergrad"
data_clean$ses_education[data_raw$cps21_education == "Master's degree"] <- "masters"
data_clean$ses_education[data_raw$cps21_education == "Professional degree or doctorate"] <- "phd"
data_clean$ses_education <- as.factor(data_clean$ses_education)
table(data_clean$ses_education)

# clean religion

table(data_raw$cps21_religion)
data_clean$ses_religion <- NA
data_clean$ses_religion[data_raw$cps21_religion == "Agnostic"] <- "agnostic"
data_clean$ses_religion[data_raw$cps21_religion == "Anglican/ Church of England"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Baptist"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Buddhist/ Buddhism"] <- "buddhist"
data_clean$ses_religion[data_raw$cps21_religion == "Catholic/ Roman Catholic/ RC"] <- "catholic"
data_clean$ses_religion[data_raw$cps21_religion == "Christian Reformed"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Don't know/ Prefer not to answer"] <- "dont_know"
data_clean$ses_religion[data_raw$cps21_religion == "Greek Orthodox/ Ukrainian Orthodox/ Russian Orthodox/ Eastern Orthodox"] <- "orthodox"
data_clean$ses_religion[data_raw$cps21_religion == "Hindu"] <- "hindu"
data_clean$ses_religion[data_raw$cps21_religion == "Jehovah's Witness"] <- "jehovahs_witness"
data_clean$ses_religion[data_raw$cps21_religion == "Jewish/ Judaism/ Jewish Orthodox"] <- "jewish"
data_clean$ses_religion[data_raw$cps21_religion == "Lutheran"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Mennonite"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Mormon/ Church of Jesus Christ of the Latter Day Saints"] <- "mormon"
data_clean$ses_religion[data_raw$cps21_religion == "Muslim/ Islam"] <- "muslim"
data_clean$ses_religion[data_raw$cps21_religion == "None/ Don't have one/ Atheist"] <- "none_atheist"
data_clean$ses_religion[data_raw$cps21_religion == "Other (please specify)"] <- "other"
data_clean$ses_religion[data_raw$cps21_religion == "Pentecostal/ Fundamentalist/ Born Again/ Evangelical"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Presbyterian"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Protestant"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Salvation Army"] <- "protestant"
data_clean$ses_religion[data_raw$cps21_religion == "Sikh/ Sikhism"] <- "sikh"
data_clean$ses_religion[data_raw$cps21_religion == "United Church of Canada"] <- "protestant"
data_clean$ses_religion <- as.factor(data_clean$ses_religion)
table(data_clean$ses_religion)

# clean age

table(data_raw$cps21_age)
data_clean$ses_age <- NA
data_clean$ses_age <- as.numeric(data_raw$cps21_age)
table(data_clean$ses_age)

# input important issue

table(data_raw$cps21_imp_iss)
data_clean$open_ended_issue <- NA
data_raw$cps21_imp_iss <- iconv(data_raw$cps21_imp_iss, from = "latin1", to = "UTF-8")
data_clean$open_ended_issue <- tolower(data_raw$cps21_imp_iss)
table(data_clean$open_ended_issue)

# Save data

saveRDS(data_clean, "_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_clean.rds")

# create a random subset of 200 respondents

data_subset <- data_clean[sample(nrow(data), 200), ]

# Save the data to a file

saveRDS(data_subset, "_SharedFolder_beyong_multiple_choices/data/ollama_cleaning_process/ces/clean/ces2021_200_subset.rds")