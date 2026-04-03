library(dplyr)
library(tidyr)
library(table1)
library(purrr)
library(Hmisc)
library(REDCapR)
library(lubridate)

# REDCap tokens
api_tok <- read.csv("/Users/choiyej/Library/CloudStorage/OneDrive-SharedLibraries-UW/Laura Pyle - Bjornstad/Biostatistics Core Shared Drive/Data Harmonization/api_tokens.csv")

t1_disco_tok <- api_tok[api_tok$Study == "T1-DISCO",]$Token

# Extract MRNs for matching
t1_disco <- redcap_read(redcap_uri = "https://redcap.ucdenver.edu/api/",
                        token = t1_disco_tok)$data

ier <- t1_disco %>%
  filter(redcap_event_name %in% c("visit_1__screen_arm_1", "visit_2__baseline_arm_1")) %>%
  dplyr::summarise(
    dplyr::across(where(is.Date), ~ if (all(is.na(.x))) as.Date(NA) else last(na.omit(.x))),
    dplyr::across(where(is.character), ~ if (all(is.na(.x))) NA_character_ else last(na.omit(.x))),
    dplyr::across(where(is.numeric), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)),
    .by = record_id) %>%
  dplyr::mutate(race = case_when(rowSums(across(starts_with("race___"), ~ !is.na(.x) & .x == 1)) > 1 ~ "More than one race",
                                 race___1 == 1 ~ "American Indian/Alaska Native",
                                 race___2 == 1 ~ "Asian",
                                 race___3 == 1 ~ "Hawaiian/Pacific Islander",
                                 race___4 == 1 ~ "Black/African American",
                                 race___5 == 1 ~ "White",
                                 race___6 == 1 ~ "Other",
                                 race___7 == 1 ~ "Unknown",
                                 TRUE ~ NA_character_),
                ethnicity = case_when(ethnicity___1 == 1 ~ "Hispanic or Latino",
                                      ethnicity___2 == 1  ~ "Not Hispanic or Latino",
                                      ethnicity___2 == 1  ~ "Unknown/Not Reported"),
                race_ethnicity_condensed = case_when(race == "White" & 
                                                       ethnicity == "Not Hispanic or Latino" ~ "Not Hispanic or Latino White",
                                                     race == "Black or African American" & 
                                                       ethnicity == "Not Hispanic or Latino" ~ "Not Hispanic or Latino Black",
                                                     ethnicity == "Hispanic or Latino" ~ "Hispanic or Latino",
                                                     T ~ "Not Hispanic or Latino Other"),
                race = case_when(race == "Black/African American & White" ~ "More than one", T ~ race),
                status = case_when(record_id %in% exclude ~ "Removed",
                                   T ~ "Currently enrolled"),
                age = as.numeric((consent_date - dob)/365),
              sex = case_when(sex == 1 ~ "Male",
                              sex == 2 ~ "Female")) %>%
  dplyr::select(race, ethnicity, sex, age, status) %>%
  dplyr::rename(Race = race,
                Ethnicity = ethnicity,
                Gender = sex,
                Age = age,
                Status = status) %>%
  dplyr::mutate(Age = floor(Age), 
                "Age Unit" = "Years") %>%
  arrange(Status)

write.csv(ier, "/Users/choiyej/Library/CloudStorage/OneDrive-SharedLibraries-UW/Laura Pyle - Bjornstad/Biostatistics Core Shared Drive/T1-DISCO/Data cleaned/2025_t1_disco_ier.csv", row.names = F)

summary_table <- ier %>%
  group_by(Race, Ethnicity, Gender) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = c(Ethnicity, Gender),
    values_from = count,
    values_fill = 0
  )
summary_table
