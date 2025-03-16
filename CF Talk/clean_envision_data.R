#-------------------------------------------------------------------------------
# Setup
#-------------------------------------------------------------------------------
# Libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)
library(childsds)
library(cgmanalysis)
library(pracma)
# Home directory
home_dir <- switch(Sys.info()["sysname"],
  "Darwin" = "/Users/timvigers/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/Vigers/CF",
  "Windows" = "C:/Users/Tim/OneDrive - The University of Colorado Denver/Vigers/CF",
  "Linux" = "/home/timvigers/OneDrive/Vigers/CF"
)
setwd(home_dir)
#-------------------------------------------------------------------------------
# Insulin
#-------------------------------------------------------------------------------
# Import all files and combine
files <- list.files(
  "./Christine Chan/EnVision CF/Data_Raw/EnVision insulin re-runs",
  full.names = T
)
dfs <- lapply(files, read.csv, na.strings = c("", -999.99))
insulin <- do.call(rbind, dfs)
# Only necessary columns
insulin <- insulin %>%
  select(LastName, TestName, Result.Numeric, Collection.Date) %>%
  rename(
    study_id = LastName, Timepoint = TestName, Insulin = Result.Numeric,
    Date = Collection.Date
  )
# Add re-runs
new_insulin <- read.csv(
  "./Christine Chan/EnVision CF/Data_Raw/20240718 190422 IOWA results.csv"
)
new_insulin <- new_insulin %>%
  select(Patient.Name, Collection.Date, Test, Reported.As) %>%
  rename(
    study_id = Patient.Name, Date = Collection.Date, Timepoint = Test,
    Insulin = Reported.As
  )
insulin <- rbind(insulin, new_insulin)
# Format times/dates
insulin$Date <- lubridate::mdy(insulin$Date)
insulin$Timepoint <- sub("Insulin ", "", insulin$Timepoint)
insulin$Timepoint <- sub("Insulin-endo ", "", insulin$Timepoint)
insulin$Timepoint <- sub(" min", "", insulin$Timepoint)
insulin$Timepoint <- as.numeric(insulin$Timepoint)
#-------------------------------------------------------------------------------
# Catecholamines (Excel files needed some manual cleaning prior to this)
#-------------------------------------------------------------------------------
files <- list.files("./Christine Chan/EnVision CF/Data_Clean/Catecholamines",
  full.names = T
)
dfs <- lapply(files, read.csv)
catecholamines <- do.call(rbind, dfs)
catecholamines$Date <- mdy(catecholamines$Date)
colnames(catecholamines) <- c(
  "study_id", "Date", "Timepoint", "Norepinephrine", "Epinephrine"
)
#-------------------------------------------------------------------------------
# Glucose
#-------------------------------------------------------------------------------
redcap <- read.csv(
  "./Christine Chan/EnVision CF/Data_Raw/EnvisionCF_DATA_2024-07-19_1236.csv",
  na.strings = ""
)
timepoints <- c(0, 10, 30, 60, 90, 120, 150, 180)
glucose <- redcap %>%
  select(
    study_id, date_visit, a1c_result, num_hospitalizations,
    all_of(paste("timepoint", timepoints, "min", sep = "_"))
  ) %>%
  filter(!is.na(date_visit)) %>%
  pivot_longer(all_of(paste("timepoint", timepoints, "min", sep = "_")),
    names_to = "Timepoint", values_to = "Glucose"
  ) %>%
  rename(Date = date_visit) %>%
  filter(!is.na(Glucose))
glucose$Timepoint <- sub("timepoint_", "", glucose$Timepoint)
glucose$Timepoint <- sub("_min", "", glucose$Timepoint)
glucose$Timepoint <- as.numeric(glucose$Timepoint)
glucose$Date <- ymd(glucose$Date)
#-------------------------------------------------------------------------------
# Demographics and visit data
#-------------------------------------------------------------------------------
visits <- redcap %>%
  select(
    study_id, date_visit, age_visit, height, weight, bmi_study_visit,
    sex, origin_race, ethnicity, redcap_data_access_group,
    cftr_mutation_1, cftr_mutation_2
  ) %>%
  filter(!is.na(date_visit)) %>%
  rename(Date = date_visit) %>%
  filter(!if_all(age_visit:ethnicity, is.na))
visits$Date <- ymd(visits$Date)
#-------------------------------------------------------------------------------
# Date fixes per Holly's 7/19 emails
#-------------------------------------------------------------------------------
insulin$Date[insulin$study_id == "CC0014" &
  insulin$Date == "2022-11-16"] <- "2022-11-21"
insulin$Date[insulin$study_id == "CC0026" &
  insulin$Date == "2021-05-26"] <- "2022-05-26"
insulin$Date[insulin$study_id == "CC0035" &
  insulin$Date == "2020-08-01"] <- "2020-08-10"
insulin$Date[insulin$study_id == "CC0041" &
  insulin$Date == "2021-09-30"] <- "2020-09-30"
insulin$Date[insulin$study_id == "CC0065" &
  insulin$Date == "2021-09-07"] <- "2021-09-08"
insulin$Date[insulin$study_id == "IA0002" &
  insulin$Date == "2021-10-28"] <- "2019-10-28"
insulin$Date[insulin$study_id == "IA0013" &
  insulin$Date == "2019-11-17"] <- "2019-11-07"
insulin$Date[insulin$study_id == "IA0034" &
  insulin$Date == "2019-10-30"] <- "2019-10-29"
insulin$Date[insulin$study_id == "IA0040" &
  insulin$Date == "2021-08-03"] <- "2020-08-03"
insulin$Date[insulin$study_id == "IA0068" &
  insulin$Date == "2020-08-10"] <- "2020-06-17"
insulin$Date[insulin$study_id == "MN0004" &
  insulin$Date == "2021-04-14"] <- "2021-04-21"
insulin$Date[insulin$study_id == "MN0004" &
  insulin$Date == "2021-04-14"] <- "2021-04-21"
insulin$Date[insulin$study_id == "MN0019" &
  insulin$Date == "2021-01-04"] <- "2021-01-06"
insulin$Date[insulin$study_id == "MN0020" &
  insulin$Date == "2021-06-19"] <- "2021-06-17"
insulin$Date[insulin$study_id == "MN0023" &
  insulin$Date == "2016-12-15"] <- "2019-12-05"
insulin$Date[insulin$study_id == "MN0046" &
  insulin$Date == "2020-04-02"] <- "2020-09-02"
insulin$Date[insulin$study_id == "MN0056" &
  insulin$Date == "2021-05-27"] <- "2021-05-26"
insulin$Date[insulin$study_id == "MN0078" &
  insulin$Date == "2021-09-16"] <- "2021-09-15"
insulin$Date[insulin$study_id == "WU0003" &
  insulin$Date == "2020-11-16"] <- "2020-11-19"
insulin$Date[insulin$study_id == "WU0004" &
  insulin$Date == "2019-08-29"] <- "2019-08-30"
insulin$Date[insulin$study_id == "WU0013" &
  insulin$Date == "2019-11-15"] <- "2019-11-05"
insulin$Date[insulin$study_id == "IA0103" &
  insulin$Date == "2021-07-26"] <- "2021-08-02"
insulin$study_id[insulin$study_id == "EVUI00001"] <- "IA0001"
insulin$study_id[insulin$study_id == "IA0119"] <- "ia0119"
insulin <-
  insulin[-which(insulin$study_id == "IA0075" & insulin$Date == "2020-02-18"), ]
insulin <-
  insulin[-which(insulin$study_id == "MN0010" & insulin$Date == "2020-01-14"), ]
catecholamines$Date[catecholamines$study_id == "IA0091" &
  catecholamines$Date == "2020-02-16"] <- "2020-12-16"
catecholamines$Date[catecholamines$study_id == "IA0096" &
  catecholamines$Date == "2021-01-09"] <- "2021-01-08"
catecholamines$Date[catecholamines$study_id == "IA0110" &
  catecholamines$Date == "2021-07-06"] <- "2021-07-08"
# Manually add insulin for IA005 6/22/20
ia005 <- data.frame(
  "study_id" = rep("IA0005", 8), "Date" = rep(ymd("2020-06-22"), 8),
  "Timepoint" = c(0, 10, 30, 60, 90, 120, 150, 180),
  "Insulin" = as.character(c(6, NA, 40, 49, 31, 26, 7, 6))
)
insulin <- rbind(insulin, ia005)
# Remove exact duplicates
insulin <- insulin %>% distinct()
# For duplicates with different insulin values, take the first (per Katie)
insulin <- insulin %>%
  group_by(study_id, Date, Timepoint) %>%
  summarise(Insulin = first(na.omit(Insulin)), .groups = "drop")
# Convert to numeric
insulin$Insulin[insulin$Insulin %in%
  c(
    "<1, hemolyzed", "<1, slightly hemolyzed", "1 Hemolyzed",
    "1 Slightly hemolyzed", "2 Slightly hemolyzed", "No Sample Received"
  )] <- NA
insulin$Insulin <- as.numeric(insulin$Insulin)
#-------------------------------------------------------------------------------
# Hypoglycemia symptom surveys
#-------------------------------------------------------------------------------
hypo_surveys <- c(
  "adren_score_baseline", "adren_score_120", "adren_score_150",
  "adren_score_180", "neuro_score_baseline", "neuro_score_120",
  "neuro_score_150", "neuro_score_180", "total_score_baseline",
  "total_score_120", "total_score_150", "total_score_180", "num_symptoms_base",
  "num_symptoms_120", "num_symptoms_150", "num_symptoms_180"
)
hypo_symptoms <- redcap %>%
  select(study_id, date_visit, all_of(hypo_surveys)) %>%
  rename(
    Date = date_visit, adren_score_0 = adren_score_baseline,
    neuro_score_0 = neuro_score_baseline, total_score_0 = total_score_baseline,
    num_symptoms_0 = num_symptoms_base
  ) %>%
  filter(
    !is.na(Date),
    !if_all(adren_score_0:num_symptoms_180, ~ is.na(.))
  )
hypo_symptoms$Date <- ymd(hypo_symptoms$Date)
#-------------------------------------------------------------------------------
# CGM data
#-------------------------------------------------------------------------------
# Clean data (does not need to be run every time)
# Many of these files appeared to be manually edited and are missing some or all
# of the first 2 rows. Some were saved as XLSX or TXT files as well.
# cleandata(
#   inputdirectory = "./Christine Chan/EnVision CF/Data_Clean/CGM/Manually Edited",
#   outputdirectory = "./Christine Chan/EnVision CF/Data_Clean/CGM/Cleaned",
#   id_filename = T, verbose = T
# )
# cgmvariables(
#   inputdirectory = "./Christine Chan/EnVision CF/Data_Clean/CGM/Cleaned",
#   outputdirectory = "./Christine Chan/EnVision CF/Data_Clean/CGM",
#   outputname = "cgm_variables"
# )
cgm <- read.csv("./Christine Chan/EnVision CF/Data_Clean/CGM/cgm_variables.csv",
  na.strings = ""
)
cgm <- cgm[-which(cgm$percent_cgm_wear == 0), ]
cgm$subject_id <- sub("_sensor_raw_data_upload", "", cgm$subject_id)
cgm$study_id <- sapply(
  stri_split_fixed(str = cgm$subject_id, pattern = "_", n = 2),
  "[", 1
)
cgm$redcap_event_name <- sapply(
  stri_split_fixed(str = cgm$subject_id, pattern = "_", n = 2),
  "[", 2
)
cgm <- left_join(cgm,
  redcap %>%
    select(study_id, redcap_event_name, date_visit) %>%
    distinct() %>% drop_na(),
  by = join_by(study_id, redcap_event_name)
)
cgm <- cgm %>% rename(Date = date_visit)
cgm$Date <- ymd(cgm$Date)
# Manually fix dates for CGM replacements/unscheduled visits
cgm$Date[cgm$study_id == "IA0001" &
  cgm$redcap_event_name == "visit_2_arm_1_replacement_cgm_raw_data_upload"] <-
  "2020-11-09"
cgm$Date[cgm$study_id == "IA0002" &
  cgm$redcap_event_name == "visit_2_arm_1_replacement_cgm_raw_data_upload"] <-
  "2020-08-10"
cgm$Date[cgm$study_id == "IA0014" &
  cgm$redcap_event_name == "visit_2_arm_1_replacement_cgm_raw_data_upload"] <-
  "2020-09-03"
cgm$Date[cgm$study_id == "IA0096" &
  cgm$redcap_event_name == "visit_1_arm_1_replacement_cgm_raw_data_upload"] <-
  "2021-01-08"
# Remove unnecessary columns
cgm$subject_id <- NULL
cgm$date_cgm_placement <- NULL
#-------------------------------------------------------------------------------
# hOGTT data
#-------------------------------------------------------------------------------
files <- list.files("./Christine Chan/EnVision CF/Data_Clean/hOGTTs",
  full.names = T
)
ogtts <- lapply(files, function(f) {
  # Import
  d <- read.csv(f,
    na.strings = c("", "not detectable", "not detecable", "no serum")
  )
  # Get run date
  date <- sub("./Christine Chan/EnVision CF/Data_Clean/hOGTTs/summary_", "", f)
  date <- sub("\\.csv", "", date)
  d$iowa_run_date <- mdy(date)
  # Format
  d$study_id <- gsub(" ", "", d$study_id)
  d$Timepoint <- as.numeric(sub(" min", "", d$Timepoint))
  return(d)
})
ogtts <- do.call(rbind, ogtts)
# Get rid of insulin values because those were re-run at CHCO
ogtts$Insulin <- NULL
# Wide version for merging later
ogtts <- ogtts %>%
  pivot_wider(names_from = Timepoint, values_from = c(C.Peptide:GIP)) %>%
  arrange(study_id, iowa_run_date) %>%
  filter(is.na(Status))
ogtts <- ogtts %>%
  group_by(study_id) %>%
  mutate(ogtt_num = row_number()) %>%
  ungroup()
#-------------------------------------------------------------------------------
# Combine everything in a wide format
#-------------------------------------------------------------------------------
# Merge
final_df <- full_join(glucose, insulin)
final_df <- full_join(final_df, catecholamines)
final_df <- final_df %>% pivot_wider(
  names_from = Timepoint, values_from = c(Glucose:Epinephrine)
)
final_df <- full_join(final_df, visits)
final_df <- full_join(final_df, hypo_symptoms)
# Add OGTT data
final_df <- final_df %>%
  arrange(study_id, Date) %>%
  group_by(study_id) %>%
  mutate(ogtt_num = row_number()) %>%
  ungroup() %>%
  select(study_id, Date, ogtt_num, everything())
final_df <- full_join(final_df, ogtts)
final_df <- final_df %>% arrange(study_id, Date)
# Add CGM
final_df <- full_join(final_df, cgm)
# Add HbA1c, etc.
a1c <- redcap %>%
  select(study_id, date_visit, a1c_result) %>%
  rename(Date = date_visit) %>%
  drop_na(a1c_result)
a1c$Date <- ymd(a1c$Date)
final_df <- full_join(final_df, a1c)
#-------------------------------------------------------------------------------
# Calculated fields
#-------------------------------------------------------------------------------
# BMI
final_df$bmi_study_visit <- NULL
final_df$bmi <- final_df$weight / ((final_df$height^2) / 10000)
final_df$bmi_perc <- sds(
  value = final_df$bmi,
  age = ifelse(final_df$age_visit < 20, final_df$age_visit, 20),
  sex = final_df$sex, male = 1, female = 2,
  ref = cdc.ref, item = "bmi", type = "perc"
) * 100
# Columns
glucose <- paste0("Glucose_", c(0, 10, 30, 60, 90, 120, 150, 180))
insulin <- paste0("Insulin_", c(0, 10, 30, 60, 90, 120, 150, 180))
# Get diagnosis
final_df$Diagnosis <- NA
final_df$Diagnosis[final_df[, glucose[1]] < 100 &
  (rowSums(final_df[, glucose[2:5]] < 200, na.rm = T) ==
    rowSums(!is.na(final_df[, glucose[2:5]]))) &
  (rowSums(final_df[, glucose[6:8]] < 140, na.rm = T) ==
    rowSums(!is.na(final_df[, glucose[6:8]])))] <- "NGT"
final_df$Diagnosis[final_df[, glucose[1]] >= 100] <- "IFG"
final_df$Diagnosis[rowSums(final_df[, glucose[2:5]] >= 200, na.rm = T) > 0] <-
  "INDET"
final_df$Diagnosis[rowSums(final_df[, glucose[6:8]] >= 140, na.rm = T) > 0] <-
  "IGF"
final_df$Diagnosis[final_df$Glucose_0 >= 126 | final_df$Glucose_120 >= 200] <-
  "CFRD"
final_df$Diagnosis <- factor(final_df$Diagnosis,
  levels = c("NGT", "IFG", "INDET", "IGF", "CFRD")
)
# iAUCs
final_df$iAUC30gluc <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[glucose[1:3]]) - as.numeric(r[glucose[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC60gluc <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[glucose[1:4]]) - as.numeric(r[glucose[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC120gluc <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[glucose[1:6]]) - as.numeric(r[glucose[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60, 90, 120)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC180gluc <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[glucose]) - as.numeric(r[glucose[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60, 90, 120, 150, 180)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC30ins <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[insulin[1:3]]) - as.numeric(r[insulin[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC60ins <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[insulin[1:4]]) - as.numeric(r[insulin[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC120ins <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[insulin[1:6]]) - as.numeric(r[insulin[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60, 90, 120)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
final_df$iAUC180ins <- apply(final_df, 1, function(r) {
  y <- as.numeric(r[insulin]) - as.numeric(r[insulin[1]])
  if (is.na(first(y)) | is.na(last(y))) {
    return(NA)
  } else {
    x <- c(0, 10, 30, 60, 90, 120, 150, 180)[!is.na(y)]
    y <- y[!is.na(y)]
    auc <- trapz(x, y)
    return(auc)
  }
})
# HOMA IR
final_df$homa_ir <- (final_df$Glucose_0 * final_df$Insulin_0) / 405
# Check for hypoglycemia
final_df$Hypo70 <-
  apply(final_df[, glucose], 1, function(r) {
    any(r < 70, na.rm = T)
  })
final_df$Hypo60 <-
  apply(final_df[, glucose], 1, function(r) {
    any(r < 60, na.rm = T)
  })
final_df$Hypo70 <- factor(final_df$Hypo70,
  levels = c(T, F),
  labels = c("Hypoglycemia < 70", "No Hypoglycemia < 70")
)
final_df$Hypo60 <- factor(final_df$Hypo60,
  levels = c(T, F),
  labels = c("Hypoglycemia < 60", "No Hypoglycemia < 60")
)
# CFTR groups
final_df$CFTR <- paste0(final_df$cftr_mutation_1, final_df$cftr_mutation_2)
final_df$CFTR <- factor(final_df$CFTR,
  levels = c("11", "12", "22", "NANA"),
  labels = c(
    "F508del homozygous", "F508del heterozygous", "Other/Other", "Unknown"
  )
)
final_df$cftr_mutation_1 <- NULL
final_df$cftr_mutation_2 <- NULL
# Final formatting
final_df$redcap_data_access_group <- gsub("\\d", "", final_df$study_id)
final_df$redcap_data_access_group <- factor(
  final_df$redcap_data_access_group,
  levels = c("CC", "ia", "IA", "MN", "WU"),
  labels = c("Colorado", "Iowa", "Iowa", "Minnesota", "Washington University")
)
# Write
write.csv(final_df,
  file = "./Christine Chan/EnVision CF/Data_Clean/envision_analysis_dataset.csv",
  row.names = F, na = ""
)
