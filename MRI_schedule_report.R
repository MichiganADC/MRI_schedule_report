#!/usr/bin/env Rscript


# Load useful libraries ----

suppressMessages( library(crayon) )
cat(cyan("Loading packages...\n"))

suppressMessages( library(dplyr) )
suppressMessages( library(readr) )
suppressMessages( library(rlang) )
suppressMessages( library(readxl) )
suppressMessages( library(stringr) )
suppressMessages( library(lubridate) )


# Helpful globals and functions ----

cat(cyan("Loading globals and helpers...\n"))

source("~/Box/Documents/R_helpers/config.R")
source("~/Box/Documents/R_helpers/helpers.R")


# Get Data ----

# _ UMMAP General ----

cat(cyan("Getting MRI data from UMMAP General...\n"))

fields_ug_mri_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "sex_value"
    , "race_value"
    , "mri_date"
    , "uds_dx"
    , "mri_elig_consent"
    , "mri_elig_safety_screen"
    , "mri_elig_yn"
  )
fields_ug_mri <- fields_ug_mri_raw %>% paste(collapse = ",")

json_ug_mri <- 
  export_redcap_records(
    token = REDCAP_API_TOKEN_UMMAP_GEN,
    fields = fields_ug_mri,
    vp = TRUE,
    # Filter for UMMAP IDs during UMMAP period
    filterLogic = paste0("(",
                         "[subject_id] >= 'UM00000001'",
                         " AND ",
                         "[subject_id] <= 'UM00009999'",
                         " AND ",
                         "[exam_date] >= '2017-03-01'",
                         ")"))
df_ug_mri <- jsonlite::fromJSON(json_ug_mri) %>% as_tibble() %>% na_if("")

# _ UMMAP UDS3

fields_u3_raw <-
  c(
    "ptid"
    , "note_mlstn_type"
    , "protocol"
  )
fields_u3 <- fields_u3_raw %>% paste(collapse = ",")

json_u3 <- 
  export_redcap_records(
    token = REDCAP_API_TOKEN_UDS3n,
    fields = fields_u3,
    vp = TRUE,
    # Filter for UMMAP IDs during UMMAP period
    filterLogic = paste0("(",
                         "[ptid] >= 'UM00000001'",
                         " AND ",
                         "[ptid] <= 'UM00009999'",
                         " AND ",
                         "[note_mlstn_type] != ''",
                         ")"))
df_u3 <- jsonlite::fromJSON(json_u3) %>% as_tibble() %>% na_if("")


# Clean Data ----

cat(cyan("Cleaning raw data...\n"))

# _ UMMAP General ----

cat(cyan("Getting list of MRI-ineligible participants...\n"))

# Get MRI-ineligible IDs from UMMAP General's `mri_elig_yn == 0`
df_inelig_ug <- df_ug_mri %>% 
  filter(mri_elig_consent == '1',
         !is.na(mri_elig_safety_screen),
         mri_elig_yn == '0') %>% 
  select(subject_id, mri_elig_yn)
inelig_ids_ug <- df_inelig_ug %>% pull(subject_id) %>% sort() %>% unique()

# _ _ Define different Dx codes ----

nl_codes <- c(26, 17) # 17 = Depression
mci_codes <- c(1, 2, 27, 28, 29, 31, 34) # 29 = ImpNoMCI
dem_codes <- c(3, 4, 5, 6, 9, 10, 11, 12, 13, 35)

df_ug_mri_cln <- df_ug_mri %>% 
  # Keep only UM IDs
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  # Keep only UM IDs associated with UM MAP range
  filter(subject_id >= "UM00000543") %>% 
  # Keep only participant-visit records with visit dates
  filter(!is.na(exam_date)) %>% 
  arrange(subject_id, exam_date) %>% 
  # select(subject_id, -redcap_event_name, exam_date, mri_date, uds_dx) %>% 
  select(subject_id, -redcap_event_name, exam_date, 
         sex_value, race_value, mri_date, uds_dx) %>% 
  # rename `sex_value` and `race_value`
  rename(sex = sex_value, race = race_value) %>% 
  # mutate `sex`
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ NA_character_
  )) %>% 
  # mutate `race`
  mutate(race = case_when(
    race == 1 ~ "White",
    race == 2 ~ "African American",
    race == 3 ~ "Asian",
    race == 4 ~ "Hispanic",
    race == 5 ~ "Other",
    race == 6 ~ "Unknown",
    TRUE ~ NA_character_
  )) %>% 
  # mutate `uds_dx` codes to English
  mutate(uds_dx = case_when(
    uds_dx %in% mci_codes ~ "MCI",
    uds_dx %in% dem_codes ~ "Dementia",
    uds_dx %in% nl_codes ~ "Normal",
    !is.na(uds_dx) ~ "Other",
    TRUE ~ NA_character_
  )) %>% 
  # Clean out record that has double-assigned UM MAP ID :(
  filter(!(subject_id == "UM00001353" & exam_date == "2017-05-01"))

# _ _ UMMAP UDS3 ----

cat(cyan("Getting list of milestoned participants...\n"))

# Get milestoned IDs from UMMAP UDS3's `note_mlstn_type` and `protocol`
df_mlstn_u3 <- df_u3 %>% 
  # Keep only UM IDs
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  # Keep only the latest Milestone form data
  group_by(ptid) %>% 
  filter(redcap_event_name == max(redcap_event_name)) %>% 
  ungroup() %>% 
  filter(note_mlstn_type == 0 | 
           (note_mlstn_type == 1 & protocol == 1) |
           (note_mlstn_type == 1 & protocol == 2))

mlstn_ids_u3 <- df_mlstn_u3 %>% pull(ptid) %>% sort() %>% unique()


# Process Data ----

cat(cyan("Processing data...\n"))

# _ Nest all but `subject_id` as df ----
# (i.e., nest `exam_date`, `mri_date`, `uds_dx`)

df_ug_mri_nest <- df_ug_mri_cln %>% 
  tidyr::nest(data = -c(subject_id))

# _ Derive `mri_action` based on data in nested df (`data`) ----

suppressWarnings(
  df_ug_mri_nest_mut <- df_ug_mri_nest %>%
    rowwise() %>%
    mutate(data_nrow = nrow(data)) %>% 
    mutate(mri_max_row = case_when(
      all(is.na(data$mri_date)) ~ NA_integer_,
      any(!is.na(data$mri_date)) ~ 
        as.integer(
          max(seq_len(nrow(data))[!is.na(data$mri_date)], na.rm = TRUE)
        ),
      TRUE ~ NA_integer_
    )) %>% 
    mutate(mri_action = case_when(
      # MRI-ineligible participants
      subject_id %in% inelig_ids_ug ~ "Ineligible",
      # Milestoned participants
      subject_id %in% mlstn_ids_u3 ~ "Milestoned",
      # no `mri_date`s at all => "Not Scanned"
      all(is.na(data$mri_date)) ~ "Not Scanned",
      # 1 visit 
      ### NL
      data_nrow == 1 && data$uds_dx[data_nrow] == "Normal" ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow]) %m+% months(23))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(23))),
      ### MCI
      data_nrow == 1 && data$uds_dx[data_nrow] == "MCI" ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow]) %m+% months(11))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(11))),
      ### Dem
      data_nrow == 1 && data$uds_dx[data_nrow] == "Dementia" ~
        "Dementia Dx: Stop Scanning",
      # 2 visits
      ### NL + NL
      data_nrow > 1 &&
        data$uds_dx[data_nrow-1] == "Normal" &&
        data$uds_dx[data_nrow] == "Normal" ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow-1]) %m+% months(23))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(23))),
      ### MCI + NL
      data_nrow > 1 &&
        data$uds_dx[data_nrow-1] == "MCI" &&
        data$uds_dx[data_nrow] == "Normal" ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow]) %m+% months(11))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(11))),
      ### XXX + MCI
      data_nrow > 1 &&
        data$uds_dx[data_nrow] == "MCI" ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow]) %m+% months(11))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(11))),
      ### XXX + Dem
      data_nrow > 1 &&
        data$uds_dx[data_nrow] == "Dementia" ~
        "Dementia Dx: Stop Scanning",
      ### MCI + `NA`
      data_nrow > 1 &&
        data$uds_dx[data_nrow-1] == "MCI" &&
        is.na(data$uds_dx[data_nrow]) ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow-1]) %m+% months(11))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(11))),
      ### NL + `NA`
      data_nrow > 1 &&
        data$uds_dx[data_nrow-1] == "Normal" &&
        is.na(data$uds_dx[data_nrow]) ~
        paste(
          "Schedule next scan ~",
          # as.character(as_date(data$exam_date[data_nrow-1]) %m+% months(23))),
          as.character(as_date(data$mri_date[mri_max_row]) %m+% months(23))),
      # catch-all
      TRUE ~ NA_character_
    )) %>% 
    ungroup()
)

# _ Unnest nested data and reshape/clean for easier digestion ----

df_ug_mri_unnest <- df_ug_mri_nest_mut %>% 
  select(-data_nrow, -mri_max_row) %>% 
  tidyr::unnest(data) %>% 
  calculate_visit_num(subject_id, exam_date) %>% 
  tidyr::gather(-subject_id, -visit_num, -sex, -race, 
                key = "key", value = "value") %>% 
  tidyr::unite("key__visit_num", key, visit_num, sep = "__") %>% 
  tidyr::spread(key = key__visit_num, value = value) %>% 
  mutate(mri_action = 
           coalesce(
             !!!syms(
               tidyselect::vars_select(names(.), 
                                       starts_with("mri_action__"))))) %>% 
  select(-starts_with("mri_action__")) %>% 
  select(subject_id, mri_action, everything())

# _ Calculate MRI priority score
df_ug_mri_unnest <- df_ug_mri_unnest %>% 
  mutate(mri_priority = 0L) %>% 
  mutate(mri_priority = if_else(mri_action != "Ineligible",
                                mri_priority + 1L, mri_priority)) %>% 
  mutate(mri_priority = if_else(sex == "Male",
                                mri_priority + 1L, mri_priority)) %>% 
  mutate(mri_priority = if_else(race == "African American", 
                                mri_priority + 1L, mri_priority)) %>% 
  mutate(mri_priority = if_else(mri_action == "Not Scanned",
                                mri_priority + 1L, mri_priority)) %>%
  mutate(mri_priority = if_else(mri_action == "Dementia Dx: Stop Scanning",
                                0L, mri_priority)) %>% 
  mutate(mri_priority = if_else(mri_action == "Ineligible", 
                                0L, mri_priority)) %>% 
  mutate(mri_priority = if_else(mri_action == "Milestoned",
                                0L, mri_priority)) %>% 
  select(subject_id, mri_action, mri_priority, everything())


# Write CSV ----

cat(cyan("Writing CSV file...\n"))

readr::write_csv(df_ug_mri_unnest, 
                 paste0("~/Box/Documents/",
                        "MADC_gen/MRI_schedule_report/",
                        "MRI_Schedule_Report.csv"),
                 na = "")

cat(magenta("\nDone.\n\n"))


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
