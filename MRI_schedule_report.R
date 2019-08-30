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

source("~/Box Sync/Documents/R_helpers/config.R")
source("~/Box Sync/Documents/R_helpers/helpers.R")


# Get Data ----
cat(cyan("Getting MRI data from MiNDSet Registry...\n"))

fields_ms_mri_raw <-
  c(
    "subject_id"
    , "exam_date"
    , "mri_date"
    , "uds_dx"
  )
fields_ms_mri <- fields_ms_mri_raw %>% paste(collapse = ",")

json_ms_mri <- 
  get_rc_data_api(token = REDCAP_API_TOKEN_MINDSET,
                  fields = fields_ms_mri,
                  vp = FALSE,
                  # Filter for UMMAP IDs during UMMAP period
                  filterLogic = paste0("(",
                                       "[subject_id] >= 'UM00000001'",
                                       " AND ",
                                       "[subject_id] <= 'UM00009999'",
                                       " AND ",
                                       "[exam_date] >= '2017-03-01'",
                                       ")"))
df_ms_mri <- jsonlite::fromJSON(json_ms_mri) %>% as_tibble() %>% na_if("")

cat(cyan("Getting list of MRI-ineligible participants...\n"))

df_inelig <- read_csv(paste0("~/Box Sync/Documents/",
                             "MADC_gen/MRI_schedule_report/",
                             "zaid_inelig_ids_cln.csv"),
                      col_types = cols(.default = col_character())) %>% 
  mutate(ptid = paste0("UM", strrep("0", 8-nchar(ID)) , ID))
inelig_ids <- df_inelig %>% pull(ptid) %>% sort() %>% unique()

# Clean Data ----
cat(cyan("Cleaning raw data...\n"))

# _ Define different Dx codes ----
nl_codes <- c(26, 17) # 17 = Depression
mci_codes <- c(1, 2, 27, 28, 29, 31, 34) # 29 = ImpNoMCI
dem_codes <- c(3, 4, 5, 6, 9, 10, 11, 12, 13, 35)

df_ms_mri_cln <- df_ms_mri %>% 
  # Keep only UM IDs
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  # Keep only UM IDs associated with UM MAP range
  filter(subject_id >= "UM00000543") %>% 
  # Keep only participant-visit records with visit dates
  filter(!is.na(exam_date)) %>% 
  arrange(subject_id, exam_date) %>% 
  select(subject_id, -redcap_event_name, exam_date, mri_date, uds_dx) %>% 
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


# Process Data ----
cat(cyan("Processing data...\n"))

# _ Nest all but `subject_id` (`exam_date`, `mri_date`, `uds_dx`) as df ----
df_ms_mri_nest <- df_ms_mri_cln %>% 
  tidyr::nest(-subject_id)

# _ Derive `mri_action` based on data in nested df (`data`) ----
# df_ms_mri_nest_mut <- df_ms_mri_nest %>%
#   rowwise() %>%
#   mutate(data_nrow = nrow(data)) %>% 
#   mutate(mri_max_row = case_when(
#     all(is.na(data$mri_date)) ~ NA_integer_,
#     any(!is.na(data$mri_date)) ~ 
#       as.integer(
#         max(seq_len(nrow(data))[!is.na(data$mri_date)], na.rm = TRUE)
#       ),
#     TRUE ~ NA_integer_
#   ))

suppressWarnings(
  df_ms_mri_nest_mut <- df_ms_mri_nest %>%
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
      subject_id %in% inelig_ids ~ "Ineligible",
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
df_ms_mri_unnest <- df_ms_mri_nest_mut %>% 
  select(-data_nrow, -mri_max_row) %>% 
  tidyr::unnest(data) %>% 
  calculate_visit_num(subject_id, exam_date) %>% 
  tidyr::gather(-subject_id, -visit_num, key = "key", value = "value") %>% 
  tidyr::unite("key__visit_num", key, visit_num, sep = "__") %>% 
  tidyr::spread(key = key__visit_num, value = value) %>% 
  mutate(mri_action = 
           coalesce(
             !!!syms(
               tidyselect::vars_select(names(.), 
                                       starts_with("mri_action__"))))) %>% 
  select(-starts_with("mri_action__")) %>% 
  select(subject_id, mri_action, everything())


# Write CSV ----
cat(cyan("Writing CSV file...\n"))

readr::write_csv(df_ms_mri_unnest, 
                 paste0("~/Box Sync/Documents/",
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
