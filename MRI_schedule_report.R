#!/usr/bin/env Rscript

# Load useful libraries ----

library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(lubridate)
library(rlang)


# Helpful globals and functions ----

source("~/Box Sync/Documents/R_helpers/config.R")
source("~/Box Sync/Documents/R_helpers/helpers.R")


# Get Data ----

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
                  filterLogic = '([exam_date] >= "2017-03-01")',
                  .opts = list(ssl.verifypeer = FALSE, verbose = FALSE))
df_ms_mri <- jsonlite::fromJSON(json_ms_mri) %>% na_if("")


# Clean Data ----

df_ms_mri_cln <- df_ms_mri %>% 
  filter(str_detect(subject_id, "^UM\\d{8}$")) %>% 
  filter(subject_id >= "UM00000543") %>% 
  filter(!is.na(exam_date)) %>% 
  filter(exam_date >= lubridate::as_date("2017-03-01")) %>% 
  arrange(subject_id, exam_date, mri_date) %>% 
  select(subject_id, -redcap_event_name, exam_date, mri_date, uds_dx)


# Process Data ----

# _ Nest `exam_date`, `mri_date`, `uds_dx` as df ----
df_ms_mri_nest <- df_ms_mri_cln %>% 
  tidyr::nest(-subject_id)

# _ Define different Dx codes ----
nl_codes <- c(26)
mci_codes <- c(1, 2, 27, 28, 31, 34)
dem_codes <- c(3, 4, 5, 6, 9, 10, 11, 12, 13, 35)

# _ Derive `mri_action` based on data in nested df (`data`) ----
df_ms_mri_nest_mut <- df_ms_mri_nest %>%
  rowwise() %>%
  mutate(data_nrow = nrow(data)) %>% 
  mutate(mri_action = case_when(
    # no `mri_date`s at all => "Never Been Scanned"
    all(is.na(data$mri_date)) ~ "Never Been Scanned",
    # 1 visit 
    ### Normal
    data_nrow == 1 && data$uds_dx[data_nrow] %in% nl_codes ~
      paste(
        "Schedule next scan ~",
        as.character(as_date(data$exam_date[data_nrow]) + months(23))),
    ### MCI
    data_nrow == 1 && data$uds_dx[data_nrow] %in% mci_codes ~
      paste(
        "Schedule next scan ~",
        as.character(as_date(data$exam_date[data_nrow]) + months(11))),
    ### Dem
    data_nrow == 1 && data$uds_dx[data_nrow] %in% dem_codes ~
      "Dementia Dx: Stop Scanning",
    # 2 visits
    ### NL + NL
    data_nrow > 1 &&
      data$uds_dx[data_nrow-1] %in% nl_codes &&
      data$uds_dx[data_nrow] %in% nl_codes ~
      paste(
        "Schedule next scan ~",
        as.character(as_date(data$exam_date[data_nrow]) + months(23))),
    ### MCI + NL
    data_nrow > 1 &&
      data$uds_dx[data_nrow-1] %in% mci_codes &&
      data$uds_dx[data_nrow] %in% nl_codes ~
      paste(
        "Schedule next scan ~",
        as.character(as_date(data$exam_date[data_nrow]) + months(11))),
    ### XXX + MCI
    data_nrow > 1 &&
      data$uds_dx[data_nrow] %in% mci_codes ~
      paste(
        "Schedule next scan ~",
        as.character(as_date(data$exam_date[data_nrow]) + months(11))),
    ### XXX + Dem
    data_nrow > 1 &&
      data$uds_dx[data_nrow] %in% dem_codes ~
      "Dementia Dx: Stop Scanning",
    # catch-all
    TRUE ~ NA_character_
  )) %>% 
  ungroup()

# _ Unnest nested data and reshape/clean for easier digestion ----
df_ms_mri_unnest <- df_ms_mri_nest_mut %>% 
  select(-data_nrow) %>% 
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
write_csv(df_ms_mri_unnest, 
          paste0("MRI_Schedule_Report_", Sys.Date(), ".csv"),
          na = "")












