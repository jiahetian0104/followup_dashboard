
rm(list = ls())
# 1. Load Packages --------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr2) # Use API

# 2. Import Data ----------------------------------------------------------

BASE_PATH <- case_when(
  Sys.info()["sysname"] == "Windows" ~ "Z:/ECHO/CHARM",
  Sys.info()["sysname"] == "Darwin"  ~ "/Volumes/Groups/ECHO/CHARM"
)

get_path <- function(relative_path) {
  file.path(BASE_PATH, relative_path)
}

participant_registration <- read_excel(
  get_path("Data/Reports/Participant Registration/ParticipantRegistration_Export_06112026.xlsx")
)

## 2.1. Set up parameters ----------------------------------------------------

base_url <- "https://echocharm.ripplescience.com/v1/export"

auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCoq"

# export-type from DevTools payload
study_id <- "REakqQKvCboEdX7BL"

team_id <- "pze6EXgGw6hLwhhRy"

timezone <- "America/New_York"


## 2.2 Variables selected in Ripple export UI ------------------------------------

vars <- c(
  "globalId",
  "birthday",
  "tags",
  "statusId",
  "Events (All or None)"
)


## 2.3 Build Body ---------------------------------------------------------------

body_list <- list(
  "access_token" = "",   # optional, usually not needed if using Authorization header
  "teamId" = team_id,
  "export-type" = study_id,
  "export-timezone" = timezone,
  "surveyExportSince" = ""
)

# Add selected variables
for (v in vars) {
  body_list[[v]] <- "on"
}


## 2.4. Data Request ---------------------------------------------------------

resp <- request(base_url) %>%
  req_headers(
    Authorization = auth_key
  ) %>%
  req_body_form(!!!body_list) %>%
  req_perform()


# Check status
resp_status(resp)


## 2.5 Get CSV text --------------------------------------------------------------

csv_text <- resp_body_string(resp)


## 2.6 Read into dataframe -------------------------------------------------------

ripple_data <- read_csv(
  I(csv_text),
  show_col_types = FALSE,
  guess_max = 5000
)



# 3. Activity Log ---------------------------------------------------------


## 3.1 Ripple Data ---------------------------------------------------------

# related events
event_cols <- c(
  ## Caregiver survey
  "event.6_11_mo_cg_survey.completed",
  "event.6_11_mo_cg_survey.completedDate",
  "event.6_11_mo_cg_survey.missed",
  "event.6_11_mo_cg_survey.missedDate",
  "event.6_11_mo_cg_survey.scheduledDate",
  
  "event.12_23mo_caregiver_survey.completed",
  "event.12_23mo_caregiver_survey.completedDate",
  "event.12_23mo_caregiver_survey.missed",
  "event.12_23mo_caregiver_survey.missedDate",
  "event.12_23mo_caregiver_survey.scheduledDate",
  
  "event.24_35mo_caregiver_survey.completed",
  "event.24_35mo_caregiver_survey.completedDate",
  "event.24_35mo_caregiver_survey.missed",
  "event.24_35mo_caregiver_survey.missedDate",
  "event.24_35mo_caregiver_survey.scheduledDate",
  
  ## IPA scheduled
  "event.12_23mo_ipa_scheduled.completed",
  "event.12_23mo_ipa_scheduled.completedDate",
  "event.12_23mo_ipa_scheduled.missed",
  "event.12_23mo_ipa_scheduled.missedDate",
  "event.12_23mo_ipa_scheduled.scheduledDate",
  
  "event.24_35mo_ipa_scheduled.completed",
  "event.24_35mo_ipa_scheduled.completedDate",
  "event.24_35mo_ipa_scheduled.missed",
  "event.24_35mo_ipa_scheduled.missedDate",
  "event.24_35mo_ipa_scheduled.scheduledDate",
  
  ## ECHO v3.01 postnatal consent
  "event.echo_v3_01_postnatal_consent.completed",
  "event.echo_v3_01_postnatal_consent.completedDate",
  "event.echo_v3_01_postnatal_consent.missed",
  "event.echo_v3_01_postnatal_consent.missedDate",
  "event.echo_v3_01_postnatal_consent.scheduledDate"
)

ripple_activity_wide <- ripple_data %>%
  mutate(
    ECHO_ID = str_trim(str_remove(globalId, "\\s*\\(.*\\)$")),
    
    status_age_window = case_when(
      str_detect(statusId, regex("^6-11 Month", ignore_case = TRUE)) ~ "6_11_month",
      str_detect(statusId, regex("^12-23 Month", ignore_case = TRUE)) ~ "12_23_month",
      str_detect(statusId, regex("^24-35 Month", ignore_case = TRUE)) ~ "24_35_month",
      TRUE ~ NA_character_
    ),
    
    status_group = coalesce(status_age_window, statusId)
  ) %>%
  select(
    ECHO_ID,
    globalId,
    statusId,
    status_age_window,
    status_group,
    birthday,
    tags,
    any_of(event_cols)
  )


## 3.2 Registration data ---------------------------------------------------


caregiver_consent <- participant_registration %>%
  select(
    ECHO_ID = EWCP_ParticipantID,
    Cycle2ProtocolEnrollmentDate,
    Cycle2ProtocolEnrollmentDatev3_01
  ) %>%
  mutate(
    ECHO_ID = as.character(ECHO_ID),
    
    v3_00_consent_date = as_date(mdy_hms(Cycle2ProtocolEnrollmentDate)),
    v3_01_consent_date = as_date(mdy_hms(Cycle2ProtocolEnrollmentDatev3_01))
  ) %>%
  select(
    ECHO_ID,
    v3_00_consent_date,
    v3_01_consent_date
  )


ripple_activity_wide_with_consent <- ripple_activity_wide %>%
  mutate(
    ECHO_ID = as.character(ECHO_ID)
  ) %>%
  left_join(
    caregiver_consent,
    by = "ECHO_ID"
  )



## Helper: convert completed field to logical --------------------------------

to_logical <- function(x) {
  case_when(
    x %in% c(TRUE, "TRUE", "True", "true", "1", 1) ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false", "0", 0) ~ FALSE,
    TRUE ~ NA
  )
}


## 3.3 Build up log for different events -----------------------------------


caregiver_survey_log <- ripple_activity_wide_with_consent %>%
  mutate(
    # Define participant eligibility based on status_group
    Eligibility = case_when(
      status_group %in% c("6_11_month", "12_23_month", "24_35_month") ~ "Yes",
      status_group == "Potential Participants" ~ "Potential Participants",
      status_group %in% c("Withdrawn", "ECHO 2 Refusal") ~ "No",
      TRUE ~ NA_character_
    ),
    
    # Use status_group directly as the dashboard age window
    Age_Window = case_when(
      status_group %in% c("6_11_month", "12_23_month", "24_35_month") ~ status_group,
      TRUE ~ NA_character_
    ),
    
    # Pull the caregiver survey completion field based on the current age window
    completed = case_when(
      Age_Window == "6_11_month" ~ to_logical(`event.6_11_mo_cg_survey.completed`),
      Age_Window == "12_23_month" ~ to_logical(`event.12_23mo_caregiver_survey.completed`),
      Age_Window == "24_35_month" ~ to_logical(`event.24_35mo_caregiver_survey.completed`),
      TRUE ~ NA
    ),
    
    # Pull the caregiver survey completion date based on the current age window
    Completion_Date = case_when(
      Age_Window == "6_11_month" ~ as_date(mdy(`event.6_11_mo_cg_survey.completedDate`)),
      Age_Window == "12_23_month" ~ as_date(mdy(`event.12_23mo_caregiver_survey.completedDate`)),
      Age_Window == "24_35_month" ~ as_date(mdy(`event.24_35mo_caregiver_survey.completedDate`)),
      TRUE ~ as.Date(NA)
    ),
    
    Completion = case_when(
      completed == TRUE ~ "Complete",
      completed == FALSE ~ "Incomplete",
      is.na(completed) & Eligibility == "Yes" ~ "No record",
      Eligibility != "Yes" ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  transmute(
    ECHO_ID,
    Event = "Caregiver Survey",
    status_group,
    Eligibility,
    Age_Window,
    Completion,
    Completion_Date
  )

ipa_scheduled_log <- ripple_activity_wide_with_consent %>%
  mutate(
    # Define participant eligibility based on status_group
    Eligibility = case_when(
      status_group %in% c("6_11_month", "12_23_month", "24_35_month") ~ "Yes",
      status_group == "Potential Participants" ~ "Potential Participants",
      status_group %in% c("Withdrawn", "ECHO 2 Refusal") ~ "No",
      TRUE ~ NA_character_
    ),
    
    # Use status_group directly as the dashboard age window
    Age_Window = case_when(
      status_group %in% c("6_11_month", "12_23_month", "24_35_month") ~ status_group,
      TRUE ~ NA_character_
    ),
    
    # Pull the IPA scheduled completion field based on the current age window
    completed = case_when(
      Age_Window == "12_23_month" ~ to_logical(`event.12_23mo_ipa_scheduled.completed`),
      Age_Window == "24_35_month" ~ to_logical(`event.24_35mo_ipa_scheduled.completed`),
      TRUE ~ NA
    ),
    
    # Pull the IPA scheduled completion date based on the current age window
    Completion_Date = case_when(
      Age_Window == "12_23_month" ~ as_date(mdy(`event.12_23mo_ipa_scheduled.completedDate`)),
      Age_Window == "24_35_month" ~ as_date(mdy(`event.24_35mo_ipa_scheduled.completedDate`)),
      TRUE ~ as.Date(NA)
    ),
    
    Completion = case_when(
      completed == TRUE ~ "Complete",
      completed == FALSE ~ "Incomplete",
      is.na(completed) & Eligibility == "Yes" ~ "No record",
      Eligibility != "Yes" ~ NA_character_,
      TRUE ~ "Other"
    )
  ) %>%
  transmute(
    ECHO_ID,
    Event = "IPA Scheduled",
    status_group,
    Eligibility,
    Age_Window,
    Completion,
    Completion_Date
  ) %>%
  filter(status_group != "6_11_month") # IPA not applicable for 6-11 month group



postnatal_consent_log <- ripple_activity_wide_with_consent %>%
  mutate(
    cutoff_date = as_date("2026-02-01"),
    
    postnatal_consent_completed = to_logical(
      `event.echo_v3_01_postnatal_consent.completed`
    ),
    
    # Define eligibility for ECHO 2 v3.01 Postnatal Consent
    Eligibility = case_when(
      postnatal_consent_completed == TRUE ~ "Yes",
      !is.na(v3_00_consent_date) & (is.na(v3_01_consent_date) | v3_01_consent_date > cutoff_date) ~ "Yes",
      TRUE ~ "No"
    ),
    
    # Define completion outcome among eligible participants
    Completion = case_when(
      postnatal_consent_completed == TRUE ~ "Complete",
      Eligibility == "Yes" & !is.na(v3_00_consent_date) & v3_01_consent_date > cutoff_date ~ "Complete",
      Eligibility == "Yes" & !is.na(v3_00_consent_date) & is.na(v3_01_consent_date) ~ "Incomplete",
      Eligibility == "No" ~ NA_character_,
      TRUE ~ "Other"
    ),
    
    Completion_Date = case_when(
      Completion == "Complete" & !is.na(v3_01_consent_date) ~ v3_01_consent_date,
      postnatal_consent_completed == TRUE ~ as_date(
        mdy(`event.echo_v3_01_postnatal_consent.completedDate`)
      ),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  filter(
    is.na(Completion_Date) | Completion_Date > cutoff_date
  )%>%
  transmute(
    ECHO_ID,
    Event = "ECHO 2 v3.01 Postnatal Consent",
    status_group,
    Eligibility,
    Age_Window = NA_character_,
    Completion,
    Completion_Date
  ) 

reconsent_log <- ripple_activity_wide_with_consent %>%
  mutate(
    cutoff_date = as_date("2026-02-01"),
    
    # Define eligibility for ECHO 2 Re-Consent
    Eligibility = case_when(
      status_group == "Potential Participants" ~ "Yes",
      is.na(v3_00_consent_date) &
        !is.na(v3_01_consent_date) &
        v3_01_consent_date > cutoff_date ~ "Yes",
      TRUE ~ "No"
    ),
    
    # Define completion outcome among eligible participants
    Completion = case_when(
      Eligibility == "Yes" &
        is.na(v3_00_consent_date) &
        !is.na(v3_01_consent_date) &
        v3_01_consent_date > cutoff_date ~ "Complete",
      
      Eligibility == "Yes" &
        is.na(v3_00_consent_date) &
        is.na(v3_01_consent_date) ~ "Incomplete",
      
      Eligibility == "No" ~ NA_character_,
      TRUE ~ "Other"
    ),
    
    Completion_Date = case_when(
      Completion == "Complete" ~ v3_01_consent_date,
      TRUE ~ as_date(NA)
    )
  ) %>%
  transmute(
    ECHO_ID,
    Event = "ECHO 2 Re-Consent",
    status_group,
    Eligibility,
    Age_Window = NA_character_,
    Completion,
    Completion_Date
  )

activity_log <- bind_rows(
  caregiver_survey_log,
  ipa_scheduled_log,
  postnatal_consent_log,
  reconsent_log
) %>%
  arrange(ECHO_ID, Event, Age_Window)


# 4. Staff Assignment History ---------------------------------------------

staff_assignment_history <- ripple_activity_wide_with_consent %>%
  mutate(
    # Extract staff name from tags
    tag_staff = str_extract(
      tags,
      regex("\\b(Anna|Jody|Cassie)\\b", ignore_case = TRUE)
    ),
    
    # Standardize staff name format
    tag_staff = str_to_title(tag_staff),
    
    # Assign default staff based on status_group if no staff tag is found
    default_staff = case_when(
      status_group == "6_11_month" ~ "Anna",
      status_group == "12_23_month" ~ "Jody",
      status_group == "24_35_month" ~ "Cassie",
      TRUE ~ NA_character_
    ),
    
    # Final staff assignment: prioritize tag-based assignment
    staff = coalesce(tag_staff, default_staff),
    
    # Record assignment source for QC
    assigned_by = case_when(
      !is.na(tag_staff) ~ "Tag",
      is.na(tag_staff) & !is.na(default_staff) ~ "Age window default",
      TRUE ~ NA_character_
    )
  ) %>%
  transmute(
    ECHO_ID,
    status_group,
    staff
  )

activity_log_with_staff <- activity_log %>%
  left_join(
    staff_assignment_history,
    by = c("ECHO_ID", "status_group")
  )


# 5. Progress -------------------------------------------------------------

# Version 1: Include eligible participants and potential participants
# Denominator includes Eligibility == "Yes" and "Potential Participants"

event_progress_include_potential <- activity_log_with_staff %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  group_by(staff, Event) %>%
  summarise(
    total_assigned = n_distinct(ECHO_ID),
    completed = n_distinct(ECHO_ID[Completion == "Complete"]),
    incomplete = n_distinct(ECHO_ID[Completion == "Incomplete"]),
    no_record = n_distinct(ECHO_ID[Completion == "No record"]),
    other = n_distinct(ECHO_ID[Completion == "Other"]),
    missing_completion = n_distinct(ECHO_ID[is.na(Completion)]),
    completion_rate = completed / total_assigned,
    .groups = "drop"
  ) %>%
  arrange(staff, Event)

# Version 2: Include eligible participants only
# Denominator includes only Eligibility == "Yes"

event_progress_yes_only <- activity_log_with_staff %>%
  filter(Eligibility == "Yes") %>%
  group_by(staff, Event) %>%
  summarise(
    total_assigned = n_distinct(ECHO_ID),
    completed = n_distinct(ECHO_ID[Completion == "Complete"]),
    incomplete = n_distinct(ECHO_ID[Completion == "Incomplete"]),
    no_record = n_distinct(ECHO_ID[Completion == "No record"]),
    other = n_distinct(ECHO_ID[Completion == "Other"]),
    missing_completion = n_distinct(ECHO_ID[is.na(Completion)]),
    completion_rate = completed / total_assigned,
    .groups = "drop"
  ) %>%
  arrange(staff, Event)


# 6. Build Streamlit dashboard data ---------------------------------------

library(readr)
library(openxlsx)
library(fs)

# Convert the activity log into the same dashboard_detail structure expected
# by streamlit_app.py.
dashboard_detail <- activity_log_with_staff %>%
  mutate(
    # Keep the existing Streamlit naming convention.
    event_short = Event,
    Outcome = Completion,
    statusId = status_group,
    status_2026_std = status_group,
    participant_status = status_group,
    child_echo_id = ECHO_ID,
    source_sheet = "2026_6_35_month",
    
    # Eligible denominator logic:
    # - Yes = eligible
    # - Potential Participants = eligible only when the dashboard toggle includes them
    # - No = not eligible
    eligible_flag = case_when(
      Eligibility %in% c("Yes", "Potential Participants") ~ 1,
      TRUE ~ 0
    ),
    
    # Score logic used by Streamlit:
    # Complete contributes 1 to the numerator.
    # Incomplete / No record / Other contributes 0 when eligible_flag == 1.
    Score = case_when(
      Outcome == "Complete" ~ 1,
      Outcome %in% c("Incomplete", "No record", "Other") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    child_echo_id,
    ECHO_ID,
    staff,
    status_group,
    statusId,
    status_2026_std,
    participant_status,
    Eligibility,
    Age_Window,
    event_short,
    Event,
    Outcome,
    Completion_Date,
    eligible_flag,
    Score,
    source_sheet
  ) %>%
  arrange(staff, event_short, child_echo_id)


summarise_dashboard <- function(data) {
  summary_by_event <- data %>%
    group_by(staff, event_short) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, Score, 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    )
  
  summary_overall <- data %>%
    group_by(staff) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, Score, 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    ) %>%
    mutate(event_short = "Overall") %>%
    relocate(event_short, .after = staff)
  
  bind_rows(summary_by_event, summary_overall) %>%
    arrange(staff, event_short)
}

# Version 1: include eligible participants plus Potential Participants.
dashboard_summary_with_potential <- dashboard_detail %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  summarise_dashboard()

# Version 2: eligible participants only; Potential Participants excluded.
dashboard_summary_without_potential <- dashboard_detail %>%
  filter(Eligibility == "Yes", status_group != "Potential Participants") %>%
  summarise_dashboard()


# 7. Save dashboard outputs for Streamlit ---------------------------------

# Project folder that contains streamlit_app.py.
APP_DIR <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard/followup_streamlit_app"

latest_dir <- file.path(APP_DIR, "data", "latest")
snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(APP_DIR, "data", "snapshots", snapshot_date)

dir_create(latest_dir)
dir_create(snapshot_dir)

# Save latest detail CSV for routine Streamlit monitoring.
write_csv(
  dashboard_detail,
  file.path(latest_dir, "dashboard_detail.csv"),
  na = ""
)

# Save dated snapshot detail CSV for historical review.
write_csv(
  dashboard_detail,
  file.path(snapshot_dir, "detail.csv"),
  na = ""
)

# Save optional snapshot summary workbook with both denominator definitions.
notes_df <- tibble::tibble(
  Note = c(
    "1. This dashboard is based on 2026_6_35_month.R and uses status_group as the dashboard participant status.",
    "2. For caregiver survey and IPA scheduled events, Age_Window is derived from status_group. For consent events, Age_Window is not applicable.",
    "3. Staff assignment prioritizes Anna/Jody/Cassie tags. If no staff tag is present, staff is assigned by status_group: 6_11_month = Anna, 12_23_month = Jody, 24_35_month = Cassie.",
    "4. eligible_flag = 1 when Eligibility is Yes or Potential Participants. Streamlit can exclude Potential Participants using the sidebar toggle because participant_status/status_2026_std is set to status_group.",
    "5. Score = 1 for Complete; Score = 0 for Incomplete, No record, or Other. Progress is numerator divided by denominator among eligible rows."
  )
)

summary_wb <- createWorkbook()

addWorksheet(summary_wb, "With Potential")
writeData(summary_wb, "With Potential", dashboard_summary_with_potential)

addWorksheet(summary_wb, "Without Potential")
writeData(summary_wb, "Without Potential", dashboard_summary_without_potential)

addWorksheet(summary_wb, "Notes")
writeData(summary_wb, "Notes", notes_df)

setColWidths(summary_wb, "With Potential", cols = 1:ncol(dashboard_summary_with_potential), widths = "auto")
setColWidths(summary_wb, "Without Potential", cols = 1:ncol(dashboard_summary_without_potential), widths = "auto")
setColWidths(summary_wb, "Notes", cols = 1:ncol(notes_df), widths = "auto")

openxlsx::saveWorkbook(
  summary_wb,
  file.path(snapshot_dir, "summary.xlsx"),
  overwrite = TRUE
)

# Optional local Excel export for quick review outside Streamlit.
review_dir <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard"
dir_create(review_dir)

review_wb <- createWorkbook()

addWorksheet(review_wb, "dashboard_detail")
writeData(review_wb, "dashboard_detail", dashboard_detail)

addWorksheet(review_wb, "With Potential")
writeData(review_wb, "With Potential", dashboard_summary_with_potential)

addWorksheet(review_wb, "Without Potential")
writeData(review_wb, "Without Potential", dashboard_summary_without_potential)

addWorksheet(review_wb, "Notes")
writeData(review_wb, "Notes", notes_df)

setColWidths(review_wb, "dashboard_detail", cols = 1:ncol(dashboard_detail), widths = "auto")
setColWidths(review_wb, "With Potential", cols = 1:ncol(dashboard_summary_with_potential), widths = "auto")
setColWidths(review_wb, "Without Potential", cols = 1:ncol(dashboard_summary_without_potential), widths = "auto")
setColWidths(review_wb, "Notes", cols = 1:ncol(notes_df), widths = "auto")

openxlsx::saveWorkbook(
  review_wb,
  file.path(review_dir, paste0("followup_dashboard_review_", snapshot_date, ".xlsx")),
  overwrite = TRUE
)
