
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
  get_path("Data/Reports/Participant Registration/ParticipantRegistration_Export_06162026.xlsx")
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

# quality check

potential_staff_missing_ID <- activity_log_with_staff %>%
  filter(
    is.na(staff) | str_trim(staff) == "" ,
      status_group == "Potential Participants"
  ) %>%
  select(ECHO_ID) %>%
  distinct()

class(ripple_activity_wide$birthday)

missing_staff_list <- ripple_activity_wide %>%
  filter(
    ECHO_ID %in% potential_staff_missing_ID$ECHO_ID
  ) %>%
  select(ECHO_ID, birthday, status_group) %>%
  mutate(
    birthday = mdy(birthday),
    # Calculate age in completed months
    age_months = interval(birthday, Sys.Date()) %/% months(1),
    # Calculate age in completed years
    age = as.integer(interval(birthday, Sys.Date()) / years(1))
  )

activity_log_with_staff <- activity_log_with_staff %>%
  mutate(
    Eligibility = case_when(
      ECHO_ID %in% missing_staff_list$ECHO_ID ~ "No",
      TRUE ~ Eligibility
    )
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


library(fs)
library(openxlsx)
# -----------------------------
# 1. Set output folder
# -----------------------------

APP_DIR <- file.path(
  "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard",
  "6_35_followup_streamlit_app"
)

latest_dir <- file.path(APP_DIR, "data", "latest")
snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(APP_DIR, "data", "snapshots", snapshot_date)

dir_create(latest_dir)
dir_create(snapshot_dir)

# -----------------------------
# 2. Standardize dashboard detail data
# -----------------------------

dashboard_detail <- activity_log_with_staff %>%
  mutate(
    # Use a display-friendly event name while keeping the original event name
    event_short = Event,
    
    # Fill missing staff assignment for dashboard grouping
    staff = if_else(is.na(staff) | str_trim(staff) == "", "Unassigned", staff),
    
    # Keep status_group as the standardized participant status for dashboard filters
    status_2026_std = status_group,
    participant_status = status_group,
    
    # Streamlit denominator logic:
    # Yes and Potential Participants are controlled by the app toggle.
    # No records are never included in the denominator.
    eligible_flag = case_when(
      Eligibility %in% c("Yes", "Potential Participants") ~ 1,
      TRUE ~ 0
    ),
    
    # Streamlit numerator logic:
    # Complete is counted as 1; all unfinished states are counted as 0.
    Score = case_when(
      Completion == "Complete" ~ 1,
      Completion %in% c("Incomplete", "No record", "Other") ~ 0,
      TRUE ~ NA_real_
    ),
    
    Outcome = Completion,
    source_sheet = "2026_6_35_month.R"
  ) %>%
  select(
    ECHO_ID,
    staff,
    Event,
    event_short,
    status_group,
    status_2026_std,
    participant_status,
    Eligibility,
    Age_Window,
    Outcome,
    Completion_Date,
    eligible_flag,
    Score,
    source_sheet
  ) %>%
  arrange(staff, Event, status_group, ECHO_ID)

# -----------------------------
# 3. Summary helper
# -----------------------------

summarise_dashboard <- function(data) {
  summary_by_event <- data %>%
    group_by(staff, event_short) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, coalesce(Score, 0), 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    )
  
  summary_overall <- data %>%
    group_by(staff) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, coalesce(Score, 0), 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    ) %>%
    mutate(event_short = "Overall") %>%
    relocate(event_short, .after = staff)
  
  bind_rows(summary_by_event, summary_overall) %>%
    arrange(staff, event_short)
}

# -----------------------------
# 4. Create two summary versions
# -----------------------------

# Version 1: include both eligible participants and Potential Participants
dashboard_summary_with_potential <- dashboard_detail %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  summarise_dashboard()

# Version 2: only include currently eligible participants
dashboard_summary_without_potential <- dashboard_detail %>%
  filter(Eligibility == "Yes") %>%
  mutate(
    eligible_flag = 1
  ) %>%
  summarise_dashboard()

# -----------------------------
# 5. Save latest and snapshot outputs
# -----------------------------

write_csv(
  dashboard_detail,
  file.path(latest_dir, "dashboard_detail.csv"),
  na = ""
)

write_csv(
  dashboard_detail,
  file.path(snapshot_dir, "detail.csv"),
  na = ""
)

write_csv(
  dashboard_summary_with_potential,
  file.path(snapshot_dir, "summary_with_potential.csv"),
  na = ""
)

write_csv(
  dashboard_summary_without_potential,
  file.path(snapshot_dir, "summary_without_potential.csv"),
  na = ""
)

summary_wb <- createWorkbook()

addWorksheet(summary_wb, "With Potential")
writeData(summary_wb, "With Potential", dashboard_summary_with_potential)

addWorksheet(summary_wb, "Without Potential")
writeData(summary_wb, "Without Potential", dashboard_summary_without_potential)

addWorksheet(summary_wb, "Detail")
writeData(summary_wb, "Detail", dashboard_detail)

saveWorkbook(
  summary_wb,
  file.path(snapshot_dir, "summary.xlsx"),
  overwrite = TRUE
)

message("Dashboard data saved to: ", APP_DIR)
message("Latest detail: ", file.path(latest_dir, "dashboard_detail.csv"))
message("Snapshot folder: ", snapshot_dir)
