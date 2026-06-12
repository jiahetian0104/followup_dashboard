
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


# 3.1 Ripple Data ---------------------------------------------------------

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


caregiver_survey_log <- bind_rows(
  ripple_activity_wide_with_consent %>%
    transmute(
      ECHO_ID,
      Event = "Caregiver Survey",
      Age_Window = "6_11_month",
      completed = to_logical(`event.6_11_mo_cg_survey.completed`),
      Completion_Date = as_date(mdy(`event.6_11_mo_cg_survey.completedDate`))
    ),
  
  ripple_activity_wide_with_consent %>%
    transmute(
      ECHO_ID,
      Event = "Caregiver Survey",
      Age_Window = "12_23_month",
      completed = to_logical(`event.12_23mo_caregiver_survey.completed`),
      Completion_Date = as_date(mdy(`event.12_23mo_caregiver_survey.completedDate`))
    ),
  
  ripple_activity_wide_with_consent %>%
    transmute(
      ECHO_ID,
      Event = "Caregiver Survey",
      Age_Window = "24_35_month",
      completed = to_logical(`event.24_35mo_caregiver_survey.completed`),
      Completion_Date = as_date(mdy(`event.24_35mo_caregiver_survey.completedDate`))
    )
) %>%
  mutate(
    Completion = case_when(
      completed == TRUE ~ "Complete",
      completed == FALSE ~ "Incomplete",
      is.na(completed) ~ "No record",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    ECHO_ID,
    Event,
    Age_Window,
    Completion,
    Completion_Date
  )

ipa_scheduled_log <- bind_rows(
  ripple_activity_wide_with_consent %>%
    transmute(
      ECHO_ID,
      Event = "IPA Scheduled",
      Age_Window = "12_23_month",
      completed = to_logical(`event.12_23mo_ipa_scheduled.completed`),
      Completion_Date = as_date(mdy(`event.12_23mo_ipa_scheduled.completedDate`))
    ),
  
  ripple_activity_wide_with_consent %>%
    transmute(
      ECHO_ID,
      Event = "IPA Scheduled",
      Age_Window = "24_35_month",
      completed = to_logical(`event.24_35mo_ipa_scheduled.completed`),
      Completion_Date = as_date(mdy(`event.24_35mo_ipa_scheduled.completedDate`))
    )
) %>%
  mutate(
    Completion = case_when(
      completed == TRUE ~ "Complete",
      completed == FALSE ~ "Incomplete",
      is.na(completed) ~ "No record",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    ECHO_ID,
    Event,
    Age_Window,
    Completion,
    Completion_Date
  )



postnatal_consent_log <- ripple_activity_wide_with_consent %>%
  transmute(
    ECHO_ID,
    Event = "ECHO 2 v3.01 Postnatal Consent",
    Age_Window = NA_character_,
    completed = to_logical(`event.echo_v3_01_postnatal_consent.completed`),
    Completion_Date = as_date(mdy(`event.echo_v3_01_postnatal_consent.completedDate`))
  ) %>%
  mutate(
    Completion = case_when(
      completed == TRUE ~ "Complete",
      completed == FALSE ~ "Incomplete",
      is.na(completed) ~ "No record",
      TRUE ~ "Other"
    )
  ) %>%
  select(ECHO_ID, Event, Age_Window, Completion, Completion_Date)


reconsent_log <- ripple_activity_wide_with_consent %>%
  mutate(
    Completion = case_when(
      is.na(v3_00_consent_date) &
        !is.na(v3_01_consent_date) &
        v3_01_consent_date > as_date("2026-02-01") ~ "Complete",
      
      TRUE ~ "Incomplete"
    ),
    
    Completion_Date = case_when(
      Completion == "Complete" ~ v3_01_consent_date,
      TRUE ~ as_date(NA)
    )
  ) %>%
  transmute(
    ECHO_ID,
    Event = "ECHO 2 Re-Consent",
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






















# 3. Ripple Events --------------------------------------------------------

# Caregiver consent from registration data
caregiver_concent <- participant_registration %>%
  select(
    EWCP_ParticipantID,
    Cycle2ProtocolEnrollmentDate,
    Cycle2ProtocolEnrollmentDatev3_01
  ) %>%
  mutate(
    Cycle2ProtocolEnrollmentDate = mdy_hms(
      Cycle2ProtocolEnrollmentDate
    ),
    
    Cycle2ProtocolEnrollmentDatev3_01 = mdy_hms(
      Cycle2ProtocolEnrollmentDatev3_01
    )
  )



event_map <- tribble(
  ~dashboard_age_window, ~event_type,              ~event_prefix,                              ~staff_by_age,
  "6_11_month",          "caregiver_survey",       "event.6_11_mo_cg_survey",                   "Anna",
  "12_23_month",         "caregiver_survey",       "event.12_23mo_caregiver_survey",             "Jody",
  "24_35_month",         "caregiver_survey",       "event.24_35mo_caregiver_survey",             "Cassie",
  
  "12_23_month",         "ipa_scheduled",          "event.12_23mo_ipa_scheduled",                "Jody",
  "24_35_month",         "ipa_scheduled",          "event.24_35mo_ipa_scheduled",                "Cassie",
  
  "6_11_month",          "echo_v3_01_postnatal_consent", "event.echo_v3_01_postnatal_consent",     "Anna",
  "12_23_month",         "echo_v3_01_postnatal_consent", "event.echo_v3_01_postnatal_consent",     "Jody",
  "24_35_month",         "echo_v3_01_postnatal_consent", "event.echo_v3_01_postnatal_consent",     "Cassie"
)

event_long <- ripple_data %>%
  select(
    globalId,
    matches("^event\\..*\\.(completed|completedDate|missed|missedDate|scheduledDate)$")
  ) %>%
  pivot_longer(
    cols = -globalId,
    names_to = c("event_name", "event_field"),
    names_pattern = "^(event\\..*)\\.(completed|completedDate|missed|missedDate|scheduledDate)$",
    values_to = "value",
    values_transform = list(value = as.character)
  ) %>%
  pivot_wider(
    names_from = event_field,
    values_from = value
  ) %>%
  mutate(
    completed = to_logical(completed),
    missed = to_logical(missed),
    event_status = case_when(
      completed == TRUE ~ "Completed",
      missed == TRUE ~ "Missed",
      completed == FALSE ~ "Not completed",
      is.na(completed) ~ "No record",
      TRUE ~ "Other"
    )
  )

# Add event type and age-window rule
event_long_dashboard <- event_long %>%
  left_join(
    event_map,
    by = c("event_name" = "event_prefix")
  ) %>%
  filter(!is.na(event_type))


# participant list with age window calculated from birthday and statusId
df_age <- ripple_data %>%
  mutate(
    birthday_date = mdy(birthday),
    age_months = interval(birthday_date, Sys.Date()) %/% months(1),
    age_window = case_when(
      age_months >= 6  & age_months <= 11 ~ "6_11_month",
      age_months >= 12 & age_months <= 23 ~ "12_23_month",
      age_months >= 24 & age_months <= 35 ~ "24_35_month",
      TRUE ~ NA_character_
    )
  )

df_age_check <- df_age %>%
  mutate(
    status_age_window = case_when(
      str_detect(statusId, regex("^6-11 Month", ignore_case = TRUE)) ~ "6_11_month",
      str_detect(statusId, regex("^12-23 Month", ignore_case = TRUE)) ~ "12_23_month",
      str_detect(statusId, regex("^24-35 Month", ignore_case = TRUE)) ~ "24_35_month",
      TRUE ~ NA_character_
    ),
    
    age_window_match = case_when(
      age_window == status_age_window ~ "Match",
      !is.na(age_window) & !is.na(status_age_window) & age_window != status_age_window ~ "Mismatch",
      !is.na(age_window) & is.na(status_age_window) ~ "Birthday in target range, status not target age status",
      is.na(age_window) & !is.na(status_age_window) ~ "Status target age status, birthday outside target range",
      TRUE ~ "Both missing / outside target range"
    )
  ) %>%
  filter(!is.na(status_age_window))


participant_base <- df_age_check %>%
  mutate(
    tags = if_else(is.na(tags) | str_trim(tags) == "", "", tags),
    
    tag_staff = str_extract(
      tags,
      regex("\\b(Anna|Jody|Cassie)\\b", ignore_case = TRUE)
    ),
    
    tag_staff = str_to_title(tag_staff)
  ) %>%
  select(
    globalId,
    statusId,
    birthday,
    age_months,
    birthday_age_window = age_window,
    dashboard_age_window = status_age_window,
    tag_staff,
    tags
  )

participant_base_with_pin <- participant_base %>%
  mutate(
    EWCP_ParticipantID = str_trim(str_remove(globalId, "\\s*\\(.*\\)$"))
  )


## 4. Create caregiver consent event table ----------------------------------

caregiver_consent_event <- participant_base_with_pin %>%
  left_join(
    caregiver_consent,
    by = "EWCP_ParticipantID"
  ) %>%
  mutate(
    event_type = "echo_2_reconsent",
    event_name = "participant_registration.echo_2_reconsent",
    
    consent_eligible = case_when(
      statusId == "Potential Participants" ~ TRUE,
      is.na(v3_00_date) & !is.na(v3_01_date) & v3_01_date > as_date("2026-02-01") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    event_status = case_when(
      is.na(v3_00_date) & !is.na(v3_01_date) & v3_01_date > as_date("2026-02-01") ~ "Completed",
      is.na(v3_00_date) & is.na(v3_01_date) ~ "Incomplete",
      TRUE ~ "Not eligible / already consented"
    ),
    
    completed = event_status == "Completed",
    completedDate = as.character(v3_01_date),
    missed = NA,
    missedDate = NA,
    scheduledDate = NA,
    
    staff = coalesce(tag_staff, case_when(
      dashboard_age_window == "6_11_month" ~ "Anna",
      dashboard_age_window == "12_23_month" ~ "Jody",
      dashboard_age_window == "24_35_month" ~ "Cassie",
      TRUE ~ NA_character_
    ))
  ) %>%
  filter(consent_eligible == TRUE) %>%
  select(
    globalId,
    statusId,
    birthday,
    age_months,
    birthday_age_window,
    dashboard_age_window,
    event_type,
    event_name,
    staff,
    event_status,
    completed,
    completedDate,
    missed,
    missedDate,
    scheduledDate,
    tags
  )


# Final long-format participant-event data 
  
  participant_event_long <- participant_base %>%
  left_join(
    event_long_dashboard,
    by = c("globalId", "dashboard_age_window")
  ) %>%
  mutate(
    staff = coalesce(tag_staff, staff_by_age)
  ) %>%
  select(
    globalId,
    statusId,
    birthday,
    age_months,
    birthday_age_window,
    dashboard_age_window,
    event_type,
    event_name,
    staff,
    event_status,
    completed,
    completedDate,
    missed,
    missedDate,
    scheduledDate,
    tags
  )


  staff_event_progress <- participant_event_long %>%
    group_by(staff, event_type) %>%
    summarise(
      total_assigned = n_distinct(globalId),
      completed = n_distinct(globalId[event_status == "Completed"]),
      missed = n_distinct(globalId[event_status == "Missed"]),
      not_completed = n_distinct(globalId[event_status == "Not completed"]),
      no_record = n_distinct(globalId[event_status == "No record"]),
      completion_rate = completed / total_assigned,
      .groups = "drop"
    ) %>%
    arrange(event_type, desc(total_assigned), staff)
  
  staff_event_progress





# 5. Save the data --------------------------------------------------------

library(openxlsx)

output_folder <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard"

output_path <- file.path(
  output_folder,
  "caregiver_survey_staff_progress.xlsx"
)

notes_df <- tibble::tibble(
  Note = c(
    "1. The participant_staff list excludes ECHO 2 Refusal, Potential Participants, and Withdrawn participants.",
    "2. dashboard_age_window is derived from statusId. birthday_age_window is calculated from birthday.",
    "3. Staff assignment is based primarily on tags. If staff tags are missing, staff is assigned based on the age window derived from statusId.",
    "4. staff_progress uses the number of participants assigned to each staff member as the denominator and calculates caregiver survey completion progress."
  )
)

wb <- createWorkbook()

addWorksheet(wb, "participant_staff")
writeData(wb, "participant_staff", participant_staff)

addWorksheet(wb, "staff_progress")
writeData(wb, "staff_progress", staff_progress)

addWorksheet(wb, "Notes")
writeData(wb, "Notes", notes_df)

# Optional: make columns easier to read
setColWidths(wb, "participant_staff", cols = 1:ncol(participant_staff), widths = "auto")
setColWidths(wb, "staff_progress", cols = 1:ncol(staff_progress), widths = "auto")
setColWidths(wb, "Notes", cols = 1:ncol(notes_df), widths = "auto")

saveWorkbook(wb, output_path, overwrite = TRUE)




