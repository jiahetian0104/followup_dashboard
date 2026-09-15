
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
  get_path("Data/Reports/Participant Registration/ParticipantRegistration_Export_09152026.xlsx")
)

## 2.1. Set up parameters ----------------------------------------------------

base_url <- "https://echocharm.ripplescience.com/v1/export"

auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCE="

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


# 4. Staff assignment history --------------------------------------------

library(readr)
library(openxlsx)
library(fs)

normalize_dashboard_staff <- function(x) {
  value <- str_squish(as.character(x))
  if_else(is.na(value) | value == "", "Unassigned", value)
}

# The app folder can be overridden when the project is deployed elsewhere.
APP_DIR <- Sys.getenv("FOLLOWUP_6_35_DASHBOARD_APP_DIR", unset = "")
if (APP_DIR == "") {
  APP_DIR <- if (basename(getwd()) == "followup_dashboard") {
    file.path(getwd(), "6_35_followup_streamlit_app")
  } else {
    file.path(getwd(), "followup_dashboard", "6_35_followup_streamlit_app")
  }
}

latest_dir <- file.path(APP_DIR, "data", "latest")
dashboard_run_time <- Sys.time()
snapshot_id <- format(dashboard_run_time, "%Y-%m-%d_%H%M%S")
snapshot_dir <- file.path(APP_DIR, "data", "snapshots", snapshot_id)

dir_create(latest_dir)
dir_create(snapshot_dir)

# Ripple is the authority for the current assignment. Age window is recorded
# for context, but an assignment interval changes only when the staff value
# changes in a later refresh.
current_staff_assignments <- ripple_activity_wide_with_consent %>%
  mutate(
    tag_staff = str_to_title(
      str_extract(tags, regex("\\b(Anna|Jody|Cassie)\\b", ignore_case = TRUE))
    ),
    default_staff = case_when(
      status_group == "6_11_month" ~ "Anna",
      status_group == "12_23_month" ~ "Jody",
      status_group == "24_35_month" ~ "Cassie",
      TRUE ~ NA_character_
    ),
    staff = normalize_dashboard_staff(coalesce(tag_staff, default_staff)),
    `Assignment Source` = case_when(
      !is.na(tag_staff) ~ "Ripple tag",
      is.na(tag_staff) & !is.na(default_staff) ~ "Age-window fallback",
      TRUE ~ "Unassigned"
    )
  ) %>%
  transmute(
    ECHO_ID,
    `Current Age Window` = status_age_window,
    `Current Status` = status_group,
    `Current Staff` = staff,
    `Assignment Source`,
    `Observed Date` = Sys.Date()
  ) %>%
  distinct(ECHO_ID, .keep_all = TRUE)

assignment_history_path <- file.path(latest_dir, "staff_assignment_history.csv")
empty_assignment_history <- tibble(
  ECHO_ID = character(),
  `Age Window at Start` = character(),
  `Age Window at Last Observation` = character(),
  staff = character(),
  `Effective Start` = as.Date(character()),
  `Effective End` = as.Date(character()),
  `First Observed` = as.Date(character()),
  `Last Observed` = as.Date(character()),
  `Assignment Source` = character()
)

staff_assignment_history <- if (file.exists(assignment_history_path)) {
  read_csv(
    assignment_history_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    mutate(
      across(
        c(`Effective Start`, `Effective End`, `First Observed`, `Last Observed`),
        as.Date
      )
    )
} else {
  empty_assignment_history
}

for (assignment_row in seq_len(nrow(current_staff_assignments))) {
  current <- current_staff_assignments[assignment_row, ]
  open_index <- which(
    staff_assignment_history$ECHO_ID == current$ECHO_ID &
      is.na(staff_assignment_history$`Effective End`)
  )

  if (length(open_index) == 0) {
    staff_assignment_history <- bind_rows(
      staff_assignment_history,
      current %>%
        transmute(
          ECHO_ID,
          `Age Window at Start` = `Current Age Window`,
          `Age Window at Last Observation` = `Current Age Window`,
          staff = `Current Staff`,
          `Effective Start` = `Observed Date`,
          `Effective End` = as.Date(NA),
          `First Observed` = `Observed Date`,
          `Last Observed` = `Observed Date`,
          `Assignment Source`
        )
    )
    next
  }

  active_index <- tail(open_index, 1)
  active_staff <- staff_assignment_history$staff[active_index]

  if (identical(active_staff, current$`Current Staff`[[1]])) {
    staff_assignment_history$`Last Observed`[active_index] <- Sys.Date()
    staff_assignment_history$`Age Window at Last Observation`[active_index] <-
      current$`Current Age Window`[[1]]
  } else if (staff_assignment_history$`Effective Start`[active_index] == Sys.Date()) {
    # Multiple refreshes on the first day are treated as corrections.
    staff_assignment_history$staff[active_index] <- current$`Current Staff`[[1]]
    staff_assignment_history$`Age Window at Start`[active_index] <-
      current$`Current Age Window`[[1]]
    staff_assignment_history$`Age Window at Last Observation`[active_index] <-
      current$`Current Age Window`[[1]]
    staff_assignment_history$`Last Observed`[active_index] <- Sys.Date()
    staff_assignment_history$`Assignment Source`[active_index] <-
      current$`Assignment Source`[[1]]
  } else {
    staff_assignment_history$`Effective End`[active_index] <- Sys.Date() - 1
    staff_assignment_history <- bind_rows(
      staff_assignment_history,
      current %>%
        transmute(
          ECHO_ID,
          `Age Window at Start` = `Current Age Window`,
          `Age Window at Last Observation` = `Current Age Window`,
          staff = `Current Staff`,
          `Effective Start` = `Observed Date`,
          `Effective End` = as.Date(NA),
          `First Observed` = `Observed Date`,
          `Last Observed` = `Observed Date`,
          `Assignment Source`
        )
    )
  }
}

staff_assignment_history <- staff_assignment_history %>%
  arrange(ECHO_ID, `Effective Start`)

write_csv(staff_assignment_history, assignment_history_path, na = "")
write_csv(
  current_staff_assignments,
  file.path(snapshot_dir, "staff_assignment_snapshot.csv"),
  na = ""
)
write_csv(
  staff_assignment_history,
  file.path(snapshot_dir, "staff_assignment_history.csv"),
  na = ""
)


# 5. Lock task responsibility and preserve completed age windows ----------

activity_log_with_current_staff <- activity_log %>%
  left_join(
    current_staff_assignments %>%
      select(ECHO_ID, `Current Staff`, `Assignment Source`),
    by = "ECHO_ID"
  ) %>%
  mutate(`Current Staff` = normalize_dashboard_staff(`Current Staff`))

task_responsibility_path <- file.path(latest_dir, "task_responsibility_ledger.csv")
history_was_initialized <- file.exists(task_responsibility_path)

empty_task_responsibility_ledger <- tibble(
  ECHO_ID = character(),
  event_short = character(),
  `Task Age Window` = character(),
  `Responsible Staff` = character(),
  `Responsibility Start` = as.Date(character()),
  `Last Observed` = as.Date(character()),
  `Assignment Source` = character()
)

task_responsibility_ledger <- if (history_was_initialized) {
  read_csv(
    task_responsibility_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    mutate(across(c(`Responsibility Start`, `Last Observed`), as.Date))
} else {
  empty_task_responsibility_ledger
}

# A task is owned by the staff member responsible when that age-window task
# first becomes applicable. Later staff changes do not transfer that task.
current_task_responsibilities <- activity_log_with_current_staff %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  transmute(
    ECHO_ID,
    event_short = Event,
    `Task Age Window` = coalesce(Age_Window, "not_applicable"),
    `Current Staff`,
    `Assignment Source`
  ) %>%
  distinct(ECHO_ID, event_short, `Task Age Window`, .keep_all = TRUE)

for (task_row in seq_len(nrow(current_task_responsibilities))) {
  current <- current_task_responsibilities[task_row, ]
  responsibility_index <- which(
    task_responsibility_ledger$ECHO_ID == current$ECHO_ID &
      task_responsibility_ledger$event_short == current$event_short &
      task_responsibility_ledger$`Task Age Window` == current$`Task Age Window`
  )

  if (length(responsibility_index) == 0) {
    task_responsibility_ledger <- bind_rows(
      task_responsibility_ledger,
      current %>%
        transmute(
          ECHO_ID,
          event_short,
          `Task Age Window`,
          `Responsible Staff` = `Current Staff`,
          `Responsibility Start` = Sys.Date(),
          `Last Observed` = Sys.Date(),
          `Assignment Source`
        )
    )
  } else {
    responsibility_index <- tail(responsibility_index, 1)
    task_responsibility_ledger$`Last Observed`[responsibility_index] <- Sys.Date()

    fill_unassigned <-
      task_responsibility_ledger$`Responsible Staff`[responsibility_index] ==
        "Unassigned" && current$`Current Staff`[[1]] != "Unassigned"

    if (fill_unassigned) {
      task_responsibility_ledger$`Responsible Staff`[responsibility_index] <-
        current$`Current Staff`[[1]]
      task_responsibility_ledger$`Assignment Source`[responsibility_index] <-
        current$`Assignment Source`[[1]]
    }
  }
}

task_responsibility_ledger <- task_responsibility_ledger %>%
  distinct(ECHO_ID, event_short, `Task Age Window`, .keep_all = TRUE) %>%
  arrange(ECHO_ID, `Task Age Window`, event_short)

write_csv(task_responsibility_ledger, task_responsibility_path, na = "")
write_csv(
  task_responsibility_ledger,
  file.path(snapshot_dir, "task_responsibility_ledger.csv"),
  na = ""
)

previous_detail_path <- file.path(latest_dir, "dashboard_detail.csv")
previous_task_history <- if (
  history_was_initialized && file.exists(previous_detail_path)
) {
  read_csv(previous_detail_path, show_col_types = FALSE, guess_max = 10000)
} else {
  tibble()
}

if (nrow(previous_task_history) > 0) {
  if (!"Completion" %in% names(previous_task_history)) {
    previous_task_history$Completion <- previous_task_history$Outcome
  }
  if (!"Task Age Window" %in% names(previous_task_history)) {
    previous_task_history$`Task Age Window` <- coalesce(
      as.character(previous_task_history$Age_Window),
      "not_applicable"
    )
  }
}

current_task_rows <- activity_log_with_current_staff %>%
  mutate(
    event_short = Event,
    `Task Age Window` = coalesce(Age_Window, "not_applicable")
  ) %>%
  left_join(
    task_responsibility_ledger %>%
      select(
        ECHO_ID, event_short, `Task Age Window`, `Responsible Staff`,
        `Responsibility Start`
      ),
    by = c("ECHO_ID", "event_short", "Task Age Window")
  ) %>%
  mutate(
    `Responsible Staff` = normalize_dashboard_staff(
      coalesce(`Responsible Staff`, `Current Staff`)
    ),
    `Task Stage` = "Current"
  )

task_grain <- c("ECHO_ID", "event_short", "Task Age Window")

historical_task_rows <- if (nrow(previous_task_history) > 0) {
  previous_task_history %>%
    mutate(
      `Task Age Window` = coalesce(
        as.character(`Task Age Window`),
        as.character(Age_Window),
        "not_applicable"
      )
    ) %>%
    anti_join(current_task_rows %>% select(all_of(task_grain)), by = task_grain) %>%
    select(
      ECHO_ID, Event, status_group, Eligibility, Age_Window, Completion,
      Completion_Date, event_short, `Task Age Window`,
      `Responsible Staff`, `Responsibility Start`
    ) %>%
    left_join(
      current_staff_assignments %>% select(ECHO_ID, `Current Staff`),
      by = "ECHO_ID"
    ) %>%
    mutate(
      `Current Staff` = normalize_dashboard_staff(`Current Staff`),
      `Responsible Staff` = normalize_dashboard_staff(`Responsible Staff`),
      `Assignment Source` = "Preserved task responsibility",
      `Task Stage` = "Historical"
    )
} else {
  tibble()
}

activity_log_with_staff <- bind_rows(current_task_rows, historical_task_rows) %>%
  distinct(ECHO_ID, event_short, `Task Age Window`, .keep_all = TRUE) %>%
  mutate(staff = `Responsible Staff`) %>%
  arrange(staff, event_short, ECHO_ID, `Task Age Window`)


# 6. Build Streamlit dashboard data and progress --------------------------

dashboard_detail <- activity_log_with_staff %>%
  mutate(
    Outcome = Completion,
    statusId = status_group,
    status_2026_std = status_group,
    participant_status = status_group,
    child_echo_id = ECHO_ID,
    source_sheet = "2026_6_35_month_dashboard.R",
    eligible_flag = if_else(
      Eligibility %in% c("Yes", "Potential Participants"), 1, 0
    ),
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
    `Current Staff`,
    `Responsible Staff`,
    `Assignment Source`,
    `Responsibility Start`,
    `Task Stage`,
    `Task Age Window`,
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
  arrange(staff, event_short, child_echo_id, `Task Age Window`)

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

dashboard_summary_with_potential <- dashboard_detail %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  summarise_dashboard()

dashboard_summary_without_potential <- dashboard_detail %>%
  filter(Eligibility == "Yes", status_group != "Potential Participants") %>%
  summarise_dashboard()

event_progress_include_potential <- activity_log_with_staff %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  count(staff, Event, Completion, name = "participants") %>%
  arrange(staff, Event, Completion)

event_progress_yes_only <- activity_log_with_staff %>%
  filter(Eligibility == "Yes") %>%
  count(staff, Event, Completion, name = "participants") %>%
  arrange(staff, Event, Completion)


# 7. Save current files and timestamped snapshot --------------------------

write_csv(dashboard_detail, file.path(latest_dir, "dashboard_detail.csv"), na = "")
write_csv(dashboard_detail, file.path(snapshot_dir, "detail.csv"), na = "")

notes_df <- tibble(
  Note = c(
    "1. Ripple is the authority for the current 6-35 month staff assignment; age window is recorded but does not itself close an assignment interval.",
    "2. On the first history-enabled run, current Ripple assignments establish the baseline.",
    "3. Each event and age-window task is permanently assigned to the staff member responsible when that task first becomes applicable, whether its outcome is Complete, Incomplete, or No record.",
    "4. Historical age-window tasks remain in dashboard_detail after the participant moves to a later age window.",
    "5. staff is the Responsible Staff used for credit; Current Staff is retained separately for operational follow-up.",
    "6. eligible_flag = 1 for Eligibility Yes or Potential Participants. Score = 1 only for Complete."
  )
)

summary_wb <- createWorkbook()
addWorksheet(summary_wb, "With Potential")
writeData(summary_wb, "With Potential", dashboard_summary_with_potential)
addWorksheet(summary_wb, "Without Potential")
writeData(summary_wb, "Without Potential", dashboard_summary_without_potential)
addWorksheet(summary_wb, "Detail")
writeData(summary_wb, "Detail", dashboard_detail)
addWorksheet(summary_wb, "Assignment History")
writeData(summary_wb, "Assignment History", staff_assignment_history)
addWorksheet(summary_wb, "Task Responsibility")
writeData(summary_wb, "Task Responsibility", task_responsibility_ledger)
addWorksheet(summary_wb, "Notes")
writeData(summary_wb, "Notes", notes_df)

for (sheet_name in names(summary_wb)) {
  setColWidths(summary_wb, sheet_name, cols = 1:50, widths = "auto")
}

saveWorkbook(
  summary_wb,
  file.path(snapshot_dir, "summary.xlsx"),
  overwrite = TRUE
)

snapshot_manifest <- tibble(
  snapshot_id = snapshot_id,
  snapshot_time = format(dashboard_run_time, "%Y-%m-%dT%H:%M:%S%z"),
  detail_rows = nrow(dashboard_detail),
  current_task_rows = sum(dashboard_detail$`Task Stage` == "Current"),
  historical_task_rows = sum(dashboard_detail$`Task Stage` == "Historical"),
  assignment_history_rows = nrow(staff_assignment_history),
  task_responsibility_rows = nrow(task_responsibility_ledger)
)

write_csv(snapshot_manifest, file.path(latest_dir, "manifest.csv"), na = "")
write_csv(snapshot_manifest, file.path(snapshot_dir, "manifest.csv"), na = "")
