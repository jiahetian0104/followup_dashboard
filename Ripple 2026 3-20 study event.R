# 0. Load Package ---------------------------------------------------------
library(tidyverse)
library(httr2)
library(readxl)
library(XLConnect) # read excel file with password protection
library(tibble)
library(openxlsx)

# 1. Ripple data ----------------------------------------------------------

## 1.1. Set up parameters ----------------------------------------------------
base_url <- "https://echocharm.ripplescience.com/v1/export"
auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCoq"

study_id <- "QhZZT24wtWCpGkKdE"   # 2026 3-20 Year Study
timezone <- "America/New_York"

# Variables
vars <- c(
  "globalId",
  "firstName",
  "lastName",
  "sex",
  "birthday",
  "race",
  "statusId",
  "Events (All or None)" 
)

# Build Body
body_list <- list(
  "teamId" = "pze6EXgGw6hLwhhRy",         
  "export-type" = "QhZZT24wtWCpGkKdE",
  "export-timezone" = "America/New_York",
  "surveyExportSince" = ""               
)

# Add selected variables
for (v in vars) {
  body_list[[v]] <- "on"
}


## 1.2. Data Request ---------------------------------------------------------

# Send Request
resp <- request(base_url) %>%
  req_headers(Authorization = auth_key) %>%
  req_body_form(!!!body_list) %>%
  req_perform()

resp_status(resp)

# Get CSV text
csv_text <- resp_body_string(resp)

# Read CSV text into a data frame
ripple_data <- read_csv(
  I(csv_text), 
  show_col_types = FALSE,
  guess_max = 2000  
)


## 1.3. Data Cleaning --------------------------------------------------------

selected_data <- ripple_data %>% 
  select(
    globalId,
    firstName,
    lastName,
    sex,
    birthday,
    race,
    statusId,
    starts_with("event.2026")
  ) %>%
  filter(!(globalId %in% c("iri0LoVJXJ734mKYy","54jpojzH4AVzTJoPD")))  %>% # delete Anna and Mallory from the result
  extract(
    globalId, 
    into = c("child_echo_id", "PIN"), 
    regex = "(.*)\\s\\((.*)\\)",
    remove = TRUE 
  ) %>% 
  relocate(child_echo_id, PIN)

# reshape data to long format
long_data <- selected_data %>%
  mutate(across(starts_with("event."), as.character)) %>%
  pivot_longer(
    cols = starts_with("event."),
    names_to = c("event", ".value"),
    names_pattern = "event\\.(.*)\\.(completedDate|completed|missedDate|missed|scheduledDate)$"
  )

# check result
colnames(long_data)


## 1.4. Filter Related Data --------------------------------------------------

# for now, we don't consider the participants with "Potential Participants" and "Withdrawn" status.
status_prefix_map <- c(
  "11-17 Year" = "2026_11_17yr_",
  "18-20 Year" = "2026_18_20yr_",
  "3-5 Year" = "2026_3_5yr_",
  "6-10 Year" = "2026_6_10yr_"
)

filtered_long_data <- long_data %>%
  # Map each status group to its expected event prefix
  mutate(
    expected_prefix = recode(statusId, !!!status_prefix_map, .default = NA_character_)
  ) %>%
  # Keep only target status groups
  filter(!is.na(expected_prefix)) %>%
  # Keep only events matching the expected prefix for each participant
  filter(str_starts(event, expected_prefix)) %>%
  # Create a shortened event name by removing the expected prefix
  mutate(
    event_short = str_remove(event, fixed(expected_prefix))
  ) %>%
  # Keep only the target event types
  filter(
    event_short %in% c("caregiver_survey", "child_survey", "ipa_scheduled")
  ) %>%
  select(-expected_prefix) %>%
  relocate(event_short, .after = event)

table(filtered_long_data$event_short) # caregiver_survey     child_survey    ipa_scheduled 
class(filtered_long_data$completedDate) # character, need to be converted to date format for later use
colnames(filtered_long_data)

# Step 1: Create a child-level caregiver completion flag
caregiver_flag <- filtered_long_data %>%
  filter(event_short == "caregiver_survey") %>%
  mutate(
    caregiver_complete = completed == "TRUE",
    caregiver_completed_date_raw = mdy(completedDate)
  ) %>%
  group_by(child_echo_id, statusId) %>%
  summarise(
    caregiver_complete = any(caregiver_complete, na.rm = TRUE),
    caregiver_completed_date = if (
      all(is.na(caregiver_completed_date_raw[caregiver_complete]))
    ) {
      as.Date(NA)
    } else {
      max(caregiver_completed_date_raw[caregiver_complete], na.rm = TRUE)
    },
    .groups = "drop"
  )

reference_date <- as.Date("2026-02-01")

# Step 2: Join caregiver flag back and create Outcome
filtered_long_data <- filtered_long_data %>%
  left_join(
    caregiver_flag,
    by = c("child_echo_id", "statusId")
  ) %>%
  mutate(
    # Convert completion flag
    completed_flag = completed == "TRUE",
    
    # Parse birthday
    child_dob = mdy(birthday),
    
    # Calculate child age at caregiver survey completion
    age_at_caregiver_completion = if_else(
      !is.na(child_dob) & !is.na(caregiver_completed_date),
      floor(time_length(interval(child_dob, caregiver_completed_date), "years")),
      NA_real_
    ),
    
    Outcome = case_when(
      # caregiver_survey and ipa_scheduled use direct completion status
      event_short %in% c("caregiver_survey", "ipa_scheduled") & completed_flag ~ "Complete",
      event_short %in% c("caregiver_survey", "ipa_scheduled") & !completed_flag ~ "Incomplete",
      
      # child_survey outside 6-10 Year also uses direct completion status
      event_short == "child_survey" & statusId != "6-10 Year" & completed_flag ~ "Complete",
      event_short == "child_survey" & statusId != "6-10 Year" & !completed_flag ~ "Incomplete",
      
      # child_survey within 6-10 Year: completed always means Complete
      event_short == "child_survey" & statusId == "6-10 Year" & completed_flag ~ "Complete",
      
      # child_survey within 6-10 Year:
      # incomplete + caregiver complete + age < 8 at caregiver completion = Ineligible
      event_short == "child_survey" & statusId == "6-10 Year" &
        !completed_flag & caregiver_complete &
        !is.na(age_at_caregiver_completion) &
        age_at_caregiver_completion < 8 ~ "Ineligible",
      
      # all other incomplete child_survey records in 6-10 Year remain Incomplete
      event_short == "child_survey" & statusId == "6-10 Year" & !completed_flag ~ "Incomplete",
      
      TRUE ~ NA_character_
    )
  )


# 2. ECHO Portal Data -----------------------------------------------------

# Detect OS
if (Sys.info()["sysname"] == "Windows") {
  BASE_PATH <- "Z:/ECHO/CHARM"
} else {
  BASE_PATH <- "/Volumes/Groups/ECHO/CHARM"
}

# Construct file path
pr_path <- file.path(
  BASE_PATH,
  "Data/Reports/Participant Registration/ParticipantRegistration_Export_06112026.xlsx"
)

# Import data
participant_registration <- read_excel(pr_path)

colnames(participant_registration)


## 2.1 Data Cleaning -------------------------------------------------------

participant_registration_clean <- participant_registration %>%
  mutate(
    EWCP_ParticipantID = as.character(EWCP_ParticipantID)
  ) %>%
  # only keep child
  filter(
    !str_detect(EWCP_ParticipantID, "[80]$")
  ) %>%
  # rename child_echo_id
  rename(
    child_echo_id = EWCP_ParticipantID
  )

participant_reconsent_long <- participant_registration_clean %>%
  mutate(
    # Parse 3.00 and 3.01 dates
    date_300 = mdy_hms(Cycle2ProtocolEnrollmentDate),
    date_301 = mdy_hms(Cycle2ProtocolEnrollmentDatev3_01),
    
    # Create helper flags
    has_300 = !is.na(date_300),
    has_301 = !is.na(date_301),
    has_301_after_ref = !is.na(date_301) & date_301 >= reference_date,
    
    # Event 1: ECHO 2 Re-Consent
    outcome_echo2_reconsent = case_when(
      !has_300 & !has_301 ~ "Incomplete",
      has_301_after_ref & !has_300 ~ "Complete",
      TRUE ~ NA_character_
    ),
    
    # Event 2: ECHO 2 v3.01 Re-Consent
    outcome_v301_reconsent = case_when(
      !has_301 & has_300 ~ "Incomplete",
      has_301_after_ref & has_300 ~ "Complete",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    child_echo_id,
    # date_300,
    # date_301,
    outcome_echo2_reconsent,
    outcome_v301_reconsent
  ) %>%
  pivot_longer(
    cols = c(outcome_echo2_reconsent, outcome_v301_reconsent),
    names_to = "event_short",
    values_to = "Outcome"
  ) %>%
  mutate(
    event_short = recode(
      event_short,
      "outcome_echo2_reconsent" = "ECHO 2 Re-Consent",
      "outcome_v301_reconsent" = "ECHO 2 v3.01 Re-Consent"
    )
  )


# 3. Call list ------------------------------------------------------------


## 3.1 Import Call list ----------------------------------------------------

PASSWORD <- "Epi$2018"

# Detect OS
if (Sys.info()["sysname"] == "Windows") {
  BASE_PATH <- "Z:/ECHO/CHARM"
} else {
  BASE_PATH <- "/Volumes/Groups/ECHO/CHARM"
}

# Helper: build full path from BASE_PATH
get_path <- function(relative_path) {
  file.path(BASE_PATH, relative_path)
}

# Helper: read unprotected Excel
read_unprotected_excel <- function(path, sheet = 1) {
  read_excel(path, sheet = sheet)
}

# Helper: read password-protected Excel with XLConnect
read_protected_excel <- function(path, sheet) {
  wb <- XLConnect::loadWorkbook(filename = path, password = PASSWORD)
  XLConnect::readWorksheet(wb, sheet = sheet, header = TRUE)
}

# File paths (UPDATED)
# =========================

amy_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Amy.xlsx"
)

cassie_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Cassie (18-20yr).xlsx"
)

jo_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Jo.xlsx"
)

jody_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Jody.xlsx"
)

nicole_23_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Nicole (2,3yr).xlsx"
)

nicole_34_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Nicole (3,4yr).xlsx"
)

toye_path <- get_path(
  "Breanna/2026 Call List Copies/2026 Call List - Toye.xlsx"
)

# Import all call lists
# =========================

call_lists <- list(
  
  # No password
  amy = read_protected_excel(amy_path, sheet = 1),
  # cassie = read_protected_excel(cassie_path, sheet = 1),
  toye = read_protected_excel(toye_path, sheet = 1),
  
  # Password protected
  jo = read_protected_excel(jo_path, sheet = 1),
  
  # Jody (sheet 2)
  jody = read_protected_excel(jody_path, sheet = 2),
  
  # Nicole split
  nicole_23 = read_protected_excel(nicole_23_path, sheet = 1) %>%
    filter(status_2026 == "3-5 Year"),
  nicole_34 = read_protected_excel(nicole_34_path, sheet = 1)
)

# # File paths
# amy_path <- get_path(
#   "FOLLOW-UP TEAM/ECHO 2 Re-Consent Call Lists/Amy/2026 Call List - Amy/2026 Working Call List - Amy Updated.xlsx"
# )
# 
# jo_path <- get_path(
#   "FOLLOW-UP TEAM/ECHO 2 Re-Consent Call Lists/Jo/2026 Call List (11-17)/2026 Call List - Jo.xlsx"
# )
# 
# jody_path <- get_path(
#   "FOLLOW-UP TEAM/ECHO 2 Re-Consent Call Lists/Jody/2026 Call List - Jody/Copy of Copy of 2026 Call List - Jody.xlsx"
# )
# 
# nicole_path <- get_path(
#   "FOLLOW-UP TEAM/ECHO 2 Re-Consent Call Lists/Nicole/2026/2026 Call List - Nicole (3,4yr) - Current.xlsx"
# )
# 
# shetoye_path <- get_path(
#   "FOLLOW-UP TEAM/ECHO 2 Re-Consent Call Lists/Shetoye/2026 Call List - Toye/2026 Call List Toye - With Caregiver Names.xlsx"
# )
# 
# # Import all call lists
# call_lists <- list(
#   
#   # No password
#   amy = read_unprotected_excel(amy_path, sheet = 1),
#   
#   # Password, only sheet
#   jo = read_protected_excel(jo_path, sheet = 1),
#   
#   # Password, first two sheets
#   jody_sheet1 = read_protected_excel(jody_path, sheet = 2), # 6-10 yr sheet
#   jody_sheet2 = read_protected_excel(jody_path, sheet = 3), # 6-10 yr potential sheet
#   
#   # Password, first six sheets
#   nicole_sheet1 = read_protected_excel(nicole_path, sheet = 1), # potential
#   nicole_sheet2 = read_protected_excel(nicole_path, sheet = 2), # updated consent
#   nicole_sheet3 = read_protected_excel(nicole_path, sheet = 3), # survey
#   nicole_sheet4 = read_protected_excel(nicole_path, sheet = 4), # survey complete
#   
#   # No password
#   shetoye = read_unprotected_excel(shetoye_path, sheet = 1)
# )

# summarize call lists
call_list_summary <- imap_dfr(call_lists, ~ tibble(
  dataset = .y,
  n_rows = nrow(.x),
  n_cols = ncol(.x),
  colnames = paste(colnames(.x), collapse = ", ")
))

call_list_summary

# check columns and first few rows of each dataset
purrr::iwalk(call_lists, ~ {
  cat("\n============================\n")
  cat("Dataset:", .y, "\n")
  cat("Columns:\n")
  print(colnames(.x))
})

purrr::iwalk(call_lists, ~ {
  cat("\n============================\n")
  cat("Dataset:", .y, "\n")
  print(head(.x, 3))
})


## 3.2 Clean Call List -----------------------------------------------------



# # Helper function: standardize one call list
# process_call_list <- function(df, id_col, staff_name, source_name, split_pin = TRUE) {
#   
#   df <- df %>%
#     mutate(
#       raw_id = as.character(.data[[id_col]])
#     ) %>%
#     filter(!is.na(raw_id), raw_id != "")
#   
#   if (split_pin) {
#     df <- df %>%
#       extract(
#         raw_id,
#         into = c("child_echo_id", "PIN"),
#         regex = "^(.*)\\s*\\((.*)\\)$",
#         remove = FALSE
#       ) %>%
#       mutate(
#         child_echo_id = str_trim(child_echo_id),
#         PIN = str_trim(PIN)
#       )
#   } else {
#     df <- df %>%
#       mutate(
#         child_echo_id = str_trim(raw_id),
#         PIN = NA_character_
#       )
#   }
#   
#   df %>%
#     transmute(
#       child_echo_id,
#       PIN,
#       staff = staff_name,
#       source_sheet = source_name
#     )
# }
# 
# staff_assignment_main <- bind_rows(
#   process_call_list(call_lists$amy,          id_col = "ECHO ID",    staff_name = "Amy",     source_name = "amy",            split_pin = TRUE),
#   process_call_list(call_lists$jo,           id_col = "Global.ID..",staff_name = "Jo",      source_name = "jo",             split_pin = FALSE),
#   process_call_list(call_lists$jody_sheet1,  id_col = "ECHO.ID.",   staff_name = "Jody",    source_name = "jody_sheet1",    split_pin = TRUE),
#   process_call_list(call_lists$jody_sheet2,  id_col = "ECHO.ID.",   staff_name = "Jody",    source_name = "jody_sheet2",    split_pin = TRUE),
#   process_call_list(call_lists$nicole_sheet1,id_col = "ECHO.ID",    staff_name = "Nicole",  source_name = "nicole_sheet1",  split_pin = FALSE),
#   process_call_list(call_lists$nicole_sheet2,id_col = "ECHO.ID",    staff_name = "Nicole",  source_name = "nicole_sheet2",  split_pin = FALSE),
#   process_call_list(call_lists$nicole_sheet3,id_col = "ECHO.ID",    staff_name = "Nicole",  source_name = "nicole_sheet3",  split_pin = FALSE),
#   process_call_list(call_lists$nicole_sheet4,id_col = "ECHO.ID",    staff_name = "Nicole",  source_name = "nicole_sheet4",  split_pin = FALSE),
#   process_call_list(call_lists$shetoye,      id_col = "ECHO ID",    staff_name = "Toye",    source_name = "Toye",           split_pin = TRUE)
# ) %>%
#   distinct()


# ---------- update

process_call_list <- function(df, id_col, status_col, staff_name, source_name, split_pin = TRUE) {
  
  df <- df %>%
    mutate(
      raw_id = as.character(.data[[id_col]]),
      status_2026_raw = as.character(.data[[status_col]])
    ) %>%
    filter(!is.na(raw_id), raw_id != "") %>%
    mutate(
      status_2026_raw = str_trim(status_2026_raw),
      status_2026_std = case_when(
        is.na(status_2026_raw) | status_2026_raw == "" ~ NA_character_,
        
        str_detect(str_to_lower(status_2026_raw), "3-5") ~ "3-5 Year",
        str_detect(str_to_lower(status_2026_raw), "2-3") ~ "2-3 Year",
        str_detect(str_to_lower(status_2026_raw), "18-20") ~ "18-20 Year",
        str_detect(str_to_lower(status_2026_raw), "potential") ~ "Potential Participants",
        str_detect(str_to_lower(status_2026_raw), "updated consent") ~ "Updated Consent",
        str_detect(str_to_lower(status_2026_raw), "survey complete") ~ "Survey Complete",
        str_detect(str_to_lower(status_2026_raw), "survey") ~ "Survey",
        
        TRUE ~ status_2026_raw
      )
    )
  
  if (split_pin) {
    df <- df %>%
      extract(
        raw_id,
        into = c("child_echo_id", "PIN"),
        regex = "^(.*)\\s*\\((.*)\\)$",
        remove = FALSE
      ) %>%
      mutate(
        child_echo_id = if_else(is.na(child_echo_id), raw_id, child_echo_id),
        child_echo_id = str_trim(child_echo_id),
        PIN = str_trim(PIN)
      )
  } else {
    df <- df %>%
      mutate(
        child_echo_id = str_trim(raw_id),
        PIN = NA_character_
      )
  }
  
  df %>%
    transmute(
      child_echo_id,
      PIN,
      status_2026_raw,
      status_2026_std,
      staff = staff_name,
      source_sheet = source_name
    )
}

staff_assignment_main <- bind_rows(
  process_call_list(
    call_lists$amy,
    id_col = "ECHO.ID",
    status_col = "X2026.Status.",
    staff_name = "Amy",
    source_name = "amy",
    split_pin = TRUE
  ),
  # process_call_list(
  #   call_lists$cassie,
  #   id_col = "ECHO.ID.",
  #   status_col = "X2026.Status.",
  #   staff_name = "Cassie",
  #   source_name = "cassie",
  #   split_pin = TRUE
  # ),
  process_call_list(
    call_lists$toye,
    id_col = "ECHO.ID",
    status_col = "X2026.Status.",
    staff_name = "Toye",
    source_name = "toye",
    split_pin = TRUE
  ),
  process_call_list(
    call_lists$jo,
    id_col = "globalId",
    status_col = "X2026.Status.",
    staff_name = "Jo",
    source_name = "jo",
    split_pin = TRUE
  ),
  process_call_list(
    call_lists$jody,
    id_col = "ECHO.ID.",
    status_col = "Next.2026.Status.",
    staff_name = "Jody",
    source_name = "jody",
    split_pin = TRUE
  ),
  process_call_list(
    call_lists$nicole_23,
    id_col = "globalId",
    status_col = "status_2026",
    staff_name = "Nicole",
    source_name = "nicole_23",
    split_pin = TRUE
  ),
  process_call_list(
    call_lists$nicole_34,
    id_col = "ECHO.ID..pin.",
    status_col = "X2026.Status.",
    staff_name = "Nicole",
    source_name = "nicole_34",
    split_pin = TRUE
  )
) %>%
  distinct()

staff_assignment_main %>%
  count(source_sheet, status_2026_raw, status_2026_std, sort = TRUE)

# check duplicate
dup_id <- staff_assignment_main %>%
  count(child_echo_id) %>%
  filter(n > 1)

dup_id # n = 0

# 4. event_eligibility ----------------------------------------------------

event_reference <- tibble::tibble(
  event_short = c(
    "caregiver_survey",
    "child_survey",
    "ipa_scheduled",
    "ECHO 2 Re-Consent",
    "ECHO 2 v3.01 Re-Consent"
  )
)

staff_event_base <- staff_assignment_main %>%
  distinct(child_echo_id, PIN, status_2026_std, staff) %>%
  crossing(event_reference)

ripple_base <- selected_data %>%
  select(
    child_echo_id,
    birthday,
    statusId
  ) %>%
  distinct()

registration_base <- participant_registration_clean %>%
  select(
    child_echo_id,
    Cycle2ProtocolEnrollmentDate,
    Cycle2ProtocolEnrollmentDatev3_01
  ) %>%
  mutate(
    Cycle2ProtocolEnrollmentDate = mdy_hms(Cycle2ProtocolEnrollmentDate),
    Cycle2ProtocolEnrollmentDatev3_01 = mdy_hms(Cycle2ProtocolEnrollmentDatev3_01)
  ) %>%
  distinct()

staff_event_denominator_base <- staff_event_base %>%
  left_join(
    ripple_base,
    by = "child_echo_id"
  ) %>%
  left_join(
    registration_base,
    by = "child_echo_id"
  ) %>%
  left_join(
    caregiver_flag,
    by = c("child_echo_id", "statusId")
  )

reference_date <- as.POSIXct("2026-02-01 00:00:00", tz = "America/Detroit")

staff_event_denominator_base <- staff_event_denominator_base %>%
  mutate(
    # Parse child birthday
    child_dob = mdy(birthday),
    
    # Calculate age at caregiver survey completion
    age_at_caregiver_completion = if_else(
      !is.na(child_dob) & !is.na(caregiver_completed_date),
      floor(time_length(interval(child_dob, caregiver_completed_date), "years")),
      NA_real_
    ),
    
    eligible_flag = case_when(
      
      # caregiver_survey: all assigned participants are eligible
      event_short == "caregiver_survey" ~ 1,
      
      # ipa_scheduled: all assigned participants are eligible
      event_short == "ipa_scheduled" ~ 1,
      
      # child_survey: eligible only if child is at least 8 years old
      # at the time caregiver survey is completed
      event_short == "child_survey" &
        !is.na(age_at_caregiver_completion) &
        age_at_caregiver_completion >= 8 ~ 1,
      
      event_short == "child_survey" ~ 0,
      
      # ECHO 2 Re-Consent
      event_short == "ECHO 2 Re-Consent" &
        (
          statusId == "Potential Participants" |
            (is.na(Cycle2ProtocolEnrollmentDate) &
               !is.na(Cycle2ProtocolEnrollmentDatev3_01) &
               Cycle2ProtocolEnrollmentDatev3_01 >= reference_date)
        ) ~ 1,
      
      event_short == "ECHO 2 Re-Consent" ~ 0,
      
      # ECHO 2 v3.01 Re-Consent
      event_short == "ECHO 2 v3.01 Re-Consent" &
        !is.na(Cycle2ProtocolEnrollmentDate) &
        (
          is.na(Cycle2ProtocolEnrollmentDatev3_01) |
            Cycle2ProtocolEnrollmentDatev3_01 >= reference_date
        ) ~ 1,
      
      event_short == "ECHO 2 v3.01 Re-Consent" ~ 0,
      
      TRUE ~ NA_real_
    )
  )



# 5. Combine Ripple and Registration Data ---------------------------------

# final ripple outcome

ripple_outcome <- filtered_long_data %>%
  select(
    child_echo_id,
    statusId,
    event_short,
    Outcome
  ) %>%
  distinct()

# final registration outcome
registration_outcome <- participant_reconsent_long %>%
  select(
    child_echo_id,
    event_short,
    Outcome
  ) %>%
  distinct()

dashboard_detail <- staff_event_denominator_base %>%
  left_join(
    ripple_outcome,
    by = c("child_echo_id", "statusId", "event_short")
  )

dashboard_detail <- dashboard_detail %>%
  left_join(
    registration_outcome %>%
      rename(Outcome_registration = Outcome),
    by = c("child_echo_id", "event_short")
  ) %>%
  mutate(
    Outcome = coalesce(Outcome, Outcome_registration)
  ) %>%
  select(-Outcome_registration)

# add score
dashboard_detail <- dashboard_detail %>%
  mutate(
    Score = case_when(
      Outcome %in% c("Complete", "Ineligible") ~ 1,
      Outcome == "Incomplete" ~ 0,
      TRUE ~ NA_real_
    )
  )

summarise_dashboard <- function(data) {
  summary_by_event <- data %>%
    group_by(staff, event_short) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, Score, 0), na.rm = TRUE),
      progress = numerator / denominator,
      .groups = "drop"
    )
  
  summary_overall <- data %>%
    group_by(staff) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, Score, 0), na.rm = TRUE),
      progress = numerator / denominator,
      .groups = "drop"
    ) %>%
    mutate(event_short = "Overall") %>%
    relocate(event_short, .after = staff)
  
  bind_rows(summary_by_event, summary_overall)
}



# ============================================================
# Save dashboard outputs for Streamlit
# ============================================================

library(openxlsx)
library(readr)
library(fs)

# Project folder
APP_DIR <- '/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard/followup_streamlit_app'

# Output folders
latest_dir <- file.path(APP_DIR, "data", "latest")
snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(APP_DIR, "data", "snapshots", snapshot_date)

dir_create(latest_dir)
dir_create(snapshot_dir)

# ------------------------------------------------------------
# 1. Save latest detail CSV
# ------------------------------------------------------------

write_csv(
  dashboard_detail,
  file.path(latest_dir, "dashboard_detail.csv"),
  na = ""
)

# ------------------------------------------------------------
# 2. Save snapshot detail CSV
# ------------------------------------------------------------

write_csv(
  dashboard_detail,
  file.path(snapshot_dir, "detail.csv"),
  na = ""
)

# ------------------------------------------------------------
# 3. Create summary tables
# ------------------------------------------------------------

dashboard_summary_with_potential <- summarise_dashboard(dashboard_detail)

dashboard_summary_without_potential <- dashboard_detail %>%
  filter(statusId != "Potential Participants" | is.na(statusId)) %>%
  summarise_dashboard()

# ------------------------------------------------------------
# 4. Save snapshot summary Excel with two sheets
# ------------------------------------------------------------

summary_wb <- createWorkbook()

addWorksheet(summary_wb, "With Potential")
writeData(summary_wb, "With Potential", dashboard_summary_with_potential)

addWorksheet(summary_wb, "Without Potential")
writeData(summary_wb, "Without Potential", dashboard_summary_without_potential)

openxlsx::saveWorkbook(
  summary_wb,
  file.path(snapshot_dir, "summary.xlsx"),
  overwrite = TRUE
)

