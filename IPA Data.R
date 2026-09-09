# 0. Load Package ---------------------------------------------------------
library(tidyverse)
library(httr2)
library(readxl)
library(XLConnect) # read excel file with password protection
library(tibble)
library(openxlsx)

# 1. 6-35 Month Ripple data ---------------------------------------------------

## 1.1. Set up parameters ----------------------------------------------------

base_url <- "https://echocharm.ripplescience.com/v1/export"

auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCE="

# export-type from DevTools payload
study_id <- "REakqQKvCboEdX7BL" # 6-35 month study

team_id <- "pze6EXgGw6hLwhhRy"

timezone <- "America/New_York"


## 1.2 Variables selected in Ripple export UI ------------------------------------

vars <- c(
  "globalId",
  "customId",
  "familyId",
  "firstName",
  "lastName",
  "birthday",
  "tags",
  "statusId",
  "Events (All or None)"
)


## 1.3 Build Body ---------------------------------------------------------------

body_list <- list(
  "access_token" = "",
  "teamId" = team_id,
  "export-type" = study_id,
  "export-timezone" = timezone,
  "surveyExportSince" = ""
)

# Add selected variables
for (v in vars) {
  body_list[[v]] <- "on"
}


## 1.4. Data Request ---------------------------------------------------------

resp <- request(base_url) %>%
  req_headers(
    Authorization = auth_key
  ) %>%
  req_body_form(!!!body_list) %>%
  # Retry transient failures (HTTP 429/503) with exponential backoff
  req_retry(max_tries = 5, max_seconds = 300) %>%
  req_perform()


# Check status
resp_status(resp)


## 1.5 Get CSV text --------------------------------------------------------------

csv_text <- resp_body_string(resp)


## 1.6 Read into dataframe -------------------------------------------------------

ripple_data_6_35_month <- read_csv(
  I(csv_text),
  show_col_types = FALSE,
  guess_max = 5000
)



# 2. 3-20 Year Ripple data ---------------------------------------------------

## 2.1. Set up parameters ----------------------------------------------------

# export-type from DevTools payload
study_id <- "QhZZT24wtWCpGkKdE" # 2026 3-20 year study

team_id <- "pze6EXgGw6hLwhhRy"

timezone <- "America/New_York"


## 2.2 Variables selected in Ripple export UI ------------------------------------

vars <- c(
  "globalId",
  "customId",
  "familyId",
  "firstName",
  "lastName",
  "sex",
  "birthday",
  "race",
  "tags",
  "statusId",
  "Events (All or None)"
)


## 2.3 Build Body ---------------------------------------------------------------

body_list <- list(
  "access_token" = "",
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
  # Retry transient failures (HTTP 429/503) with exponential backoff
  req_retry(max_tries = 5, max_seconds = 300) %>%
  req_perform()


# Check status
resp_status(resp)


## 2.5 Get CSV text --------------------------------------------------------------

csv_text <- resp_body_string(resp)


## 2.6 Read into dataframe -------------------------------------------------------

ripple_data_3_20_year <- read_csv(
  I(csv_text),
  show_col_types = FALSE,
  guess_max = 2000
) 


# 3. Participant ID Dictionary and Note ID Extraction --------------------

# Build a one-row-per-participant dictionary from both Ripple exports.
# globalId is parsed into the ECHO child ID and PIN, e.g.
# "LTS268-01-A (680)" -> child_echo_id = "LTS268-01-A", PIN = "680".
participant_id_catalog <- bind_rows(
    ripple_data_6_35_month %>% mutate(cohort = "6-35 month"),
    ripple_data_3_20_year %>% mutate(cohort = "3-20 year")
  ) %>%
  mutate(
    globalId = trimws(globalId),
    child_echo_id = str_remove(globalId, "\\s*\\(\\d+\\)$"),
    PIN = str_extract(globalId, "\\d+(?=\\)$)")
  ) %>%
  select(
    cohort,
    child_echo_id,
    PIN,
    customId,
    familyId,
    firstName,
    lastName,
    birthday,
    statusId,
    globalId
  ) %>%
  distinct()


# Normalize names before using them as a fallback participant identifier.
# This removes differences caused only by case, punctuation, accents, or
# repeated spaces while keeping first and last name as separate fields.
normalize_person_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    na_if("")
}


# Extract all confirmed IDs (customId, familyId, child_echo_id) that appear
# in a free-text note, verified against the catalog. Returns a long tibble
# with one row per matched ID.
extract_note_ids <- function(note, catalog) {
  if (is.na(note) || trimws(note) == "") {
    return(tibble(
      id_type = character(),
      matched_id = character(),
      child_echo_id = character()
    ))
  }
  tokens <- str_extract_all(note, "[A-Za-z0-9]+")[[1]]
  tokens_upper <- toupper(tokens)
  echo_ids <- toupper(str_extract_all(
    note, "[A-Za-z]{2,4}[0-9]{3,4}-[0-9]{2}-[A-Za-z]")[[1]])

  bind_rows(
    catalog %>%
      filter(
        !is.na(child_custom_id) &
          toupper(child_custom_id) %in% tokens_upper
      ) %>%
      transmute(
        id_type = "child_custom_id",
        matched_id = child_custom_id,
        child_echo_id
      ),
    catalog %>%
      filter(!is.na(child_id) & toupper(child_id) %in% tokens_upper) %>%
      transmute(
        id_type = "crosswalk_child_id",
        matched_id = child_id,
        child_echo_id
      ),
    catalog %>%
      filter(!is.na(child_echo_id) & toupper(child_echo_id) %in% echo_ids) %>%
      transmute(id_type = "child_echo_id", matched_id = child_echo_id, child_echo_id),
    catalog %>%
      filter(!is.na(mom_id) & toupper(mom_id) %in% tokens_upper) %>%
      transmute(id_type = "mom_id", matched_id = mom_id, child_echo_id),
    catalog %>%
      filter(!is.na(mom_echo_id) & toupper(mom_echo_id) %in% echo_ids) %>%
      transmute(id_type = "mom_echo_id", matched_id = mom_echo_id, child_echo_id),
    catalog %>%
      filter(
        !is.na(familyId) &
          toupper(as.character(familyId)) %in% tokens_upper
      ) %>%
      transmute(id_type = "familyId", matched_id = as.character(familyId), child_echo_id)
  ) %>%
    distinct()
}


# Match one Calendly row to participants. Explicit child ECHO/custom IDs in
# meeting notes are tried first and may return multiple children. A mother or
# family ID is only used together with the child's full name. When no usable
# ID is found, a unique full-name match across the catalog is the fallback.
match_cal_row_to_cohort <- function(note, cal_fn, cal_ln, catalog) {
  empty <- tibble(
    res_fn = NA_character_,
    res_ln = NA_character_,
    matched_by = NA_character_,
    matched_child_echo_id = NA_character_,
    matched_custom_id = NA_character_,
    matched_family_id = NA_character_,
    matched_birthday = NA_character_,
    matched_cohort = NA_character_
  )
  if (nrow(catalog) == 0) {
    return(empty)
  }

  # Older cohort-specific calls do not contain the Crosswalk aliases. Add
  # empty versions so the same matching function remains reusable.
  if (!"child_custom_id" %in% names(catalog)) {
    catalog$child_custom_id <- as.character(catalog$customId)
  }
  if (!"mom_id" %in% names(catalog)) catalog$mom_id <- NA_character_
  if (!"mom_echo_id" %in% names(catalog)) catalog$mom_echo_id <- NA_character_
  if (!"child_id" %in% names(catalog)) catalog$child_id <- NA_character_

  catalog <- catalog %>%
    mutate(across(
      any_of(c(
        "child_echo_id", "child_custom_id", "customId", "familyId",
        "child_id", "mom_id", "mom_echo_id"
      )),
      as.character
    ))

  format_matches <- function(candidates, method) {
    candidates %>%
      filter(!is.na(child_echo_id)) %>%
      distinct(child_echo_id, .keep_all = TRUE) %>%
      transmute(
        res_fn = as.character(firstName),
        res_ln = as.character(lastName),
        matched_by = method,
        matched_child_echo_id = as.character(child_echo_id),
        matched_custom_id = as.character(child_custom_id),
        matched_family_id = as.character(familyId),
        matched_birthday = as.character(birthday),
        matched_cohort = as.character(cohort)
      )
  }

  hits <- extract_note_ids(note, catalog)

  if (nrow(hits) > 0) {
    # Explicit child IDs may intentionally return multiple children when
    # several IDs are written in one note. Mother/family IDs only narrow the
    # family and must be disambiguated by the child's normalized full name.
    for (type in c(
      "child_echo_id", "child_custom_id", "crosswalk_child_id",
      "mom_id", "mom_echo_id", "familyId"
    )) {
      ids <- unique(hits$matched_id[hits$id_type == type])
      if (length(ids) == 0) next
      if (type == "child_echo_id") {
        cand <- catalog %>% filter(!is.na(child_echo_id) & child_echo_id %in% ids)
      } else if (type == "child_custom_id") {
        cand <- catalog %>%
          filter(!is.na(child_custom_id) & child_custom_id %in% ids)
      } else if (type == "crosswalk_child_id") {
        cand <- catalog %>% filter(!is.na(child_id) & child_id %in% ids)
      } else if (type == "mom_id") {
        cand <- catalog %>% filter(!is.na(mom_id) & mom_id %in% ids)
      } else if (type == "mom_echo_id") {
        cand <- catalog %>%
          filter(!is.na(mom_echo_id) & mom_echo_id %in% ids)
      } else {
        cand <- catalog %>%
          filter(!is.na(familyId) & as.character(familyId) %in% ids)
      }
      if (nrow(cand) == 0) next

      if (type %in% c("mom_id", "mom_echo_id", "familyId")) {
        # Parent/family identifiers never identify a child by themselves.
        # Require the Calendly child name and never expand all siblings.
        by_name <- cand %>%
          filter(
            normalize_person_name(firstName) == cal_fn,
            normalize_person_name(lastName) == cal_ln
          ) %>%
          distinct(child_echo_id, .keep_all = TRUE)
        if (nrow(by_name) == 1) {
          return(format_matches(by_name, paste0(type, "+name")))
        }
      } else {
        matched <- format_matches(cand, type)
        if (nrow(matched) > 0) {
          return(matched)
        }
      }
    }
  }

  hit <- catalog %>%
    filter(
      normalize_person_name(firstName) == cal_fn,
      normalize_person_name(lastName) == cal_ln
    ) %>%
    distinct(child_echo_id, .keep_all = TRUE)
  if (nrow(hit) == 1) {
    return(format_matches(hit, "name"))
  }

  empty
}


# Resolve every Calendly row against a participant catalog and expand rows
# when one note explicitly contains multiple child IDs.
resolve_calendly_cohort <- function(cal_data, catalog) {
  cal_data %>%
    mutate(
      cal_fn = normalize_person_name(`Child First Name`),
      cal_ln = normalize_person_name(`Child Last Name`)
    ) %>%
    rowwise() %>%
    mutate(res = list(match_cal_row_to_cohort(
      `Meeting Notes Plain`, cal_fn, cal_ln, catalog
    ))) %>%
    ungroup() %>%
    unnest(res) %>%
    mutate(
      join_fn = coalesce(res_fn, `Child First Name`),
      join_ln = coalesce(res_ln, `Child Last Name`)
    ) %>%
    select(-cal_fn, -cal_ln, -res_fn, -res_ln)
}



# 4. 3-20 Year Call List -------------------------------------------------

## 4.1 Import Call list ----------------------------------------------------

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



# 5. Calendly Data -------------------------------------------------------

# Crosswalk used by Calendly participant matching. In addition to child
# identifiers, it maps a mother's ECHO/custom ID to her linked children;
# child name is still required to select one child from that family.
# Reading through get_path() keeps the shared-drive root portable by OS.
charm_cohort_list_path <- get_path(
  "Data/Miscellaneous/Global Crosswalk/CHARM_Cohort_List.xlsx"
)

charm_cohort_list <- read_excel(
  charm_cohort_list_path,
  sheet = "Cohort_List",
  col_types = "text"
) %>%
  transmute(
    crosswalk_cohort = str_squish(cohort),
    crosswalk_family_id = str_squish(familyId),
    mom_echo_id = str_squish(mom_echo_id),
    mom_pin = str_squish(mom_pin),
    child_echo_id = str_squish(child_echo_id),
    child_pin = str_squish(child_pin),
    mom_id = str_squish(mom_id),
    child_id = str_squish(child_id)
  ) %>%
  filter(!is.na(child_echo_id), child_echo_id != "") %>%
  distinct(child_echo_id, .keep_all = TRUE)


# Combine Ripple attributes (name and birthday) with all Crosswalk aliases.
# A full join keeps children that are in the Crosswalk but temporarily absent
# from either Ripple export, so an explicit child/mother ID can still resolve.
calendly_participant_catalog <- participant_id_catalog %>%
  mutate(across(
    any_of(c("child_echo_id", "PIN", "customId", "familyId")),
    as.character
  )) %>%
  full_join(
    charm_cohort_list,
    by = "child_echo_id",
    relationship = "many-to-one"
  ) %>%
  mutate(
    cohort = coalesce(cohort, crosswalk_cohort),
    PIN = coalesce(PIN, child_pin),
    familyId = coalesce(familyId, crosswalk_family_id),
    child_custom_id = coalesce(customId, child_id)
  ) %>%
  select(
    cohort,
    child_echo_id,
    PIN,
    child_custom_id,
    customId,
    familyId,
    firstName,
    lastName,
    birthday,
    statusId,
    globalId,
    mom_echo_id,
    mom_pin,
    mom_id,
    child_id
  ) %>%
  distinct()


calendly_token <- "eyJraWQiOiIxY2UxZTEzNjE3ZGNmNzY2YjNjZWJjY2Y4ZGM1YmFmYThhNjVlNjg0MDIzZjdjMzJiZTgzNDliMjM4MDEzNWI0IiwidHlwIjoiUEFUIiwiYWxnIjoiRVMyNTYifQ.eyJpc3MiOiJodHRwczovL2F1dGguY2FsZW5kbHkuY29tIiwiaWF0IjoxNzg4MzY0NzkzLCJqdGkiOiI3YmRlNGQxMS01NDVjLTRlYjgtYTM0Yy03MzM1MmU5MWUxMzUiLCJ1c2VyX3V1aWQiOiJiM2M3NWI1Ni05OWE3LTQ0NGQtYWFhMi00NzgzMzI0MDM3OWIiLCJzY29wZSI6ImF2YWlsYWJpbGl0eTpyZWFkIGF2YWlsYWJpbGl0eTp3cml0ZSBldmVudF90eXBlczpyZWFkIGV2ZW50X3R5cGVzOndyaXRlIGxvY2F0aW9uczpyZWFkIHJvdXRpbmdfZm9ybXM6cmVhZCBzaGFyZXM6d3JpdGUgc2NoZWR1bGVkX2V2ZW50czpyZWFkIHNjaGVkdWxlZF9ldmVudHM6d3JpdGUgc2NoZWR1bGluZ19saW5rczp3cml0ZSBncm91cHM6cmVhZCBvcmdhbml6YXRpb25zOnJlYWQgb3JnYW5pemF0aW9uczp3cml0ZSB1c2VyczpyZWFkIGNvbnRhY3RzOnJlYWQgY29udGFjdHM6d3JpdGUgbWVldGluZ19yZWNhcHM6cmVhZCBtZWV0aW5nX3JlY2Fwczp3cml0ZSBhY3Rpdml0eV9sb2c6cmVhZCBkYXRhX2NvbXBsaWFuY2U6d3JpdGUgb3V0Z29pbmdfY29tbXVuaWNhdGlvbnM6cmVhZCB3ZWJob29rczpyZWFkIHdlYmhvb2tzOndyaXRlIn0.9sebF0Tr0Q4BsdvunTLk6g3uRXRfkVz9V17-yB1RVwGWtgzdxm-v9oG0x9FzbjbijlDK-di-hCx5zAS4oWCOqQ"

base_url <- "https://api.calendly.com"

# Get current Calendly user
resp_user <- request(
  paste0(base_url, "/users/me")
) %>%
  req_headers(
    Authorization = paste("Bearer", calendly_token)
  ) %>%
  req_perform()

user_info <- resp_body_json(
  resp_user,
  simplifyVector = FALSE
)

organization_uri <- user_info$resource$current_organization

# all scheduled events
get_all_calendly_events <- function(
    organization_uri,
    token
) {
  
  next_url <- paste0(
    base_url,
    "/scheduled_events"
  )
  
  all_results <- list()
  first_request <- TRUE
  
  while (!is.null(next_url)) {
    
    req <- request(next_url) %>%
      req_headers(
        Authorization = paste("Bearer", token)
      )
    
    if (first_request) {
      req <- req %>%
        req_url_query(
          organization = organization_uri,
          count = 100
        )
    }
    
    resp <- req %>%
      req_perform()
    
    result <- resp_body_json(
      resp,
      simplifyVector = FALSE
    )
    
    all_results <- c(
      all_results,
      result$collection
    )
    
    next_url <- result$pagination$next_page
    
    first_request <- FALSE
  }
  
  all_results
}

events_raw <- get_all_calendly_events(
  organization_uri = organization_uri,
  token = calendly_token
)

length(events_raw)


# get invitees info
get_event_invitees <- function(
    event_uri,
    token
) {
  
  event_uuid <- basename(event_uri)
  
  next_url <- paste0(
    base_url,
    "/scheduled_events/",
    event_uuid,
    "/invitees?count=100"
  )
  
  all_results <- list()
  
  while (!is.null(next_url)) {
    
    resp <- request(next_url) %>%
      req_headers(
        Authorization = paste("Bearer", token)
      ) %>%
      req_perform()
    
    result <- resp_body_json(
      resp,
      simplifyVector = FALSE
    )
    
    all_results <- c(
      all_results,
      result$collection
    )
    
    next_url <- result$pagination$next_page
  }
  
  all_results
}


calendly_raw <- map(
  events_raw,
  function(event) {
    
    invitees <- get_event_invitees(
      event_uri = event$uri,
      token = calendly_token
    )
    
    list(
      event = event,
      invitees = invitees
    )
  }
)


# Extract the answer to a booking question by partial question text
# (also catches the "Hold old is your child?" typo variant).
get_booking_answer <- function(questions_and_answers, question_pattern) {
  for (qa in questions_and_answers) {
    if (grepl(question_pattern, trimws(qa$question), ignore.case = TRUE)) {
      return(qa$answer)
    }
  }
  NA_character_
}


# Split a free-text child name answer into first and last name: the first
# token is kept as the first name and the last token as the last name.
# Trailing generational suffixes (Jr, Sr, II, III, IV) are dropped first.
# A single-token answer is kept as the first name with an NA last name.
parse_child_name <- function(child_name) {
  if (is.na(child_name)) {
    return(list(first = NA_character_, last = NA_character_))
  }
  
  name <- gsub("\\s+", " ", trimws(child_name))
  name <- gsub("\\s+(Jr|Sr|II|III|IV)\\.?$", "", name, ignore.case = TRUE)
  
  parts <- strsplit(name, " ", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  
  if (length(parts) >= 2) {
    list(
      first = parts[1],
      last = parts[length(parts)]
    )
  } else if (length(parts) == 1) {
    list(
      first = parts[1],
      last = NA_character_
    )
  } else {
    list(
      first = NA_character_,
      last = NA_character_
    )
  }
}


# Name of the Calendly user who owns/hosts the scheduled event (the staff
# member who set up the invite). Multiple members are joined with "; ".
get_event_host_name <- function(event) {
  host_names <- sapply(
    event$event_memberships,
    function(m) m$user_name
  )
  if (length(host_names) == 0) {
    NA_character_
  } else {
    paste(host_names, collapse = "; ")
  }
}


calendly_export <- map_dfr(
  calendly_raw,
  function(x) {
    
    event <- x$event
    
    map_dfr(
      x$invitees,
      function(invitee) {
        
        # Raw booking answer and parsed parts for the child name question
        child_name <- get_booking_answer(
          invitee$questions_and_answers,
          "child's name"
        )
        child_name_parts <- parse_child_name(child_name)
        
        tibble(
          `Event UUID` = basename(event$uri),
          
          `Invitee UUID` = basename(invitee$uri),
          
          `Event Type Name` = event$name,
          
          # Free-text meeting notes on the event; may contain ECHO/study
          # IDs and requires cleaning based on its content.
          `Meeting Notes Plain` =
            if (is.null(event$meeting_notes_plain)) NA_character_ else event$meeting_notes_plain,
          
          `Meeting Host` =
            get_event_host_name(event),
          
          `Invitee Name` = invitee$name,
          
          `Invitee Email` = invitee$email,
          
          `Invitee Time Zone` = invitee$timezone,
          
          `Child Name` = child_name,
          
          `Child First Name` = child_name_parts$first,
          
          `Child Last Name` = child_name_parts$last,
          
          `Child Age` =
            get_booking_answer(
              invitee$questions_and_answers,
              "old is your child"
            ),
          
          `Start Date & Time` = event$start_time,
          
          `End Date & Time` = event$end_time,
          
          `Event Created Date & Time` =
            invitee$created_at,
          
          `Canceled` =
            invitee$status == "canceled",
          
          `No Show` =
            !is.null(invitee$no_show),
          
          `No Show URI` =
            if (is.null(invitee$no_show)) NA_character_ else invitee$no_show$uri,
          
          `No Show Created Date & Time` =
            if (is.null(invitee$no_show)) NA_character_ else invitee$no_show$created_at
        )
      }
    )
  }
)


# Cleaned Calendly export for IPA follow-up bookings ---------------------

# Parse the free-text age answer into months. Explicit units are honored.
# For a bare number (for example, "18"), the participant's birthday and
# appointment date are used to decide whether it means months or years.
# Cohort is the fallback when a reference age is unavailable.
parse_child_age_months <- function(age_text, reference_months = NA_real_,
                                   cohort = NA_character_) {
  if (is.na(age_text) || str_squish(age_text) == "") {
    return(NA_real_)
  }

  age <- age_text %>%
    str_to_lower() %>%
    str_replace_all("one and a half", "1.5") %>%
    str_replace_all("\\bone\\b", "1") %>%
    str_replace_all("'", "") %>%
    str_squish()

  year_match <- str_match(
    age,
    "([0-9]+(?:\\.[0-9]+)?)\\s*(?:years?|yrs?|yr|y)\\b"
  )[, 2]
  month_match <- str_match(
    age,
    "([0-9]+(?:\\.[0-9]+)?)\\s*(?:months?|mos?|mo|mths?)\\b"
  )[, 2]

  years <- suppressWarnings(as.numeric(year_match))
  months <- suppressWarnings(as.numeric(month_match))

  if (!is.na(years) || !is.na(months)) {
    return(coalesce(years, 0) * 12 + coalesce(months, 0))
  }

  value <- suppressWarnings(parse_number(age, na = c("", "NA")))
  if (is.na(value)) {
    return(NA_real_)
  }

  # Values above 20 cannot be study age in years and are treated as months.
  if (value > 20) {
    return(value)
  }

  month_candidate <- value
  year_candidate <- value * 12

  if (!is.na(reference_months)) {
    candidates <- c(month_candidate, year_candidate)
    return(candidates[which.min(abs(candidates - reference_months))])
  }

  if (!is.na(cohort) && cohort == "6-35 month") {
    return(month_candidate)
  }
  if (!is.na(cohort) && cohort == "3-20 year") {
    return(year_candidate)
  }

  # Without participant context, decimals and values up to 20 are most
  # plausibly years in this study; retain a QA flag downstream.
  year_candidate
}


parse_calendly_datetime <- function(x) {
  parse_date_time(
    x,
    orders = c("ymd HMSz", "ymd HMS", "mdy HMS", "mdy HM"),
    tz = "UTC",
    quiet = TRUE
  )
}


parse_participant_birthday <- function(x) {
  as_date(parse_date_time(
    x,
    orders = c("ymd", "mdy", "dmy"),
    quiet = TRUE
  ))
}


calculate_age_months <- function(birthday, appointment_datetime) {
  if (is.na(birthday) || is.na(appointment_datetime) ||
      as_date(appointment_datetime) < birthday) {
    return(NA_real_)
  }
  floor(time_length(
    interval(birthday, as_date(appointment_datetime)),
    unit = "month"
  ))
}


assign_ipa_age_band <- function(age_months) {
  case_when(
    age_months >= 12 & age_months < 24 ~ "12_23_month",
    age_months >= 24 & age_months < 36 ~ "24_35_month",
    age_months >= 36 & age_months < 72 ~ "3_5yr",
    age_months >= 72 & age_months < 132 ~ "6_10yr",
    age_months >= 132 & age_months < 216 ~ "11_17yr",
    age_months >= 216 & age_months < 252 ~ "18_20yr",
    TRUE ~ NA_character_
  )
}

# Keep only assessment booking types (Event Type Name may carry trailing
# spaces in the source data, so it is trimmed before matching).
assessment_types <- c(
  "Detroit Assessment",
  "East Lansing Assessment",
  "ECHO In-Person Assessment",
  "Flint Assessment",
  "Grand Rapids Assessments",
  "Nicole - Traverse City",
  "Traverse City Assessment"
)

calendly_assessment_rows <- calendly_export %>%
  filter(trimws(`Event Type Name`) %in% assessment_types) %>%
  mutate(
    `Meeting Notes Plain` = `Meeting Notes Plain` %>%
      str_replace_all("[\\r\\n\\t]+", " ") %>%
      str_squish() %>%
      na_if(""),
    `Event Start Date & Time Parsed` =
      parse_calendly_datetime(`Start Date & Time`),
    `Event End Date & Time Parsed` =
      parse_calendly_datetime(`End Date & Time`),
    `Event Created Date & Time Parsed` =
      parse_calendly_datetime(`Event Created Date & Time`),
    `No Show Created Date & Time Parsed` =
      parse_calendly_datetime(`No Show Created Date & Time`),
    `Event Start Date` = as_date(with_tz(
      `Event Start Date & Time Parsed`,
      tzone = "America/New_York"
    )),
    `Event Created Date` = as_date(with_tz(
      `Event Created Date & Time Parsed`,
      tzone = "America/New_York"
    )),
    `No Show Created Date` =
      as_date(with_tz(
        `No Show Created Date & Time Parsed`,
        tzone = "America/New_York"
      ))
  ) %>%
  # The IPA year is determined by the appointment date, not the date on
  # which the booking happened (a 2026 IPA may have been booked in 2025).
  filter(year(`Event Start Date`) == 2026)


# Resolve participant identity before cleaning age. IDs confirmed in the
# meeting notes are preferred; normalized child first + last name is the
# fallback. The matched birthday provides a reliable reference for bare
# numeric age answers.
calendly_participant_resolved <- resolve_calendly_cohort(
  calendly_assessment_rows,
  calendly_participant_catalog
) %>%
  rename(
    `Participant ID` = matched_child_echo_id,
    `Participant Custom ID` = matched_custom_id,
    `Participant Family ID` = matched_family_id,
    `Participant Birthday Raw` = matched_birthday,
    `Participant Cohort` = matched_cohort,
    `Participant Match Method` = matched_by
  )


calendly_export_clean <- calendly_participant_resolved %>%
  mutate(
    `Child Age Raw` = as.character(`Child Age`),
    `Participant Birthday` =
      parse_participant_birthday(`Participant Birthday Raw`),
    .child_age_months_calculated = map2_dbl(
      `Participant Birthday`,
      `Event Start Date & Time Parsed`,
      calculate_age_months
    ),
    .child_age_months_entered = pmap_dbl(
      list(
        `Child Age Raw`,
        .child_age_months_calculated,
        `Participant Cohort`
      ),
      parse_child_age_months
    ),
    # Birthday-based age is the source of truth when it is available.
    .child_age_months = coalesce(
      .child_age_months_calculated,
      .child_age_months_entered
    ),
    # All age fields exposed in the cleaned dataset use years.
    `Child Age` = round(.child_age_months / 12, 2),
    `Child Age Entered` = round(.child_age_months_entered / 12, 2),
    `Child Age Calculated` = round(.child_age_months_calculated / 12, 2),
    `Child Age Difference` = if_else(
      !is.na(.child_age_months_calculated) &
        !is.na(.child_age_months_entered),
      round(abs(
        .child_age_months_calculated - .child_age_months_entered
      ) / 12, 2),
      NA_real_
    ),
    `Child Age Unit` = "years",
    `IPA Age Band` = assign_ipa_age_band(.child_age_months),
    `Child Age Source` = case_when(
      !is.na(.child_age_months_calculated) ~ "birthday + appointment date",
      !is.na(.child_age_months_entered) ~ "Calendly answer",
      TRUE ~ NA_character_
    ),
    `Calendly Record Status` = case_when(
      `No Show` %in% TRUE ~ "No-show",
      `Canceled` %in% TRUE ~ "Canceled",
      TRUE ~ "Not canceled/no-show"
    ),
    `Calendly QA Flag` = case_when(
      is.na(`Participant ID`) ~ "Participant ID unresolved; name key used",
      is.na(`Child Age`) ~ "Child age unresolved",
      is.na(`IPA Age Band`) ~ "Child age outside IPA bands",
      `Child Age Difference` > 0.5 ~
        "Entered age differs from birthday-based age by >0.5 years",
      TRUE ~ NA_character_
    ),
    # Canonical participant ID is used whenever available. Name fallback is
    # deliberately based on both child first and last name; Invitee UUID is
    # used only when neither can identify a child.
    .participant_key = case_when(
      !is.na(`Participant ID`) ~ paste0("id:", `Participant ID`),
      !is.na(normalize_person_name(join_fn)) &
        !is.na(normalize_person_name(join_ln)) ~ paste0(
          "name:", normalize_person_name(join_fn), "|",
          normalize_person_name(join_ln)
        ),
      TRUE ~ paste0("invitee:", `Invitee UUID`)
    ),
    # Preserve separate IPA periods. If an age band cannot be determined,
    # use cleaned age in years so distinct known ages are not collapsed.
    .age_band_key = coalesce(
      `IPA Age Band`,
      if_else(
        !is.na(`Child Age`),
        paste0("age_years_", `Child Age`),
        "age_unknown"
      )
    )
  ) %>%
  # One Calendly row per participant per IPA age band. This removes an
  # earlier canceled/no-show appointment when a later appointment exists,
  # while retaining the newest canceled/no-show record when there is no
  # later booking.
  group_by(.participant_key, .age_band_key) %>%
  arrange(
    desc(`Event Start Date & Time Parsed`),
    desc(`Event Created Date & Time Parsed`),
    desc(`Invitee UUID`),
    .by_group = TRUE
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    `Event UUID`,
    `Invitee UUID`,
    `Event Type Name`,
    `Meeting Notes Plain`,
    `Meeting Host`,
    `Invitee Name`,
    `Child Name`,
    `Child First Name`,
    `Child Last Name`,
    `Child Age Raw`,
    `Child Age`,
    `Child Age Unit`,
    `Child Age Source`,
    `Child Age Entered`,
    `Child Age Calculated`,
    `Child Age Difference`,
    `IPA Age Band`,
    `Participant ID`,
    `Participant Custom ID`,
    `Participant Family ID`,
    `Participant Cohort`,
    `Participant Match Method`,
    `Participant Birthday`,
    `Calendly Record Status`,
    `Calendly QA Flag`,
    `Start Date & Time`,
    `End Date & Time`,
    `Event Start Date`,
    `Event Created Date & Time`,
    `Event Created Date`,
    `Canceled`,
    `No Show`,
    `No Show Created Date & Time`,
    `No Show Created Date`
  )



# 6. 6-35 month Data Cleaning -------------------------------------------------------

## 6.1 Select IPA follow-up variables for 6-35 month participants ---------

# Keep participant identifiers plus the IPA events used for follow-up
# tracking: the scheduled visit and the completed visit.
# Only the age-window events without the "2025_" prefix are kept, matching
# the event naming used by the follow-up dashboard scripts.
ipa_data_6_35_month <- ripple_data_6_35_month %>%
  select(
    globalId,
    firstName,
    lastName,
    birthday,
    tags,
    statusId,
    # IPA scheduled and completed visit events (12-23 and 24-35 month
    # windows); only the columns used downstream are kept (.completed,
    # .completedDate). Other event columns (.missed, .scheduledDate, ...)
    # are dropped.
    event.12_23mo_ipa_scheduled.completed,
    event.12_23mo_ipa_scheduled.completedDate,
    event.24_35mo_ipa_scheduled.completed,
    event.24_35mo_ipa_scheduled.completedDate,
    event.12_23mo_ipa_complete.completed,
    event.12_23mo_ipa_complete.completedDate,
    event.24_35mo_ipa_complete.completed,
    event.24_35mo_ipa_complete.completedDate
  ) %>%
  filter(!statusId %in% c("ECHO 2 Refusal", "Withdrawn"))


## 6.2 Extract Follow-up Team Member (FTM) from tags ----------------------

# Tags are pipe-separated (e.g., "St. Joe|FTM Nicole"). A participant's FTM
# appears either as an "FTM <first name>" tag or, for Jody, as a bare
# "<first name>" tag. The extracted value contains the name only (no "FTM"
# prefix); multiple FTMs on one row would be joined with "; ".
ftm_names_from_tags <- c("Jody", "Nicole", "Anna", "Cassie")

extract_ftm_from_tags <- function(tags) {
  if (is.na(tags) || trimws(tags) == "") {
    return(NA_character_)
  }
  parts <- trimws(strsplit(tags, "\\|")[[1]])
  hit <- character(0)
  for (p in parts) {
    if (grepl("^FTM\\s*\\S+", p, ignore.case = TRUE)) {
      hit <- c(hit, trimws(sub("^FTM\\s*", "", p, ignore.case = TRUE)))
    } else if (p %in% ftm_names_from_tags) {
      hit <- c(hit, p)
    }
  }
  if (length(hit) == 0) {
    NA_character_
  } else {
    paste(unique(hit), collapse = "; ")
  }
}

ipa_data_6_35_month <- ipa_data_6_35_month %>%
  mutate(FTM = map_chr(tags, extract_ftm_from_tags)) %>%
  relocate(FTM, .after = tags)



## 6.3 Merge Calendly no-show records into IPA participant data -----------

# Layered matching: each Calendly row is first resolved to a 6-35 month
# participant using IDs in the meeting notes (customId > child_echo_id >
# familyId); a row without a usable ID falls back to child name matching.
# matched_by records how each row was attached; a participant with several
# bookings gets one row per booking (many-to-many).
cal_match_6_35 <- resolve_calendly_cohort(
  calendly_export_clean,
  participant_id_catalog %>% filter(cohort == "6-35 month")
)

ipa_merged_data <- ipa_data_6_35_month %>%
  mutate(
    fn_key = tolower(trimws(firstName)),
    ln_key = tolower(trimws(lastName))
  ) %>%
  left_join(
    cal_match_6_35 %>%
      mutate(
        fn_key = tolower(trimws(join_fn)),
        ln_key = tolower(trimws(join_ln))
      ) %>%
      select(-join_fn, -join_ln),
    by = c("fn_key", "ln_key"),
    relationship = "many-to-many"
  ) %>%
  select(-fn_key, -ln_key)

## 6.4 Determine IPA outcome with No-show consideration ------------------

# The participant's current age window is derived from statusId:
#   - "12-23 Month ..." -> 12_23_month
#   - "24-35 Month ..." -> 24_35_month
# IPA assessments apply to these two windows only.
# The IPA outcome is then:
#   - "No-show":    a matched Calendly record has No Show = TRUE
#   - "Complete":   the IPA Complete event is TRUE
#   - "Incomplete": the IPA Scheduled event is TRUE but the IPA Complete
#                   event is FALSE
# A window with no IPA Scheduled event (e.g., not yet scheduled) is left NA.
ipa_merged_data <- ipa_merged_data %>%
  mutate(
    # Current age window based on statusId
    IPA_Age_Window = case_when(
      str_detect(statusId, "^12-23 Month") ~ "12_23_month",
      str_detect(statusId, "^24-35 Month") ~ "24_35_month",
      TRUE ~ NA_character_
    ),
    # Event flags for the current age window
    IPA_Scheduled_Completed = case_when(
      IPA_Age_Window == "12_23_month" ~ event.12_23mo_ipa_scheduled.completed,
      IPA_Age_Window == "24_35_month" ~ event.24_35mo_ipa_scheduled.completed,
      TRUE ~ NA
    ),
    IPA_Complete_Completed = case_when(
      IPA_Age_Window == "12_23_month" ~ event.12_23mo_ipa_complete.completed,
      IPA_Age_Window == "24_35_month" ~ event.24_35mo_ipa_complete.completed,
      TRUE ~ NA
    ),
    # Outcome dates from the current age window events
    IPA_Complete_Date = case_when(
      IPA_Age_Window == "12_23_month" ~
        as_date(mdy(event.12_23mo_ipa_complete.completedDate)),
      IPA_Age_Window == "24_35_month" ~
        as_date(mdy(event.24_35mo_ipa_complete.completedDate)),
      TRUE ~ as.Date(NA)
    ),
    IPA_Scheduled_Date = case_when(
      IPA_Age_Window == "12_23_month" ~
        as_date(mdy(event.12_23mo_ipa_scheduled.completedDate)),
      IPA_Age_Window == "24_35_month" ~
        as_date(mdy(event.24_35mo_ipa_scheduled.completedDate)),
      TRUE ~ as.Date(NA)
    ),
    # Outcome: No-show takes precedence over event-based outcomes
    IPA_Outcome = case_when(
      `No Show` == TRUE & !is.na(IPA_Age_Window) ~ "No-show",
      IPA_Complete_Completed == TRUE ~ "Complete",
      IPA_Scheduled_Completed == TRUE & IPA_Complete_Completed == FALSE ~ "Incomplete",
      TRUE ~ NA_character_
    ),
    IPA_Outcome_Date = case_when(
      IPA_Outcome == "No-show" ~ `No Show Created Date`,
      IPA_Outcome == "Complete" ~ IPA_Complete_Date,
      IPA_Outcome == "Incomplete" ~ IPA_Scheduled_Date,
      TRUE ~ as.Date(NA)
    )
  ) %>%
  # Keep only the derived age window/outcome columns
  select(
    -IPA_Scheduled_Completed,
    -IPA_Complete_Completed,
    -IPA_Complete_Date,
    -IPA_Scheduled_Date
  )



# 7. 3-20 Year Data Cleaning ---------------------------------------------

## 7.1 Select IPA variables for 3-20 year participants --------------------
ipa_data_3_20_year <- ripple_data_3_20_year %>% 
  select(
    globalId,
    firstName,
    lastName,
    sex,
    birthday,
    race,
    statusId,
    # Keep participant identifiers plus the IPA Scheduled and IPA Complete
    # events for the four 3-20 year age bands.
    matches("^event\\.2026_(3_5yr|6_10yr|11_17yr|18_20yr)_ipa_(scheduled|complete)\\.")
  ) %>%
  filter(!(globalId %in% c("iri0LoVJXJ734mKYy","54jpojzH4AVzTJoPD")))  %>% # delete Anna and Mallory from the result
  # filter(statusId != "Withdrawn") %>%
  extract(
    globalId, 
    into = c("child_echo_id", "PIN"), 
    regex = "(.*)\\s\\((.*)\\)",
    remove = TRUE 
  ) %>% 
  relocate(child_echo_id, PIN)



## 7.2 Merge Calendly no-show records into 3-20 year IPA data ------------

# Layered matching against the 3-20 year catalog: IDs in the meeting notes
# first (customId > child_echo_id > familyId), then child name as fallback.
# Rows are attached by child_echo_id.
cal_match_3_20 <- resolve_calendly_cohort(
  calendly_export_clean,
  participant_id_catalog %>% filter(cohort == "3-20 year")
)

ipa_merged_data_3_20 <- ipa_data_3_20_year %>%
  left_join(
    cal_match_3_20 %>%
      mutate(child_echo_id = matched_child_echo_id) %>%
      select(-join_fn, -join_ln),
    by = "child_echo_id",
    relationship = "many-to-many"
  )



## 7.3 Determine IPA outcome for 3-20 year participants --------------------

# Current age window from statusId:
#   - "3-5 Year"   -> 3_5yr
#   - "6-10 Year"  -> 6_10yr
#   - "11-17 Year" -> 11_17yr
#   - "18-20 Year" -> 18_20yr
# Outcome from the current age window events, with No-show first:
#   - "No-show":    a matched Calendly record has No Show = TRUE
#   - "Complete":   the IPA Complete event is TRUE
#   - "Incomplete": the IPA Scheduled event is TRUE but the IPA Complete
#                   event is FALSE
# Participants without an applicable age window are left NA.
ipa_merged_data_3_20 <- ipa_merged_data_3_20 %>%
  mutate(
    IPA_Age_Window = case_when(
      str_detect(statusId, "^3-5 Year") ~ "3_5yr",
      str_detect(statusId, "^6-10 Year") ~ "6_10yr",
      str_detect(statusId, "^11-17 Year") ~ "11_17yr",
      str_detect(statusId, "^18-20 Year") ~ "18_20yr",
      TRUE ~ NA_character_
    ),
    # Event flags for the current age window
    IPA_Scheduled_Completed = case_when(
      IPA_Age_Window == "3_5yr" ~ event.2026_3_5yr_ipa_scheduled.completed,
      IPA_Age_Window == "6_10yr" ~ event.2026_6_10yr_ipa_scheduled.completed,
      IPA_Age_Window == "11_17yr" ~ event.2026_11_17yr_ipa_scheduled.completed,
      IPA_Age_Window == "18_20yr" ~ event.2026_18_20yr_ipa_scheduled.completed,
      TRUE ~ NA
    ),
    IPA_Complete_Completed = case_when(
      IPA_Age_Window == "3_5yr" ~ event.2026_3_5yr_ipa_complete.completed,
      IPA_Age_Window == "6_10yr" ~ event.2026_6_10yr_ipa_complete.completed,
      IPA_Age_Window == "11_17yr" ~ event.2026_11_17yr_ipa_complete.completed,
      IPA_Age_Window == "18_20yr" ~ event.2026_18_20yr_ipa_complete.completed,
      TRUE ~ NA
    ),
    # Outcome dates from the current age window events
    IPA_Complete_Date = case_when(
      IPA_Age_Window == "3_5yr" ~
        as_date(mdy(event.2026_3_5yr_ipa_complete.completedDate)),
      IPA_Age_Window == "6_10yr" ~
        as_date(mdy(event.2026_6_10yr_ipa_complete.completedDate)),
      IPA_Age_Window == "11_17yr" ~
        as_date(mdy(event.2026_11_17yr_ipa_complete.completedDate)),
      IPA_Age_Window == "18_20yr" ~
        as_date(mdy(event.2026_18_20yr_ipa_complete.completedDate)),
      TRUE ~ as.Date(NA)
    ),
    IPA_Scheduled_Date = case_when(
      IPA_Age_Window == "3_5yr" ~
        as_date(mdy(event.2026_3_5yr_ipa_scheduled.completedDate)),
      IPA_Age_Window == "6_10yr" ~
        as_date(mdy(event.2026_6_10yr_ipa_scheduled.completedDate)),
      IPA_Age_Window == "11_17yr" ~
        as_date(mdy(event.2026_11_17yr_ipa_scheduled.completedDate)),
      IPA_Age_Window == "18_20yr" ~
        as_date(mdy(event.2026_18_20yr_ipa_scheduled.completedDate)),
      TRUE ~ as.Date(NA)
    ),
    IPA_Outcome = case_when(
      `No Show` == TRUE & !is.na(IPA_Age_Window) ~ "No-show",
      IPA_Complete_Completed == TRUE ~ "Complete",
      IPA_Scheduled_Completed == TRUE & IPA_Complete_Completed == FALSE ~ "Incomplete",
      TRUE ~ NA_character_
    ),
    IPA_Outcome_Date = case_when(
      IPA_Outcome == "No-show" ~ `No Show Created Date`,
      IPA_Outcome == "Complete" ~ IPA_Complete_Date,
      IPA_Outcome == "Incomplete" ~ IPA_Scheduled_Date,
      TRUE ~ as.Date(NA)
    )
  ) %>%
  # Keep only the derived age window/outcome columns
  select(
    -IPA_Scheduled_Completed,
    -IPA_Complete_Completed,
    -IPA_Complete_Date,
    -IPA_Scheduled_Date
  )


# 8. Final FTM Participant Task Checklist --------------------------------

# The objects in this section are the final participant-level deliverables.
# IPA status uses Ripple plus the cleaned/latest Calendly record. All other
# task statuses use Ripple only, following the supplied Task logic table.

to_logical_flag <- function(x) {
  value <- str_to_lower(str_squish(as.character(x)))
  case_when(
    value %in% c("true", "1", "yes", "y") ~ TRUE,
    value %in% c("false", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}


parse_ripple_date <- function(x) {
  as_date(parse_date_time(
    as.character(x),
    orders = c("mdy HMS", "mdy HM", "mdy", "ymd HMSz", "ymd HMS", "ymd"),
    quiet = TRUE
  ))
}


column_or_na <- function(data, column_name) {
  if (column_name %in% names(data)) {
    data[[column_name]]
  } else {
    rep(NA, nrow(data))
  }
}


# One participant row from each Ripple cohort, with a common age-band key
# and FTM field. The event columns are retained for task evaluation below.
participant_base_6_35 <- ripple_data_6_35_month %>%
  mutate(
    `Participant ID` = str_squish(str_remove(globalId, "\\s*\\([^)]*\\)$")),
    PIN = str_extract(globalId, "(?<=\\()[^)]*(?=\\)$)"),
    `Participant Cohort` = "6-35 month",
    `IPA Age Band` = case_when(
      str_detect(statusId, regex("^12-23 Month", ignore_case = TRUE)) ~
        "12_23_month",
      str_detect(statusId, regex("^24-35 Month", ignore_case = TRUE)) ~
        "24_35_month",
      TRUE ~ NA_character_
    ),
    FTM = map_chr(tags, extract_ftm_from_tags)
  ) %>%
  filter(!statusId %in% c("Withdrawn", "ECHO 2 Refusal")) %>%
  select(
    `Participant ID`, PIN, customId, familyId, firstName, lastName,
    birthday, statusId, `Participant Cohort`, `IPA Age Band`, FTM,
    starts_with("event.")
  )


staff_by_participant <- staff_assignment_main %>%
  select(child_echo_id, staff) %>%
  filter(!is.na(child_echo_id), child_echo_id != "") %>%
  distinct(child_echo_id, .keep_all = TRUE)


participant_base_3_20 <- ripple_data_3_20_year %>%
  mutate(
    `Participant ID` = str_squish(str_remove(globalId, "\\s*\\([^)]*\\)$")),
    PIN = str_extract(globalId, "(?<=\\()[^)]*(?=\\)$)"),
    `Participant Cohort` = "3-20 year",
    `IPA Age Band` = case_when(
      str_detect(statusId, regex("^3-5 Year", ignore_case = TRUE)) ~ "3_5yr",
      str_detect(statusId, regex("^6-10 Year", ignore_case = TRUE)) ~ "6_10yr",
      str_detect(statusId, regex("^11-17 Year", ignore_case = TRUE)) ~ "11_17yr",
      str_detect(statusId, regex("^18-20 Year", ignore_case = TRUE)) ~ "18_20yr",
      TRUE ~ NA_character_
    ),
    FTM_from_tags = map_chr(tags, extract_ftm_from_tags)
  ) %>%
  filter(
    !statusId %in% c("Withdrawn", "ECHO 2 Refusal"),
    !globalId %in% c("iri0LoVJXJ734mKYy", "54jpojzH4AVzTJoPD")
  ) %>%
  left_join(
    staff_by_participant,
    by = c("Participant ID" = "child_echo_id")
  ) %>%
  mutate(FTM = coalesce(staff, FTM_from_tags)) %>%
  select(
    `Participant ID`, PIN, customId, familyId, firstName, lastName,
    birthday, statusId, `Participant Cohort`, `IPA Age Band`, FTM,
    starts_with("event.")
  )


participant_task_base <- bind_rows(
  participant_base_6_35,
  participant_base_3_20
) %>%
  filter(!is.na(`IPA Age Band`)) %>%
  distinct(`Participant ID`, `Participant Cohort`, .keep_all = TRUE)


# Calendly is already one row per participant and IPA age band. This second
# defensive check guarantees a single latest row before the Ripple join.
calendly_ipa_latest <- calendly_export_clean %>%
  filter(!is.na(`Participant ID`), !is.na(`IPA Age Band`)) %>%
  arrange(
    `Participant ID`,
    `IPA Age Band`,
    desc(`Event Start Date`),
    desc(`Event Created Date & Time`)
  ) %>%
  group_by(`Participant ID`, `IPA Age Band`) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    `Participant ID`,
    `IPA Age Band`,
    Calendly_No_Show = `No Show`,
    Calendly_Canceled = `Canceled`,
    Calendly_Status = `Calendly Record Status`,
    Calendly_Appointment_Date = `Event Start Date`,
    Calendly_No_Show_Date = `No Show Created Date`,
    Calendly_Match_Method = `Participant Match Method`,
    Calendly_Invitee_UUID = `Invitee UUID`
  )


ipa_event_map <- tribble(
  ~age_band, ~scheduled_col, ~complete_col, ~scheduled_date_col, ~complete_date_col,
  "12_23_month",
  "event.12_23mo_ipa_scheduled.completed",
  "event.12_23mo_ipa_complete.completed",
  "event.12_23mo_ipa_scheduled.completedDate",
  "event.12_23mo_ipa_complete.completedDate",
  "24_35_month",
  "event.24_35mo_ipa_scheduled.completed",
  "event.24_35mo_ipa_complete.completed",
  "event.24_35mo_ipa_scheduled.completedDate",
  "event.24_35mo_ipa_complete.completedDate",
  "3_5yr",
  "event.2026_3_5yr_ipa_scheduled.completed",
  "event.2026_3_5yr_ipa_complete.completed",
  "event.2026_3_5yr_ipa_scheduled.completedDate",
  "event.2026_3_5yr_ipa_complete.completedDate",
  "6_10yr",
  "event.2026_6_10yr_ipa_scheduled.completed",
  "event.2026_6_10yr_ipa_complete.completed",
  "event.2026_6_10yr_ipa_scheduled.completedDate",
  "event.2026_6_10yr_ipa_complete.completedDate",
  "11_17yr",
  "event.2026_11_17yr_ipa_scheduled.completed",
  "event.2026_11_17yr_ipa_complete.completed",
  "event.2026_11_17yr_ipa_scheduled.completedDate",
  "event.2026_11_17yr_ipa_complete.completedDate",
  "18_20yr",
  "event.2026_18_20yr_ipa_scheduled.completed",
  "event.2026_18_20yr_ipa_complete.completed",
  "event.2026_18_20yr_ipa_scheduled.completedDate",
  "event.2026_18_20yr_ipa_complete.completedDate"
)


build_ipa_task_rows <- function(participant_data, calendly_data, event_map) {
  pmap_dfr(
    event_map,
    function(age_band, scheduled_col, complete_col,
             scheduled_date_col, complete_date_col) {
      participant_subset <- participant_data %>%
        filter(`IPA Age Band` == age_band)

      scheduled_flag <- to_logical_flag(
        column_or_na(participant_subset, scheduled_col)
      )
      complete_flag <- to_logical_flag(
        column_or_na(participant_subset, complete_col)
      )
      scheduled_date <- parse_ripple_date(
        column_or_na(participant_subset, scheduled_date_col)
      )
      complete_date <- parse_ripple_date(
        column_or_na(participant_subset, complete_date_col)
      )

      participant_subset %>%
        select(
          `Participant ID`, `Participant Cohort`, FTM, firstName, lastName,
          birthday, statusId, `IPA Age Band`
        ) %>%
        left_join(
          calendly_data %>% filter(`IPA Age Band` == age_band),
          by = c("Participant ID", "IPA Age Band")
        ) %>%
        mutate(
          Task = "In-Person Assessments",
          Outcome = case_when(
            complete_flag %in% TRUE ~ "Complete",
            Calendly_No_Show %in% TRUE ~ "No-Show",
            scheduled_flag %in% TRUE & complete_flag %in% FALSE ~ "Incomplete",
            TRUE ~ "No record"
          ),
          Score = case_when(
            Outcome == "Complete" ~ 1,
            Outcome == "No-Show" ~ 0.25,
            Outcome == "Incomplete" ~ 0,
            TRUE ~ NA_real_
          ),
          Source = "Ripple & Calendly",
          Outcome_Date = case_when(
            Outcome == "Complete" ~ complete_date,
            Outcome == "No-Show" ~ Calendly_No_Show_Date,
            Outcome == "Incomplete" ~ scheduled_date,
            TRUE ~ as.Date(NA)
          ),
          Ripple_Completed_Field = complete_col,
          Ripple_Scheduled_Field = scheduled_col
        )
    }
  )
}


ipa_task_checklist <- build_ipa_task_rows(
  participant_task_base,
  calendly_ipa_latest,
  ipa_event_map
)


# Ripple-only tasks from the supplied Task table. age_bands is a list-column
# so each task is created only for participants to whom it applies.
ripple_task_map <- tibble(
  Task = c(
    "12-23mo Child Urine",
    "12-23mo Child Bloodspot",
    "12-23mo Mom Bloodspot",
    "12-23mo Hair",
    "24-35mo Mom Urine",
    "3-5yr Child Saliva",
    "11-17yr Child Hair",
    "3yrs+ Tooth 1",
    "3yrs+ Tooth 2",
    "3yrs+ Tooth 3"
  ),
  age_bands = list(
    "12_23_month",
    "12_23_month",
    "12_23_month",
    "12_23_month",
    "24_35_month",
    "3_5yr",
    "11_17yr",
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr"),
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr"),
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr")
  ),
  completed_col = c(
    "event.12_23mo_child_urine_diaper_.completed",
    "event.12_23mo_child_bloodspot.completed",
    "event.12_23mo_mom_bloodspot.completed",
    "event.12_23mo_child_hair.completed",
    "event.24_35mo_cg_urine.completed",
    "event.2026_3_5yr_child_saliva.completed",
    "event.2026_11_17yr_child_hair.completed",
    "event.2026_tooth_1_kit_received.completed",
    "event.2026_tooth_2_kit_received.completed",
    "event.2026_tooth_3_kit_received.completed"
  ),
  completed_date_col = c(
    "event.12_23mo_child_urine_diaper_.completedDate",
    "event.12_23mo_child_bloodspot.completedDate",
    "event.12_23mo_mom_bloodspot.completedDate",
    "event.12_23mo_child_hair.completedDate",
    "event.24_35mo_cg_urine.completedDate",
    "event.2026_3_5yr_child_saliva.completedDate",
    "event.2026_11_17yr_child_hair.completedDate",
    "event.2026_tooth_1_kit_received.completedDate",
    "event.2026_tooth_2_kit_received.completedDate",
    "event.2026_tooth_3_kit_received.completedDate"
  )
)


build_ripple_task_rows <- function(participant_data, task_map) {
  pmap_dfr(
    task_map,
    function(Task, age_bands, completed_col, completed_date_col) {
      participant_subset <- participant_data %>%
        filter(`IPA Age Band` %in% age_bands)

      complete_flag <- to_logical_flag(
        column_or_na(participant_subset, completed_col)
      )
      completion_date <- parse_ripple_date(
        column_or_na(participant_subset, completed_date_col)
      )

      participant_subset %>%
        transmute(
          `Participant ID`,
          `Participant Cohort`,
          FTM,
          firstName,
          lastName,
          birthday,
          statusId,
          `IPA Age Band`,
          Task = Task,
          Outcome = case_when(
            complete_flag %in% TRUE ~ "Complete",
            complete_flag %in% FALSE ~ "Incomplete",
            TRUE ~ "No record"
          ),
          Score = case_when(
            Outcome == "Complete" ~ 1,
            Outcome == "Incomplete" ~ 0,
            TRUE ~ NA_real_
          ),
          Source = "Ripple",
          Outcome_Date = if_else(
            Outcome == "Complete",
            completion_date,
            as.Date(NA)
          ),
          Ripple_Completed_Field = completed_col,
          Ripple_Scheduled_Field = NA_character_,
          Calendly_No_Show = NA,
          Calendly_Canceled = NA,
          Calendly_Status = NA_character_,
          Calendly_Appointment_Date = as.Date(NA),
          Calendly_No_Show_Date = as.Date(NA),
          Calendly_Match_Method = NA_character_,
          Calendly_Invitee_UUID = NA_character_
        )
    }
  )
}


ripple_task_checklist <- build_ripple_task_rows(
  participant_task_base,
  ripple_task_map
)


# Primary long-form checklist: one row per participant per applicable task.
participant_task_checklist_long <- bind_rows(
  ipa_task_checklist,
  ripple_task_checklist
) %>%
  arrange(FTM, `Participant Cohort`, `Participant ID`, Task) %>%
  select(
    FTM,
    `Participant ID`,
    firstName,
    lastName,
    birthday,
    `Participant Cohort`,
    statusId,
    `IPA Age Band`,
    Task,
    Outcome,
    Score,
    Source,
    Outcome_Date,
    everything()
  )


# Preserve FTM assignment history and lock credit when a task first reaches a
# terminal outcome. On the first dashboard run, current assignments become the
# baseline for all existing Complete and No-Show records.
ipa_dashboard_app_dir <- Sys.getenv("IPA_DASHBOARD_APP_DIR", unset = "")

if (ipa_dashboard_app_dir == "") {
  ipa_dashboard_app_dir <- if (basename(getwd()) == "followup_dashboard") {
    file.path(getwd(), "ipa_streamlit_app")
  } else {
    file.path(getwd(), "followup_dashboard", "ipa_streamlit_app")
  }
}

ipa_dashboard_latest_dir <- file.path(ipa_dashboard_app_dir, "data", "latest")
dashboard_run_time <- Sys.time()
dashboard_snapshot_id <- format(dashboard_run_time, "%Y-%m-%d_%H%M%S")
ipa_dashboard_snapshot_dir <- file.path(
  ipa_dashboard_app_dir,
  "data",
  "snapshots",
  dashboard_snapshot_id
)

dir.create(ipa_dashboard_latest_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ipa_dashboard_snapshot_dir, recursive = TRUE, showWarnings = FALSE)

normalize_dashboard_ftm <- function(x) {
  value <- str_squish(as.character(x))
  if_else(is.na(value) | value == "", "Unassigned", value)
}

current_ftm_assignments <- participant_task_checklist_long %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `IPA Age Band`,
    FTM = normalize_dashboard_ftm(FTM),
    `Assignment Source` = if_else(
      `Participant Cohort` == "6-35 month",
      "Ripple",
      "Call List"
    ),
    `Observed Date` = Sys.Date()
  ) %>%
  distinct(`Participant ID`, `Participant Cohort`, .keep_all = TRUE)

assignment_history_path <- file.path(
  ipa_dashboard_latest_dir,
  "ftm_assignment_history.csv"
)

empty_assignment_history <- tibble(
  `Participant ID` = character(),
  `Participant Cohort` = character(),
  `Age Band at Start` = character(),
  `Age Band at Last Observation` = character(),
  FTM = character(),
  `Effective Start` = as.Date(character()),
  `Effective End` = as.Date(character()),
  `First Observed` = as.Date(character()),
  `Last Observed` = as.Date(character()),
  `Assignment Source` = character()
)

ftm_assignment_history <- if (file.exists(assignment_history_path)) {
  history_loaded <- read_csv(
    assignment_history_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
  if (!"Age Band at Start" %in% names(history_loaded)) {
    history_loaded$`Age Band at Start` <- history_loaded$`IPA Age Band`
  }
  if (!"Age Band at Last Observation" %in% names(history_loaded)) {
    history_loaded$`Age Band at Last Observation` <- history_loaded$`IPA Age Band`
  }
  history_loaded %>%
    mutate(
      across(
        c(`Effective Start`, `Effective End`, `First Observed`, `Last Observed`),
        as.Date
      )
    ) %>%
    select(-any_of("IPA Age Band"))
} else {
  empty_assignment_history
}

for (assignment_row in seq_len(nrow(current_ftm_assignments))) {
  current <- current_ftm_assignments[assignment_row, ]
  open_index <- which(
    ftm_assignment_history$`Participant ID` == current$`Participant ID` &
      ftm_assignment_history$`Participant Cohort` == current$`Participant Cohort` &
      is.na(ftm_assignment_history$`Effective End`)
  )

  if (length(open_index) == 0) {
    ftm_assignment_history <- bind_rows(
      ftm_assignment_history,
      current %>%
        transmute(
          `Participant ID`,
          `Participant Cohort`,
          `Age Band at Start` = `IPA Age Band`,
          `Age Band at Last Observation` = `IPA Age Band`,
          FTM,
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
  active_ftm <- ftm_assignment_history$FTM[active_index]

  if (identical(active_ftm, current$FTM[[1]])) {
    ftm_assignment_history$`Last Observed`[active_index] <- Sys.Date()
    ftm_assignment_history$`Age Band at Last Observation`[active_index] <-
      current$`IPA Age Band`[[1]]
  } else if (ftm_assignment_history$`Effective Start`[active_index] == Sys.Date()) {
    # Multiple refreshes on the baseline day are treated as corrections.
    ftm_assignment_history$FTM[active_index] <- current$FTM[[1]]
    ftm_assignment_history$`Age Band at Start`[active_index] <-
      current$`IPA Age Band`[[1]]
    ftm_assignment_history$`Age Band at Last Observation`[active_index] <-
      current$`IPA Age Band`[[1]]
    ftm_assignment_history$`Last Observed`[active_index] <- Sys.Date()
    ftm_assignment_history$`Assignment Source`[active_index] <-
      current$`Assignment Source`[[1]]
  } else {
    ftm_assignment_history$`Effective End`[active_index] <- Sys.Date() - 1
    ftm_assignment_history <- bind_rows(
      ftm_assignment_history,
      current %>%
        transmute(
          `Participant ID`,
          `Participant Cohort`,
          `Age Band at Start` = `IPA Age Band`,
          `Age Band at Last Observation` = `IPA Age Band`,
          FTM,
          `Effective Start` = `Observed Date`,
          `Effective End` = as.Date(NA),
          `First Observed` = `Observed Date`,
          `Last Observed` = `Observed Date`,
          `Assignment Source`
        )
    )
  }
}

ftm_assignment_history <- ftm_assignment_history %>%
  arrange(`Participant ID`, `Participant Cohort`, `Effective Start`)

write_csv(ftm_assignment_history, assignment_history_path, na = "")
write_csv(
  current_ftm_assignments,
  file.path(ipa_dashboard_snapshot_dir, "ftm_assignment_snapshot.csv"),
  na = ""
)
write_csv(
  ftm_assignment_history,
  file.path(ipa_dashboard_snapshot_dir, "ftm_assignment_history.csv"),
  na = ""
)


# Task responsibility is locked as soon as an age-band task first becomes
# applicable, regardless of whether it is Complete, No-Show, Incomplete, or has
# No record. This prevents a missed task from moving to a later FTM after the
# participant ages into a new band.
task_responsibility_path <- file.path(
  ipa_dashboard_latest_dir,
  "task_responsibility_ledger.csv"
)
empty_task_responsibility_ledger <- tibble(
  `Participant ID` = character(),
  `Participant Cohort` = character(),
  `IPA Age Band` = character(),
  Task = character(),
  `Responsible FTM` = character(),
  `Responsibility Start` = as.Date(character()),
  `Last Observed` = as.Date(character()),
  `Assignment Source` = character()
)

task_responsibility_ledger <- if (file.exists(task_responsibility_path)) {
  read_csv(
    task_responsibility_path,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  ) %>%
    mutate(across(c(`Responsibility Start`, `Last Observed`), as.Date))
} else {
  empty_task_responsibility_ledger
}

current_task_responsibilities <- participant_task_checklist_long %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `IPA Age Band`,
    Task,
    `Current FTM` = normalize_dashboard_ftm(FTM),
    `Assignment Source` = if_else(
      `Participant Cohort` == "6-35 month",
      "Ripple",
      "Call List"
    )
  ) %>%
  distinct(`Participant ID`, `Participant Cohort`, `IPA Age Band`, Task, .keep_all = TRUE)

for (task_row in seq_len(nrow(current_task_responsibilities))) {
  current <- current_task_responsibilities[task_row, ]
  responsibility_index <- which(
    task_responsibility_ledger$`Participant ID` == current$`Participant ID` &
      task_responsibility_ledger$`Participant Cohort` == current$`Participant Cohort` &
      task_responsibility_ledger$`IPA Age Band` == current$`IPA Age Band` &
      task_responsibility_ledger$Task == current$Task
  )

  if (length(responsibility_index) == 0) {
    task_responsibility_ledger <- bind_rows(
      task_responsibility_ledger,
      current %>%
        transmute(
          `Participant ID`,
          `Participant Cohort`,
          `IPA Age Band`,
          Task,
          `Responsible FTM` = `Current FTM`,
          `Responsibility Start` = Sys.Date(),
          `Last Observed` = Sys.Date(),
          `Assignment Source`
        )
    )
  } else {
    responsibility_index <- tail(responsibility_index, 1)
    task_responsibility_ledger$`Last Observed`[responsibility_index] <- Sys.Date()
    fill_unassigned <-
      task_responsibility_ledger$`Responsible FTM`[responsibility_index] ==
        "Unassigned" &&
      current$`Current FTM`[[1]] != "Unassigned"

    if (fill_unassigned) {
      task_responsibility_ledger$`Responsible FTM`[responsibility_index] <-
        current$`Current FTM`[[1]]
      task_responsibility_ledger$`Assignment Source`[responsibility_index] <-
        current$`Assignment Source`[[1]]
    }
  }
}

task_responsibility_ledger <- task_responsibility_ledger %>%
  distinct(
    `Participant ID`, `Participant Cohort`, `IPA Age Band`, Task,
    .keep_all = TRUE
  ) %>%
  arrange(`Participant ID`, `Participant Cohort`, `IPA Age Band`, Task)

write_csv(task_responsibility_ledger, task_responsibility_path, na = "")
write_csv(
  task_responsibility_ledger,
  file.path(ipa_dashboard_snapshot_dir, "task_responsibility_ledger.csv"),
  na = ""
)

previous_task_history_path <- file.path(
  ipa_dashboard_latest_dir,
  "task_checklist_long.csv"
)
previous_task_history <- if (file.exists(previous_task_history_path)) {
  read_csv(previous_task_history_path, show_col_types = FALSE, guess_max = 10000)
} else {
  tibble()
}

current_task_rows <- participant_task_checklist_long %>%
  mutate(`Current FTM` = normalize_dashboard_ftm(FTM)) %>%
  left_join(
    task_responsibility_ledger %>%
      select(
        `Participant ID`,
        `Participant Cohort`,
        `IPA Age Band`,
        Task,
        `Responsible FTM`,
        `Responsibility Start`
      ),
    by = c("Participant ID", "Participant Cohort", "IPA Age Band", "Task")
  ) %>%
  mutate(
    `Task Stage` = "Current",
    FTM = `Responsible FTM`
  ) %>%
  mutate(
    across(
      any_of(
        c(
          "birthday", "Outcome_Date", "Calendly_Appointment_Date",
          "Calendly_No_Show_Date", "Responsibility Start"
        )
      ),
      as.character
    )
  ) %>%
  relocate(FTM, `Current FTM`, `Responsible FTM`, `Task Stage`)

task_grain <- c("Participant ID", "Participant Cohort", "IPA Age Band", "Task")

historical_task_rows <- if (nrow(previous_task_history) > 0) {
  previous_task_history %>%
    anti_join(current_task_rows %>% select(all_of(task_grain)), by = task_grain) %>%
    select(
      -any_of(
        c(
          "FTM", "Current FTM", "Credited FTM", "Credited Outcome",
          "Credit Locked Date", "Reporting FTM", "Task Stage",
          "Responsible FTM", "Responsibility Start"
        )
      )
    ) %>%
    left_join(
      current_ftm_assignments %>%
        select(`Participant ID`, `Participant Cohort`, `Current FTM` = FTM),
      by = c("Participant ID", "Participant Cohort")
    ) %>%
    left_join(
      task_responsibility_ledger %>%
        select(
          `Participant ID`, `Participant Cohort`, `IPA Age Band`, Task,
          `Responsible FTM`, `Responsibility Start`
        ),
      by = task_grain
    ) %>%
    mutate(
      `Current FTM` = normalize_dashboard_ftm(`Current FTM`),
      `Task Stage` = "Historical",
      FTM = `Responsible FTM`
    ) %>%
    mutate(
      across(
        any_of(
          c(
            "birthday", "Outcome_Date", "Calendly_Appointment_Date",
            "Calendly_No_Show_Date", "Responsibility Start"
          )
        ),
        as.character
      )
    ) %>%
    relocate(FTM, `Current FTM`, `Responsible FTM`, `Task Stage`)
} else {
  tibble()
}

participant_task_checklist_long <- bind_rows(
  current_task_rows,
  historical_task_rows
) %>%
  distinct(
    `Participant ID`, `Participant Cohort`, `IPA Age Band`, Task,
    .keep_all = TRUE
  ) %>%
  arrange(FTM, `Participant Cohort`, `Participant ID`, `IPA Age Band`, Task)


# Wide checklist: one row per participant, convenient for FTM follow-up.
participant_task_checklist_wide <- participant_task_checklist_long %>%
  select(
    `Current FTM`,
    `Participant ID`,
    firstName,
    lastName,
    birthday,
    `Participant Cohort`,
    statusId,
    `IPA Age Band`,
    Task,
    Outcome,
    Score,
    `Responsible FTM`,
    `Task Stage`
  ) %>%
  pivot_wider(
    names_from = Task,
    values_from = c(Outcome, Score, `Responsible FTM`),
    names_glue = "{Task} {.value}"
  ) %>%
  rename(FTM = `Current FTM`)


participant_task_totals <- participant_task_checklist_long %>%
  group_by(
    `Current FTM`,
    `Participant ID`,
    firstName,
    lastName,
    `Participant Cohort`,
    `IPA Age Band`,
    `Task Stage`
  ) %>%
  summarise(
    `Applicable Tasks` = n(),
    `Tasks Complete` = sum(Outcome == "Complete"),
    `Tasks No-Show` = sum(Outcome == "No-Show"),
    `Tasks Incomplete` = sum(Outcome == "Incomplete"),
    `Tasks No Record` = sum(Outcome == "No record"),
    `Total Score` = if_else(
      all(is.na(Score)),
      NA_real_,
      sum(Score, na.rm = TRUE)
    ),
    .groups = "drop"
  )


participant_task_checklist_wide <- participant_task_checklist_wide %>%
  left_join(
    participant_task_totals,
    by = c(
      "FTM" = "Current FTM", "Participant ID", "firstName", "lastName",
      "Participant Cohort", "IPA Age Band", "Task Stage"
    )
  ) %>%
  arrange(FTM, `Participant Cohort`, `Participant ID`)


# FTM-level monitoring table for quick workload and completion review.
ftm_task_summary <- participant_task_checklist_long %>%
  mutate(FTM = coalesce(FTM, "Unassigned")) %>%
  count(FTM, `Participant Cohort`, Task, Outcome, name = "Participants") %>%
  arrange(FTM, `Participant Cohort`, Task, Outcome)


dashboard_exports <- list(
  "task_checklist_long.csv" = participant_task_checklist_long,
  "task_checklist_wide.csv" = participant_task_checklist_wide,
  "ftm_task_summary.csv" = ftm_task_summary
)

walk2(
  dashboard_exports,
  names(dashboard_exports),
  ~ write_csv(.x, file.path(ipa_dashboard_latest_dir, .y), na = "")
)

walk2(
  dashboard_exports,
  names(dashboard_exports),
  ~ write_csv(.x, file.path(ipa_dashboard_snapshot_dir, .y), na = "")
)

dashboard_export_manifest <- tibble(
  generated_at = format(dashboard_run_time, "%Y-%m-%d %H:%M:%S %Z"),
  snapshot_id = dashboard_snapshot_id,
  snapshot_date = format(Sys.Date(), "%Y-%m-%d"),
  participant_rows = n_distinct(participant_task_checklist_long$`Participant ID`),
  task_rows = nrow(participant_task_checklist_long)
)

write_csv(
  dashboard_export_manifest,
  file.path(ipa_dashboard_latest_dir, "manifest.csv"),
  na = ""
)

write_csv(
  dashboard_export_manifest,
  file.path(ipa_dashboard_snapshot_dir, "manifest.csv"),
  na = ""
)
