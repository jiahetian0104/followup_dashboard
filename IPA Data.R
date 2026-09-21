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

# The 3-20 call lists are no longer an input to IPA responsibility. Calendly
# owns IPA FTM assignment, and Ripple statusId owns the participant's current
# age band and task eligibility. Keep an empty compatibility table so the
# downstream roster code can retain the audit field without reading call-list
# workbooks or blocking a refresh when the shared drive is unavailable.
staff_assignment_main <- tibble(
  child_echo_id = character(),
  staff = character()
)



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
    token,
    min_start_time,
    max_start_time
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
          min_start_time = min_start_time,
          max_start_time = max_start_time,
          sort = "start_time:asc",
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
  token = calendly_token,
  min_start_time = "2025-01-01T00:00:00Z",
  max_start_time = "2027-01-01T00:00:00Z"
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


# Convert the Calendly host/inviter name into the stable short FTM labels used
# by the dashboard. The patterns cover the full Calendly display names as well
# as existing short labels, so one staff member is not split across two rows.
# An unrecognized host is retained as written for review.
ipa_ftm_name_patterns <- c(
  Amy = "\\bAmy\\b",
  Andrew = "\\bAndrew\\b",
  Anna = "\\bAnna\\b",
  Breanna = "\\bBreanna\\b",
  Cassie = "\\bCassie\\b",
  Jo = "\\b(?:Jo|Jolyn)\\b",
  Jody = "\\bJody\\b",
  Kowlini = "\\bKowlini\\b",
  Nicole = "\\bNicole\\b",
  Toye = "\\b(?:Toye|Shetoye)\\b"
)

normalize_calendly_ftm <- function(host_name) {
  if (is.na(host_name) || str_squish(host_name) == "") {
    return(NA_character_)
  }

  host_members <- str_split(host_name, ";", simplify = FALSE)[[1]] %>%
    str_squish() %>%
    discard(~ .x == "")

  normalized_members <- map_chr(host_members, function(member) {
    known_hits <- names(ipa_ftm_name_patterns)[
      map_lgl(
        ipa_ftm_name_patterns,
        ~ str_detect(member, regex(.x, ignore_case = TRUE))
      )
    ]

    if (length(known_hits) == 1) {
      known_hits[[1]]
    } else {
      member
    }
  })

  paste(unique(normalized_members), collapse = "; ") %>% na_if("")
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
    age_months >= 6 & age_months < 12 ~ "6_11_month",
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
  # which the booking happened. Keep 2025 as last-known FTM evidence while
  # 2026 remains the only year used to evaluate current IPA outcomes.
  mutate(`IPA Year` = year(`Event Start Date`)) %>%
  filter(`IPA Year` %in% c(2025L, 2026L))


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
    `Calendly FTM` = map_chr(`Meeting Host`, normalize_calendly_ftm),
    `Calendly FTM QA Flag` = case_when(
      is.na(`Calendly FTM`) ~ "Calendly host/inviter missing",
      str_detect(`Calendly FTM`, fixed(";")) ~
        "Multiple Calendly hosts; combined assignment retained",
      TRUE ~ NA_character_
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
  group_by(.participant_key, .age_band_key, `IPA Year`) %>%
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
    `Calendly FTM`,
    `Calendly FTM QA Flag`,
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
    `IPA Year`,
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



# 6. Final FTM Participant Task Checklist --------------------------------

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


# Tags are pipe-separated (e.g., "St. Joe|FTM Nicole"). These former roster
# assignments are retained for migration QC only; Calendly host is the
# authoritative IPA assignment below.
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


# One participant row from each Ripple cohort, with a common age-band key.
# The old Ripple/Call List FTM is retained temporarily and later renamed to
# Previous Roster FTM. Event columns are retained for task evaluation below.
participant_base_6_35 <- ripple_data_6_35_month %>%
  mutate(
    `Participant ID` = str_squish(str_remove(globalId, "\\s*\\([^)]*\\)$")),
    PIN = str_extract(globalId, "(?<=\\()[^)]*(?=\\)$)"),
    `Participant Cohort` = "6-35 month",
    `Age Group` = case_when(
      str_detect(statusId, regex("^6-11 Month", ignore_case = TRUE)) ~
        "6_11_month",
      str_detect(statusId, regex("^12-23 Month", ignore_case = TRUE)) ~
        "12_23_month",
      str_detect(statusId, regex("^24-35 Month", ignore_case = TRUE)) ~
        "24_35_month",
      str_detect(statusId, regex("^Potential Participants?$", ignore_case = TRUE)) ~
        "potential",
      TRUE ~ "needs_review"
    ),
    `IPA Age Band` = if_else(
      `Age Group` %in% c("12_23_month", "24_35_month"),
      `Age Group`,
      NA_character_
    ),
    `Task Eligible` = !is.na(`IPA Age Band`),
    `Eligibility Status` = case_when(
      `Task Eligible` ~ "Task eligible",
      `Age Group` == "6_11_month" ~ "6-11 months",
      `Age Group` == "potential" ~ "Potential participants",
      TRUE ~ "Needs review"
    ),
    FTM = map_chr(tags, extract_ftm_from_tags)
  ) %>%
  filter(!statusId %in% c("Withdrawn", "ECHO 2 Refusal")) %>%
  select(
    `Participant ID`, PIN, customId, familyId, firstName, lastName,
    birthday, statusId, `Participant Cohort`, `Age Group`, `IPA Age Band`,
    `Task Eligible`, `Eligibility Status`, FTM,
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
    `Age Group` = case_when(
      str_detect(statusId, regex("^3-5 Year", ignore_case = TRUE)) ~ "3_5yr",
      str_detect(statusId, regex("^6-10 Year", ignore_case = TRUE)) ~ "6_10yr",
      str_detect(statusId, regex("^11-17 Year", ignore_case = TRUE)) ~ "11_17yr",
      str_detect(statusId, regex("^18-20 Year", ignore_case = TRUE)) ~ "18_20yr",
      str_detect(statusId, regex("^Potential Participants?$", ignore_case = TRUE)) ~
        "potential",
      TRUE ~ "needs_review"
    ),
    `IPA Age Band` = if_else(
      `Age Group` %in% c("3_5yr", "6_10yr", "11_17yr", "18_20yr"),
      `Age Group`,
      NA_character_
    ),
    `Task Eligible` = !is.na(`IPA Age Band`),
    `Eligibility Status` = case_when(
      `Task Eligible` ~ "Task eligible",
      `Age Group` == "potential" ~ "Potential participants",
      TRUE ~ "Needs review"
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
    birthday, statusId, `Participant Cohort`, `Age Group`, `IPA Age Band`,
    `Task Eligible`, `Eligibility Status`, FTM,
    starts_with("event.")
  )


participant_roster <- bind_rows(
  participant_base_6_35,
  participant_base_3_20
) %>%
  rename(`Previous Roster FTM` = FTM) %>%
  distinct(`Participant ID`, `Participant Cohort`, .keep_all = TRUE)


# Calendly is already one row per participant, IPA age band, and appointment
# year. Current task outcomes use 2026 only. The 2025 records are retained
# separately as last-known FTM evidence and never affect 2026 outcomes.
calendly_ipa_latest <- calendly_export_clean %>%
  filter(
    `IPA Year` == 2026L,
    !is.na(`Participant ID`),
    !is.na(`IPA Age Band`)
  ) %>%
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
    Calendly_FTM = `Calendly FTM`,
    Calendly_Host_Raw = `Meeting Host`,
    Calendly_FTM_QA = `Calendly FTM QA Flag`,
    Calendly_Assignment_Date = `Event Created Date`,
    Calendly_Event_UUID = `Event UUID`,
    Calendly_No_Show = `No Show`,
    Calendly_Canceled = `Canceled`,
    Calendly_Status = `Calendly Record Status`,
    Calendly_Appointment_Date = `Event Start Date`,
    Calendly_No_Show_Date = `No Show Created Date`,
    Calendly_Match_Method = `Participant Match Method`,
    Calendly_Invitee_UUID = `Invitee UUID`
  )


# Build a year-specific assignment lookup. Explicit twin IDs have already been
# expanded upstream; mother/family IDs have already been restricted by the
# child's full name.
build_calendly_ftm_lookup <- function(
    calendly_data,
    ipa_year,
    require_age_band = TRUE
) {
  year_rows <- calendly_data %>%
    filter(
      `IPA Year` == ipa_year,
      !is.na(`Participant ID`)
    )

  if (require_age_band) {
    year_rows <- year_rows %>% filter(!is.na(`IPA Age Band`))
  }

  year_rows %>%
    arrange(
      `Participant ID`, `IPA Age Band`,
      desc(`Event Start Date`), desc(`Event Created Date`)
    ) %>%
    group_by(`Participant ID`, `IPA Age Band`) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      `Participant ID`,
      `IPA Age Band`,
      `Calendly Year` = ipa_year,
      FTM = `Calendly FTM`,
      `Calendly Host Raw` = `Meeting Host`,
      `Calendly Appointment Date` = `Event Start Date`,
      `Calendly Assignment Date` = `Event Created Date`,
      `Calendly Event UUID` = `Event UUID`,
      `Calendly Invitee UUID` = `Invitee UUID`,
      `Calendly Match Method` = `Participant Match Method`,
      `Calendly FTM QA Flag` = `Calendly FTM QA Flag`,
      `Assignment Source` = if_else(
        !is.na(FTM) & str_squish(FTM) != "",
        paste0("Calendly host - ", ipa_year),
        paste0("Calendly host unavailable - ", ipa_year)
      )
    ) %>%
    distinct(`Participant ID`, `IPA Age Band`, .keep_all = TRUE)
}


calendly_ftm_lookup_2026 <- build_calendly_ftm_lookup(
  calendly_export_clean,
  2026L,
  require_age_band = TRUE
)

calendly_ftm_lookup_2025 <- build_calendly_ftm_lookup(
  calendly_export_clean,
  2025L,
  require_age_band = FALSE
)

# Exportable evidence table at participant × appointment year × age-band
# grain. Assignment decisions below apply an explicit year and age priority.
calendly_ftm_lookup <- bind_rows(
  calendly_ftm_lookup_2026,
  calendly_ftm_lookup_2025
) %>%
  arrange(desc(`Calendly Year`), `Participant ID`, `IPA Age Band`)


# Latest usable 2025 Calendly FTM for each participant. This is a last-resort
# supplement after both 2026 same-age and 2026 previous-age searches fail.
calendly_ftm_2025_latest <- calendly_ftm_lookup_2025 %>%
  filter(!is.na(FTM), str_squish(FTM) != "") %>%
  arrange(
    `Participant ID`,
    desc(`Calendly Appointment Date`),
    desc(`Calendly Assignment Date`)
  ) %>%
  group_by(`Participant ID`) %>%
  slice(1) %>%
  ungroup() %>%
  rename_with(~ paste0("2025 ", .x), -`Participant ID`)


# Current-year Calendly lookup for outcome and assignment priority.
calendly_ftm_lookup_current <- calendly_ftm_lookup_2026 %>%
  distinct(`Participant ID`, `IPA Age Band`, .keep_all = TRUE)


# Rows that cannot safely enter the assignment lookup remain available for
# manual review instead of being matched to a participant or FTM by guesswork.
calendly_ftm_lookup_qc <- calendly_export_clean %>%
  filter(
    is.na(`Participant ID`) |
      is.na(`IPA Age Band`) |
      is.na(`Calendly FTM`) |
      str_squish(`Calendly FTM`) == ""
  ) %>%
  transmute(
    `Event UUID`,
    `Invitee UUID`,
    `Event Start Date`,
    `IPA Year`,
    `Participant ID`,
    `IPA Age Band`,
    `Meeting Host`,
    `Calendly FTM`,
    `Participant Match Method`,
    `Calendly QA Flag`,
    `Calendly FTM QA Flag`,
    `Meeting Notes Plain`,
    `Child Name`
  )


# Assignment preference: 2026 same age band, 2026 immediately preceding age
# band, then the participant's latest usable 2025 Calendly FTM. The 2025 level
# supplies responsibility only and never enters 2026 outcome evaluation.
ipa_previous_age_band_map <- tribble(
  ~`IPA Age Band`, ~`Previous IPA Age Band`,
  "12_23_month", "6_11_month",
  "24_35_month", "12_23_month",
  "3_5yr", "24_35_month",
  "6_10yr", "3_5yr",
  "11_17yr", "6_10yr",
  "18_20yr", "11_17yr"
)

calendly_ftm_exact <- calendly_ftm_lookup_current %>%
  rename_with(~ paste0("Exact ", .x), -c(`Participant ID`, `IPA Age Band`))

calendly_ftm_previous <- calendly_ftm_lookup_current %>%
  rename(
    `Previous IPA Age Band` = `IPA Age Band`
  ) %>%
  rename_with(
    ~ paste0("Previous ", .x),
    -c(`Participant ID`, `Previous IPA Age Band`)
  )

participant_calendly_assignment_lookup <- participant_roster %>%
  select(
    `Participant ID`, `Participant Cohort`, `IPA Age Band`, `Task Eligible`
  ) %>%
  left_join(ipa_previous_age_band_map, by = "IPA Age Band") %>%
  left_join(
    calendly_ftm_exact,
    by = c("Participant ID", "IPA Age Band")
  ) %>%
  left_join(
    calendly_ftm_previous,
    by = c("Participant ID", "Previous IPA Age Band")
  ) %>%
  left_join(
    calendly_ftm_2025_latest,
    by = "Participant ID"
  ) %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `IPA Age Band`,
    `Task Eligible`,
    FTM = coalesce(`Exact FTM`, `Previous FTM`, `2025 FTM`),
    `Calendly FTM Matched Age Band` = case_when(
      !is.na(`Exact FTM`) ~ `IPA Age Band`,
      !is.na(`Previous FTM`) ~ `Previous IPA Age Band`,
      !is.na(`2025 FTM`) ~ `2025 IPA Age Band`,
      TRUE ~ NA_character_
    ),
    `Calendly Assignment Year` = case_when(
      !is.na(`Exact FTM`) ~ 2026L,
      !is.na(`Previous FTM`) ~ 2026L,
      !is.na(`2025 FTM`) ~ 2025L,
      TRUE ~ NA_integer_
    ),
    `Calendly Assignment Rule` = case_when(
      !is.na(`Exact FTM`) ~ "2026 same age band",
      !is.na(`Previous FTM`) ~ "2026 previous age band fallback",
      !is.na(`2025 FTM`) ~ "2025 latest Calendly FTM fallback",
      TRUE ~ "Unmatched"
    ),
    `Calendly Host Raw` = coalesce(
      `Exact Calendly Host Raw`, `Previous Calendly Host Raw`,
      `2025 Calendly Host Raw`
    ),
    `Calendly Appointment Date` = coalesce(
      `Exact Calendly Appointment Date`, `Previous Calendly Appointment Date`,
      `2025 Calendly Appointment Date`
    ),
    `Calendly Assignment Date` = coalesce(
      `Exact Calendly Assignment Date`, `Previous Calendly Assignment Date`,
      `2025 Calendly Assignment Date`
    ),
    `Calendly Event UUID` = coalesce(
      `Exact Calendly Event UUID`, `Previous Calendly Event UUID`,
      `2025 Calendly Event UUID`
    ),
    `Calendly Invitee UUID` = coalesce(
      `Exact Calendly Invitee UUID`, `Previous Calendly Invitee UUID`,
      `2025 Calendly Invitee UUID`
    ),
    `Calendly Match Method` = coalesce(
      `Exact Calendly Match Method`, `Previous Calendly Match Method`,
      `2025 Calendly Match Method`
    ),
    `Calendly FTM QA Flag` = coalesce(
      `Exact Calendly FTM QA Flag`, `Previous Calendly FTM QA Flag`,
      `2025 Calendly FTM QA Flag`
    ),
    `Assignment Source` = case_when(
      !is.na(`Exact FTM`) ~ "Calendly host - 2026 same age band",
      !is.na(`Previous FTM`) ~
        "Calendly host - 2026 previous age band fallback",
      !is.na(`2025 FTM`) ~
        "Calendly host - 2025 latest FTM fallback",
      TRUE ~ "Calendly host unavailable"
    )
  ) %>%
  distinct(
    `Participant ID`, `Participant Cohort`, `IPA Age Band`, .keep_all = TRUE
  )


# The former Ripple/Call List FTM remains available only for QC. Dashboard FTM
# comes from the same-age Calendly host or the labeled previous-band fallback.
participant_roster <- participant_roster %>%
  left_join(
    participant_calendly_assignment_lookup %>%
      select(-`Task Eligible`),
    by = c("Participant ID", "Participant Cohort", "IPA Age Band")
  )


# The full roster retains 6-11 month and potential participants for optional
# dashboard review. Only task-eligible participants enter the task denominator.
participant_task_base <- participant_roster %>%
  filter(`Task Eligible`, !is.na(`IPA Age Band`))


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
          birthday, statusId, `IPA Age Band`,
          any_of(c(
            "Previous Roster FTM", "Assignment Source",
            "Calendly FTM Matched Age Band", "Calendly Assignment Year",
            "Calendly Assignment Rule", "Calendly Host Raw",
            "Calendly Appointment Date", "Calendly Assignment Date",
            "Calendly Event UUID", "Calendly Invitee UUID",
            "Calendly Match Method", "Calendly FTM QA Flag"
          ))
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
          `Previous Roster FTM`,
          firstName,
          lastName,
          birthday,
          statusId,
          `IPA Age Band`,
          `Assignment Source`,
          `Calendly FTM Matched Age Band`,
          `Calendly Assignment Year`,
          `Calendly Assignment Rule`,
          `Calendly Host Raw`,
          `Calendly Appointment Date`,
          `Calendly Assignment Date`,
          `Calendly Event UUID`,
          `Calendly Invitee UUID`,
          `Calendly Match Method`,
          `Calendly FTM QA Flag`,
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


# Preserve Calendly-based FTM assignment history and task responsibility.
# Existing Ripple/Call List history remains visible as legacy evidence, but a
# new Calendly interval is opened when the assignment source changes.
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

participant_roster_export <- participant_roster %>%
  transmute(
    FTM = normalize_dashboard_ftm(FTM),
    `Previous Roster FTM` = normalize_dashboard_ftm(`Previous Roster FTM`),
    `Participant ID`,
    PIN,
    firstName,
    lastName,
    birthday,
    `Participant Cohort`,
    statusId,
    `Age Group`,
    `IPA Age Band`,
    `Task Eligible`,
    `Eligibility Status`,
    `Calendly FTM Matched Age Band`,
    `Calendly Assignment Year`,
    `Calendly Assignment Rule`,
    `Calendly Host Raw`,
    `Calendly Appointment Date`,
    `Calendly Assignment Date`,
    `Calendly Event UUID`,
    `Calendly Invitee UUID`,
    `Calendly Match Method`,
    `Calendly FTM QA Flag`,
    `Assignment Source` = case_when(
      !is.na(`Assignment Source`) ~ `Assignment Source`,
      !`Task Eligible` ~ "Not applicable - no IPA age band",
      TRUE ~ "Calendly host unavailable"
    )
  ) %>%
  arrange(FTM, `Participant Cohort`, `Age Group`, `Participant ID`)


current_ftm_assignments <- participant_roster_export %>%
  filter(`Task Eligible`) %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `Age Group`,
    FTM,
    `Assignment Source`,
    `Calendly FTM Matched Age Band`,
    `Calendly Assignment Year`,
    `Calendly Assignment Rule`,
    `Calendly Host Raw`,
    `Calendly Appointment Date`,
    `Calendly Assignment Date`,
    `Calendly Event UUID`,
    `Calendly Invitee UUID`,
    `Calendly Match Method`,
    `Calendly FTM QA Flag`,
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

# Calendly is now the authoritative IPA assignment source. Close every still-
# open legacy Ripple/Call List interval at the methodology cutover, including
# potential participants that do not yet create a current IPA assignment row.
# This prevents the audit history from implying that a legacy owner is still
# active after the dashboard has switched to Calendly-based attribution.
legacy_open_index <- which(
  is.na(ftm_assignment_history$`Effective End`) &
    (
      is.na(ftm_assignment_history$`Assignment Source`) |
        !str_detect(
          ftm_assignment_history$`Assignment Source`,
          regex("^Calendly", ignore_case = TRUE)
        )
    )
)

if (length(legacy_open_index) > 0) {
  ftm_assignment_history$`Effective End`[legacy_open_index] <- Sys.Date() - 1
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
          `Age Band at Start` = `Age Group`,
          `Age Band at Last Observation` = `Age Group`,
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
  active_source <- ftm_assignment_history$`Assignment Source`[active_index]
  same_assignment <-
    identical(active_ftm, current$FTM[[1]]) &&
    identical(active_source, current$`Assignment Source`[[1]])

  if (same_assignment) {
    ftm_assignment_history$`Last Observed`[active_index] <- Sys.Date()
    ftm_assignment_history$`Age Band at Last Observation`[active_index] <-
      current$`Age Group`[[1]]
  } else if (ftm_assignment_history$`Effective Start`[active_index] == Sys.Date()) {
    # Multiple refreshes on the baseline day are treated as corrections.
    ftm_assignment_history$FTM[active_index] <- current$FTM[[1]]
    ftm_assignment_history$`Age Band at Start`[active_index] <-
      current$`Age Group`[[1]]
    ftm_assignment_history$`Age Band at Last Observation`[active_index] <-
      current$`Age Group`[[1]]
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
          `Age Band at Start` = `Age Group`,
          `Age Band at Last Observation` = `Age Group`,
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

# One-time source migration: responsibility previously initialized from Ripple
# or the 3-20 call list is replaced by the preferred Calendly host. The same
# age band is authoritative; only an immediately preceding age band can fill an
# otherwise unresolved assignment. When neither has a usable host, the task is
# explicitly Unassigned rather than silently retaining the old source.
calendly_ledger_assignments <- participant_calendly_assignment_lookup %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `IPA Age Band`,
    `Calendly Responsible FTM` = normalize_dashboard_ftm(FTM),
    `Calendly Assignment Source` = `Assignment Source`
  )

calendly_assignment_priority <- function(source) {
  source <- coalesce(as.character(source), "")
  case_when(
    str_detect(source, regex("2026 same age band|same age band", ignore_case = TRUE)) ~ 4L,
    str_detect(source, regex("2026 previous age band|previous age band fallback", ignore_case = TRUE)) ~ 3L,
    str_detect(source, regex("2025 latest", ignore_case = TRUE)) ~ 2L,
    str_detect(source, regex("unavailable", ignore_case = TRUE)) ~ 1L,
    TRUE ~ 0L
  )
}

task_responsibility_ledger <- task_responsibility_ledger %>%
  left_join(
    calendly_ledger_assignments,
    by = c("Participant ID", "Participant Cohort", "IPA Age Band")
  ) %>%
  mutate(
    .legacy_assignment_source =
      is.na(`Assignment Source`) |
      !str_detect(`Assignment Source`, regex("^Calendly", ignore_case = TRUE)),
    .fill_resolved_calendly_host =
      coalesce(`Responsible FTM`, "Unassigned") == "Unassigned" &
      !is.na(`Calendly Responsible FTM`) &
      `Calendly Responsible FTM` != "Unassigned",
    .upgrade_to_higher_priority =
      calendly_assignment_priority(`Calendly Assignment Source`) >
        calendly_assignment_priority(`Assignment Source`) &
      !is.na(`Calendly Responsible FTM`) &
      `Calendly Responsible FTM` != "Unassigned",
    .clarify_2026_source_label =
      calendly_assignment_priority(`Calendly Assignment Source`) ==
        calendly_assignment_priority(`Assignment Source`) &
      str_detect(
        coalesce(`Calendly Assignment Source`, ""),
        regex("Calendly host - 2026", ignore_case = TRUE)
      ) &
      !str_detect(
        coalesce(`Assignment Source`, ""),
        regex("Calendly host - 2026", ignore_case = TRUE)
      ) &
      coalesce(`Responsible FTM`, "Unassigned") ==
        coalesce(`Calendly Responsible FTM`, "Unassigned"),
    .update_from_calendly =
      .legacy_assignment_source | .fill_resolved_calendly_host |
      .upgrade_to_higher_priority | .clarify_2026_source_label,
    `Responsible FTM` = if_else(
      .update_from_calendly,
      coalesce(`Calendly Responsible FTM`, "Unassigned"),
      `Responsible FTM`
    ),
    `Assignment Source` = if_else(
      .update_from_calendly,
      if_else(
        !is.na(`Calendly Responsible FTM`) &
          `Calendly Responsible FTM` != "Unassigned",
        paste0(`Calendly Assignment Source`, " (source migration)"),
        "Calendly host unavailable (source migration)"
      ),
      `Assignment Source`
    )
  ) %>%
  select(
    -`Calendly Responsible FTM`, -`Calendly Assignment Source`,
    -.legacy_assignment_source,
    -.fill_resolved_calendly_host, -.upgrade_to_higher_priority,
    -.clarify_2026_source_label,
    -.update_from_calendly
  )

current_task_responsibilities <- participant_task_checklist_long %>%
  transmute(
    `Participant ID`,
    `Participant Cohort`,
    `IPA Age Band`,
    Task,
    `Current FTM` = normalize_dashboard_ftm(FTM),
    `Assignment Source` = coalesce(
      `Assignment Source`,
      "Calendly host unavailable"
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


# Operational QA exports are refreshed with the dashboard so previously
# generated review lists never become stale after assignment logic changes.
completed_participants_without_calendly_ftm <-
  participant_task_checklist_long %>%
  filter(`Responsible FTM` == "Unassigned", Outcome == "Complete") %>%
  group_by(
    `Participant ID`, firstName, lastName, birthday,
    `Participant Cohort`, statusId, `IPA Age Band`, `Task Stage`
  ) %>%
  summarise(
    `Completed Task Count` = n(),
    `Completed Tasks` = str_c(sort(unique(Task)), collapse = "; "),
    `Completion Dates` = str_c(
      sort(unique(na.omit(as.character(Outcome_Date)))), collapse = "; "
    ),
    `Task Sources` = str_c(sort(unique(Source)), collapse = "; "),
    `Calendly FTM Match Status` =
      paste(
        "No 2026 Calendly FTM in the same or immediately previous age band",
        "and no usable 2025 Calendly FTM"
      ),
    .groups = "drop"
  )

completed_unassigned_with_other_age_calendly_ftm <-
  completed_participants_without_calendly_ftm %>%
  select(
    `Participant ID`, firstName, lastName, `Participant Cohort`, statusId,
    `Current Unmatched Age Band` = `IPA Age Band`,
    `Completed Tasks`, `Completion Dates`
  ) %>%
  left_join(
    calendly_ftm_lookup %>%
      transmute(
        `Participant ID`,
        `Other Matched Age Band` = `IPA Age Band`,
        `Other Calendly Year` = `Calendly Year`,
        `Other Age Band FTM` = FTM,
        `Other Age Band Appointment Date` = `Calendly Appointment Date`,
        `Other Age Band Assignment Date` = `Calendly Assignment Date`,
        `Calendly Host Raw`,
        `Calendly Match Method`
      ),
    by = "Participant ID"
  ) %>%
  filter(
    !is.na(`Other Age Band FTM`),
    `Other Matched Age Band` != `Current Unmatched Age Band`
  )

calendly_previous_age_fallback_assignments <-
  participant_calendly_assignment_lookup %>%
  filter(
    `Calendly Assignment Rule` == "2026 previous age band fallback"
  ) %>%
  arrange(`Participant Cohort`, `IPA Age Band`, FTM, `Participant ID`)

calendly_2025_fallback_assignments <-
  participant_calendly_assignment_lookup %>%
  filter(
    `Calendly Assignment Rule` == "2025 latest Calendly FTM fallback"
  ) %>%
  arrange(`Participant Cohort`, `IPA Age Band`, FTM, `Participant ID`)


dashboard_exports <- list(
  "calendly_ftm_lookup.csv" = calendly_ftm_lookup,
  "participant_calendly_assignment_lookup.csv" =
    participant_calendly_assignment_lookup,
  "calendly_ftm_lookup_qc.csv" = calendly_ftm_lookup_qc,
  "participant_roster.csv" = participant_roster_export,
  "calendly_previous_age_fallback_assignments.csv" =
    calendly_previous_age_fallback_assignments,
  "calendly_2025_fallback_assignments.csv" =
    calendly_2025_fallback_assignments,
  "completed_participants_without_calendly_ftm.csv" =
    completed_participants_without_calendly_ftm,
  "completed_unassigned_with_other_age_calendly_ftm.csv" =
    completed_unassigned_with_other_age_calendly_ftm,
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
  participant_rows = nrow(participant_roster_export),
  task_eligible_participants = sum(participant_roster_export$`Task Eligible`),
  potential_participants = sum(
    participant_roster_export$`Eligibility Status` == "Potential participants"
  ),
  calendly_assignment_rows = nrow(calendly_ftm_lookup),
  calendly_2026_same_age_assignments = sum(
    participant_roster_export$`Calendly Assignment Rule` ==
      "2026 same age band",
    na.rm = TRUE
  ),
  calendly_2026_previous_age_fallbacks = sum(
    participant_roster_export$`Calendly Assignment Rule` ==
      "2026 previous age band fallback",
    na.rm = TRUE
  ),
  calendly_2025_fallbacks = sum(
    participant_roster_export$`Calendly Assignment Rule` ==
      "2025 latest Calendly FTM fallback",
    na.rm = TRUE
  ),
  calendly_assignment_qc_rows = nrow(calendly_ftm_lookup_qc),
  task_eligible_unassigned = sum(
    participant_roster_export$`Task Eligible` &
      participant_roster_export$FTM == "Unassigned"
  ),
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
