ipa_assessment_types <- c("Detroit Assessment", "East Lansing Assessment",
  "ECHO In-Person Assessment", "ECHO Remote Spirometry", "Flint Assessment",
  "Grand Rapids Assessments", "Nicole - Traverse City", "Traverse City Assessment")

fetch_ripple_2026 <- function(auth, since = "2026-01-01") {
  message("Fetching combined Ripple participants/events/contacts/surveys: 6-35 month")
  infant <- fetch_ripple_study("REakqQKvCboEdX7BL", list(survey3="4PsVDDIam9qybK21J", survey4="ZE-phZDg2hDm7OziS"), auth, since)
  message("Fetching combined Ripple participants/events/contacts/surveys: 3-20 year")
  child <- fetch_ripple_study("QhZZT24wtWCpGkKdE", list(survey7="kFOE-iNu8LYL6_Qp1", survey8="QiQQS_3MQYq6iPaeS",
    survey9="mi63YrTXz1_DTBQnQ", survey10="NcIj8oFbEjY6PgaNA"), auth, since) %>%
    filter(!globalId %in% c("iri0LoVJXJ734mKYy", "54jpojzH4AVzTJoPD"))
  list(infant=infant, child=child)
}

# API acquisition: two combined Ripple requests and 2026 Calendly requests.
read_literal_config <- function(path, wanted) {
  result <- list()
  for (expr in parse(path)) {
    if (is.call(expr) && as.character(expr[[1]]) %in% c("<-", "=") &&
        is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% wanted &&
        is.character(expr[[3]]) && length(expr[[3]]) == 1L)
      result[[as.character(expr[[2]])]] <- expr[[3]]
  }
  result
}

fetch_ripple_study <- function(study_id, surveys, auth, since = "2026-01-01") {
  body <- list(access_token = "", teamId = "pze6EXgGw6hLwhhRy",
    `export-type` = study_id, `export-timezone` = "America/New_York",
    surveyExportSince = since)
  for (field in c("globalId", "customId", "familyId", "firstName", "lastName",
      "birthday", "tags", "statusId", "Participant Contacts", "Events (All or None)"))
    body[[field]] <- "on"
  body <- c(body, surveys)
  response <- request("https://echocharm.ripplescience.com/v1/export") %>%
    req_headers(Authorization = auth) %>% req_body_form(!!!body) %>%
    req_timeout(180) %>% req_retry(max_tries = 5, max_seconds = 300) %>% req_perform()
  value <- resp_body_string(response)
  if (!nzchar(trimws(value)) || grepl("^\\s*(<|\\{|\\[)", value))
    stop("Ripple returned a non-CSV response")
  data <- read_csv(I(value), col_types = cols(.default = col_character()), show_col_types = FALSE)
  required <- c("globalId", "customId", "familyId", "firstName", "lastName", "birthday", "statusId", "tags")
  if (!all(required %in% names(data)) || nrow(problems(data))) stop("Invalid Ripple export schema/CSV")
  data
}

build_participant_catalog <- function(ripple_data_6_35_month, ripple_data_3_20_year, crosswalk_path) {
participant_identity_fields <- c(
  "globalId", "customId", "familyId", "firstName", "lastName",
  "birthday", "statusId"
)

participant_id_catalog <- bind_rows(
    ripple_data_6_35_month %>%
      select(any_of(participant_identity_fields)) %>%
      mutate(cohort = "6-35 month"),
    ripple_data_3_20_year %>%
      select(any_of(participant_identity_fields)) %>%
      mutate(cohort = "3-20 year")
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


participant_contact_name_aliases <- bind_rows(
  extract_ripple_contact_name_aliases(ripple_data_6_35_month),
  extract_ripple_contact_name_aliases(ripple_data_3_20_year)
) %>%
  distinct()


charm_cohort_list <- read_excel(crosswalk_path, sheet = "Cohort_List", col_types = "text") %>%
  transmute(crosswalk_cohort = str_squish(cohort), crosswalk_family_id = str_squish(familyId),
    mom_echo_id = str_squish(mom_echo_id), mom_pin = str_squish(mom_pin),
    child_echo_id = str_squish(child_echo_id), child_pin = str_squish(child_pin),
    mom_id = str_squish(mom_id), child_id = str_squish(child_id)) %>%
  filter(!is.na(child_echo_id), child_echo_id != "") %>% distinct(child_echo_id, .keep_all = TRUE)
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
  distinct() %>%
  left_join(
    participant_contact_name_aliases,
    by = "child_echo_id",
    relationship = "many-to-many"
  )



calendly_participant_catalog
}

fetch_calendly_2026 <- function(calendly_token) {
base_url <- "https://api.calendly.com"
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
      req_timeout(120) %>% req_retry(max_tries = 5, max_seconds = 300) %>% req_perform()
    
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
  min_start_time = "2026-01-01T00:00:00-05:00",
  max_start_time = "2027-01-01T00:00:00-05:00"
)

message("Calendly events retrieved: ", length(events_raw))
# Invitee requests are needed only for the same assessment types retained by cleaning.
events_raw <- keep(events_raw, ~ trimws(.x$name) %in% ipa_assessment_types)
message("Assessment events to inspect: ", length(events_raw))


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
      req_timeout(120) %>% req_retry(max_tries = 5, max_seconds = 300) %>% req_perform()
    
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


calendly_raw <- imap(
  events_raw,
  function(event, index) {
    if (index %% 100L == 0L) message("Calendly assessment events read: ", index, "/", length(events_raw))
    
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



calendly_raw
}

clean_calendly_2026 <- function(calendly_raw, calendly_participant_catalog) {
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

          # A reschedule cancels the old invitee and creates a linked new one.
          # Keep the original flag and both links for counting actions/chains.
          `Rescheduled` =
            if (is.null(invitee$rescheduled)) NA else isTRUE(invitee$rescheduled),

          `Old Invitee URI` =
            if (is.null(invitee$old_invitee)) NA_character_ else invitee$old_invitee,

          `New Invitee URI` =
            if (is.null(invitee$new_invitee)) NA_character_ else invitee$new_invitee,
          
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


# Keep only assessment booking types (Event Type Name may carry trailing
# spaces in the source data, so it is trimmed before matching).
assessment_types <- ipa_assessment_types

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
  # Use appointment year in the local study timezone.
  mutate(`IPA Year` = year(`Event Start Date`)) %>%
  filter(`IPA Year` == 2026L)


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
    `Participant Match Method` = matched_by,
    `Participant Match QA` = participant_match_qa
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
      !is.na(`Participant Match QA`) ~ `Participant Match QA`,
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
  # Retain all resolved assessment events here. Outcome logic selects the
  # latest appointment, while assignment logic separately ranks direct and
  # Invitee-contact evidence before selecting an FTM.
  arrange(
    .participant_key,
    .age_band_key,
    `IPA Year`,
    desc(`Event Start Date & Time Parsed`),
    desc(`Event Created Date & Time Parsed`),
    desc(`Invitee UUID`)
  ) %>%
  select(
    `Event UUID`,
    `Invitee UUID`,
    `Event Type Name`,
    `Meeting Notes Plain`,
    `Meeting Host`,
    `Calendly FTM`,
    `Calendly FTM QA Flag`,
    `Invitee Name`,
    `Invitee Email`,
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
    `Participant Match QA`,
    `Participant Birthday`,
    `Calendly Record Status`,
    `Calendly QA Flag`,
    `Start Date & Time`,
    `End Date & Time`,
    `Event Start Date`,
    `Event Created Date & Time`,
    `Event Created Date`,
    `Canceled`,
    `Rescheduled`,
    `Old Invitee URI`,
    `New Invitee URI`,
    `No Show`,
    `No Show Created Date & Time`,
    `No Show Created Date`
  )




calendly_export_clean
}
