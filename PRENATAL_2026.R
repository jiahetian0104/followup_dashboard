# PRENATAL_2026
# Export Ripple prenatal data, follow-up extracts, and Power BI reporting tables.
# The Authorization value is configured below for the Ripple export request.

library(dplyr)
library(httr2)
library(readr)

# 1. Ripple data ----------------------------------------------------------

## 1.1. Set up parameters -------------------------------------------------
# Ripple export API and study settings.
base_url <- "https://echocharm.ripplescience.com/v1/export"
auth_key <- "Basic dGlhbmppYWhAbXN1LmVkdTpUamg2MTI0MjUyMDAwMDEwNCE="
team_id <- "pze6EXgGw6hLwhhRy"
timezone <- "America/New_York"
study_id <- "hsGq26e46PfHRofdJ" # Prenatal and 0-5 month study.

# Find the script's folder so both CSV files are saved beside this R file.
script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_argument) > 0) {
  script_path <- sub("^--file=", "", script_argument[[1]])
} else {
  source_paths <- lapply(sys.frames(), function(frame) frame$ofile)
  source_paths <- Filter(Negate(is.null), source_paths)
  script_path <- if (length(source_paths) > 0) tail(source_paths, 1)[[1]] else "PRENATAL_2026.R"
}
output_dir <- dirname(normalizePath(script_path, mustWork = TRUE))

# Preserve every field requested by the original prenatal export. The all-events
# option supplies both prenatal and 0-5 month event completion dates.
vars <- c(
  "statusId",
  "globalId",
  "customId",
  "familyId",
  "firstName",
  "lastName",
  "dateCreated",
  "race",
  "ethnicity",
  "birthday",
  "cv.last_menstral_period",
  "cv.estimated_due_date",
  "cv.consent_genetics",
  "cv.consent_birth_certificate",
  "cv.permission_for_placenta",
  "cv.permission_for_cord_blood",
  "dateSignedConsentForm",
  "Events (All or None)"
)

# Build the form body expected by Ripple; each requested field is marked "on".
body_list <- list(
  "access_token" = "",
  "teamId" = team_id,
  "export-type" = study_id,
  "export-timezone" = timezone,
  "surveyExportSince" = ""
)
for (v in vars) {
  body_list[[v]] <- "on"
}

## 1.2. Request and parse the study export -------------------------------
resp <- request(base_url) %>%
  req_headers(Authorization = auth_key) %>%
  req_body_form(!!!body_list) %>%
  # Retry temporary server and rate-limit errors.
  req_retry(max_tries = 5, max_seconds = 300) %>%
  req_perform()

# req_perform() raises an error for unsuccessful responses.
resp_status(resp)
csv_text <- resp_body_string(resp)
study_prenatal <- read_csv(
  I(csv_text),
  show_col_types = FALSE,
  guess_max = 2000,
  col_types = cols(.default = col_character())
)

# Normalize participant identifiers BEFORE selecting or converting event fields.
# globalId: ECHO identifier + Ripple PIN, e.g. CDX218-01-0 (277).
# customId: local CHARM identifier, e.g. 4180P1 or 4148F1.
# Correct reversed columns when both ID formats match.
# Keep raw values for audit; identifiers and PIN remain text (including leading zeros).
normalize_prenatal_ids <- function(data) {
  data$globalId_raw <- as.character(data$globalId)
  data$customId_raw <- as.character(data$customId)
  global <- toupper(trimws(data$globalId_raw))
  local <- toupper(trimws(data$customId_raw))
  global[!is.na(global) & global == ""] <- NA_character_
  local[!is.na(local) & local == ""] <- NA_character_
  pattern <- "^([A-Z]{3}[0-9]{3}-[0-9]{2}-[0-9]+)\\s*\\(\\s*([0-9]+)\\s*\\)$"
  # Ripple sometimes exports these two identifiers in the opposite columns.
  # Swap only when BOTH formats confirm the reversal; never infer from site alone.
  local_pattern <- "^[0-9]{4}[PF][0-9]+$"
  data$id_swapped <- !is.na(global) & !is.na(local) &
    grepl(local_pattern, global) & grepl(pattern, local, perl = TRUE)
  original_global <- global
  global[data$id_swapped] <- local[data$id_swapped]
  local[data$id_swapped] <- original_global[data$id_swapped]
  if (any(data$id_swapped)) {
    message("Corrected swapped globalId/customId for ", sum(data$id_swapped), " participants.")
  }
  # A local ID can also appear in globalId while customId is empty.
  # Recover the known local ID; the absent ECHO identifier cannot be inferred.
  data$id_moved_to_custom <- !is.na(global) & grepl(local_pattern, global) & is.na(local)
  local[data$id_moved_to_custom] <- global[data$id_moved_to_custom]
  global[data$id_moved_to_custom] <- NA_character_
  if (any(data$id_moved_to_custom)) {
    message("Moved local IDs into empty customId for ", sum(data$id_moved_to_custom), " participants.")
  }
  # Manual ID mapping confirmed by the user (2026-10-07).
  # Apply after repairing column placement, before extracting echo_id and PIN.
  global[!is.na(local) & local == "4218P1"] <- "CLF149-01-0 (146)"
  valid_global <- !is.na(global) & grepl(pattern, global, perl = TRUE)
  data$echo_id <- NA_character_
  data$PIN <- NA_character_
  data$echo_id[valid_global] <- sub(pattern, "\\1", global[valid_global], perl = TRUE)
  data$PIN[valid_global] <- sub(pattern, "\\2", global[valid_global], perl = TRUE)
  global[valid_global] <- paste0(data$echo_id[valid_global], " (", data$PIN[valid_global], ")")
  data$globalId <- global
  data$customId <- local
  if (any(!valid_global)) {
    warning(sum(!valid_global), " globalId values do not match echo_id (PIN); check raw IDs.")
  }
  valid_custom <- !is.na(local) & grepl(local_pattern, local)
  if (any(!valid_custom)) {
    warning(sum(!valid_custom), " customId values do not match a local ID such as 4180P1; check raw IDs.")
  }
  data
}
study_prenatal <- normalize_prenatal_ids(study_prenatal) %>%
  relocate(customId, .after = globalId)

# Used by prenatal dates, follow-up dates, birthdays and dateCreated.
# Retain missing dates; never silently turn an unrecognized nonblank date into NA.
parse_ripple_date <- function(value) {
  if (inherits(value, "Date")) return(value)
  if (inherits(value, "POSIXt")) return(as.Date(value, tz = timezone))
  value <- trimws(as.character(value))
  value[value == ""] <- NA_character_
  result <- as.Date(rep(NA_character_, length(value)))
  iso_date <- !is.na(value) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
  result[iso_date] <- as.Date(value[iso_date], format = "%Y-%m-%d")
  iso_time <- !is.na(value) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ]", value)
  if (any(iso_time)) {
    timestamp <- suppressWarnings(parse_datetime(value[iso_time], locale = locale(tz = timezone)))
    result[iso_time] <- as.Date(timestamp, tz = timezone)
  }
  us_date <- !is.na(value) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", value)
  result[us_date] <- as.Date(sub("^([0-9]{1,2}/[0-9]{1,2}/[0-9]{4}).*$", "\\1", value[us_date]), format = "%m/%d/%Y")
  if (any(!is.na(value) & is.na(result))) {
    stop("Unrecognized Ripple date values; check date formats before exporting.")
  }
  result
}

# Select prenatal milestones and give the exported event names shorter labels.
prenatal_2026 <- study_prenatal %>%
  select(
    globalId, customId, echo_id, PIN, globalId_raw, customId_raw, id_swapped, id_moved_to_custom, statusId,
    `event.consent_into_study.completedDate`,
    `event.dmc_enrollment_form.completedDate`,
    `event.estimated_due_date.completedDate`,
    `event.actigraphy_sleep_diary.completedDate`,
    `event.household_water.completedDate`,
    `event.first_visit_survey.completedDate`,
    `event.first_visit_anthropometrics.completedDate`,
    `event.cord_blood_collection.completedDate`,
    `event.first_prenatal_urine.completedDate`,
    `event.first_prenatal_blood.completedDate`,
    `event.second_visit_survey.completedDate`,
    `event.second_visit_anthropometrics.completedDate`,
    `event.second_prenatal_urine.completedDate`,
    `event.second_prenatal_blood.completedDate`,
    `event.child_is_born.completedDate`,
    `event.maternal_medical_record_abstraction.completedDate`,
    `event.date_of_pregnancy_loss.completedDate`
  ) %>%
  rename(
    consent_date = `event.consent_into_study.completedDate`,
    estduedate = `event.estimated_due_date.completedDate`,
    dmc_enroll = `event.dmc_enrollment_form.completedDate`,
    pn1_survey = `event.first_visit_survey.completedDate`,
    pn1_anthro = `event.first_visit_anthropometrics.completedDate`,
    pn1_urine = `event.first_prenatal_urine.completedDate`,
    pn1_blood = `event.first_prenatal_blood.completedDate`,
    actigraphy = `event.actigraphy_sleep_diary.completedDate`,
    pn2_survey = `event.second_visit_survey.completedDate`,
    pn2_anthro = `event.second_visit_anthropometrics.completedDate`,
    pn2_urine = `event.second_prenatal_urine.completedDate`,
    pn2_blood = `event.second_prenatal_blood.completedDate`,
    cordblood = `event.cord_blood_collection.completedDate`,
    mmra = `event.maternal_medical_record_abstraction.completedDate`,
    child_dob = `event.child_is_born.completedDate`,
    pregnancy_loss = `event.date_of_pregnancy_loss.completedDate`
  ) %>%
  # IDs remain text; all event completion dates use month/day/year in Ripple.
  mutate(across(-c(globalId, customId, echo_id, PIN, globalId_raw, customId_raw, id_swapped, id_moved_to_custom, statusId), ~ parse_ripple_date(.x)))

write_csv(prenatal_2026, file.path(output_dir, "prenatal_2026.csv"), na = "")

# Build the 0–5 month follow-up extract from the same prenatal export.
study_0to5 <- study_prenatal %>%
  select(
    globalId, customId, echo_id, PIN, globalId_raw, customId_raw, id_swapped, id_moved_to_custom, statusId, birthday, dateCreated,
    `event.0_5_month_visit_scheduled.completedDate`,
    `event.vib_request_submitted.completedDate`,
    `event.diaper_kit_sent.completedDate`,
    `event.0_5_month_survey.completedDate`,
    `event.child_anthropometrics.completedDate`,
    `event.maternal_anthropometrics.completedDate`,
    `event.postnatal_consent.completedDate`,
    `event.partner_contact_info_requested.completedDate`,
    `event.child_urine.completedDate`,
    `event.child_stool.completedDate`,
    `event.maternal_hair.completedDate`,
    `event.maternal_bloodspot.completedDate`,
    `event.breast_milk.completedDate`
  ) %>%
  rename(
    visit_scheduled = `event.0_5_month_visit_scheduled.completedDate`,
    vib_request = `event.vib_request_submitted.completedDate`,
    diaper_sent = `event.diaper_kit_sent.completedDate`,
    survey = `event.0_5_month_survey.completedDate`,
    child_anthro = `event.child_anthropometrics.completedDate`,
    mom_anthro = `event.maternal_anthropometrics.completedDate`,
    postnatal_consent = `event.postnatal_consent.completedDate`,
    partner_contact = `event.partner_contact_info_requested.completedDate`,
    child_urine = `event.child_urine.completedDate`,
    child_stool = `event.child_stool.completedDate`,
    mom_hair = `event.maternal_hair.completedDate`,
    mom_bs = `event.maternal_bloodspot.completedDate`,
    breast_milk = `event.breast_milk.completedDate`
  ) %>%
  # Parse event dates, birthday and dateCreated before eligibility/age calculations.
  mutate(across(-c(globalId, customId, echo_id, PIN, globalId_raw, customId_raw, id_swapped, id_moved_to_custom, statusId),
                ~ parse_ripple_date(.x)))

write_csv(study_0to5, file.path(output_dir, "study_0to5.csv"), na = "")


# 2. Power BI data preparation --------------------------------------------
# Reference: Prenatal_Report_2026.Rmd. Keep the two original extracts above.
# Actuals and monthly tables filter each event by its own date in report_year.
# The progress table keeps all participants/events, including missing dates.
report_year <- 2026L
report_as_of <- as.Date(format(Sys.time(), tz = timezone, format = "%Y-%m-%d"))

# Optional explicitly confirmed event-level exclusions.
# Prenatal 1/Actigraphy and Prenatal 2 year eligibility is calculated below.
# Example row structure: customId, event_code, reason (use codes from definitions).
not_eligible_events <- tibble(customId = character(), event_code = character(),
                             reason = character())

# A pure transformation function also allows offline validation without an API call.
build_prenatal_powerbi <- function(prenatal, followup, report_year, report_as_of,
                                  not_eligible_events = tibble(customId = character(),
                                    event_code = character(), reason = character())) {
  stopifnot(length(report_year) == 1L, !is.na(report_year),
            length(report_as_of) == 1L, inherits(report_as_of, "Date"),
            !is.na(report_as_of))
  prenatal <- prenatal %>% rename(water = `event.household_water.completedDate`)
  # Use a namespaced local ID as fallback when the ECHO ID is missing.
  # This key is for joins/counts only; it does not fabricate a globalId.
  add_participant_key <- function(tbl) {
    tbl %>% mutate(participant_key = case_when(
      !is.na(echo_id) & !is.na(PIN) ~ paste0("global:", globalId),
      !is.na(customId) & grepl("^[0-9]{4}[PF][0-9]+$", customId) ~ paste0("custom:", customId),
      TRUE ~ NA_character_
    ))
  }
  prenatal <- add_participant_key(prenatal)
  followup <- add_participant_key(followup)
  # Do not silently multiply counts or arbitrarily discard conflicting records.
  for (tbl in list(prenatal, followup)) {
    if (any(is.na(tbl$participant_key)) || anyDuplicated(tbl$participant_key)) {
      stop("Power BI requires one row per valid globalId or fallback customId in each extract.")
    }
  }
  if (!setequal(prenatal$participant_key, followup$participant_key)) {
    stop("Prenatal and follow-up extracts contain different participants.")
  }
  specimen_fields <- c("child_urine", "child_stool", "mom_hair", "mom_bs", "breast_milk")
  participants <- prenatal %>%
    left_join(select(followup, participant_key, all_of(specimen_fields)), by = "participant_key") %>%
    # Site prefixes belong to the local customId, not the ECHO globalId.
    mutate(site = case_when(
      substr(customId, 1, 2) %in% c("41", "42") ~ "Detroit",
      substr(customId, 1, 2) %in% c("44", "45") ~ "Flint",
      substr(customId, 1, 2) %in% c("47", "48") ~ "Traverse City",
      TRUE ~ "Unknown"
    ))
  if (any(participants$site == "Unknown")) {
    warning("Unmapped customId prefixes retained under site = Unknown.")
  }

  # One completion is one participant x event, NOT specimen tube/aliquot count.
  # Water follows the Rmd's biospecimen grouping. Cord blood is retained as a
  # data element, but excluded from is_biospecimen to match its displayed
  # biospecimen totals (the Rmd does not include cord blood in those totals).
  # Use is_prenatal_element for the screenshot's prenatal treemap.
  definitions <- tibble::tribble(
    ~event_code, ~event_label, ~stage, ~is_biospecimen, ~is_prenatal_element,
    "consent_date", "Enrollments", "Milestone", FALSE, FALSE,
    "child_dob", "Births", "Milestone", FALSE, FALSE,
    "pn1_survey", "Prenatal 1 Survey", "Prenatal", FALSE, TRUE,
    "pn1_anthro", "Prenatal 1 Anthro", "Prenatal", FALSE, TRUE,
    "pn1_urine", "Prenatal 1 Urine", "Prenatal", TRUE, TRUE,
    "pn1_blood", "Prenatal 1 Blood", "Prenatal", TRUE, TRUE,
    "pn2_survey", "Prenatal 2 Survey", "Prenatal", FALSE, TRUE,
    "pn2_anthro", "Prenatal 2 Anthro", "Prenatal", FALSE, TRUE,
    "pn2_urine", "Prenatal 2 Urine", "Prenatal", TRUE, TRUE,
    "pn2_blood", "Prenatal 2 Blood", "Prenatal", TRUE, TRUE,
    "actigraphy", "Actigraphy", "Prenatal", FALSE, TRUE,
    "water", "Water", "Prenatal", TRUE, TRUE,
    "cordblood", "Cord Blood", "Prenatal", FALSE, TRUE,
    "mmra", "Maternal Medical Record Abstraction", "Prenatal", FALSE, TRUE,
    "child_urine", "Child Urine", "0-5 Month", TRUE, FALSE,
    "child_stool", "Child Stool", "0-5 Month", TRUE, FALSE,
    "mom_hair", "Maternal Hair", "0-5 Month", TRUE, FALSE,
    "mom_bs", "Maternal Bloodspot", "0-5 Month", TRUE, FALSE,
    "breast_milk", "Breast Milk", "0-5 Month", TRUE, FALSE,
    "estduedate", "Projected Births", "Projection", FALSE, FALSE
  )
  terminal_statuses <- c("0-5 Month Complete", "0-5 Month Detroit",
                        "0-5 Month Traverse City", "0-5 Month Flint", "Pregnancy Loss")
  event_rows <- bind_rows(lapply(definitions$event_code, function(code) {
    participants %>%
      transmute(participant_key, globalId, customId, echo_id, PIN, globalId_raw, customId_raw, id_swapped, id_moved_to_custom, statusId, site,
                event_code = code, event_date = .data[[code]],
                child_dob, pregnancy_loss)
  })) %>%
    left_join(definitions, by = "event_code")

  # One row per participant x prenatal data element; keep missing dates and
  # dates outside report_year so filtering does not erase the progress denominator.
  # Eligibility rules confirmed by the user:
  # PN1 + Actigraphy: consent/enrollment year = report_year.
  # PN2: year of estimated due date minus 12 weeks (84 days) = report_year.
  # Water: enrolled in report_year and on/after implementation (2026-03-01).
  # Other element eligibility rules remain pending.
  if (anyDuplicated(not_eligible_events[c("customId", "event_code")]) ||
      any(is.na(not_eligible_events$customId) | not_eligible_events$customId == "") ||
      any(is.na(not_eligible_events$reason) | trimws(not_eligible_events$reason) == "") ||
      any(!not_eligible_events$event_code %in% definitions$event_code[definitions$is_prenatal_element]) ||
      any(!not_eligible_events$customId %in% participants$customId)) {
    stop("Not Eligible entries must have a unique participant/event pair and a reason.")
  }
  progress <- event_rows %>%
    filter(is_prenatal_element) %>%
    left_join(select(participants, participant_key, consent_date, estduedate), by = "participant_key") %>%
    left_join(not_eligible_events %>% mutate(not_eligible = TRUE),
              by = c("customId", "event_code"), na_matches = "never")
  if (any(!is.na(progress$event_date) & !is.na(progress$not_eligible))) {
    stop("A completed event is also marked Not Eligible; resolve the conflicting evidence.")
  }
  progress <- progress %>%
    mutate(enrollment_year = as.integer(format(consent_date, "%Y")),
           prenatal2_start_date = estduedate - 12L * 7L,
           prenatal2_start_year = as.integer(format(prenatal2_start_date, "%Y")),
           `Eligibility Date` = case_when(
             event_code %in% c("pn1_survey", "pn1_anthro", "pn1_urine", "pn1_blood", "actigraphy") ~ consent_date,
             event_code %in% c("pn2_survey", "pn2_anthro", "pn2_urine", "pn2_blood") ~ prenatal2_start_date,
             event_code == "water" ~ consent_date,
             TRUE ~ as.Date(NA_character_)
           ),
           Eligible = case_when(
             !is.na(not_eligible) ~ FALSE,
             event_code %in% c("pn1_survey", "pn1_anthro", "pn1_urine", "pn1_blood", "actigraphy") ~ enrollment_year == report_year,
             event_code %in% c("pn2_survey", "pn2_anthro", "pn2_urine", "pn2_blood") ~ prenatal2_start_year == report_year,
             event_code == "water" ~ enrollment_year == report_year & consent_date >= as.Date("2026-03-01"),
             TRUE ~ NA
           ),
           `Data Element Name` = event_label,
           `Complete Status` = case_when(
             !is.na(Eligible) & !Eligible ~ "Not Eligible",
             !is.na(event_date) ~ "Complete",
             TRUE ~ "Incomplete"
           ),
           `Complete Date` = event_date,
           `Has Completion Record` = !is.na(event_date),
           `Eligibility Basis` = case_when(
             !is.na(not_eligible) ~ reason,
             event_code %in% c("pn1_survey", "pn1_anthro", "pn1_urine", "pn1_blood", "actigraphy") & is.na(consent_date) ~ "Missing enrollment date",
             event_code %in% c("pn1_survey", "pn1_anthro", "pn1_urine", "pn1_blood", "actigraphy") ~ paste0("Enrollment year must equal ", report_year),
             event_code %in% c("pn2_survey", "pn2_anthro", "pn2_urine", "pn2_blood") & is.na(estduedate) ~ "Missing estimated due date",
             event_code %in% c("pn2_survey", "pn2_anthro", "pn2_urine", "pn2_blood") ~ paste0("Year of estimated due date minus 84 days must equal ", report_year),
             event_code == "water" & is.na(consent_date) ~ "Missing enrollment date",
             event_code == "water" ~ paste0("Enrollment year must equal ", report_year, "; enrolled on or after 2026-03-01"),
             TRUE ~ "Eligibility rule pending"
           ),
           `Eligibility Confirmed` = !is.na(Eligible),
           completion_year = as.integer(format(event_date, "%Y")),
           birth_year = as.integer(format(child_dob, "%Y")),
           edd_year = as.integer(format(estduedate, "%Y")),
           edd_month_number = as.integer(format(estduedate, "%m")),
           edd_month_name = month.name[edd_month_number],
           edd_month_start = as.Date(format(estduedate, "%Y-%m-01")),
           projected_birth = !is.na(estduedate) &
             coalesce(estduedate >= report_as_of, FALSE) &
             is.na(child_dob) & is.na(pregnancy_loss) &
             !statusId %in% terminal_statuses,
           report_year = report_year,
           report_as_of = report_as_of) %>%
    select(participant_key, globalId, customId, echo_id, PIN, site, statusId,
           event_code, `Data Element Name`, `Complete Status`, `Complete Date`,
           Eligible, `Eligibility Confirmed`, `Eligibility Date`, `Eligibility Basis`,
           `Has Completion Record`, consent_date, child_dob, pregnancy_loss, estduedate, prenatal2_start_date,
           enrollment_year, birth_year, prenatal2_start_year, completion_year,
           projected_birth, edd_year, edd_month_number, edd_month_name, edd_month_start,
           is_biospecimen, report_year, report_as_of) %>%
    arrange(site, customId, event_code)
  stopifnot(nrow(progress) == nrow(participants) * sum(definitions$is_prenatal_element))

  # Event-level comparison: count distinct participants within each event.
  # All Sites rows are totals; do not sum them together with individual sites.
  event_summary <- bind_rows(progress, progress %>% mutate(site = "All Sites")) %>%
    group_by(site, event_code, `Data Element Name`) %>%
    summarise(
      `Participant Count` = n_distinct(participant_key),
      `Completed in Report Year` = sum(`Has Completion Record` & completion_year == report_year, na.rm = TRUE),
      `Eligible Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible, na.rm = TRUE),
      `Eligible Complete Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & `Has Completion Record`, na.rm = TRUE),
      `Eligible Complete in Report Year` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & `Has Completion Record` & completion_year == report_year, na.rm = TRUE),
      `Eligible Incomplete Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & !`Has Completion Record`, na.rm = TRUE),
      `Not Eligible Count` = sum(!Eligible, na.rm = TRUE),
      `Eligibility Unknown Count` = sum(is.na(Eligible)),
      .groups = "drop"
    ) %>%
    mutate(`Completion Rate` = if_else(!is.na(`Eligible Count`) & `Eligible Count` > 0L,
                                     `Eligible Complete Count` / `Eligible Count`, NA_real_),
           report_year = report_year, report_as_of = report_as_of) %>%
    arrange(site, event_code)
  stopifnot(all(event_summary$`Eligible Complete Count` + event_summary$`Eligible Incomplete Count` ==
                event_summary$`Eligible Count`, na.rm = TRUE))

  event_rows <- event_rows %>%
    filter(!is.na(event_date), format(event_date, "%Y") == as.character(report_year))

  # Projection is a CURRENT pending-birth snapshot, not a historical forecast.
  # Same terminal status exclusions as the Rmd's pending-DOB QA section.
  # Missing status is retained unless an actual birth/loss date rules it out.
  events <- event_rows %>%
    filter(event_code != "estduedate" |
             (is.na(child_dob) & is.na(pregnancy_loss) &
                !statusId %in% terminal_statuses & event_date >= report_as_of)) %>%
    select(-child_dob, -pregnancy_loss) %>%
    mutate(year = report_year,
           month_number = as.integer(format(event_date, "%m")),
           month_name = month.name[month_number],
           month_start = as.Date(format(event_date, "%Y-%m-01")),
           event_count = 1L,
           report_as_of = report_as_of) %>%
    arrange(event_date, site, globalId, event_code)
  # Preserve the Rmd's full-year actuals, including any future completion dates;
  # warn instead of silently changing its definition.
  if (any(events$event_code != "estduedate" & events$event_date > report_as_of)) {
    warning("Future completion dates found in Power BI actuals; review event dates.")
  }

  # Full 12-month grid ensures a true zero is exported rather than a missing row.
  # Unknown is always present so totals never silently omit unmapped participants.
  monthly <- expand.grid(
    site = c("Detroit", "Flint", "Traverse City", "Unknown"),
    month_number = 1:12,
    event_code = definitions$event_code,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    left_join(count(events, site, month_number, event_code, name = "event_count"),
              by = c("site", "month_number", "event_code")) %>%
    left_join(definitions, by = "event_code") %>%
    mutate(event_count = coalesce(event_count, 0L),
           year = report_year,
           month_name = month.name[month_number],
           month_start = as.Date(sprintf("%04d-%02d-01", report_year, month_number)),
           report_as_of = report_as_of) %>%
    arrange(month_number, site, event_code)
  stopifnot(sum(monthly$event_count) == nrow(events))
  list(events = events, monthly = monthly, definitions = definitions, progress = progress, event_summary = event_summary)
}

powerbi <- build_prenatal_powerbi(prenatal_2026, study_0to5,
                                  report_year, report_as_of, not_eligible_events)
# Write each run into its own snapshot. Publish latest only after all exports succeed.
powerbi_root <- file.path(output_dir, "powerbi")
latest_dir <- file.path(powerbi_root, "latest")
snapshot_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%OS6Z", tz = "UTC"), "_", Sys.getpid())
powerbi_dir <- file.path(powerbi_root, "snapshots", snapshot_id)
if (dir.exists(powerbi_dir)) stop("Snapshot already exists: ", powerbi_dir)
dir.create(powerbi_dir, showWarnings = FALSE, recursive = TRUE)
write_csv(powerbi$events, file.path(powerbi_dir, "prenatal_events.csv"), na = "")
write_csv(powerbi$monthly, file.path(powerbi_dir, "prenatal_monthly.csv"), na = "")
write_csv(powerbi$definitions, file.path(powerbi_dir, "event_definitions.csv"), na = "")
write_csv(powerbi$progress, file.path(powerbi_dir, "prenatal_progress.csv"), na = "")

# Enrollment summary from the long table: one count per participant, not per event.
prenatal_enrollment_summary <- powerbi$progress %>%
  filter(enrollment_year == report_year) %>%
  distinct(site, participant_key) %>%
  count(site, name = "Enrollment Count") %>%
  right_join(tibble(site = c("Detroit", "Flint", "Traverse City", "Unknown")),
             by = "site") %>%
  mutate(`Enrollment Count` = coalesce(`Enrollment Count`, 0L),
         report_year = report_year, report_as_of = report_as_of) %>%
  arrange(desc(`Enrollment Count`), site)
write_csv(prenatal_enrollment_summary,
          file.path(powerbi_dir, "prenatal_enrollment_summary.csv"), na = "")

# Actual births use their own year, regardless of enrollment or event eligibility.
prenatal_birth_summary <- powerbi$progress %>%
  filter(birth_year == report_year) %>%
  distinct(site, participant_key) %>%
  count(site, name = "Birth Count") %>%
  right_join(tibble(site = c("Detroit", "Flint", "Traverse City", "Unknown")),
             by = "site") %>%
  mutate(`Birth Count` = coalesce(`Birth Count`, 0L),
         report_year = report_year, report_as_of = report_as_of) %>%
  arrange(desc(`Birth Count`), site)
write_csv(prenatal_birth_summary,
          file.path(powerbi_dir, "prenatal_birth_summary.csv"), na = "")

# Current pending births by estimated due month; deduplicate the long event rows.
prenatal_projected_birth_summary <- powerbi$progress %>%
  filter(projected_birth, edd_year == report_year) %>%
  distinct(site, participant_key, edd_month_number) %>%
  count(site, edd_month_number, name = "Projected Birth Count") %>%
  right_join(expand.grid(site = c("Detroit", "Flint", "Traverse City", "Unknown"),
                        edd_month_number = 1:12, stringsAsFactors = FALSE) %>% as_tibble(),
             by = c("site", "edd_month_number")) %>%
  mutate(`Projected Birth Count` = coalesce(`Projected Birth Count`, 0L),
         edd_month_name = month.name[edd_month_number],
         edd_month_start = as.Date(sprintf("%04d-%02d-01", report_year, edd_month_number)),
         report_year = report_year, report_as_of = report_as_of) %>%
  arrange(edd_month_number, site)
write_csv(prenatal_projected_birth_summary,
          file.path(powerbi_dir, "prenatal_projected_birth_summary.csv"), na = "")



write_csv(powerbi$event_summary, file.path(powerbi_dir, "prenatal_event_summary.csv"), na = "")
print(powerbi$event_summary %>% filter(site == "All Sites"), width = Inf)

# Progress visuals: import prenatal_progress.csv (participant x prenatal element).
# - Slicers: site, Data Element Name, Complete Status; Complete Date is a Date.
# - Progress numerator: COUNTROWS with Eligible = TRUE and Complete Status = Complete.
# - Actual completion counts regardless of report-year eligibility: filter
#   Has Completion Record = TRUE, or use the existing events/monthly tables.
# - Completion activity in 2026: filter completion_year = 2026.
# - Enrollment cohort in 2026: filter enrollment_year = 2026 instead.
#   Enrollment bars must DISTINCTCOUNT(participant_key), since each participant
#   appears once per data element in the progress table.
# - A completion-date filter removes undated Incomplete/Not Eligible rows;
#   use participant/cohort filters when comparing status counts or progress.
# - Completion rate: within Eligible = TRUE, completed rows / all rows.
#   Unknown eligibility (blank Eligible) and Not Eligible are excluded.
# - PN2 eligibility year is based on EDD minus 84 days, NOT its completion date.
# - Missing enrollment/EDD dates and other pending rules leave Eligible
#   blank. Their Complete/Incomplete status only describes recorded completion.
# - Not Eligible refers to this reporting year; any actual completion date is
#   retained, even when the record is outside the report-year eligibility cohort.
# - Progress is a current snapshot; it does not reconstruct historical status.

# Power BI quick start (monthly supplies the existing count visuals):
# - Import prenatal_monthly.csv; set month_start/report_as_of to Date,
#   month_number/year/event_count to Whole Number, flags to True/False.
# - Site slicer: site. Sort month_name by month_number.
# - Values for every visual: SUM(event_count), never COUNTROWS(monthly).
# - Enrollment/birth charts: event_code = consent_date / child_dob.
# - Monthly cards: also filter month_number to the desired reporting month.
# - Biospecimens card: is_biospecimen = TRUE (prenatal + 0-5 month, as in Rmd).
# - Treemap: event_label; filter is_prenatal_element = TRUE.
# - Projected births: event_code = estduedate; axis month_name, legend site.
# - Detail/drill-through: prenatal_events.csv has one row per participant/event.
#   These are alternative grains: do not append detail rows to monthly totals.
# - No enrollment-year cohort restriction is applied to completions or births.
# - pn2_blood uses its own date; the Rmd biospecimen section accidentally copies
#   pn1_blood into pn2_blood. That typo is intentionally NOT reproduced here.
message("Power BI tables saved to: ", powerbi_dir)

# 3. 0-5 month Power BI tables --------------------------------------------
# Population: F-category customId records only (confirmed by the user).
# Eligibility: dateCreated in report_year, independent of event completion year.
# Aging out: completed calendar age 4-5 months; DOB event preferred, then birthday.
month0to5_definitions <- tibble::tribble(
  ~event_code, ~event_label, ~is_biospecimen,
  "postnatal_consent", "Postnatal Consent", FALSE,
  "survey", "Survey", FALSE,
  "child_anthro", "Child Anthro", FALSE,
  "mom_anthro", "Mom Anthro", FALSE,
  "visit_scheduled", "Visit Scheduled", FALSE,
  "vib_request", "VIB Request", FALSE,
  "diaper_sent", "Diaper Sent", FALSE,
  "partner_contact", "Partner Contact", FALSE,
  "child_urine", "Child Urine", TRUE,
  "child_stool", "Child Stool", TRUE,
  "mom_hair", "Mom Hair", TRUE,
  "mom_bs", "Mom Bloodspot", TRUE,
  "breast_milk", "Breast Milk", TRUE
)
month0to5_participants <- study_0to5 %>%
  filter(!is.na(customId), grepl("^[0-9]{4}F[0-9]+$", customId)) %>%
  mutate(participant_key = case_when(
    !is.na(echo_id) & !is.na(PIN) ~ paste0("global:", globalId),
    !is.na(customId) & grepl("^[0-9]{4}[PF][0-9]+$", customId) ~ paste0("custom:", customId),
    TRUE ~ NA_character_
  )) %>%
  left_join(powerbi$progress %>% distinct(participant_key, site, child_dob), by = "participant_key") %>%
  mutate(birth_date = coalesce(child_dob, birthday),
         birth_date_source = case_when(!is.na(child_dob) ~ "child_dob",
                                       !is.na(birthday) ~ "birthday", TRUE ~ NA_character_),
         creation_year = as.integer(format(dateCreated, "%Y")),
         Eligible = creation_year == report_year,
         # Add six calendar months with month-end clipping (e.g. Aug 31 -> Feb 28).
         age_out_year = as.integer(format(birth_date, "%Y")) +
           (as.integer(format(birth_date, "%m")) + 5L) %/% 12L,
         age_out_month = (as.integer(format(birth_date, "%m")) + 5L) %% 12L + 1L,
         age_out_month_start = as.Date(if_else(is.na(birth_date), NA_character_,
           sprintf("%04d-%02d-01", age_out_year, age_out_month))),
         age_out_next_start = as.Date(if_else(is.na(birth_date), NA_character_,
           sprintf("%04d-%02d-01", age_out_year + age_out_month %/% 12L,
                   age_out_month %% 12L + 1L))),
         age_out_date = age_out_month_start +
           pmin(as.integer(format(birth_date, "%d")), as.integer(age_out_next_start - age_out_month_start)) - 1L,
         current_month_start = as.Date(format(report_as_of, "%Y-%m-01")),
         next_month_start = as.Date(sprintf("%04d-%02d-01",
           as.integer(format(report_as_of, "%Y")) + as.integer(format(report_as_of, "%m")) %/% 12L,
           as.integer(format(report_as_of, "%m")) %% 12L + 1L)),
         age_months = if_else(is.na(birth_date) | birth_date > report_as_of, NA_integer_,
           (as.integer(format(report_as_of, "%Y")) - as.integer(format(birth_date, "%Y"))) * 12L +
             as.integer(format(report_as_of, "%m")) - as.integer(format(birth_date, "%m")) -
             as.integer(as.integer(format(report_as_of, "%d")) <
               pmin(as.integer(format(birth_date, "%d")), as.integer(next_month_start - current_month_start)))),
         days_to_age_out = as.integer(age_out_date - report_as_of),
         aging_out = !is.na(Eligible) & Eligible & !is.na(age_months) & age_months >= 4L & age_months < 6L,
         survey_complete = !is.na(survey),
         visit_scheduled_complete = !is.na(visit_scheduled),
         report_year = report_year, report_as_of = report_as_of) %>%
  select(participant_key, globalId, customId, echo_id, PIN, site, statusId,
         dateCreated, creation_year, Eligible, child_dob, birthday, birth_date,
         birth_date_source, age_months, age_out_date, days_to_age_out, aging_out,
         survey_complete, visit_scheduled_complete, all_of(month0to5_definitions$event_code),
         report_year, report_as_of)
if (anyNA(month0to5_participants$participant_key) || anyDuplicated(month0to5_participants$participant_key) ||
    anyNA(month0to5_participants$site)) {
  stop("0-5 month roster keys/sites must match the prenatal roster uniquely.")
}
month0to5_progress <- bind_rows(lapply(month0to5_definitions$event_code, function(code) {
  month0to5_participants %>%
    select(participant_key, globalId, customId, site, statusId, dateCreated,
           creation_year, Eligible, birth_date, age_months, age_out_date, aging_out,
           report_year, report_as_of) %>%
    mutate(event_code = code, `Complete Date` = month0to5_participants[[code]])
})) %>%
  left_join(month0to5_definitions, by = "event_code") %>%
  mutate(`Data Element Name` = event_label,
         `Has Completion Record` = !is.na(`Complete Date`),
         `Complete Status` = case_when(!is.na(Eligible) & !Eligible ~ "Not Eligible",
                                      `Has Completion Record` ~ "Complete", TRUE ~ "Incomplete"),
         `Eligibility Date` = dateCreated,
         `Eligibility Confirmed` = !is.na(Eligible),
         `Eligibility Basis` = if_else(is.na(dateCreated), "Missing dateCreated",
           paste0("dateCreated year must equal ", report_year)),
         completion_year = as.integer(format(`Complete Date`, "%Y")),
         completion_month_number = as.integer(format(`Complete Date`, "%m")),
         completion_month_name = month.name[completion_month_number],
         completion_month_start = as.Date(format(`Complete Date`, "%Y-%m-01"))) %>%
  select(-event_label) %>% arrange(site, customId, event_code)
stopifnot(nrow(month0to5_progress) == nrow(month0to5_participants) * nrow(month0to5_definitions),
          !anyDuplicated(month0to5_progress[c("participant_key", "event_code")]))
month0to5_event_summary <- bind_rows(month0to5_progress,
                                   month0to5_progress %>% mutate(site = "All Sites")) %>%
  group_by(site, event_code, `Data Element Name`) %>%
  summarise(`Completed in Report Year` = sum(completion_year == report_year, na.rm = TRUE),
            `Eligible Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible, na.rm = TRUE),
            `Eligible Complete Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & `Has Completion Record`, na.rm = TRUE),
            `Eligible Complete in Report Year` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & completion_year == report_year, na.rm = TRUE),
            `Eligible Incomplete Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible & !`Has Completion Record`, na.rm = TRUE),
            `Not Eligible Count` = sum(!Eligible, na.rm = TRUE),
            `Eligibility Unknown Count` = sum(is.na(Eligible)), .groups = "drop") %>%
  mutate(`Completion Rate` = if_else(!is.na(`Eligible Count`) & `Eligible Count` > 0L,
                                    `Eligible Complete Count` / `Eligible Count`, NA_real_),
         report_year = report_year, report_as_of = report_as_of)
month0to5_monthly <- month0to5_progress %>%
  filter(completion_year == report_year) %>%
  count(site, event_code, completion_month_number, name = "event_count") %>%
  right_join(expand.grid(site = c("Detroit", "Flint", "Traverse City", "Unknown"),
                        event_code = month0to5_definitions$event_code,
                        completion_month_number = 1:12, stringsAsFactors = FALSE) %>% as_tibble(),
             by = c("site", "event_code", "completion_month_number")) %>%
  left_join(month0to5_definitions, by = "event_code") %>%
  mutate(event_count = coalesce(event_count, 0L),
         completion_month_name = month.name[completion_month_number],
         completion_month_start = as.Date(sprintf("%04d-%02d-01", report_year, completion_month_number)),
         report_year = report_year, report_as_of = report_as_of)
month0to5_workflow <- bind_rows(month0to5_participants,
                              month0to5_participants %>% mutate(site = "All Sites")) %>%
  group_by(site) %>%
  summarise(`Eligible Count` = if (all(is.na(Eligible))) NA_integer_ else sum(Eligible, na.rm = TRUE),
            `Surveys Completed` = sum(format(survey, "%Y") == as.character(report_year), na.rm = TRUE),
            `Visits Scheduled` = sum(format(visit_scheduled, "%Y") == as.character(report_year), na.rm = TRUE),
            `Diapers Sent` = sum(format(diaper_sent, "%Y") == as.character(report_year), na.rm = TRUE),
            `VIB Requests` = sum(format(vib_request, "%Y") == as.character(report_year), na.rm = TRUE),
            `Linked Surveys` = `Surveys Completed`,
            `Linked Visits` = sum(format(survey, "%Y") == as.character(report_year) &
              format(visit_scheduled, "%Y") == as.character(report_year), na.rm = TRUE),
            `Linked Diapers` = sum(format(survey, "%Y") == as.character(report_year) &
              format(visit_scheduled, "%Y") == as.character(report_year) &
              format(diaper_sent, "%Y") == as.character(report_year), na.rm = TRUE), .groups = "drop") %>%
  mutate(report_year = report_year, report_as_of = report_as_of)
month0to5_aging_out <- month0to5_participants %>% filter(aging_out) %>%
  select(participant_key, globalId, customId, site, dateCreated, birth_date,
         birth_date_source, age_months, age_out_date, days_to_age_out,
         survey_complete, visit_scheduled_complete, report_year, report_as_of) %>%
  arrange(age_out_date, site, customId)
stopifnot(sum(month0to5_monthly$event_count) == sum(month0to5_progress$completion_year == report_year, na.rm = TRUE))
write_csv(month0to5_participants, file.path(powerbi_dir, "month0to5_participants.csv"), na = "")
write_csv(month0to5_progress, file.path(powerbi_dir, "month0to5_progress.csv"), na = "")
write_csv(month0to5_definitions, file.path(powerbi_dir, "month0to5_event_definitions.csv"), na = "")
write_csv(month0to5_event_summary, file.path(powerbi_dir, "month0to5_event_summary.csv"), na = "")
write_csv(month0to5_monthly, file.path(powerbi_dir, "month0to5_monthly.csv"), na = "")
write_csv(month0to5_workflow, file.path(powerbi_dir, "month0to5_workflow.csv"), na = "")
write_csv(month0to5_aging_out, file.path(powerbi_dir, "month0to5_aging_out.csv"), na = "")
message("0-5 month Power BI tables saved to: ", powerbi_dir)

# Publish the complete snapshot as latest; historical snapshots are never overwritten.
write_csv(tibble(snapshot_id = basename(powerbi_dir), report_year = report_year,
                 report_as_of = report_as_of,
                 exported_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
          file.path(powerbi_dir, "snapshot_info.csv"), na = "")
latest_stage <- tempfile(".latest-stage-", tmpdir = dirname(latest_dir))
dir.create(latest_stage)
snapshot_files <- list.files(powerbi_dir, full.names = TRUE)
if (!all(file.copy(snapshot_files, latest_stage))) stop("Could not stage latest exports")
latest_backup <- tempfile(".latest-backup-", tmpdir = dirname(latest_dir))
had_latest <- dir.exists(latest_dir)
if (had_latest && !file.rename(latest_dir, latest_backup)) stop("Could not preserve previous latest")
if (!file.rename(latest_stage, latest_dir)) {
  if (had_latest) file.rename(latest_backup, latest_dir)
  stop("Could not publish latest exports; previous latest restored")
}
if (had_latest) unlink(latest_backup, recursive = TRUE)
message("Dashboard latest: ", latest_dir, "\nSnapshot retained: ", powerbi_dir)
