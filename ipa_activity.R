# Stateless 2026 calculations. Inputs are this refresh's API responses only.
ipa_activity_flag <- function(x) {
  value <- tolower(trimws(as.character(x)))
  case_when(value %in% c("true", "1", "yes", "y") ~ TRUE,
    value %in% c("false", "0", "no", "n") ~ FALSE, TRUE ~ NA)
}
ipa_activity_date <- function(x) {
  as_date(parse_date_time(as.character(x),
    orders = c("mdy HMS", "mdy HM", "mdy", "ymd HMSz", "ymd HMS", "ymd"), quiet = TRUE))
}
ipa_activity_column <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA, nrow(data))
}
ipa_activity_sum <- function(x) {
  if (anyNA(x)) NA_integer_ else as.integer(sum(x))
}

build_2026_exports <- function(ripple_6_35, ripple_3_20, calendly, report_year = 2026L) {
  # Missing event columns are schema failures, never silent "no records".
  for (name in c("infant", "child")) {
    data <- if (name == "infant") ripple_6_35 else ripple_3_20
    bands <- if (name == "infant") c("12_23_month", "24_35_month") else c("3_5yr", "6_10yr", "11_17yr", "18_20yr")
    mapped <- ipa_event_map %>% filter(age_band %in% bands)
    tasks <- ripple_task_map %>% filter(map_lgl(age_bands, ~ any(.x %in% bands)))
    required <- c(mapped$complete_col, mapped$complete_date_col, tasks$completed_col, tasks$completed_date_col)
    missing <- setdiff(required, names(data))
    if (length(missing)) stop("Missing ", name, " event fields: ", paste(missing, collapse = ", "))
  }
  tests <- c("iri0LoVJXJ734mKYy", "54jpojzH4AVzTJoPD")
  participants <- bind_rows(
    mutate(ripple_6_35, `Participant Cohort` = "6-35 month"),
    mutate(ripple_3_20, `Participant Cohort` = "3-20 year")
  ) %>% filter(!globalId %in% tests) %>% mutate(
    `Participant ID` = str_squish(str_remove(globalId, "\\s*\\([^)]*\\)$")),
    `Current Age Band` = case_when(
      str_detect(statusId, regex("^6-11 Month", TRUE)) ~ "6_11_month",
      str_detect(statusId, regex("^12-23 Month", TRUE)) ~ "12_23_month",
      str_detect(statusId, regex("^24-35 Month", TRUE)) ~ "24_35_month",
      str_detect(statusId, regex("^3-5 Year", TRUE)) ~ "3_5yr",
      str_detect(statusId, regex("^6-10 Year", TRUE)) ~ "6_10yr",
      str_detect(statusId, regex("^11-17 Year", TRUE)) ~ "11_17yr",
      str_detect(statusId, regex("^18-20 Year", TRUE)) ~ "18_20yr",
      TRUE ~ "needs_review"
    ))
  if (anyNA(participants$`Participant ID`) || any(participants$`Participant ID` == "") ||
      anyDuplicated(participants[c("Participant ID", "Participant Cohort")]))
    stop("Missing or duplicate participant IDs in Ripple source; resolve before publishing")

  surveys <- build_ipa_surveys(ripple_6_35, ripple_3_20, report_year)
  survey_duplicates <- build_ipa_survey_duplicate_qc(surveys$history, report_year)
  completions <- surveys$completions %>% mutate(`Completion Date` = as_date(`Completed at`))
  keys <- c("Participant ID", "IPA Age Band")
  history <- calendly %>% filter(`IPA Year` == report_year) %>% mutate(
    Canceled = ipa_activity_flag(Canceled), `No Show` = ipa_activity_flag(`No Show`),
    Rescheduled = ipa_activity_flag(Rescheduled),
    Rescheduled = if_else(!is.na(`New Invitee URI`) & nzchar(`New Invitee URI`), TRUE, Rescheduled),
    `True Cancel` = case_when(Canceled %in% FALSE | Rescheduled %in% TRUE ~ FALSE,
      Canceled %in% TRUE & Rescheduled %in% FALSE ~ TRUE, TRUE ~ NA),
    FTM = case_when(is.na(`Calendly FTM`) | `Calendly FTM` == "" ~ "Unassigned",
      str_detect(`Calendly FTM`, fixed(";")) ~ paste0("Shared hosts: ", `Calendly FTM`),
      TRUE ~ `Calendly FTM`)
  ) %>% select(`Participant ID`, `Participant Cohort`, `IPA Age Band`, `IPA Year`,
    FTM, `Event UUID`, `Invitee UUID`, `Event Type Name`, `Event Start Date`,
    `Start Date & Time`, `Event Created Date & Time`, Canceled, `True Cancel`, `No Show`,
    Rescheduled, `Old Invitee URI`, `New Invitee URI`, `Participant Match Method`,
    `Calendly QA Flag`, `Calendly FTM QA Flag`) %>%
    distinct(`Invitee UUID`, `Participant ID`, `Participant Cohort`, `IPA Age Band`, .keep_all = TRUE) %>%
    mutate(`Match Status` = case_when(
      is.na(`Participant ID`) ~ "Participant unresolved",
      is.na(`IPA Age Band`) ~ "Age band unresolved",
      is.na(`Invitee UUID`) ~ "Booking ID missing", TRUE ~ "Matched"))
  valid <- history %>% filter(`Match Status` == "Matched")
  workload <- valid %>% group_by(FTM, `Participant ID`, `Participant Cohort`, `IPA Age Band`) %>%
    summarise(`Appointment Count` = n(), `No-show Count` = ipa_activity_sum(`No Show`),
      `Cancel Count` = ipa_activity_sum(`True Cancel`),
      `Reschedule Count` = ipa_activity_sum(Rescheduled), .groups = "drop")

  # Compare independent completion evidence at child × age-band grain.
  events <- pmap_dfr(ipa_event_map, function(age_band, scheduled_col, complete_col,
                                           scheduled_date_col, complete_date_col) {
    raw_date <- as.character(ipa_activity_column(participants, complete_date_col))
    participants %>% transmute(`Participant ID`, `Participant Cohort`, `IPA Age Band` = age_band,
      `Event Complete` = ipa_activity_flag(ipa_activity_column(participants, complete_col)),
      `Event Completion Date` = ipa_activity_date(raw_date), `Event Date Raw` = raw_date,
      `Event Field` = complete_col, `Event Date Field` = complete_date_col,
      `Year-specific Field` = str_detect(complete_col, paste0("event.", report_year, "_")))
  }) %>% mutate(
    `Event Complete 2026` = `Event Complete` %in% TRUE &
      (year(`Event Completion Date`) %in% report_year |
       (is.na(`Event Completion Date`) & `Year-specific Field`)),
    `Event Date QC` = case_when(
      !is.na(`Event Date Raw`) & nzchar(str_trim(`Event Date Raw`)) & is.na(`Event Completion Date`) ~ "Unparseable event date",
      `Event Complete` %in% TRUE & is.na(`Event Completion Date`) ~ "Completed event missing date",
      `Event Complete` %in% TRUE & !year(`Event Completion Date`) %in% report_year ~ "Completed event outside 2026",
      TRUE ~ NA_character_))
  event_summary <- events %>% group_by(across(all_of(keys))) %>% summarise(
    `Participant Cohort` = first(`Participant Cohort`),
    `Event Complete` = any(`Event Complete` %in% TRUE),
    `Event Complete 2026` = any(`Event Complete 2026`),
    `Event Completion Date` = if (all(is.na(`Event Completion Date`))) as.Date(NA) else
      max(`Event Completion Date`, na.rm = TRUE),
    `Event Date Raw` = na_if(paste(sort(unique(na.omit(`Event Date Raw`))), collapse = "; "), ""),
    `Event Date Field` = paste(sort(unique(`Event Date Field`)), collapse = "; "),
    `Event Date QC` = na_if(paste(sort(unique(na.omit(`Event Date QC`))), collapse = "; "), ""),
    .groups = "drop")
  survey_qc <- completions %>% select(all_of(keys), `Survey Cohort` = `Participant Cohort`, `Completion Date`, `Attributed FTM`)
  comparison <- full_join(event_summary, survey_qc, by = keys) %>% mutate(
    `Participant Cohort` = coalesce(`Survey Cohort`, `Participant Cohort`),
    `Survey Complete` = !is.na(`Completion Date`),
    `Event Complete` = coalesce(`Event Complete`, FALSE),
    `Event Complete 2026` = coalesce(`Event Complete 2026`, FALSE),
    `QC Status` = case_when(
      `Survey Complete` & `Event Complete 2026` ~ "Both complete",
      `Survey Complete` ~ "Survey only",
      `Event Complete 2026` ~ "Event only",
      `Event Complete` ~ "Event year/date needs review", TRUE ~ "Neither complete"),
    `Date Difference Days` = as.integer(`Event Completion Date` - `Completion Date`)
  )

  # Pair same-day unmatched evidence across bands, rather than presenting
  # the two sides as unrelated missing completions. Never alter either source.
  conflicts <- comparison %>% filter(`QC Status` == "Survey only") %>%
    select(`Participant ID`, `Participant Cohort`, `Survey Age Band` = `IPA Age Band`,
      `Completion Date`, `Attributed FTM`) %>%
    inner_join(comparison %>% filter(`QC Status` == "Event only") %>%
      select(`Participant ID`, `Event Age Band` = `IPA Age Band`,
        `Completion Date` = `Event Completion Date`),
      by = c("Participant ID", "Completion Date"), relationship = "many-to-many") %>%
    filter(`Survey Age Band` != `Event Age Band`) %>%
    mutate(`QC Issue` = "Same-date survey/event age-band conflict")
  conflict_sides <- bind_rows(
    conflicts %>% transmute(`Participant ID`, `IPA Age Band` = `Survey Age Band`,
      `Other Source Age Band` = `Event Age Band`),
    conflicts %>% transmute(`Participant ID`, `IPA Age Band` = `Event Age Band`,
      `Other Source Age Band` = `Survey Age Band`)
  ) %>% group_by(across(all_of(keys))) %>% summarise(
    `Other Source Age Band` = paste(sort(unique(`Other Source Age Band`)), collapse = "; "), .groups = "drop")
  comparison <- comparison %>% left_join(conflict_sides, by = keys) %>% mutate(
    `Evidence Status` = `QC Status`,
    `QC Relevant` = `Survey Complete` | `Event Complete 2026`,
    `QC Status` = if_else(!is.na(`Other Source Age Band`), "Age-band conflict", `QC Status`),
    `QC Issue` = case_when(
      !is.na(`Other Source Age Band`) ~ "Same-date survey/event age-band conflict",
      `QC Status` == "Both complete" & !is.na(`Date Difference Days`) & `Date Difference Days` != 0L ~ "Completion date difference",
      `QC Status` == "Survey only" ~ "Survey completion without a 2026 same-band event completion",
      `QC Status` == "Event only" ~ "2026 event completion without a same-band completed survey",
      `QC Relevant` & !is.na(`Event Date QC`) ~ `Event Date QC`,
      TRUE ~ NA_character_))

  # Survey latest completion owns specimens; Calendly fallback stays in the same band/year.
  survey_owners <- surveys$latest %>% select(all_of(keys), `Survey FTM` = `Attributed FTM`,
    `Survey Assignment Time` = `Completed at`)
  booking_owners <- valid %>% filter(FTM != "Unassigned") %>%
    arrange(desc(`Start Date & Time`), desc(`Event Created Date & Time`), `Invitee UUID`) %>%
    distinct(across(all_of(keys)), .keep_all = TRUE) %>%
    select(all_of(keys), `Calendly FTM` = FTM, `Calendly Assignment Date` = `Event Start Date`)
  specimens_all <- pmap_dfr(ripple_task_map, function(Task, age_bands, completed_col, completed_date_col) {
    raw_date <- as.character(ipa_activity_column(participants, completed_date_col))
    dates <- ipa_activity_date(raw_date)
    bands <- if (length(age_bands) == 1L) rep(age_bands, nrow(participants)) else {
      age_months <- map2_dbl(parse_participant_birthday(participants$birthday),
        as.POSIXct(dates, tz = "America/New_York"), calculate_age_months)
      calculated <- assign_ipa_age_band(age_months)
      # Collection age determines tooth age band. Unknown/out-of-scope ages stay in QC.
      if_else(calculated %in% age_bands, calculated, "needs_review")
    }
    participants %>% transmute(`Participant ID`, `Participant Cohort`,
      statusId, `Current Age Band`, `IPA Age Band` = bands, Task = Task,
      `Collected` = ipa_activity_flag(ipa_activity_column(participants, completed_col)),
      `Collection Date` = dates, `Collection Date Raw` = raw_date,
      `Ripple Field` = completed_col)
  }) %>% mutate(`QC Issue` = case_when(
    !is.na(`Collection Date Raw`) & nzchar(str_trim(`Collection Date Raw`)) & is.na(`Collection Date`) ~ "Unparseable collection date",
    Collected %in% TRUE & is.na(`Collection Date`) ~ "Collected flag missing date",
    year(`Collection Date`) %in% report_year & !Collected %in% TRUE ~ "2026 date without collected flag",
    Collected %in% TRUE & year(`Collection Date`) %in% report_year & `IPA Age Band` == "needs_review" ~ "Collection age band unresolved",
    TRUE ~ NA_character_))
  specimens <- specimens_all %>% filter(Collected %in% TRUE, year(`Collection Date`) == report_year) %>%
    left_join(survey_owners, by = keys, relationship = "many-to-one") %>%
    left_join(booking_owners, by = keys, relationship = "many-to-one") %>% mutate(
      FTM = coalesce(`Survey FTM`, `Calendly FTM`, "Unassigned"),
      `Assignment Source` = case_when(!is.na(`Survey FTM`) ~ "Latest 2026 same-band survey",
        !is.na(`Calendly FTM`) ~ "Latest 2026 same-band Calendly host", TRUE ~ "Unassigned"))
  roster <- participants %>% select(`Participant ID`, `Participant Cohort`, statusId, `Current Age Band`)
  summary <- workload %>% group_by(FTM) %>% summarise(
    `Appointment Count` = sum(`Appointment Count`),
    across(c(`No-show Count`, `Cancel Count`, `Reschedule Count`), ipa_activity_sum), .groups = "drop")
  stopifnot(!anyDuplicated(completions[keys]),
    sum(workload$`Appointment Count`) == nrow(valid),
    !anyDuplicated(specimens[c("Participant ID", "Participant Cohort", "Task")]))
  list(
    "ipa_survey_submissions.csv" = surveys$history,
    "ipa_survey_completions.csv" = completions,
    "ipa_survey_latest.csv" = surveys$latest,
    "ipa_survey_duplicate_summary.csv" = survey_duplicates$summary,
    "ipa_survey_duplicate_qc.csv" = survey_duplicates$detail,
    "ipa_event_evidence.csv" = events,
    "ipa_survey_event_comparison.csv" = comparison,
    "ipa_survey_event_qc.csv" = comparison %>% filter(`QC Relevant`,
      !is.na(`QC Issue`) | !is.na(`Event Date QC`)),
    "ipa_age_band_conflicts.csv" = conflicts,
    "ipa_appointment_history.csv" = history,
    "ipa_appointment_history_qc.csv" = history %>% filter(`Match Status` != "Matched" |
      is.na(`True Cancel`) | is.na(Rescheduled) | is.na(`No Show`) |
      FTM == "Unassigned" | startsWith(FTM, "Shared hosts:") |
      !is.na(`Calendly QA Flag`) | !is.na(`Calendly FTM QA Flag`)),
    "participant_ipa_ftm_records.csv" = workload,
    "ftm_ipa_workload.csv" = summary,
    "biospecimen_collections.csv" = specimens,
    "biospecimen_qc.csv" = specimens_all %>% filter(!is.na(`QC Issue`)),
    "participant_roster.csv" = roster)
}
