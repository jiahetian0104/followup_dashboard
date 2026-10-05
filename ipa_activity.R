# IPA appointment activity and completion credit, separate from biospecimens.
# Requires tidyverse and lubridate, already loaded by IPA Data.R.

ipa_activity_flag <- function(x) {
  value <- tolower(trimws(as.character(x)))
  case_when(
    value %in% c("true", "1", "yes", "y") ~ TRUE,
    value %in% c("false", "0", "no", "n") ~ FALSE,
    TRUE ~ NA
  )
}

ipa_activity_date <- function(x) {
  # Use the same supported Ripple formats as the existing task pipeline.
  lubridate::as_date(lubridate::parse_date_time(
    as.character(x),
    orders = c("mdy HMS", "mdy HM", "mdy", "ymd HMSz", "ymd HMS", "ymd"),
    quiet = TRUE
  ))
}

ipa_activity_column <- function(data, name) {
  if (name %in% names(data)) data[[name]] else rep(NA, nrow(data))
}

ipa_activity_sum <- function(x) {
  # Missing cancellation/rescheduling evidence must never become a zero.
  if (any(is.na(x))) NA_integer_ else as.integer(sum(x))
}

build_ipa_activity_exports <- function(
    calendly_data, participant_data, event_map, historical_tasks = tibble(),
    report_year = 2026L, completion_evidence = NULL
) {
  keys <- c("Participant ID", "Participant Cohort", "IPA Age Band")
  for (column in c("Rescheduled", "Old Invitee URI", "New Invitee URI")) {
    if (!column %in% names(calendly_data)) {
      calendly_data[[column]] <- rep(NA, nrow(calendly_data))
    }
  }
  all_history <- calendly_data %>%
    filter(`IPA Year` %in% c(report_year, report_year - 1L)) %>%
    mutate(
      `Canceled` = ipa_activity_flag(`Canceled`),
      `No Show` = ipa_activity_flag(`No Show`),
      `Rescheduled` = ipa_activity_flag(`Rescheduled`),
      `Old Invitee URI` = as.character(`Old Invitee URI`),
      `New Invitee URI` = as.character(`New Invitee URI`),
      # A successor link is independently verified rescheduling evidence.
      `Rescheduled` = if_else(
        !is.na(`New Invitee URI`) & nzchar(`New Invitee URI`), TRUE, `Rescheduled`
      ),
      `True Cancel` = case_when(
        `Canceled` %in% FALSE | `Rescheduled` %in% TRUE ~ FALSE,
        `Canceled` %in% TRUE & `Rescheduled` %in% FALSE ~ TRUE,
        TRUE ~ NA
      ),
      `Event Start Date` = ipa_activity_date(`Event Start Date`),
      FTM = case_when(
        is.na(`Calendly FTM`) | trimws(`Calendly FTM`) == "" ~ "Unassigned",
        grepl(";", `Calendly FTM`, fixed = TRUE) ~
          paste0("Shared hosts: ", `Calendly FTM`),
        TRUE ~ `Calendly FTM`
      ),
      `IPA Year` = as.integer(`IPA Year`)
    ) %>%
    select(
      all_of(keys), `IPA Year`, FTM, `Event UUID`, `Invitee UUID`,
      `Event Start Date`, `Canceled`, `True Cancel`, `No Show`, `Rescheduled`,
      `Old Invitee URI`, `New Invitee URI`,
      any_of(c("Event Created Date", "Start Date & Time", "Event Created Date & Time",
               "Meeting Host", "Participant Match Method"))
    ) %>%
    # One booking per child. Explicit twin bookings retain one row per child.
    distinct(`Participant ID`, `Participant Cohort`, `IPA Age Band`,
             `Invitee UUID`, .keep_all = TRUE)

  history <- all_history %>% filter(`IPA Year` == report_year)

  history_qc <- history %>% filter(
    is.na(`Participant ID`) | is.na(`Participant Cohort`) |
      is.na(`IPA Age Band`) | is.na(`Invitee UUID`) |
      is.na(`True Cancel`) | is.na(`Rescheduled`) | is.na(`No Show`) |
      FTM == "Unassigned" | startsWith(FTM, "Shared hosts:")
  )
  valid_history <- history %>% filter(
    !is.na(`Participant ID`), !is.na(`Participant Cohort`),
    !is.na(`IPA Age Band`), !is.na(`Invitee UUID`)
  )

  # Saved evidence permits replaying attribution rules without refetching APIs.
  ripple_records <- if (!is.null(completion_evidence)) {
    completion_evidence %>% select(all_of(keys), `IPA Complete`, `Completion Date`) %>%
      mutate(`IPA Complete` = ipa_activity_flag(`IPA Complete`) %in% TRUE,
             `Completion Date` = ipa_activity_date(`Completion Date`))
  } else pmap_dfr(event_map, function(
      age_band, scheduled_col, complete_col, scheduled_date_col, complete_date_col
  ) {
    completed <- ipa_activity_flag(ipa_activity_column(participant_data, complete_col))
    scheduled <- ipa_activity_flag(ipa_activity_column(participant_data, scheduled_col))
    complete_date <- ipa_activity_date(ipa_activity_column(participant_data, complete_date_col))
    schedule_date <- ipa_activity_date(ipa_activity_column(participant_data, scheduled_date_col))
    # Year-specific 3+ fields and undated completion flags remain visible, with
    # missing dates marked for review. Dated infant records from 2025 are excluded.
    completed_in_year <- completed %in% TRUE &
      (is.na(complete_date) | lubridate::year(complete_date) == report_year)
    recorded_in_year <- completed_in_year |
      (scheduled %in% TRUE & !is.na(schedule_date) &
         lubridate::year(schedule_date) == report_year)
    participant_data %>%
      transmute(
        `Participant ID`, `Participant Cohort`, `IPA Age Band` = age_band,
        `IPA Complete` = completed_in_year,
        `Completion Date` = if_else(completed_in_year, complete_date, as.Date(NA)),
        .recorded = recorded_in_year
      ) %>%
      filter(.recorded) %>% select(-.recorded)
  })

  past_completions <- if (nrow(historical_tasks) > 0) {
    historical_tasks %>%
      filter(Task == "In-Person Assessments", Outcome == "Complete") %>%
      transmute(
        across(all_of(keys)), `IPA Complete` = TRUE,
        `Completion Date` = ipa_activity_date(Outcome_Date)
      ) %>%
      filter(is.na(`Completion Date`) |
               lubridate::year(`Completion Date`) == report_year)
  } else {
    ripple_records[0, ]
  }

  completions <- bind_rows(ripple_records, past_completions) %>%
    filter(!is.na(`Participant ID`), !is.na(`Participant Cohort`)) %>%
    arrange(desc(`IPA Complete`), desc(`Completion Date`)) %>%
    distinct(across(all_of(keys)), .keep_all = TRUE)

  # Completion attribution ignores cohort differences but preserves Ripple's
  # cohort/band in the credited task. Older bookings supply evidence only.
  band_order <- c("6_11_month", "12_23_month", "24_35_month", "3_5yr",
                  "6_10yr", "11_17yr", "18_20yr")
  booking_evidence <- all_history %>% filter(
    !is.na(`Participant ID`), !is.na(`IPA Age Band`), !is.na(`Invitee UUID`)
  ) %>% mutate(
    .appointment_time = coalesce(
      lubridate::parse_date_time(
        as.character(ipa_activity_column(., "Start Date & Time")),
        orders = c("ymd HMSz", "ymd HMS", "mdy HMS", "mdy HM"), quiet = TRUE
      ), as.POSIXct(`Event Start Date`, tz = "America/New_York")
    ),
    .created_time = coalesce(
      lubridate::parse_date_time(
        as.character(ipa_activity_column(., "Event Created Date & Time")),
        orders = c("ymd HMSz", "ymd HMS", "mdy HMS", "mdy HM"), quiet = TRUE
      ), as.POSIXct(ipa_activity_date(ipa_activity_column(., "Event Created Date")),
                   tz = "America/New_York")
    )
  )
  unique_host <- function(rows) {
    if (nrow(rows) > 0 && n_distinct(rows$FTM) == 1 &&
        !any(rows$FTM == "Unassigned") &&
        !any(startsWith(rows$FTM, "Shared hosts:"))) first(rows$FTM)
    else NA_character_
  }
  completion_hosts <- map_dfr(seq_len(nrow(completions)), function(i) {
    record <- completions[i, ]
    candidates <- booking_evidence %>% filter(`Participant ID` == record$`Participant ID`)
    same_band <- candidates %>% filter(`IPA Age Band` == record$`IPA Age Band`)
    # Only fall back one age band when the requested band has no bookings.
    band_index <- match(record$`IPA Age Band`, band_order)
    previous <- nrow(same_band) == 0 && !is.na(band_index) && band_index > 1
    chosen <- if (previous) candidates %>% filter(
      `IPA Age Band` == band_order[band_index - 1]
    ) else same_band
    exact <- chosen %>% filter(
      `Canceled` %in% FALSE, `No Show` %in% FALSE,
      `Event Start Date` == record$`Completion Date`
    )
    host <- unique_host(exact)
    attribution <- if (previous) "Previous-age-band completion-date match" else "Matched completion date"
    evidence_date <- as.Date(NA)
    if (is.na(host)) {
      latest <- chosen %>% filter(!is.na(.appointment_time)) %>%
        arrange(desc(.appointment_time), desc(.created_time))
      if (nrow(latest) > 0) latest <- latest %>% filter(
        .appointment_time %in% first(.appointment_time),
        .created_time %in% first(.created_time)
      )
      host <- unique_host(latest)
      if (!is.na(host)) {
        evidence_date <- first(latest$`Event Start Date`)
        attribution <- if (previous) "Latest previous-age-band Calendly fallback" else
          "Latest same-age-band Calendly fallback"
      } else attribution <- if (nrow(exact) > 1 && n_distinct(exact$FTM) > 1)
        "Multiple hosts on completion date" else if (nrow(chosen) == 0)
          "No same/previous-age-band booking in reporting or prior year" else
            "Completion host unresolved"
    }
    if (!record$`IPA Complete`) {
      host <- NA_character_; attribution <- "Not complete"; evidence_date <- as.Date(NA)
    } else if (is.na(host)) host <- "Pending verification"
    record %>% select(all_of(keys)) %>% mutate(
      `Completion FTM` = host, `Completion Attribution` = attribution,
      `Completion Fallback Appointment Date` = evidence_date
    )
  })

  appointment_counts <- valid_history %>%
    group_by(across(all_of(keys))) %>%
    summarise(
      `Appointment Count` = n(),
      `No-show Count` = ipa_activity_sum(`No Show`),
      `Cancel Count` = ipa_activity_sum(`True Cancel`),
      `Reschedule Count` = ipa_activity_sum(`Rescheduled`), .groups = "drop"
    )

  records <- full_join(appointment_counts, completions, by = keys) %>%
    left_join(completion_hosts, by = keys) %>%
    mutate(
      across(c(`No-show Count`, `Cancel Count`, `Reschedule Count`),
             ~ if_else(is.na(`Appointment Count`), 0L, .x)),
      `Appointment Count` = coalesce(`Appointment Count`, 0L),
      `IPA Complete` = coalesce(`IPA Complete`, FALSE),
      `Completion Attribution` = coalesce(`Completion Attribution`, "Not complete"),
      `IPA Year` = as.integer(report_year)
    ) %>% arrange(`Participant Cohort`, `Participant ID`, `IPA Age Band`)

  workload <- valid_history %>%
    group_by(across(all_of(keys)), FTM) %>%
    summarise(
      `Appointment Count` = n(),
      `No-show Count` = ipa_activity_sum(`No Show`),
      `Cancel Count` = ipa_activity_sum(`True Cancel`),
      `Reschedule Count` = ipa_activity_sum(`Rescheduled`), .groups = "drop"
    )
  credit <- records %>% filter(`IPA Complete`) %>%
    transmute(across(all_of(keys)), FTM = `Completion FTM`, `Complete Count` = 1L)

  ftm_records <- full_join(workload, credit, by = c(keys, "FTM")) %>%
    mutate(
      across(c(`No-show Count`, `Cancel Count`, `Reschedule Count`),
             ~ if_else(is.na(`Appointment Count`), 0L, .x)),
      `Appointment Count` = coalesce(`Appointment Count`, 0L),
      `Complete Count` = coalesce(`Complete Count`, 0L)
    ) %>%
    left_join(records %>% select(
      all_of(keys), `IPA Year`, `IPA Complete`, `Completion Date`,
      `Completion FTM`, `Completion Attribution`, `Completion Fallback Appointment Date`
    ), by = keys) %>%
    arrange(FTM, `Participant ID`, `IPA Age Band`)

  ftm_summary <- ftm_records %>% group_by(FTM) %>% summarise(
    `Participants` = n_distinct(`Participant ID`),
    `Participant Age Bands` = n(),
    across(c(`Complete Count`, `Appointment Count`, `No-show Count`,
             `Cancel Count`, `Reschedule Count`), ipa_activity_sum),
    .groups = "drop"
  ) %>% mutate(
    `Weighted Points` = `Complete Count` + 0.25 * `No-show Count`,
    `Weighted IPA Performance` = if_else(
      FTM %in% c("Unassigned", "Pending verification") | startsWith(FTM, "Shared hosts:"),
      NA_real_, `Weighted Points` / `Participant Age Bands`
    )
  )
  stopifnot(
    sum(ftm_summary$`Complete Count`) == sum(records$`IPA Complete`),
    sum(ftm_summary$`Appointment Count`) == nrow(valid_history),
    !anyDuplicated(records[keys]),
    !anyDuplicated(ftm_records[c(keys, "FTM")])
  )
  list(
    "ipa_appointment_history.csv" = history,
    "ipa_appointment_history_qc.csv" = history_qc,
    "participant_ipa_records.csv" = records,
    "participant_ipa_ftm_records.csv" = ftm_records,
    "ftm_ipa_summary.csv" = ftm_summary,
    "ipa_completion_evidence.csv" = completions,
    "ipa_completion_booking_evidence.csv" = all_history
  )
}
