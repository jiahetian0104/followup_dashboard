# Survey extraction and original user-confirmed corrections. No API calls here.
# IPA uses first completion; biospecimen assignment uses latest completion.
build_ipa_surveys <- function(ripple_6_35, ripple_3_20, report_year = 2026L) {
report_timezone <- "America/New_York"
age_groups <- c(
  "12-23 Month", "24-35 Month", "3-5 Year",
  "6-10 Year", "11-17 Year", "18-20 Year"
)

# Explicit exclusions already used by the existing dashboard.
test_global_ids <- c("iri0LoVJXJ734mKYy", "54jpojzH4AVzTJoPD")

# User-verified attribution, supplied on October 14, 2026.
# Match both age group and Global ID; do not replace every Item 1/Item 2
# with one name. Apply only while the original value remains a placeholder.
ftm_corrections <- tibble::tribble(
  ~`Survey age group`, ~globalId, ~`Verified FTM`,
  "6-10 Year",    "WGX824-01-A (713)", "Andrew",
  "3-5 Year",     "NDX702-01-A (152)", "Andrew",
  "12-23 Month",  "SMQ889-01-A (911)", "Kowlini",
  "12-23 Month",  "JXP185-01-A (581)", "Kowlini",
  "12-23 Month",  "PGM615-01-A (153)", "Kowlini",
  "12-23 Month",  "YYB078-01-A (680)", "Kowlini",
  "12-23 Month",  "LNT397-01-A (614)", "Kowlini",
  "12-23 Month",  "KJR619-01-A (284)", "Kowlini",
  "12-23 Month",  "MMV961-01-A (581)", "Kowlini",
  "12-23 Month",  "KXF530-01-A (186)", "Kowlini",
  "12-23 Month",  "SJH036-01-A (987)", "Kowlini",
  "12-23 Month",  "BRL402-01-A (723)", "Andrew",
  "12-23 Month",  "QVW747-01-A (109)", "Kowlini"
)

# Corrections to individual historical submissions, not participant-wide rules.
# Fixed age groups and completion timestamps prevent later submissions from
# inheriting these changes. Only replace the originally reported wrong value.
submission_corrections <- tibble::tribble(
  ~customId, ~`Survey age group`, ~`Submission index`, ~completion_time,
  ~old_site, ~corrected_site, ~old_ftm, ~corrected_ftm,
  "82098M1", "6-10 Year", 0L, "03/29/2026 20:39",
  "grandrapids", "detroit", NA_character_, NA_character_,
  "82423M1", "3-5 Year", 0L, "07/17/2026 08:58",
  NA_character_, NA_character_, "Nicole", "Anna"
) %>%
  mutate(`Completed at` = mdy_hm(completion_time, tz = report_timezone)) %>%
  select(-completion_time)

# Extract the completion time and corresponding FTM for each survey instance.
# Event completion fields are not used to establish IPA survey completion.
extract_ipa_submissions <- function(data, study_name) {
  completion_columns <- names(data)[str_detect(
    names(data), " IPA Survey\\.[0-9]+\\.EnrollNow Survey Completed At$"
  )]
  if (!length(completion_columns)) {
    stop("No IPA survey completion columns found for ", study_name)
  }

  submissions <- lapply(completion_columns, function(completion_column) {
    parts <- str_match(
      completion_column,
      "^(.+) IPA Survey\\.([0-9]+)\\.EnrollNow Survey Completed At$"
    )
    age_group <- parts[1L, 2L]
    if (!age_group %in% age_groups) {
      stop("Unexpected IPA survey age group: ", age_group)
    }
    prefix <- str_remove(completion_column, fixed("EnrollNow Survey Completed At"))
    # Ripple renamed the 24-35 Month FTM field; support both schemas.
    staff_candidates <- paste0(prefix, if (age_group == "24-35 Month") c("IPA_FTM", "ftm_member") else "IPA_FTM")
    available <- staff_candidates[staff_candidates %in% names(data)]
    if (!length(available)) stop("Missing FTM column for ", prefix)
    staff_values <- rep(NA_character_, nrow(data))
    staff_sources <- rep(NA_character_, nrow(data))
    for (field in available) {
      candidate <- na_if(str_squish(as.character(data[[field]])), "")
      conflict <- !is.na(staff_values) & !is.na(candidate) & staff_values != candidate
      if (any(conflict)) stop("Conflicting survey FTM fields for ", prefix)
      use <- is.na(staff_values) & !is.na(candidate)
      staff_values[use] <- candidate[use]
      staff_sources[use] <- field
    }
    site_column <- paste0(prefix, "IPA_Site")
    if (!site_column %in% names(data)) stop("Missing site column: ", site_column)

    tibble::tibble(
      Study = study_name,
      `Survey age group` = age_group,
      globalId = str_squish(as.character(data$globalId)),
      customId = str_squish(as.character(data$customId)),
      `Submission index` = as.integer(parts[1L, 3L]),
      `Original completion time` = as.character(data[[completion_column]]),
      `Original FTM value` = staff_values,
      `Original site value` = as.character(data[[site_column]]),
      `FTM source field` = staff_sources
    )
  })

  bind_rows(submissions) %>%
    filter(
      !globalId %in% test_global_ids,
      !is.na(`Original completion time`),
      str_trim(`Original completion time`) != ""
    ) %>%
    mutate(
      `Completed at` = mdy_hm(
        `Original completion time`, tz = report_timezone, quiet = TRUE
      )
    )
}

ipa_submission_detail <- bind_rows(
  extract_ipa_submissions(ripple_6_35, "6-35 Month"),
  extract_ipa_submissions(ripple_3_20, "3-20 Year")
)

# Fail visibly rather than silently omitting a submitted record with a bad date.
if (anyNA(ipa_submission_detail$`Completed at`)) {
  stop("Some IPA completion times could not be parsed. Check ipa_submission_detail.")
}
if (any(is.na(ipa_submission_detail$globalId) | ipa_submission_detail$globalId == "")) {
  stop("A submitted IPA survey is missing its Global ID.")
}

ipa_submission_detail <- ipa_submission_detail %>%
  left_join(ftm_corrections, by = c("Survey age group", "globalId")) %>%
  left_join(
    submission_corrections,
    by = c("customId", "Survey age group", "Submission index", "Completed at")
  ) %>%
  mutate(
    ftm_normalized = str_to_title(str_squish(`Original FTM value`)),
    correct_record_ftm = !is.na(corrected_ftm) &
      !is.na(ftm_normalized) & ftm_normalized == old_ftm,
    correct_record_site = !is.na(corrected_site) &
      !is.na(`Original site value`) &
      str_remove_all(str_to_lower(`Original site value`), "\\s+") == old_site,
    `Attribution source` = case_when(
      correct_record_ftm ~ "Submission-specific correction (2026-10-16)",
      ftm_normalized %in% c("Item 1", "Item 2") & !is.na(`Verified FTM`) ~
        "User-verified mapping (2026-10-14)",
      TRUE ~ "Ripple survey"
    ),
    `Attributed FTM` = case_when(
      correct_record_ftm ~ corrected_ftm,
      ftm_normalized %in% c("Item 1", "Item 2") & !is.na(`Verified FTM`) ~ `Verified FTM`,
      ftm_normalized %in% c("Andrew", "Andrew Davis") ~ "Andrew",
      ftm_normalized %in% c("Kowlini", "Kowlini Wickramasinghe") ~ "Kowlini",
      is.na(ftm_normalized) | ftm_normalized == "" ~ "Unconfirmed: missing FTM",
      ftm_normalized %in% c("Item 1", "Item 2") ~ paste0("Unconfirmed: ", ftm_normalized),
      TRUE ~ ftm_normalized
    ),
    Site = if_else(correct_record_site, corrected_site, `Original site value`),
    `Site source` = if_else(
      correct_record_site, "Submission-specific correction (2026-10-16)", "Ripple survey"
    ),
    Site = case_when(
      str_remove_all(str_to_lower(str_squish(Site)), "[^a-z]") == "detroit" ~ "Detroit",
      str_remove_all(str_to_lower(str_squish(Site)), "[^a-z]") == "eastlansing" ~ "East Lansing",
      str_remove_all(str_to_lower(str_squish(Site)), "[^a-z]") == "flint" ~ "Flint",
      str_remove_all(str_to_lower(str_squish(Site)), "[^a-z]") == "grandrapids" ~ "Grand Rapids",
      str_remove_all(str_to_lower(str_squish(Site)), "[^a-z]") == "traversecity" ~ "Traverse City",
      TRUE ~ Site
    ),
    `Completion year` = year(`Completed at`),
    `Completion month` = format(`Completed at`, "%Y-%m", tz = report_timezone),
    `Included in 2026` = if_else(`Completion year` == report_year, "Yes", "No")
  ) %>%
  arrange(`Completed at`, `Survey age group`, globalId, `Submission index`) %>%
  select(
    Study, `Survey age group`, globalId, `Submission index`, `Completed at`,
    `Completion year`, `Completion month`, `Attributed FTM`,
    `Original FTM value`, `Attribution source`, `FTM source field`,
    `Original completion time`, `Included in 2026`, customId,
    Site, `Original site value`, `Site source`
  )

# Keep all extracted submissions available in R for checking exclusions.
# The original API CSVs also retain every submitted instance.
ipa_submission_detail$`Attributed FTM` <- map_chr(ipa_submission_detail$`Attributed FTM`, normalize_calendly_ftm)
ipa_submission_history <- ipa_submission_detail %>% mutate(
    `Participant ID` = str_squish(str_remove(globalId, "\\s*\\([^)]*\\)$")),
    `IPA Age Band` = unname(c("12-23 Month"="12_23_month", "24-35 Month"="24_35_month",
      "3-5 Year"="3_5yr", "6-10 Year"="6_10yr", "11-17 Year"="11_17yr",
      "18-20 Year"="18_20yr")[`Survey age group`]),
    `Participant Cohort` = if_else(Study == "6-35 Month", "6-35 month", "3-20 year")
  )

# First completed submission in the reporting year controls IPA completion.
  in_year <- ipa_submission_history %>% filter(`Completion year` == report_year)
  first <- in_year %>% arrange(`Completed at`, `Submission index`) %>%
    distinct(`Participant ID`, `IPA Age Band`, .keep_all = TRUE)
  latest <- in_year %>% arrange(desc(`Completed at`), desc(`Submission index`)) %>%
    distinct(`Participant ID`, `IPA Age Band`, .keep_all = TRUE)
  list(history = ipa_submission_history, completions = first, latest = latest)
}


build_ipa_survey_duplicate_qc <- function(history, report_year = 2026L) {
  keys <- c("Participant ID", "IPA Age Band")
  # Apply the same year, grain and timestamp/index tie-break as completion selection.
  detail <- history %>% filter(`Completion year` == report_year) %>%
    arrange(`Completed at`, `Submission index`) %>%
    group_by(across(all_of(keys))) %>%
    mutate(`Submission Count` = n(), `Submission Order` = row_number(),
      `Counted in IPA` = row_number() == 1L,
      `QC Issue` = "Multiple completed surveys in the same age band") %>%
    filter(`Submission Count` > 1L) %>% ungroup() %>%
    arrange(`Participant ID`, `IPA Age Band`, `Submission Order`)
  summary <- detail %>% group_by(across(all_of(keys))) %>% summarise(
    `Participant Cohort` = first(`Participant Cohort`),
    `Survey age group` = first(`Survey age group`),
    `Submission Count` = n(), `Extra Submissions` = n() - 1L,
    `First Completion Time` = first(`Completed at`),
    `Latest Completion Time` = last(`Completed at`),
    `First FTM` = first(`Attributed FTM`), `Latest FTM` = last(`Attributed FTM`),
    `FTMs Recorded` = paste(unique(`Attributed FTM`), collapse = "; "),
    `QC Issue` = first(`QC Issue`), .groups = "drop")
  list(summary = summary, detail = detail)
}
