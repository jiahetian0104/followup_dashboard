# Export the requested Ripple survey responses for both studies.
# Run this file with Source in RStudio or with Rscript.
# Results remain available as ripple_survey_3_20_year and
# ripple_survey_6_35_month; original CSV responses are saved separately.

library(httr2)
library(readr)

# Explicit folder so Source and Run work regardless of the working directory.
# On another computer, change this path or set RIPPLE_SURVEY_SCRIPT_DIR.
SCRIPT_DIR <- Sys.getenv(
  "RIPPLE_SURVEY_SCRIPT_DIR",
  unset = "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard"
)
if (!dir.exists(SCRIPT_DIR)) {
  stop("SCRIPT_DIR does not exist. Set it to your followup_dashboard folder.")
}
SCRIPT_DIR <- normalizePath(SCRIPT_DIR, mustWork = TRUE)

# Read only literal configuration assignments. Do not source the dashboard:
# sourcing it would also run its data cleaning and overwrite dashboard outputs.
read_existing_config <- function(filename) {
  expressions <- parse(file.path(SCRIPT_DIR, filename))
  wanted <- c("base_url", "auth_key", "study_id", "team_id", "timezone")
  config <- list()
  for (expr in expressions) {
    if (is.call(expr) && is.symbol(expr[[1L]]) &&
        as.character(expr[[1L]]) %in% c("<-", "=")) {
      name <- as.character(expr[[2L]])
      if (length(name) == 1L && name %in% wanted &&
          is.character(expr[[3L]]) && length(expr[[3L]]) == 1L) {
        config[[name]] <- expr[[3L]]
      }
    }
  }
  if (!all(wanted %in% names(config))) {
    stop("Missing API configuration in ", filename)
  }
  # Optional environment override; never print the authentication value.
  config$auth_key <- Sys.getenv("RIPPLE_AUTH_KEY", unset = config$auth_key)
  config
}

# Keep the same blank surveyExportSince as the existing API requests.
# To request a specific start date, set e.g. "2026-01-01" here, or set
# RIPPLE_SURVEY_EXPORT_SINCE. A blank value uses Ripple's server default;
# it does not guarantee that all historical responses will be included.
survey_export_since <- Sys.getenv("RIPPLE_SURVEY_EXPORT_SINCE", unset = "")
if (nzchar(survey_export_since) &&
    (!grepl("^\\d{4}-\\d{2}-\\d{2}$", survey_export_since) ||
     is.na(as.Date(survey_export_since, format = "%Y-%m-%d")))) {
  stop("survey_export_since must be blank or a valid YYYY-MM-DD date.")
}

study_requests <- list(
  `3_20_year` = list(
    config_file = "Ripple 2026 3-20 study event.R",
    fields = c("globalId", "customId", "statusId", "tags"),
    surveys = list(
      survey7 = "kFOE-iNu8LYL6_Qp1",
      survey8 = "QiQQS_3MQYq6iPaeS",
      survey9 = "mi63YrTXz1_DTBQnQ",
      survey10 = "NcIj8oFbEjY6PgaNA"
    )
  ),
  `6_35_month` = list(
    config_file = "2026_6_35_month_dashboard.R",
    fields = c("globalId", "customId", "tags"),
    surveys = list(
      survey3 = "4PsVDDIam9qybK21J",
      survey4 = "ZE-phZDg2hDm7OziS"
    )
  )
)

run_id <- format(Sys.time(), "%Y-%m-%d_%H%M%S", tz = "America/Detroit")
output_dir <- file.path(SCRIPT_DIR, "survey_exports", run_id)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

export_surveys <- function(study_name, specification) {
  config <- read_existing_config(specification$config_file)
  body_list <- list(
    access_token = "",
    teamId = config$team_id,
    `export-type` = config$study_id,
    `export-timezone` = config$timezone,
    surveyExportSince = survey_export_since,
    # Preserve the supplied empty value. The current server still includes
    # event columns with this value; keep them in the original export.
    `Events (All or None)` = ""
  )
  # Use the existing scripts' "on" checkbox convention for requested fields.
  # surveyN parameters carry survey IDs rather than checkbox values.
  for (field in specification$fields) body_list[[field]] <- "on"
  body_list <- c(body_list, specification$surveys)

  message("Requesting survey export: ", study_name)
  resp <- request(config$base_url) |>
    req_headers(Authorization = config$auth_key) |>
    req_body_form(!!!body_list) |>
    req_timeout(120) |>
    req_retry(max_tries = 3, max_seconds = 180) |>
    req_perform()

  csv_text <- resp_body_string(resp)
  content_type <- resp_header(resp, "content-type")
  if (is.null(content_type)) content_type <- ""
  if (!nzchar(trimws(csv_text)) ||
      grepl("text/html|application/json", content_type, ignore.case = TRUE) ||
      grepl("^\\s*(<|\\{|\\[)", csv_text)) {
    stop("Ripple returned an empty or non-CSV response for ", study_name)
  }

  # Preserve IDs, answers and dates as text; do not infer numeric ID types.
  data <- read_csv(
    I(csv_text),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
  if (!"globalId" %in% names(data)) {
    stop("Expected globalId column is missing from the ", study_name, " export.")
  }
  if (nrow(problems(data))) {
    stop("CSV parsing problems in ", study_name, "; inspect the response before use.")
  }
  csv_path <- file.path(output_dir, paste0("ripple_surveys_", study_name, ".csv"))
  writeBin(resp_body_raw(resp), csv_path)
  message("Saved ", nrow(data), " rows and ", ncol(data), " columns: ", csv_path)
  list(data = data, path = csv_path, study_id = config$study_id)
}

# Attempt both exports even if one study fails. Do not print response bodies
# or request headers, which may contain private data or authentication details.
survey_exports <- list()
export_errors <- character()
for (study_name in names(study_requests)) {
  result <- tryCatch(
    export_surveys(study_name, study_requests[[study_name]]),
    error = function(e) {
      message("Export failed for ", study_name, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(result)) {
    export_errors <- c(export_errors, study_name)
  } else {
    survey_exports[[study_name]] <- result
  }
}

ripple_survey_3_20_year <- survey_exports[["3_20_year"]]$data
ripple_survey_6_35_month <- survey_exports[["6_35_month"]]$data

if (length(survey_exports)) {
  manifest <- do.call(rbind, lapply(names(survey_exports), function(study_name) {
    result <- survey_exports[[study_name]]
    data.frame(
      study = study_name,
      study_id = result$study_id,
      survey_parameters = paste(names(study_requests[[study_name]]$surveys), collapse = ";"),
      survey_ids = paste(unlist(study_requests[[study_name]]$surveys), collapse = ";"),
      survey_export_since = survey_export_since,
      exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "America/Detroit"),
      rows = nrow(result$data),
      columns = ncol(result$data),
      file = basename(result$path)
    )
  }))
  write_csv(manifest, file.path(output_dir, "manifest.csv"))
}
if (length(export_errors)) {
  stop("Incomplete survey export: ", paste(export_errors, collapse = ", "), call. = FALSE)
}


# 2. Summarize 2026 IPA survey completions by FTM ---------------------------
# Continue directly from the two data frames returned by the API above.
# Keep the latest completed submission per participant and age group within
# each reporting year. Different age groups remain separate.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(openxlsx)

report_year <- 2026L
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
    staff_column <- paste0(
      prefix, if (age_group == "24-35 Month") "ftm_member" else "IPA_FTM"
    )
    if (!staff_column %in% names(data)) stop("Missing FTM column: ", staff_column)
    site_column <- paste0(prefix, "IPA_Site")
    if (!site_column %in% names(data)) stop("Missing site column: ", site_column)

    tibble::tibble(
      Study = study_name,
      `Survey age group` = age_group,
      globalId = str_squish(as.character(data$globalId)),
      customId = str_squish(as.character(data$customId)),
      `Submission index` = as.integer(parts[1L, 3L]),
      `Original completion time` = as.character(data[[completion_column]]),
      `Original FTM value` = as.character(data[[staff_column]]),
      `Original site value` = as.character(data[[site_column]]),
      `FTM source field` = staff_column
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
  extract_ipa_submissions(ripple_survey_6_35_month, "6-35 Month"),
  extract_ipa_submissions(ripple_survey_3_20_year, "3-20 Year")
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
ipa_submission_history <- ipa_submission_detail

# Deduplicate within each year so later-year submissions cannot remove a
# historical completion from the 2026 report. Choose by completed timestamp,
# not by .0/.1 position. A larger index breaks ties at the same timestamp.
# Keep the whole selected row, including its FTM and site.
ipa_submission_detail <- ipa_submission_history %>%
  group_by(globalId, `Survey age group`, `Completion year`) %>%
  arrange(desc(`Completed at`), desc(`Submission index`), .by_group = TRUE) %>%
  slice_head(n = 1L) %>%
  ungroup() %>%
  arrange(`Completed at`, `Survey age group`, globalId)

duplicates_removed_2026 <- sum(ipa_submission_history$`Completion year` == report_year) -
  sum(ipa_submission_detail$`Completion year` == report_year)
message("Earlier same-age-group submissions excluded for 2026: ", duplicates_removed_2026)

# This deduplicated record-level dataset feeds both summaries.
ipa_2026_detail <- ipa_submission_detail %>%
  filter(`Completion year` == report_year)

# Annual FTM x age-group counts, attributed to the latest submission's FTM.
ipa_ftm_summary <- ipa_2026_detail %>%
  count(`Attributed FTM`, `Survey age group`, name = "Completions") %>%
  complete(
    `Attributed FTM`, `Survey age group` = age_groups,
    fill = list(Completions = 0L)
  ) %>%
  pivot_wider(names_from = `Survey age group`, values_from = Completions) %>%
  rename(FTM = `Attributed FTM`) %>%
  select(FTM, all_of(age_groups)) %>%
  mutate(Total = rowSums(pick(all_of(age_groups)))) %>%
  arrange(FTM)

# Month is determined by survey completion time, not the Calendly appointment.
ipa_ftm_monthly <- ipa_2026_detail %>%
  mutate(Month = month(`Completed at`)) %>%
  count(`Attributed FTM`, Month, name = "Completions") %>%
  complete(`Attributed FTM`, Month = 1:12, fill = list(Completions = 0L)) %>%
  mutate(Month = factor(month.abb[Month], levels = month.abb)) %>%
  pivot_wider(names_from = Month, values_from = Completions) %>%
  rename(FTM = `Attributed FTM`) %>%
  select(FTM, all_of(month.abb)) %>%
  mutate(Total = rowSums(pick(all_of(month.abb)))) %>%
  arrange(FTM)

add_total_row <- function(data) {
  totals <- data %>% summarise(across(where(is.numeric), sum))
  bind_rows(data, mutate(totals, FTM = "Total", .before = 1))
}
ipa_ftm_summary <- add_total_row(ipa_ftm_summary)
ipa_ftm_monthly <- add_total_row(ipa_ftm_monthly)

# Reconcile the two summaries with the included submissions before saving.
stopifnot(
  tail(ipa_ftm_summary$Total, 1L) == nrow(ipa_2026_detail),
  tail(ipa_ftm_monthly$Total, 1L) == nrow(ipa_2026_detail),
  identical(ipa_ftm_summary$FTM, ipa_ftm_monthly$FTM),
  identical(ipa_ftm_summary$Total, ipa_ftm_monthly$Total)
)


# 3. Save English summary and detail tables -------------------------------
# Save beside this run's original API exports, so each refresh is traceable.
write_csv(ipa_ftm_summary, file.path(output_dir, "ipa_2026_ftm_summary.csv"), na = "")
write_csv(ipa_ftm_monthly, file.path(output_dir, "ipa_2026_ftm_monthly.csv"), na = "")
write_csv(ipa_submission_detail, file.path(output_dir, "ipa_submission_detail.csv"), na = "")

summary_workbook <- createWorkbook()
addWorksheet(summary_workbook, "FTM Summary", gridLines = FALSE)
addWorksheet(summary_workbook, "Submissions", gridLines = FALSE)
title_style <- createStyle(fontSize = 16, textDecoration = "bold", fontColour = "#243447")
header_style <- createStyle(
  fgFill = "#294D73", fontColour = "#FFFFFF", textDecoration = "bold",
  halign = "center", valign = "center", wrapText = TRUE
)
total_style <- createStyle(fgFill = "#E8EEF5", textDecoration = "bold")

writeData(summary_workbook, "FTM Summary", "2026 IPA Survey Completions by FTM", startRow = 2, colNames = FALSE)
addStyle(summary_workbook, "FTM Summary", title_style, rows = 2, cols = 1)
writeData(
  summary_workbook, "FTM Summary",
  paste0("Latest completed submission per participant and age group within 2026. Export: ", run_id),
  startRow = 4, colNames = FALSE
)
writeData(summary_workbook, "FTM Summary", ipa_ftm_summary, startRow = 6, headerStyle = header_style)
annual_total_row <- 6L + nrow(ipa_ftm_summary)
addStyle(summary_workbook, "FTM Summary", total_style, rows = annual_total_row, cols = 1:8, gridExpand = TRUE)

unconfirmed_count <- sum(str_starts(ipa_2026_detail$`Attributed FTM`, "Unconfirmed:"))
summary_notes <- c(
  paste0("Reporting window: Jan 1, ", report_year, " inclusive to Jan 1, ", report_year + 1L, " exclusive; ", report_timezone, "."),
  paste0("Manual attribution uses the user-verified mapping; original FTM values remain in Submissions. Unconfirmed submissions: ", unconfirmed_count, "."),
  "Known test IDs are removed from both the summary and the submission detail.",
  paste0("Earlier same-age-group submissions excluded in 2026: ", duplicates_removed_2026, ". FTM and site follow the retained record."),
  "Counts reflect this API export. A blank surveyExportSince uses Ripple's server default."
)
writeData(summary_workbook, "FTM Summary", summary_notes, startRow = annual_total_row + 2L, colNames = FALSE)
monthly_title_row <- annual_total_row + length(summary_notes) + 4L
writeData(summary_workbook, "FTM Summary", "Monthly completions by FTM (submissions)", startRow = monthly_title_row, colNames = FALSE)
writeData(summary_workbook, "FTM Summary", ipa_ftm_monthly, startRow = monthly_title_row + 1L, headerStyle = header_style)
addStyle(
  summary_workbook, "FTM Summary", total_style,
  rows = monthly_title_row + 1L + nrow(ipa_ftm_monthly), cols = 1:14, gridExpand = TRUE
)
writeData(
  summary_workbook, "FTM Summary",
  "The current month may be partial; future-month zeros indicate no records in this export.",
  startRow = monthly_title_row + nrow(ipa_ftm_monthly) + 3L, colNames = FALSE
)
setColWidths(summary_workbook, "FTM Summary", cols = 1, widths = 36)
setColWidths(summary_workbook, "FTM Summary", cols = 2:14, widths = 16)
setRowHeights(summary_workbook, "FTM Summary", rows = c(6, monthly_title_row + 1L), heights = 30)

writeData(
  summary_workbook, "Submissions",
  paste0("Source: original Ripple survey exports in ", output_dir), startRow = 1, colNames = FALSE
)
writeData(
  summary_workbook, "Submissions",
  "Latest completion per participant, age group and year; test accounts removed. Non-2026 records are excluded from the summary.",
  startRow = 2, colNames = FALSE
)
if (nrow(ipa_submission_detail)) {
  writeDataTable(summary_workbook, "Submissions", ipa_submission_detail, startRow = 4, tableName = "IPASubmissions", tableStyle = "TableStyleMedium2")
} else {
  writeData(summary_workbook, "Submissions", ipa_submission_detail, startRow = 4, headerStyle = header_style)
}
setColWidths(summary_workbook, "Submissions", cols = 1:17, widths = c(18, 23, 29, 21, 24, 20, 22, 36, 25, 42, 65, 28, 22, 20, 22, 25, 48))
setRowHeights(summary_workbook, "Submissions", rows = 4, heights = 30)
freezePane(summary_workbook, "Submissions", firstActiveRow = 5, firstActiveCol = 4)
if (nrow(ipa_submission_detail)) {
  addStyle(
    summary_workbook, "Submissions", createStyle(numFmt = "yyyy-mm-dd hh:mm"),
    rows = 5:(nrow(ipa_submission_detail) + 4L), cols = 5, gridExpand = TRUE
  )
}
summary_path <- file.path(output_dir, "IPA_survey_2026_FTM_summary.xlsx")
saveWorkbook(summary_workbook, summary_path, overwrite = TRUE)
print(ipa_ftm_summary, n = Inf, width = Inf)
message("Summary saved: ", summary_path)
