# Export the requested Ripple survey responses for both studies.
# Run this file with Source in RStudio or with Rscript.
# Results remain available as ripple_survey_3_20_year and
# ripple_survey_6_35_month; original CSV responses are saved separately.

library(httr2)
library(readr)

find_script_dir <- function() {
  source_files <- lapply(sys.frames(), function(frame) frame$ofile)
  source_files <- Filter(function(x) is.character(x) && length(x) == 1L, source_files)
  if (length(source_files)) {
    return(dirname(normalizePath(tail(source_files, 1L)[[1L]])))
  }
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    script_path <- gsub("~+~", " ", sub("^--file=", "", file_arg[[1L]]), fixed = TRUE)
    return(dirname(normalizePath(script_path, mustWork = TRUE)))
  }
  candidates <- c(getwd(), file.path(getwd(), "followup_dashboard"))
  found <- candidates[file.exists(file.path(candidates, "2026_6_35_month_dashboard.R"))]
  if (!length(found)) stop("Run from the project folder or use Source on this file.")
  normalizePath(found[[1L]])
}

SCRIPT_DIR <- find_script_dir()

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
