# CHARM IPA 2026 refresh entry point. Run with Source or Rscript.
# Required: CALENDLY_TOKEN; RIPPLE_AUTH_KEY can reuse existing local Ripple config.
# Optional: IPA_CROSSWALK_PATH, IPA_DASHBOARD_APP_DIR, IPA_RAW_RUN (offline replay).
suppressPackageStartupMessages({library(tidyverse); library(httr2); library(readxl)})
script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file)) {
  argument <- commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))]
  if (length(argument)) script_file <- sub("^--file=", "", argument[[1]])
}
script_dir <- Sys.getenv("IPA_SCRIPT_DIR", unset = "")
if (!nzchar(script_dir)) script_dir <- if (!is.null(script_file)) dirname(script_file) else
  if (basename(getwd()) == "followup_dashboard") getwd() else file.path(getwd(), "followup_dashboard")
script_dir <- normalizePath(script_dir, mustWork = TRUE)
if (file.exists(file.path(script_dir, ".Renviron.ipa"))) readRenviron(file.path(script_dir, ".Renviron.ipa"))
for (module in c("ipa_matching.R", "ipa_fields.R", "ipa_surveys.R", "ipa_sources.R", "ipa_activity.R"))
  source(file.path(script_dir, module), local = TRUE)

app_dir <- Sys.getenv("IPA_DASHBOARD_APP_DIR", file.path(script_dir, "ipa_streamlit_app"))
data_dir <- file.path(app_dir, "data")
report_year <- 2026L
run_time <- Sys.time()
run_id <- format(run_time, "%Y-%m-%d_%H%M%S", tz = "America/Detroit")
replay <- Sys.getenv("IPA_RAW_RUN", "")
if (nzchar(replay)) {
  raw <- readRDS(file.path(replay, "api_inputs.rds"))
  if (!identical(raw$schema_version, "ipa-2026-v2")) stop("Unsupported raw input schema")
  source_mode <- "Saved API response replay"
  raw_path <- normalizePath(replay, mustWork = TRUE)
} else {
  auth <- Sys.getenv("RIPPLE_AUTH_KEY", "")
  if (!nzchar(auth)) {
    config <- read_literal_config(file.path(script_dir, "Ripple 2026 3-20 study event.R"), "auth_key")
    auth <- config$auth_key
  }
  token <- Sys.getenv("CALENDLY_TOKEN", "")
  if (!nzchar(token) && exists("calendly_token", inherits = TRUE)) token <- get("calendly_token", inherits = TRUE)
  if (is.null(auth) || !nzchar(auth) || !nzchar(token)) stop("Set RIPPLE_AUTH_KEY and CALENDLY_TOKEN before refresh")
  crosswalk <- Sys.getenv("IPA_CROSSWALK_PATH", if (Sys.info()["sysname"] == "Windows")
    "Z:/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/CHARM_Cohort_List.xlsx" else
    "/Volumes/Groups/ECHO/CHARM/Data/Miscellaneous/Global Crosswalk/CHARM_Cohort_List.xlsx")
  if (!file.exists(crosswalk)) stop("Crosswalk unavailable; set IPA_CROSSWALK_PATH")
  since <- "2026-01-01"
  ripple <- fetch_ripple_2026(auth, since)
  infant <- ripple$infant
  child <- ripple$child
  catalog <- build_participant_catalog(infant, child, crosswalk)
  message("Fetching 2026 Calendly bookings and invitees")
  calendly_raw <- fetch_calendly_2026(token)
  raw <- list(schema_version = "ipa-2026-v2", fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "America/Detroit"),
    survey_export_since = since, ripple_6_35 = infant, ripple_3_20 = child,
    participant_catalog = catalog, calendly = calendly_raw)
  raw_path <- file.path(data_dir, "raw", run_id)
  if (dir.exists(raw_path)) stop("Run ID collision; try again")
  dir.create(raw_path, recursive = TRUE)
  saveRDS(raw, file.path(raw_path, "api_inputs.rds"))
  source_mode <- "Live API refresh"
}
message("Resolving Calendly identity and building survey/event/specimen tables")
calendly <- clean_calendly_2026(raw$calendly, raw$participant_catalog)
exports <- build_2026_exports(raw$ripple_6_35, raw$ripple_3_20, calendly, report_year)
manifest <- tibble(schema_version = "ipa-2026-v2", report_year = report_year,
  generated_at = format(run_time, "%Y-%m-%d %H:%M:%S %Z", tz = "America/Detroit"), snapshot_id = run_id,
  fetched_at = raw$fetched_at, ripple_fetched_at = raw$ripple_fetched_at %||% raw$fetched_at,
  calendly_fetched_at = raw$calendly_fetched_at %||% raw$fetched_at, source_mode = source_mode, survey_export_since = raw$survey_export_since,
  raw_run = raw_path, completion_rule = "First completed survey per child/age band within 2026",
  specimen_owner_rule = "Latest 2026 same-band survey; latest 2026 same-band booking; Unassigned",
  survey_submissions_2026 = sum(exports$ipa_survey_submissions.csv$`Completion year` == report_year),
  survey_completions = nrow(exports$ipa_survey_completions.csv),
  survey_duplicate_groups = nrow(exports$ipa_survey_duplicate_summary.csv),
  survey_duplicate_extra_submissions = sum(exports$ipa_survey_duplicate_summary.csv$`Extra Submissions`),
  bookings = nrow(exports$ipa_appointment_history.csv),
  booking_qc_rows = nrow(exports$ipa_appointment_history_qc.csv),
  survey_event_qc_rows = nrow(exports$ipa_survey_event_qc.csv),
  biospecimen_collections = nrow(exports$biospecimen_collections.csv))
exports$manifest.csv <- manifest

# Complete a new immutable snapshot before replacing latest. Old snapshots remain intact.
snapshot <- file.path(data_dir, "snapshots", run_id)
if (dir.exists(snapshot)) stop("Snapshot already exists")
staging <- file.path(data_dir, paste0(".staging_", run_id))
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
walk2(exports, names(exports), ~ write_csv(.x, file.path(staging, .y), na = ""))
for (name in names(exports)) {
  check <- read_csv(file.path(staging, name), col_types = cols(.default = col_character()), show_col_types = FALSE)
  if (nrow(check) != nrow(exports[[name]]) || nrow(problems(check))) stop("Output verification failed: ", name)
}
dir.create(dirname(snapshot), recursive = TRUE, showWarnings = FALSE)
if (!file.rename(staging, snapshot)) stop("Could not finalize snapshot")
latest <- file.path(data_dir, "latest")
next_latest <- file.path(data_dir, paste0(".next_", run_id))
dir.create(next_latest)
if (!all(file.copy(list.files(snapshot, full.names = TRUE), next_latest))) stop("Could not prepare latest")
previous <- file.path(data_dir, paste0(".previous_", run_id))
if (dir.exists(latest) && !file.rename(latest, previous)) stop("Could not move prior latest; snapshot is intact")
if (!file.rename(next_latest, latest)) {
  if (dir.exists(previous)) file.rename(previous, latest)
  stop("Could not switch latest; previous restored")
}
# Preserve the replaced batch for audit without reading it into calculations.
if (dir.exists(previous)) {
  if (!file.rename(previous, file.path(data_dir, "snapshots", paste0("replaced_", run_id))))
    warning("Prior latest is retained in ", previous)
}
message("2026 dashboard ready. Completions: ", nrow(exports$ipa_survey_completions.csv),
  "; bookings: ", nrow(exports$ipa_appointment_history.csv),
  "; specimen collections: ", nrow(exports$biospecimen_collections.csv), ". Snapshot: ", run_id)
