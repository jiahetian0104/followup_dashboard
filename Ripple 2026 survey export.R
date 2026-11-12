# Optional survey-only export. Uses the same extraction/corrections as the dashboard.
# For the complete dashboard refresh, run IPA Data.R instead.
suppressPackageStartupMessages({library(tidyverse); library(httr2); library(readxl); library(openxlsx)})
script_file <- tryCatch(sys.frame(1)$ofile, error=function(e) NULL)
if (is.null(script_file)) {
  argument <- commandArgs(FALSE)[grepl("^--file=",commandArgs(FALSE))]
  if (length(argument)) script_file <- sub("^--file=","",argument[[1]])
}
SCRIPT_DIR <- Sys.getenv("RIPPLE_SURVEY_SCRIPT_DIR","")
if (!nzchar(SCRIPT_DIR)) SCRIPT_DIR <- if (!is.null(script_file)) dirname(script_file) else
  if (basename(getwd())=="followup_dashboard") getwd() else file.path(getwd(),"followup_dashboard")
SCRIPT_DIR <- normalizePath(SCRIPT_DIR,mustWork=TRUE)
if (file.exists(file.path(SCRIPT_DIR, ".Renviron.ipa"))) readRenviron(file.path(SCRIPT_DIR, ".Renviron.ipa"))
for (module in c("ipa_matching.R","ipa_sources.R","ipa_surveys.R")) source(file.path(SCRIPT_DIR,module),local=TRUE)
auth <- Sys.getenv("RIPPLE_AUTH_KEY","")
if (!nzchar(auth)) auth <- read_literal_config(file.path(SCRIPT_DIR,"Ripple 2026 3-20 study event.R"),"auth_key")$auth_key
if (is.null(auth) || !nzchar(auth)) stop("Set RIPPLE_AUTH_KEY")
ripple <- fetch_ripple_2026(auth)
ripple_survey_6_35_month <- ripple$infant
ripple_survey_3_20_year <- ripple$child
surveys <- build_ipa_surveys(ripple$infant,ripple$child)
ipa_submission_history <- surveys$history
ipa_submission_detail <- surveys$completions
ipa_2026_detail <- surveys$completions
ipa_ftm_summary <- surveys$completions %>% count(`Attributed FTM`,`Survey age group`,name="Completions") %>%
  pivot_wider(names_from=`Survey age group`,values_from=Completions,values_fill=0L) %>% rename(FTM=`Attributed FTM`)
ipa_ftm_monthly <- surveys$completions %>% mutate(Month=format(`Completed at`,"%Y-%m",tz="America/New_York")) %>%
  count(`Attributed FTM`,Month,name="Completions") %>%
  pivot_wider(names_from=Month,values_from=Completions,values_fill=0L) %>% rename(FTM=`Attributed FTM`)
run_id <- format(Sys.time(),"%Y-%m-%d_%H%M%S",tz="America/Detroit")
output_dir <- file.path(SCRIPT_DIR,"survey_exports",run_id)
if (dir.exists(output_dir)) stop("Export folder already exists")
dir.create(output_dir,recursive=TRUE)
exports <- list("ripple_surveys_6_35_month.csv"=ripple$infant,"ripple_surveys_3_20_year.csv"=ripple$child,
  "ipa_submission_history.csv"=surveys$history,"ipa_submission_detail.csv"=surveys$completions,
  "ipa_2026_ftm_summary.csv"=ipa_ftm_summary,"ipa_2026_ftm_monthly.csv"=ipa_ftm_monthly,
  "ipa_survey_latest.csv"=surveys$latest)
walk2(exports,names(exports),~write_csv(.x,file.path(output_dir,.y),na=""))
write_csv(tibble(schema_version="ipa-2026-v2",exported_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z",tz="America/Detroit"),
  survey_export_since="2026-01-01",completion_rule="First completed survey per child/age band within 2026",
  completions=nrow(surveys$completions)),file.path(output_dir,"manifest.csv"))
write.xlsx(list("FTM Summary"=ipa_ftm_summary,"Monthly"=ipa_ftm_monthly,
  "First completions"=surveys$completions,"All submissions"=surveys$history),
  file.path(output_dir,"IPA_survey_2026_FTM_summary.xlsx"),asTable=TRUE,overwrite=FALSE)
message("Survey export ready: ",nrow(surveys$completions)," completions. ",output_dir)
