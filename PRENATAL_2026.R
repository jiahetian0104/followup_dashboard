# PRENATAL_2026
# Export Ripple prenatal data and create two follow-up CSV reports.
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
  guess_max = 2000
)

# Select prenatal milestones and give the exported event names shorter labels.
prenatal_2026 <- study_prenatal %>%
  select(
    globalId, customId, statusId,
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
  mutate(across(-c(globalId, customId, statusId), ~ as.Date(.x, format = "%m/%d/%Y")))

write_csv(prenatal_2026, file.path(output_dir, "prenatal_2026.csv"), na = "")

# Build the 0–5 month follow-up extract from the same prenatal export.
study_0to5 <- study_prenatal %>%
  select(
    globalId, customId, statusId, birthday,
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
  # Keep birthday in its original exported form; convert only event dates.
  mutate(across(-c(globalId, customId, statusId, birthday),
                ~ as.Date(.x, format = "%m/%d/%Y")))

write_csv(study_0to5, file.path(output_dir, "study_0to5.csv"), na = "")
