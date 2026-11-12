suppressPackageStartupMessages(library(tidyverse))
for (module in c("ipa_matching.R", "ipa_fields.R", "ipa_surveys.R", "ipa_activity.R"))
  source(file.path("followup_dashboard", module))

fixture <- function(ids, cohort) {
  data <- tibble(globalId = ids, customId = paste0("custom",seq_along(ids)), familyId = "family",
    firstName = "Child", lastName = "Test", birthday = "2024-01-01", statusId = "Withdrawn", tags = NA_character_)
  groups <- if (cohort == "infant") c("12-23 Month", "24-35 Month") else
    c("3-5 Year", "6-10 Year", "11-17 Year", "18-20 Year")
  for (group in groups) for (index in 0:1) {
    prefix <- paste0(group," IPA Survey.",index,".")
    data[[paste0(prefix,"EnrollNow Survey Completed At")]] <- NA_character_
    data[[paste0(prefix,if (group == "24-35 Month") "ftm_member" else "IPA_FTM")]] <- NA_character_
    data[[paste0(prefix,"IPA_Site")]] <- NA_character_
  }
  for (column in c(ipa_event_map$complete_col,ipa_event_map$scheduled_col,ripple_task_map$completed_col)) data[[column]] <- "false"
  for (column in c(ipa_event_map$complete_date_col,ipa_event_map$scheduled_date_col,ripple_task_map$completed_date_col)) data[[column]] <- NA_character_
  data
}
infant <- fixture(c("A (001)","B (002)","C (003)"),"infant")
child <- fixture("D (004)","child")
# Intentionally put the earliest timestamp at the larger submission index.
infant$`12-23 Month IPA Survey.0.EnrollNow Survey Completed At` <- c("06/01/2026 10:00", NA, NA)
infant$`12-23 Month IPA Survey.1.EnrollNow Survey Completed At` <- c("02/01/2026 10:00", NA, NA)
infant$`12-23 Month IPA Survey.0.IPA_FTM` <- c("Nicole",NA,NA)
infant$`12-23 Month IPA Survey.1.IPA_FTM` <- c("Anna",NA,NA)
infant$`12-23 Month IPA Survey.0.IPA_Site` <- c("Latest site",NA,NA)
infant$`12-23 Month IPA Survey.1.IPA_Site` <- c("First site",NA,NA)
# B is event-only. C has a dated collection but no completed survey.
infant$event.12_23mo_ipa_complete.completed <- c("false","true","false")
infant$event.12_23mo_ipa_complete.completedDate <- c(NA,"03/01/2026",NA)
infant$event.12_23mo_child_urine_diaper_.completed <- "true"
infant$event.12_23mo_child_urine_diaper_.completedDate <- c("03/01/2026",NA,"12/31/2025")
infant$event.12_23mo_child_bloodspot.completed <- c("false","true","true")
infant$event.12_23mo_child_bloodspot.completedDate <- c(NA,"03/01/2026","bad date")
bookings <- tibble(`Participant ID`=c("A","B","B",NA),`Participant Cohort`="6-35 month",
  `IPA Age Band`="12_23_month",`IPA Year`=2026L,`Calendly FTM`=c("Jody","Jo","Jo","Jo"),
  `Event UUID`=paste0("e",1:4),`Invitee UUID`=paste0("i",1:4),`Event Type Name`="Detroit Assessment",
  `Event Start Date`=as.Date("2026-03-01"),`Start Date & Time`="2026-03-01T12:00:00Z",
  `Event Created Date & Time`="2026-02-01T12:00:00Z",Canceled=c(FALSE,TRUE,TRUE,FALSE),
  `No Show`=c(TRUE,FALSE,FALSE,NA),Rescheduled=c(FALSE,TRUE,FALSE,NA),
  `Old Invitee URI`=NA_character_,`New Invitee URI`=c(NA,"successor",NA,NA),
  `Participant Match Method`="child_name",`Calendly QA Flag`=NA_character_,`Calendly FTM QA Flag`=NA_character_)
exports <- build_2026_exports(infant,child,bookings)
first <- exports$ipa_survey_completions.csv
stopifnot(nrow(first)==1L,first$`Attributed FTM`=="Anna",first$Site=="First site",
  first$`Completion Date`==as.Date("2026-02-01"),nrow(exports$ipa_survey_submissions.csv)==2L)
stopifnot(exports$ipa_survey_latest.csv$`Attributed FTM`=="Nicole")
specimens <- exports$biospecimen_collections.csv
stopifnot(nrow(specimens)==2L, specimens$FTM[specimens$`Participant ID`=="A"]=="Nicole",
  specimens$FTM[specimens$`Participant ID`=="B"]=="Jo", all(specimens$statusId=="Withdrawn"))
qc <- exports$ipa_survey_event_comparison.csv
stopifnot(qc$`QC Status`[qc$`Participant ID`=="A" & qc$`IPA Age Band`=="12_23_month"]=="Survey only",
  qc$`QC Status`[qc$`Participant ID`=="B" & qc$`IPA Age Band`=="12_23_month"]=="Event only")
workload <- exports$participant_ipa_ftm_records.csv
stopifnot(sum(workload$`Appointment Count`)==3L, sum(workload$`Cancel Count`)==1L,
  sum(workload$`Reschedule Count`)==1L, sum(workload$`No-show Count`)==1L,
  nrow(exports$ipa_appointment_history.csv)==4L,
  nrow(exports$ipa_appointment_history_qc.csv)==1L,
  nrow(exports$biospecimen_qc.csv)==2L,
  !any(grepl("Weighted|Responsible|Current FTM",names(workload))))
# Repeat computation is independent of previous outputs.
stopifnot(identical(exports,build_2026_exports(infant,child,bookings)))
# Unknown flags propagate instead of being zeroed.
bookings$`No Show`[1] <- NA
unknown <- build_2026_exports(infant,child,bookings)
stopifnot(is.na(unknown$participant_ipa_ftm_records.csv$`No-show Count`[unknown$participant_ipa_ftm_records.csv$FTM=="Jody"]))
# Preserve user-confirmed placeholder mapping from the original survey script.
manual <- fixture("SMQ889-01-A (911)","infant")
manual$`12-23 Month IPA Survey.0.EnrollNow Survey Completed At` <- "02/01/2026 10:00"
manual$`12-23 Month IPA Survey.0.IPA_FTM` <- "Item 1"
manual$`12-23 Month IPA Survey.0.IPA_Site` <- "Detroit"
stopifnot(build_ipa_surveys(manual,child)$completions$`Attributed FTM`=="Kowlini")
# Earlier years and later repeat records do not change the first 2026 completion.
manual$`12-23 Month IPA Survey.1.EnrollNow Survey Completed At` <- "02/01/2025 10:00"
manual$`12-23 Month IPA Survey.1.IPA_FTM` <- "Anna"
manual$`12-23 Month IPA Survey.1.IPA_Site` <- "Other"
stopifnot(nrow(build_ipa_surveys(manual,child)$completions)==1L)
cat("2026 survey first/latest, specimen attribution, double-sided QC, actions and replay tests passed\n")
if (nzchar(Sys.getenv("IPA_TEST_OUTPUT_DIR"))) {
  path <- Sys.getenv("IPA_TEST_OUTPUT_DIR")
  dir.create(file.path(path,"latest"),recursive=TRUE,showWarnings=FALSE)
  walk2(exports,names(exports),~write_csv(.x,file.path(path,"latest",.y),na=""))
  write_csv(tibble(schema_version="ipa-2026-v2",generated_at="2026-10-19 10:00:00 EDT",
    fetched_at="2026-10-19T10:00:00-0400",source_mode="Synthetic test fixture"),file.path(path,"latest","manifest.csv"))
}
# Newly returned 24-35 Month IPA_FTM schema remains compatible with ftm_member.
renamed <- fixture("E (005)","infant")
names(renamed)[names(renamed)=="24-35 Month IPA Survey.0.ftm_member"] <- "24-35 Month IPA Survey.0.IPA_FTM"
renamed$`24-35 Month IPA Survey.0.EnrollNow Survey Completed At` <- "03/01/2026 10:00"
renamed$`24-35 Month IPA Survey.0.IPA_FTM` <- "Jolyn"
renamed$`24-35 Month IPA Survey.0.IPA_Site` <- "eastLansing"
renamed_result <- build_ipa_surveys(renamed,child)$completions
stopifnot(renamed_result$`Attributed FTM`=="Jo",renamed_result$Site=="East Lansing",
  renamed_result$`Original site value`=="eastLansing")
# Keep the submission-specific site/FTM corrections, not just placeholder mappings.
corrected <- fixture("F (006)","child")
corrected$customId <- "82098M1"
corrected$`6-10 Year IPA Survey.0.EnrollNow Survey Completed At` <- "03/29/2026 20:39"
corrected$`6-10 Year IPA Survey.0.IPA_FTM` <- "Anna"
corrected$`6-10 Year IPA Survey.0.IPA_Site` <- "grandrapids"
stopifnot(build_ipa_surveys(infant,corrected)$completions %>% filter(`Participant ID`=="F") %>% pull(Site)=="Detroit")
# Source schema changes must not silently turn recorded events into empty results.
broken <- infant %>% select(-all_of(ipa_event_map$complete_col[[1]]))
stopifnot(inherits(try(build_2026_exports(broken,child,bookings),silent=TRUE),"try-error"))
cat("Field alias, canonical site names, original corrections and source-schema checks passed\n")
# Same-day, different-band evidence is one paired conflict, not two unrelated missing records.
conflict_child <- child
conflict_child$`6-10 Year IPA Survey.0.EnrollNow Survey Completed At` <- '06/09/2026 10:00'
conflict_child$`6-10 Year IPA Survey.0.IPA_FTM` <- 'Jo'
conflict_child$`6-10 Year IPA Survey.0.IPA_Site` <- 'Detroit'
conflict_child$event.2026_11_17yr_ipa_complete.completed <- 'true'
conflict_child$event.2026_11_17yr_ipa_complete.completedDate <- '06/09/2026'
old_only <- fixture('OLD (007)','infant')
old_only$event.12_23mo_ipa_complete.completed <- 'true'
old_only$event.12_23mo_ipa_complete.completedDate <- '12/19/2025'
conflict_exports <- build_2026_exports(bind_rows(infant,old_only),conflict_child,bookings)
pairs <- conflict_exports$ipa_age_band_conflicts.csv
stopifnot(nrow(pairs)==1L,pairs$`Survey Age Band`=='6_10yr',pairs$`Event Age Band`=='11_17yr',
  pairs$`Completion Date`==as.Date('2026-06-09'))
sides <- conflict_exports$ipa_survey_event_comparison.csv %>% filter(`Participant ID`=='D', `QC Status`=='Age-band conflict')
stopifnot(nrow(sides)==2L,all(!is.na(sides$`Other Source Age Band`)),
  !'OLD' %in% conflict_exports$ipa_survey_event_qc.csv$`Participant ID`,
  'OLD' %in% conflict_exports$ipa_event_evidence.csv$`Participant ID`)
# Two genuinely completed bands on one day are not a cross-band conflict.
conflict_child$`11-17 Year IPA Survey.0.EnrollNow Survey Completed At` <- '06/09/2026 11:00'
conflict_child$`11-17 Year IPA Survey.0.IPA_FTM` <- 'Jo'
conflict_child$`11-17 Year IPA Survey.0.IPA_Site` <- 'Detroit'
conflict_child$event.2026_6_10yr_ipa_complete.completed <- 'true'
conflict_child$event.2026_6_10yr_ipa_complete.completedDate <- '06/09/2026'
stopifnot(nrow(build_2026_exports(infant,conflict_child,bookings)$ipa_age_band_conflicts.csv)==0L)
# Date-only discrepancy preserves both dates and the exact exported source field.
date_infant <- infant
date_infant$event.12_23mo_ipa_complete.completed[1] <- 'true'
date_infant$event.12_23mo_ipa_complete.completedDate[1] <- '02/03/2026'
date_row <- build_2026_exports(date_infant,child,bookings)$ipa_survey_event_qc.csv %>%
  filter(`Participant ID`=='A',`IPA Age Band`=='12_23_month')
stopifnot(date_row$`QC Issue`=='Completion date difference',date_row$`Date Difference Days`==2L,
  date_row$`Event Date Raw`=='02/03/2026',date_row$`Event Date Field`=='event.12_23mo_ipa_complete.completedDate')
cat('Paired age-band conflicts, legitimate same-day bands, historical QC exclusion and date provenance checks passed\n')
# Duplicate QC uses child/age-band/year, preserves both submissions, and selects
# the same chronological first record as completion credit (not submission zero).
duplicates <- build_ipa_survey_duplicate_qc(build_ipa_surveys(infant,child)$history)
stopifnot(nrow(duplicates$summary)==1L,nrow(duplicates$detail)==2L,
  duplicates$summary$`Submission Count`==2L,duplicates$summary$`Extra Submissions`==1L,
  duplicates$summary$`First FTM`=='Anna',duplicates$summary$`Latest FTM`=='Nicole',
  sum(duplicates$detail$`Counted in IPA`)==1L,
  duplicates$detail$`Submission index`[duplicates$detail$`Counted in IPA`]==1L)
# An earlier-year submission is not a duplicate of the one reporting-year row.
stopifnot(nrow(build_ipa_survey_duplicate_qc(build_ipa_surveys(manual,child)$history)$summary)==0L)
# Different age bands are separate assessments, not duplicates.
stopifnot(nrow(build_ipa_survey_duplicate_qc(build_ipa_surveys(infant[2:3,],conflict_child)$history)$summary)==0L)
# Exact timestamp ties follow the completion rule's smaller-index tie-break.
tied <- build_ipa_surveys(infant,child)$history
tied$`Completed at` <- rep(tied$`Completed at`[1],nrow(tied))
tied_qc <- build_ipa_survey_duplicate_qc(tied)
stopifnot(tied_qc$detail$`Submission index`[tied_qc$detail$`Counted in IPA`]==0L)
cat('Survey duplicate scope, retained first submission, cross-age/year exclusions and ties passed\n')
