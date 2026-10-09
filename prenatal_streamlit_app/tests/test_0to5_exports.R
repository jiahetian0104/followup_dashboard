# Synthetic fixtures only; no API calls. Run from the workspace root.
library(dplyr)
library(readr)
code <- as.list(parse('followup_dashboard/PRENATAL_2026.R'))
assigns <- function(x, name) is.call(x) && identical(x[[1]], as.name('<-')) && identical(x[[2]], as.name(name))
eval(Filter(function(x) assigns(x,'parse_ripple_date'), code)[[1]])
timezone <- 'America/New_York'
stopifnot(identical(as.character(parse_ripple_date(c('3/1/2026','2026-03-01','2026-01-01T00:30:00Z','',NA))),
                   c('2026-03-01','2026-03-01','2025-12-31',NA,NA)))
invalid <- tryCatch(parse_ripple_date('not a date'), error=identity)
stopifnot(inherits(invalid,'error'))
report_year <- 2026L
report_as_of <- as.Date('2026-10-08')
powerbi_dir <- tempfile('month0to5-test-')
dir.create(powerbi_dir)
latest_dir <- file.path(powerbi_dir, "..", paste0(basename(powerbi_dir), "-latest"))
study_0to5 <- tibble(globalId=paste0('TEST',1:6), customId=paste0('410',1:6,'F1'),
                    echo_id=paste0('TEST',1:6), PIN=as.character(1:6), statusId='Active',
                    birthday=as.Date(c('2026-06-09','2026-05-08','2026-04-08','2026-06-09','2026-08-31','2026-10-09')),
                    dateCreated=as.Date(c('2026-01-01','2025-12-31',NA,'2026-12-31','2026-01-01','2026-02-01')))
fields <- c('postnatal_consent','survey','child_anthro','mom_anthro','visit_scheduled','diaper_sent','vib_request',
            'child_urine','child_stool','mom_hair','mom_bs','breast_milk','partner_contact')
for (field in fields) study_0to5[[field]] <- as.Date(rep(NA_character_,6))
study_0to5$survey[1:2] <- as.Date(c('2026-01-01','2026-10-01'))
study_0to5$visit_scheduled[1:2] <- as.Date(c('2026-01-02','2026-09-30'))
study_0to5$diaper_sent[1:2] <- as.Date(c('2026-01-03','2026-10-02'))
study_0to5$child_urine[1:2] <- as.Date(c('2026-10-01','2025-12-31'))
powerbi <- list(progress=tibble(participant_key=paste0('global:TEST',1:6),site='Detroit',
                               child_dob=as.Date(c('2026-06-08',rep(NA_character_,5)))))
# A P-category record can have the same creation year and child DOB; it is
# excluded from this follow-up population before calculating any denominator.
parent <- study_0to5[1,]
parent$customId <- '4101P1'
parent$globalId <- 'TESTP'
parent$echo_id <- 'TESTP'
study_0to5 <- bind_rows(study_0to5,parent)
powerbi$progress <- bind_rows(powerbi$progress,tibble(participant_key='global:TESTP',site='Detroit',child_dob=as.Date('2026-06-08')))
start <- which(vapply(code,assigns,logical(1),name='month0to5_definitions'))
for (i in seq.int(start,length(code))) eval(code[[i]])
stopifnot(identical(month0to5_participants$Eligible,c(TRUE,FALSE,NA,TRUE,TRUE,TRUE)),
          identical(month0to5_participants$age_months,c(4L,5L,6L,3L,1L,NA_integer_)),
          sum(month0to5_participants$aging_out)==1,
          month0to5_participants$birth_date[1]==as.Date('2026-06-08'),
          month0to5_participants$birth_date_source[1]=='child_dob',
          month0to5_participants$birth_date_source[2]=='birthday',
          month0to5_participants$age_out_date[5]==as.Date('2027-02-28'),
          nrow(month0to5_progress)==78,
          all(month0to5_progress$`Complete Status`[month0to5_progress$customId=='4102F1']=='Not Eligible'),
          nrow(month0to5_monthly)==624)
survey_summary <- month0to5_event_summary %>% filter(site=='All Sites',event_code=='survey')
stopifnot(survey_summary$`Eligible Count`==4,survey_summary$`Eligible Complete Count`==1,
          survey_summary$`Completed in Report Year`==2,
          survey_summary$`Eligibility Unknown Count`==1)
workflow <- month0to5_workflow %>% filter(site=='All Sites')
stopifnot(workflow$`Surveys Completed`==2,workflow$`Visits Scheduled`==2,
          workflow$`Linked Visits`==2,workflow$`Linked Diapers`==2)
# Calendar month-end anniversary: Aug 31 to Feb 28 is six completed months.
report_as_of <- as.Date('2027-02-28')
for (i in seq.int(start,length(code))) eval(code[[i]])
stopifnot(month0to5_participants$age_months[5]==6L, !month0to5_participants$aging_out[5])
cat('PASS: 0-5 month date parsing, creation-year eligibility, unknown dates, completion counts, linked workflow, DOB priority, aging boundaries and month-end deadlines\n')

stopifnot(file.exists(file.path(latest_dir,"month0to5_progress.csv")),
          identical(readLines(file.path(latest_dir,"month0to5_progress.csv")), readLines(file.path(powerbi_dir,"month0to5_progress.csv"))))
