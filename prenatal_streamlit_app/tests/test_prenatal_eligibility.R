# Synthetic fixtures; no live Ripple call. Run from the workspace root.
library(dplyr)
expr <- as.list(parse('followup_dashboard/PRENATAL_2026.R'))
for (nm in c('normalize_prenatal_ids','build_prenatal_powerbi')) {
  fun <- Filter(function(x) is.call(x) && identical(x[[1]],as.name('<-')) && identical(x[[2]],as.name(nm)),expr)
  eval(fun[[1]])
}
p <- tibble(globalId=c('AAA111-01-0 (1)','BBB222-01-0 (2)','CCC333-01-0 (3)','DDD444-01-0 (4)','EEE555-01-0 (5)'),customId=c('4101P1','4401P1','4701P1','4102P1','4402P1'),statusId='Active')
p <- normalize_prenatal_ids(p)
fields <- c('consent_date','child_dob','pn1_survey','pn1_anthro','pn1_urine','pn1_blood','pn2_survey','pn2_anthro','pn2_urine','pn2_blood','actigraphy','event.household_water.completedDate','cordblood','mmra','estduedate','pregnancy_loss')
for (col in fields) p[[col]] <- as.Date(rep(NA_character_,5))
p$consent_date <- as.Date(c('2026-01-01','2025-12-31',NA,'2026-12-31','2026-06-01'))
p$estduedate <- as.Date(c('2026-03-26','2026-03-25','2027-03-25','2027-03-26',NA))
p$pn1_survey[1:2] <- as.Date(c('2026-02-01','2026-02-02'))
p$pn2_survey[3] <- as.Date('2027-01-01')
p$`event.household_water.completedDate`[1] <- as.Date('2026-02-03')
f <- p %>% select(globalId,customId,echo_id,PIN)
for (col in c('child_urine','child_stool','mom_hair','mom_bs','breast_milk')) f[[col]] <- as.Date(rep(NA_character_,5))
r <- build_prenatal_powerbi(p,f,2026L,as.Date('2026-10-07'))
pn1 <- r$progress %>% filter(event_code=='pn1_survey') %>% arrange(customId)
check_row <- function(id, code) r$progress %>% filter(customId==id,event_code==code)
stopifnot(nrow(r$progress)==60,
          check_row('4101P1','pn1_survey')$`Complete Status`=='Complete',
          check_row('4102P1','actigraphy')$`Complete Status`=='Incomplete',
          check_row('4401P1','pn1_survey')$`Complete Status`=='Not Eligible',
          check_row('4401P1','pn1_survey')$`Has Completion Record`,
          !is.na(check_row('4401P1','pn1_survey')$`Complete Date`),
          is.na(check_row('4701P1','pn1_survey')$Eligible),
          check_row('4101P1','pn2_survey')$Eligible,
          !check_row('4401P1','pn2_survey')$Eligible,
          check_row('4701P1','pn2_survey')$Eligible,
          check_row('4701P1','pn2_survey')$`Complete Status`=='Complete',
          !check_row('4102P1','pn2_survey')$Eligible,
          is.na(check_row('4402P1','pn2_survey')$Eligible),
          check_row('4101P1','water')$`Complete Status`=='Not Eligible',
          check_row('4101P1','water')$`Has Completion Record`,
          check_row('4102P1','water')$Eligible,
          is.na(check_row('4701P1','water')$Eligible),
          sum(r$monthly$event_count)==nrow(r$events),
          sum(r$events$event_code=='pn1_survey')==2)
# All four PN2 elements share the same EDD rule; PN1/Actigraphy share enrollment.
stopifnot(all(r$progress$Eligible[r$progress$customId=='4101P1' & grepl('^pn[12]_|^actigraphy$',r$progress$event_code)]))
empty <- build_prenatal_powerbi(p[0,],f[0,],2026L,as.Date('2026-10-07'))
stopifnot(nrow(empty$progress)==0)
cat('PASS: enrollment boundaries, EDD minus 84-day year boundaries, missing dates, Water eligibility, actual completion retention, all related items, empty data and count reconciliation\n')
pp <- p
pp$estduedate <- as.Date('2026-10-07') + c(0,-1,2,3,4)
pp$pregnancy_loss[3] <- as.Date('2026-10-06')
pp$child_dob[4] <- as.Date('2026-10-06')
pp$statusId[5] <- '0-5 Month Complete'
z <- build_prenatal_powerbi(pp,f,2026L,as.Date('2026-10-07'))
zz <- z$progress %>% distinct(participant_key,projected_birth)
stopifnot(sum(zz$projected_birth)==1,sum(z$events$event_code=='estduedate')==1,
          !anyNA(z$progress$projected_birth))
cat('PASS: projection report-date boundary, overdue/birth/loss/status exclusions and long-table reconciliation\n')

water_cases <- p
water_cases$consent_date <- as.Date(c("2026-02-28", "2026-03-01", "2026-12-31", "2027-01-01", NA))
water_test <- build_prenatal_powerbi(water_cases,f,2026L,as.Date("2026-10-07"))
water <- water_test$progress %>% filter(event_code=="water")
expected <- tibble(customId=water_cases$customId, eligible=c(FALSE,TRUE,TRUE,FALSE,NA))
check <- left_join(water,expected,by="customId")
stopifnot(identical(check$Eligible, check$eligible),
          all(check$`Eligibility Date`==check$consent_date,na.rm=TRUE),
          all(check$`Eligibility Confirmed`==!is.na(check$Eligible)))
cat("PASS: Water implementation date inclusive, prior day exclusion, report-year scope and missing enrollment dates\n")
