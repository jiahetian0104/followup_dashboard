suppressPackageStartupMessages(library(tidyverse))
source("followup_dashboard/ipa_activity.R")
stopifnot(identical(ipa_activity_date(c("04/03/2026 10:00", "2026-04-03", NA)),
                    as.Date(c("2026-04-03", "2026-04-03", NA))))

# A: old booking rescheduled, new booking truly cancelled. B: completed.
# C: two hosts handled prior no-shows; only the completion-date host gets credit.
# D: two eligible hosts on completion day, so credit remains pending.
history <- tibble(
  `Participant ID` = c("A", "A", "B", "C", "C", "C", "D", "D"),
  `Participant Cohort` = "cohort", `IPA Age Band` = "x", `IPA Year` = 2026L,
  `Calendly FTM` = c("FTM 1", "FTM 1", "FTM 2", "FTM 1", "FTM 2", "FTM 2", "FTM 1", "FTM 2"),
  `Event UUID` = paste0("e", 1:8), `Invitee UUID` = paste0("i", 1:8),
  `Event Start Date` = as.Date(c("2026-04-01", "2026-04-02", "2026-04-03",
                               "2026-04-01", "2026-04-02", "2026-04-03", "2026-04-03", "2026-04-03")),
  Canceled = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  `No Show` = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE),
  Rescheduled = c(TRUE, rep(FALSE, 7)),
  `Old Invitee URI` = c(NA, "https://api.calendly.com/invitees/i1", rep(NA, 6)),
  `New Invitee URI` = c("https://api.calendly.com/invitees/i2", rep(NA, 7))
)
participants <- tibble(
  `Participant ID` = c("A", "B", "C", "D", "E"), `Participant Cohort` = "cohort",
  scheduled = TRUE, completed = c(FALSE, TRUE, TRUE, TRUE, TRUE),
  scheduled_date = "2026-04-03", completed_date = c(NA, "2026-04-03", "2026-04-03", "2026-04-03", NA)
)
event_map <- tibble(age_band = "x", scheduled_col = "scheduled", complete_col = "completed",
                    scheduled_date_col = "scheduled_date", complete_date_col = "completed_date")
out <- build_ipa_activity_exports(history, participants, event_map)
records <- out$participant_ipa_records.csv
ftm <- out$participant_ipa_ftm_records.csv
stopifnot(
  records$`Cancel Count`[records$`Participant ID` == "A"] == 1L,
  records$`Reschedule Count`[records$`Participant ID` == "A"] == 1L,
  records$`Appointment Count`[records$`Participant ID` == "A"] == 2L,
  records$`No-show Count`[records$`Participant ID` == "C"] == 2L,
  records$`Completion FTM`[records$`Participant ID` == "C"] == "FTM 2",
  records$`Completion FTM`[records$`Participant ID` == "D"] == "Pending verification",
  records$`Completion FTM`[records$`Participant ID` == "E"] == "Pending verification",
  sum(ftm$`Complete Count`) == 4L,
  sum(ftm$`Cancel Count`) == 1L,
  sum(ftm$`Reschedule Count`) == 1L,
  ftm$`Complete Count`[ftm$`Participant ID` == "C" & ftm$FTM == "FTM 1"] == 0L
)
# Duplicated source rows must not inflate counts; missing flags stay unknown.
unknown <- bind_rows(history, history[1, ])
unknown$Rescheduled[2] <- NA
out_unknown <- build_ipa_activity_exports(unknown, participants, event_map)
stopifnot(
  is.na(out_unknown$participant_ipa_records.csv$`Cancel Count`[
    out_unknown$participant_ipa_records.csv$`Participant ID` == "A"]),
  out_unknown$participant_ipa_records.csv$`Appointment Count`[
    out_unknown$participant_ipa_records.csv$`Participant ID` == "A"] == 2L
)
# Multiple successive reschedules count actions, not chains or affected people.
chain <- history[rep(1, 3), ]
chain$`Invitee UUID` <- c("chain1", "chain2", "chain3")
chain$Canceled <- c(TRUE, TRUE, FALSE)
chain$Rescheduled <- c(TRUE, TRUE, FALSE)
chain$`New Invitee URI` <- c("uri/chain2", "uri/chain3", NA)
chain$`Old Invitee URI` <- c(NA, "uri/chain1", "uri/chain2")
chain_out <- build_ipa_activity_exports(chain, participants, event_map)
stopifnot(
  chain_out$participant_ipa_records.csv$`Reschedule Count`[
    chain_out$participant_ipa_records.csv$`Participant ID` == "A"] == 2L,
  sum(chain_out$participant_ipa_records.csv$`Cancel Count`) == 0L
)
# Empty appointment exports still preserve Ripple completions for verification.
empty <- build_ipa_activity_exports(history[0, ], participants, event_map)
stopifnot(sum(empty$ftm_ipa_summary.csv$`Complete Count`) == 4L,
          sum(empty$ftm_ipa_summary.csv$`Appointment Count`) == 0L)
cat("IPA activity checks passed\n")

# Ripple's month/day/year dates must exclude previous-year completion flags.
dated <- participants
dated$completed_date[dated$`Participant ID` == "E"] <- "04/03/2025 10:00"
dated_out <- build_ipa_activity_exports(history, dated, event_map)
stopifnot(sum(dated_out$ftm_ipa_summary.csv$`Complete Count`) == 3L,
          !dated_out$participant_ipa_records.csv$`IPA Complete`[
            dated_out$participant_ipa_records.csv$`Participant ID` == "E"])

# No date match: credit goes to the latest same-band booking, even if canceled.
fallback_participants <- participants
fallback_participants$completed_date[fallback_participants$`Participant ID` == "B"] <- "2026-04-04"
fallback_history <- history
fallback_history$Canceled[fallback_history$`Participant ID` == "B"] <- TRUE
fallback_out <- build_ipa_activity_exports(fallback_history, fallback_participants, event_map)
b <- fallback_out$participant_ipa_records.csv %>% filter(`Participant ID` == "B")
stopifnot(b$`Completion FTM` == "FTM 2",
          b$`Completion Attribution` == "Latest same-age-band Calendly fallback",
          b$`Completion Fallback Appointment Date` == as.Date("2026-04-03"))

# Exact-date credit remains preferred even when a later booking has another host.
later <- history[history$`Participant ID` == "B", ]
later$`Event Start Date` <- as.Date("2026-04-05")
later$`Invitee UUID` <- "laterB"
later$`Calendly FTM` <- "FTM 1"
preferred_out <- build_ipa_activity_exports(bind_rows(history, later), participants, event_map)
b_preferred <- preferred_out$participant_ipa_records.csv %>% filter(`Participant ID` == "B")
stopifnot(b_preferred$`Completion FTM` == "FTM 2",
          b_preferred$`Completion Attribution` == "Matched completion date")

# A different age band cannot supply fallback credit; latest tied hosts stay pending.
other_band <- history[1, ]
other_band$`Participant ID` <- "E"
other_band$`IPA Age Band` <- "different_band"
different_out <- build_ipa_activity_exports(bind_rows(history, other_band), participants, event_map)
stopifnot(different_out$participant_ipa_records.csv$`Completion FTM`[
  different_out$participant_ipa_records.csv$`Participant ID` == "E" &
    different_out$participant_ipa_records.csv$`IPA Age Band` == "x"] == "Pending verification")

# Percentage counts every participant-age-band once per FTM, including cancel-only A.
summary <- out$ftm_ipa_summary.csv
ftm1 <- summary %>% filter(FTM == "FTM 1")
stopifnot(ftm1$`Participant Age Bands` == 3L,
          ftm1$`Weighted Points` == 0.25,
          ftm1$`Weighted IPA Performance` == 0.25 / 3,
          is.na(summary$`Weighted IPA Performance`[summary$FTM == "Pending verification"]))
cat("IPA fallback and weighted-performance checks passed\n")

# Cohort differences must not block participant/date matching.
cross_cohort <- history
cross_cohort$`Participant Cohort`[cross_cohort$`Participant ID` == 'B'] <- 'other cohort'
cross_out <- build_ipa_activity_exports(cross_cohort, participants, event_map)
stopifnot(cross_out$participant_ipa_records.csv$`Completion FTM`[
  cross_out$participant_ipa_records.csv$`Participant ID` == 'B' &
    cross_out$participant_ipa_records.csv$`IPA Complete`] == 'FTM 2')

# Previous-band date match, historical evidence, and same-band priority.
new_p <- participants[2, ] %>% mutate(`Participant ID`='P', firstName='Private', lastName='Name')
new_map <- event_map %>% mutate(age_band='6_10yr')
prev <- history[3, ] %>% mutate(`Participant ID`='P', `IPA Age Band`='3_5yr', `Calendly FTM`='Previous')
prev_out <- build_ipa_activity_exports(prev,new_p,new_map)
stopifnot(prev_out$participant_ipa_records.csv$`Completion FTM`[
  prev_out$participant_ipa_records.csv$`IPA Complete`] == 'Previous',
  !any(c('firstName','lastName') %in% names(prev_out$participant_ipa_records.csv)))
old <- prev %>% mutate(`IPA Age Band`='6_10yr', `IPA Year`=2025L,
  `Event Start Date`=as.Date('2025-05-01'), `Invitee UUID`='old', `Calendly FTM`='Historical')
old_out <- build_ipa_activity_exports(bind_rows(prev,old),new_p,new_map)
stopifnot(old_out$participant_ipa_records.csv$`Completion FTM`[
  old_out$participant_ipa_records.csv$`IPA Complete`] == 'Historical',
  nrow(old_out$ipa_appointment_history.csv)==1L,
  sum(old_out$ftm_ipa_summary.csv$`Appointment Count`)==1L)
previous_old <- old %>% mutate(`IPA Age Band`='3_5yr')
previous_old_out <- build_ipa_activity_exports(previous_old,new_p,new_map)
stopifnot(previous_old_out$participant_ipa_records.csv$`Completion FTM` == 'Historical',
  nrow(previous_old_out$ipa_appointment_history.csv)==0L)
# Do not skip two age bands, and do not resolve a tie by arbitrary row order.
far <- prev %>% mutate(`IPA Age Band`='24_35_month')
far_out <- build_ipa_activity_exports(far,new_p,new_map)
stopifnot(far_out$participant_ipa_records.csv$`Completion FTM`[
  far_out$participant_ipa_records.csv$`IPA Complete`] == 'Pending verification')
tied <- prev %>% mutate(`Calendly FTM`='Different', `Invitee UUID`='tie')
tied_out <- build_ipa_activity_exports(bind_rows(prev,tied),new_p,new_map)
stopifnot(tied_out$participant_ipa_records.csv$`Completion FTM`[
  tied_out$participant_ipa_records.csv$`IPA Complete`] == 'Pending verification')
cat('Expanded completion evidence checks passed\n')
