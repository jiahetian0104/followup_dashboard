# CHARM IPA 2026 Dashboard

The dashboard covers three independent measures: completed IPA surveys, Calendly appointment activity, and biospecimens collected in 2026. There are no weighted scores, responsibility ledgers, previous-output inputs or 2025 booking fallbacks.

## Refresh and launch

Run `followup_dashboard/IPA Data.R` with Source in RStudio or `Rscript` from the workspace. The entry point finds its sibling modules regardless of working directory. Required R packages: tidyverse (including lubridate), httr2 and readxl. The optional survey-only Excel exporter also uses openxlsx. XLConnect and Java are no longer required by this pipeline.

The existing credentials have been moved to `followup_dashboard/.Renviron.ipa`, a private local file excluded by `.gitignore`. Do not commit this file. It supplies `RIPPLE_AUTH_KEY` (the complete Authorization value) and `CALENDLY_TOKEN` (the Calendly personal access token). Environment-based configuration is also supported when the local file is absent. The legacy Ripple configuration can supply the Ripple key if neither environment nor local configuration supplies it.

The Crosswalk default is the existing shared-drive path to `Data/Miscellaneous/Global Crosswalk/CHARM_Cohort_List.xlsx`, sheet `Cohort_List`. Set `IPA_CROSSWALK_PATH` to use another location. Set `IPA_DASHBOARD_APP_DIR` to change the dashboard output directory.

From this app directory:

```bash
python3 -m pip install -r requirements.txt
streamlit run streamlit_app.py
```

The dashboard's **Reload saved data** button reloads files; it does not call APIs. Select Latest or an immutable snapshot in the sidebar. Age and cohort filters apply across tabs; each tab has its own FTM attribution filter.

## Code structure

| File | Responsibility |
|---|---|
| `../IPA Data.R` | Acquire inputs, build a complete batch, verify outputs, publish snapshot/latest |
| `../ipa_sources.R` | Two combined Ripple study requests; 2026 Calendly requests; identity catalog |
| `../ipa_matching.R` | Existing identity, contact, name, host and appointment-age functions, preserved |
| `../ipa_fields.R` | IPA and specimen event-field mappings |
| `../ipa_surveys.R` | Survey submissions, original manually verified corrections, first/latest completions, site labels |
| `../ipa_activity.R` | Stateless booking counts, survey/event QC, dated specimen collections and owners |
| `dashboard_data.py` | Read/validate/filter batches and summarize counts |
| `streamlit_app.py` | Completion, booking, specimen, QC and snapshot views |

`../Ripple 2026 survey export.R` remains an optional survey-only CSV/Excel exporter. It uses the same functions and first-completion definition; its manual correction rules are shared in `ipa_surveys.R`, not maintained as a second copy.

## Sources and year scope

Each refresh requests participants, contacts, events and the selected IPA surveys together from each of the two Ripple studies. `surveyExportSince` is explicitly `2026-01-01`. The returned survey schema is validated, and completion time is independently filtered to the reporting year. Missing required event fields fail the refresh rather than silently producing zero collections.

Calendly events are requested from local midnight January 1, 2026 through local midnight January 1, 2027, exclusive at the end. Only the original eight assessment booking types are processed. No-show, true cancellation and rescheduling flags retain their previous definitions. The same assessment type filter is applied before requesting invitees to avoid unnecessary calls for unrelated events.

Withdrawn participants remain available to assess completed surveys and 2026 specimen collections. Known test records are excluded. Current age group still comes from Ripple status; appointment age still comes from birthday plus appointment date, with the existing age-answer fallback. Current status does not remove an otherwise valid 2026 collection.

## Completion definition

- One completed IPA per **Participant ID × survey age band** within 2026.
- Use the **earliest completed timestamp in 2026**, not the latest submission and not submission index zero. A smaller submission index breaks exact timestamp ties.
- FTM, site and month follow that first completed submission. There is no Calendly inference of completion FTM.
- All returned completed submissions, including repeat submissions, remain in `ipa_survey_submissions.csv` for audit; they are not all counted.
- `ipa_survey_latest.csv` separately retains the latest 2026 completed submission for specimen assignment.
- Original placeholder-FTM mappings and submission-specific FTM/site corrections remain applied only within their original conditions. Original values, actual source fields and correction provenance remain in exports.
- Ripple's `24-35 Month` survey supports both `IPA_FTM` and its older `ftm_member` field. Conflicting nonempty values stop the refresh rather than being selected arbitrarily.
- Site labels are Detroit, East Lansing, Flint, Grand Rapids and Traverse City. Original site values remain available.
- Reporting dates/months use America/New_York. CSV timestamp serialization to UTC is converted back before dashboard month grouping.

## Appointment workload

Each row represents one child-booking, not necessarily one physical meeting. Explicit multi-child bookings retain one row per child. Bookings are deduplicated by invitee, participant, cohort and age band. All preserved actions stay with the booking host, independently of survey completion FTM.

- No-show counts each flagged appointment.
- True Cancel requires Canceled TRUE and Rescheduled FALSE.
- Reschedule counts each old booking in a chain. A New Invitee URI is also positive reschedule evidence.
- Missing action flags propagate as unknown instead of zero.
- Shared hosts remain one combined host label.
- Unresolved participant/age-band/booking IDs remain in appointment history and QC and are excluded from matched workload totals.
- Future appointments within the reporting year remain part of the annual booking records.

## Survey/event QC

Compare evidence at **Participant ID × age band**, independently of repeat survey submissions or cohort transitions. The main completion count always uses Survey.

The comparison reports Both complete, Survey only, Event only, Neither complete and Event year/date needs review. All event flags, parsed dates, raw dates, source fields and date differences remain inspectable. Year-specific 2026 event fields can establish event completion with an undated flag, but missing dates are flagged. An undated infant event field cannot establish its completion year. Completed events dated outside 2026 do not establish a 2026 event completion. Date differences are QC evidence, not automatic corrections.

## Biospecimens

Count only records with a completed collection flag and a valid completion date in 2026, regardless of present age/status. Counts are collections, with no inferred incomplete-task denominator or completion percentage.

FTM priority, always within the same child and age band:

1. Latest completed 2026 Survey's FTM.
2. Latest 2026 Calendly host, ordered by appointment time then creation time.
3. Unassigned.

An unresolved survey FTM remains visibly unconfirmed instead of silently transferring credit. A fallback booking may be canceled/no-show; its purpose is responsibility attribution, not evidence of specimen collection. Combined hosts stay combined.

Age-specific specimen fields retain their mapped age bands. Tooth fields apply across ages 3+; collection age from birthday and collection date selects the band. An unresolved tooth age band remains in collection totals and QC. Missing/unparseable collection dates are excluded from totals and retained in QC. A 2026 date without a completed flag is also flagged.

## Saved files and snapshots

- `data/raw/<run-id>/api_inputs.rds`: acquired API evidence and identity catalog, with no authentication values. This directory is private source evidence and is excluded from version control.
- `data/latest/`: the most recent fully built batch, schema `ipa-2026-v2`.
- `data/snapshots/<run-id>/`: immutable output for each successful calculation.
- `data/snapshots/replaced_<run-id>/`: preserves the previous latest batch, including independently modified legacy outputs. These backup copies do not produce duplicate points in snapshot trends.

Raw API responses allow offline replay by setting `IPA_RAW_RUN` to a saved raw run directory before running `IPA Data.R`. Replay never contacts APIs or reads the previous dashboard outputs. The manifest retains API acquisition time, calculation time, source mode, reporting window and metric rules. A replay is explicitly labeled in the page.

Publishing writes and checks a complete staging batch, finalizes the snapshot, prepares a new latest directory and switches latest only after success. If the latest switch fails, the previous latest is restored. Original legacy snapshots remain selectable in a reference-only view and are excluded from new methodology trends. Snapshot trend lines show successive refresh counts, not completion-month trajectories.

Main outputs:

| File | Grain / purpose |
|---|---|
| `ipa_survey_completions.csv` | First 2026 completion per child/age band |
| `ipa_survey_submissions.csv` | All returned completed submissions |
| `ipa_survey_latest.csv` | Latest 2026 completion per child/age band, specimen owner evidence |
| `ipa_event_evidence.csv` | Raw/parsed event completion evidence per child/cohort/age band |
| `ipa_survey_event_comparison.csv` | Independent survey/event presence comparison |
| `ipa_survey_event_qc.csv` | Mismatches and date exceptions |
| `ipa_appointment_history.csv` | 2026 child-booking actions |
| `ipa_appointment_history_qc.csv` | Matching, age, host and status exceptions |
| `participant_ipa_ftm_records.csv` | Child/cohort/age band/host workload counts, no completion credit or weighted score |
| `ftm_ipa_workload.csv` | Host workload totals |
| `biospecimen_collections.csv` | Dated completed 2026 collections and assignment evidence |
| `biospecimen_qc.csv` | Missing/bad dates, conflicting flags, unresolved collection age |
| `participant_roster.csv` | ID/cohort/current status/current age band, without responsibility assignment |
| `manifest.csv` | Batch schema, times, rules and reconciliation counts |

## Verification

From the workspace root:

```bash
Rscript followup_dashboard/ipa_streamlit_app/tests/test_ipa_activity.R
python3 -m unittest discover -s followup_dashboard/ipa_streamlit_app/tests
```

Tests cover earliest vs latest submission, repeat handling, year scope, original corrections, renamed survey fields, canonical sites, year-end/month-end timezone boundaries, withdrawn-participant collections, survey-vs-booking attribution, bidirectional event QC, unknown action flags, reschedule/cancel counting, stateless replay, schema validation, snapshot methodology separation and rendered Streamlit filters.

### QC refinement (October 19, 2026)

Same-day Survey-only and Event-only evidence in different age bands is paired as an explicit **Age-band conflict**. `ipa_age_band_conflicts.csv` shows one row per paired conflict with both age bands, date and Survey FTM; the detailed comparison retains both evidence rows and their original Evidence Status. Two genuine same-band matches on the same day do not create a conflict.

Review queues include only records with a 2026 Survey completion or a 2026 event completion. An event dated before 2026, with no 2026 Survey, remains in the source comparison/evidence but is excluded from actionable QC. Date-only differences remain separate. Event Date Raw and Event Date Field identify the actual exported date and its exact source field. Ripple and Calendly acquisition times are recorded separately when source responses are refreshed independently.

### Presentation refinements (October 19, 2026)

Completion age groups keep the original automatic Plotly color assignment. Only age groups present in the filtered completions appear in the legend; 24–35 months will appear and receive an automatic color when completions are available. Completion-month chart values and hover labels use month names, ordered January through December.

Appointment totals appear in their own host-level table. No-show, Cancel and Reschedule are shown separately as appointment actions; Appointment Count is not a parallel action series. The appointment grand total adds bookings but deduplicates participants and participant/cohort/age-band combinations across hosts.

The biospecimen tab explains the same-child/same-band/2026 FTM priority directly on the page. Its monthly collection table replaces the former monthly stacked chart, with specimen task columns and row/column totals that follow the active filters. Only months with recorded collections are shown.

### Duplicate Survey QC

The Data QC tab includes **Survey duplicates · same age band**. A duplicate group is one Participant ID and survey age band with more than one completed submission within 2026. Different age bands and submissions outside the reporting year do not create duplicate groups. These checks use the full submission history before selecting first completions.

`ipa_survey_duplicate_summary.csv` reports group counts, extra submissions, first/latest completion times and FTMs. `ipa_survey_duplicate_qc.csv` retains every submission in each duplicate group, ordered by completed time then submission index, and marks exactly one row `Counted in IPA = TRUE`. Completion credit remains unchanged. Both tables follow the dashboard's age/cohort filters; the detail table and summary are downloadable. Earlier snapshots without these exports explicitly show that duplicate QC was unavailable for that batch.
