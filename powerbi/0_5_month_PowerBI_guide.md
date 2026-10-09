# 2026 0–5 Month Power BI page

The data is exported by `../PRENATAL_2026.R`. All tables use its snapshot date.

## Recommended model

Import `month0to5_participants.csv` and `month0to5_progress.csv`.
Create a one-to-many relationship:

`month0to5_participants[participant_key]` → `month0to5_progress[participant_key]`

Use single-direction filtering from participants to progress. Use `site` from
the participant table for the shared site slicer. Set IDs to Text; date fields
to Date; Eligible and other flags to True/False; ages/counts to Whole Number.
Do not append participant rows, progress rows or summary rows to each other.

## Definitions

- **Eligibility:** dateCreated in 2026. Missing creation dates are unknown.
  The 0–5 month roster contains F-category customId records only.
  Participants are counted separately by participant_key; P-category records
  are excluded from these exports.
- **Actual completion count:** completion date in 2026, independent of eligibility.
- **Eligible completion rate:** recorded completion among eligible participants /
  eligible participants. Completion can be recorded outside 2026.
- **Biospecimens:** child urine/stool, maternal hair/bloodspot, breast milk;
  one completion per participant/sample event, not per tube.
- **Aging out:** eligible people aged at least 4 and under 6 completed calendar
  months at the snapshot date. DOB uses child_dob, then birthday. Each row carries
  the source used, six-month deadline and remaining days. The R export calculates
  month-end birthdays consistently (e.g. Aug 31 → Feb 28).

Future-dated completion records are retained in annual counts and marked for
review on the dashboard. The snapshot date is the extraction date, rather than
an automatic cutoff applied to every recorded completion date.

## Measures

```dax
Eligible Participants =
CALCULATE(
    DISTINCTCOUNT('month0to5_participants'[participant_key]),
    'month0to5_participants'[Eligible] = TRUE()
)

Completed Events 2026 =
CALCULATE(
    COUNTROWS('month0to5_progress'),
    'month0to5_progress'[completion_year] = 2026
)

Surveys 2026 =
CALCULATE([Completed Events 2026], 'month0to5_progress'[event_code] = "survey")

Visits Scheduled 2026 =
CALCULATE([Completed Events 2026], 'month0to5_progress'[event_code] = "visit_scheduled")

Diapers Sent 2026 =
CALCULATE([Completed Events 2026], 'month0to5_progress'[event_code] = "diaper_sent")

VIB Requests 2026 =
CALCULATE([Completed Events 2026], 'month0to5_progress'[event_code] = "vib_request")

Biospecimens 2026 =
CALCULATE([Completed Events 2026], 'month0to5_progress'[is_biospecimen] = TRUE())

Eligible Event Count =
CALCULATE(COUNTROWS('month0to5_progress'), 'month0to5_progress'[Eligible] = TRUE())

Eligible Completed Event Count =
CALCULATE(
    [Eligible Event Count],
    'month0to5_progress'[Has Completion Record] = TRUE()
)

Eligible Completion Rate =
DIVIDE([Eligible Completed Event Count], [Eligible Event Count])
```

These event counts are valid because the progress export has exactly one row
per participant × event. Put Data Element Name on the axis to calculate each
element's rate. The overall rate across multiple elements is an event-level
rate, not a participant completion rate. `Eligible Participants` uses the roster;
do not filter the roster by the completion status of an event.

## Page mapping

| Visual | Field / measure |
|---|---|
| Site buttons | participants.site |
| Monthly survey card | Surveys 2026, filtered by completion_month_number |
| Monthly specimen card | Biospecimens 2026, filtered by completion_month_number |
| Monthly visits scheduled card | Visits Scheduled 2026, filtered by completion_month_number |
| Completed elements bars | progress.Data Element Name + Completed Events 2026 |
| Survey / Visit / Diaper workflow | Surveys 2026, Visits Scheduled 2026, Diapers Sent 2026 |
| IPA vs VIB | Visits Scheduled 2026 vs VIB Requests 2026 |
| Aging out table | participants.aging_out = TRUE; customId, age_months, age_out_date, survey_complete, visit_scheduled_complete |
| Eligible progress | Data Element Name, Eligible Completed Event Count, Eligible Completion Rate |

Use visual-level month filters for the three monthly cards. Do not apply a
completion-date page filter when calculating incomplete/eligible denominators:
undated rows would disappear. Sort completion_month_name by completion_month_number.

The workflow's annual stage counts are independent; ratios between them are
count ratios, not same-participant conversion rates. `month0to5_workflow.csv`
also supplies same-participant intersections for
survey → visit scheduling → diaper records within the year. These describe
record coverage and do not assume that the dates represent a strict workflow order. IPA here means
visit scheduling, not documented attendance.

## Additional exports

- `month0to5_event_definitions.csv`: 13 event labels and specimen flags.
- `month0to5_event_summary.csv`: site/event actual and eligible counts/rates.
- `month0to5_monthly.csv`: site/month/event zero-filled counts.
- `month0to5_workflow.csv`: independent and linked annual workflow counts.
- `month0to5_aging_out.csv`: current eligible 4–5 month roster.

The event/workflow summaries contain `All Sites` total rows. Select either
those totals or the individual sites when aggregating; never sum both.
