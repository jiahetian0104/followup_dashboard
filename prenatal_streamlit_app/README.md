# Prenatal dashboard

Local Streamlit dashboard using the exports from `../PRENATAL_2026.R`.

## Run

From this folder:

```bash
python3 -m pip install -r requirements.txt
python3 -m streamlit run streamlit_app.py --server.address 127.0.0.1 --server.port 8511
```

Open http://127.0.0.1:8511. The app listens on the local machine only.
The required libraries were already available on the development machine.

## Data and refresh

- `../powerbi/latest/prenatal_progress.csv`: participant × prenatal data element,
  including completion dates, eligibility and participant milestone dates.
- `../powerbi/latest/prenatal_events.csv`: complete event export for prenatal and
  0–5 month biospecimens. Older reversed identifiers are matched against the
  corrected progress roster. Unmatched or duplicate specimen records produce
  an unavailable state instead of a partial count.
- Run `../PRENATAL_2026.R` to retrieve new Ripple data and refresh both files.
  The dashboard never reads API credentials or contacts Ripple itself.
- Click **Reload CSV data** after a refresh. File modification times also
  invalidate the cached data on the next Streamlit interaction.
- Optionally set `PRENATAL_DASHBOARD_DATA_DIR` to another export folder.
- The source snapshot date is shown separately from the time the app is opened.

## Views and filter scope

- Site dropdown allows any combination of Detroit, Flint and Traverse City.
  Unknown is offered only when present in the roster. Deselecting all sites
  shows an explicit empty-selection message. **Reset filters** restores all
  sites and the snapshot month.
- **Monthly cards** changes only the three cards. Annual charts and current
  pending-birth projections retain their named periods.
- **Biospecimens** includes prenatal + 0–5 month collections.
  Each completion counts once per participant × sample event, not per tube.
  Water follows the source report's grouping. Cord blood is not in its
  biospecimen total, matching the existing R report.
- **Overview** shows actual completions in the reporting year, annual distinct
  enrollments and births, and current pending births by estimated due month.
- **Eligibility & progress** shows recorded completions among eligible
  participants. Its element selector affects that view and its export only.
  Completion rate is recomputed from the filtered numerator and denominator.
- Downloads contain aggregates for the currently selected sites/elements;
  participant IDs are not shown or exported by the dashboard.

## Definitions

Enrollment uses `consent_date`; births use `child_dob`. Both count unique
`participant_key` values and do not count repeated long-table rows.

Actual completed data elements use `Has Completion Record` and completion year,
independent of reporting-year eligibility. Prenatal 1 and Actigraphy eligibility
uses enrollment year. Prenatal 2 uses the year of EDD minus 84 days.
Water is eligible for participants enrolled in the reporting year and on or
after March 1, 2026 (inclusive). Missing enrollment dates remain unknown.
Cord blood and medical record eligibility remains unknown until confirmed.
An unknown denominator is blank, not zero. Recorded completion dates outside
the report year remain in eligible progress; dates after the snapshot are
flagged for review.

Projected births use the R script's snapshot flag: no birth/loss date,
not in the excluded Pregnancy Loss or 0–5 month statuses, and EDD on or after
the snapshot date. Overdue records are excluded. This is a current pending
population, not a reconstruction of earlier forecasts.

## Verification

```bash
python3 -m unittest discover -s tests -v
```

Checks cover distinct counts, completions outside the eligibility cohort,
unknown denominators, zero months, legacy ID matching, duplicate rejection,
and Streamlit site/month/reset/empty interactions. Rendered verification
is performed separately in the local browser.

## 0–5 month page

Use the sidebar **Report → 0–5 Month** to open the new page in the same app.
It reads `month0to5_participants.csv` and `month0to5_progress.csv` from the same
export directory. The roster contains **F-category local IDs only**.
Eligibility is **dateCreated in the reporting year** within that population.
The R parser supports US dates, ISO dates and timestamps, preserves timezone
boundaries and rejects unrecognized nonblank dates.

The page includes monthly surveys/specimens/visits scheduled, annual element
bars, Survey→Visit→Diaper workflow, IPA scheduling vs VIB requests, eligible
progress, and an aging-out list. Site filters apply to all sections. The month
selector affects the cards only. The activity restriction checkbox limits
actual charts to eligible participants; eligibility progress and the aging-out
list always use the creation-year eligibility rule for the chosen sites.

The default workflow shows independent annual event counts for comparison
with the original Power BI page. **Linked participant coverage** requires
survey, scheduling and diaper records for the same participant within
the reporting year; it can be lower than the independent counts. A zero initial
survey count has no defined percent-of-initial ratio.

Aging out uses ages 4–5 completed calendar months and shows the local ID,
six-month deadline, days remaining, Survey and Visit status. DOB priority is
child_dob then birthday. The optional outstanding filter keeps participants
missing a survey or scheduled-visit record. This local operational list includes
participant identifiers as requested; it is not a published site.

Power BI model/measure instructions are in `../powerbi/0_5_month_PowerBI_guide.md`.
Run the additional R export checks from the workspace root:

```bash
Rscript followup_dashboard/prenatal_streamlit_app/tests/test_0to5_exports.R
Rscript followup_dashboard/prenatal_streamlit_app/tests/test_prenatal_eligibility.R
```

## Latest and historical snapshots

Each successful R export writes all dashboard CSVs into
`../powerbi/snapshots/<UTC timestamp>_<process ID>/` and publishes the complete
set to `../powerbi/latest/`. The dashboard defaults to `latest/`. The sidebar **Data version** dropdown can select any complete historical snapshot for both reports.
`snapshot_info.csv` records the export time, reporting year and snapshot date.
A failed export leaves the previous `latest/` intact.

For GitHub deployment, push the app code and updated `powerbi/latest/` files
on the deployed branch. Push `powerbi/snapshots/` as well to make those historical versions available online. Snapshots are no longer ignored by Git. The CSVs contain participant-level data and belong
in the private deployment discussed for this project.
