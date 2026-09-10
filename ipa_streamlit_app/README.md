# CHARM IPA Task Dashboard

This Streamlit app gives FTM staff a filterable participant-level view of IPA and Ripple task progress.

## Data flow

Running `../IPA Data.R` writes dashboard-ready files to:

- `data/latest/`: replaced after every refresh
- `data/snapshots/YYYY-MM-DD_HHMMSS/`: one immutable time point per successful refresh

The dashboard uses `task_checklist_long.csv` for task metrics and
`participant_roster.csv` for the complete current roster. The roster retains
6–11 month and potential participants even though they do not yet have
applicable IPA task rows. The wide checklist and FTM summary are also exported
for downstream use.

The refresh also maintains two persistent audit tables:

- `data/latest/ftm_assignment_history.csv`: effective-dated FTM history. It records the age band at assignment start and at the latest observation, but only an administrator's actual FTM change opens a new assignment interval. For 6–35 months the assignment source is Ripple; for 3–20 years it is the current Call List.
- `data/latest/task_responsibility_ledger.csv`: locks every participant-age-band-task row to the FTM responsible when that task first becomes applicable, regardless of outcome.

The first run is the baseline: every existing task is attributed to each participant's current FTM. When a participant enters a later age band, prior Complete, No-Show, Incomplete, and No record rows remain with the prior FTM; newly applicable tasks are assigned to the new current FTM.

## Run locally

From this directory:

```bash
python3 -m pip install -r requirements.txt
streamlit run streamlit_app.py
```

The sidebar lets users choose Latest, a timestamped snapshot, or a manually uploaded checklist CSV. `Task responsibility` includes current and historical age-band tasks under their locked responsible FTM. `Current caseload` shows only currently applicable tasks under the participant's current FTM. Filters are available for participant scope, FTM, task, age group, outcome, and participant cohort. The Participant roster tab lets FTMs explicitly open Potential participants or 6–11 month participants without adding them to task-progress denominators. The Snapshot trend tab applies task filters to every saved refresh, draws one series per FTM, and supports weighted progress, completion rate, task counts, and follow-up volume.

## Metric definitions

- **Complete:** score 1
- **No-Show:** score 0.25
- **Incomplete:** score 0
- **No record:** no source record; displayed separately and counted as 0 in weighted progress
- **Weighted progress:** sum of task scores divided by the number of applicable participant-task rows

For IPA tasks, a participant's latest Calendly appointment within the same IPA age band controls the outcome. A completed latest appointment remains Complete even if an earlier appointment was a no-show. Non-IPA task outcomes come from Ripple.

## Optional path override

If the R script is run from another location, set `IPA_DASHBOARD_APP_DIR` to this app directory before running it.
