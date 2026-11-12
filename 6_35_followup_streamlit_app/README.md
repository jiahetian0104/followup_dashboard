# CHARM Follow-up Dashboard

This folder contains a Streamlit dashboard for monitoring staff-level event progress from `2026_6_35_month_dashboard.R`.

## Folder structure

```text
followup_streamlit_app/
├── streamlit_app.py
├── requirements.txt
├── build_dashboard_data_appendix.R
└── data/
    ├── latest/
    │   ├── dashboard_detail.csv
    │   ├── manifest.csv
    │   ├── staff_assignment_history.csv
    │   └── task_responsibility_ledger.csv
    └── snapshots/
        └── YYYY-MM-DD_HHMMSS/
            ├── detail.csv
            ├── manifest.csv
            ├── staff_assignment_snapshot.csv
            ├── staff_assignment_history.csv
            ├── task_responsibility_ledger.csv
            └── summary.xlsx
```

## Workflow

1. Run `2026_6_35_month_dashboard.R` from the `followup_dashboard` project folder.
2. The script updates the assignment ledgers, latest dashboard CSV, and a timestamped snapshot.
3. Deploy `streamlit_app.py` with `requirements.txt` on Streamlit Community Cloud.

Set `FOLLOWUP_6_35_DASHBOARD_APP_DIR` only when the app folder is stored somewhere other than the default project location.

## Staff credit and assignment history

- The first history-enabled run uses the current Ripple assignment as the baseline; it does not infer older assignments.
- `staff_assignment_history.csv` records every observed staff interval and the age window at the start and latest observation.
- Age-window changes are contextual records only. A new assignment interval starts only when the staff value changes.
- `task_responsibility_ledger.csv` locks each Event × Age Window task to the staff member responsible when that task first becomes applicable.
- Complete, Incomplete, and No record outcomes stay with that responsible staff member. A later assignment does not transfer prior task credit or workload.
- The dashboard field `staff` is the credited **Responsible Staff**. `Current Staff` remains available in detail data for current follow-up work.
- Historical task rows remain in the dashboard after a participant moves to the next age window.

The bundled `build_dashboard_data_appendix.R` is retained only as a legacy reference. The history-enabled workflow is implemented in the main R script and should be used for updates.

## Historical trends

The **Historical trends** tab recalculates Progress, Denominator, Numerator, or Detail records from every saved snapshot. It supports Overall, Staff, and Event comparisons, a snapshot date range, and the sidebar filters. When comparing by Staff, the tab provides its own Event selector so a specific event can be compared across staff without changing the current-status Event filter.

New refreshes use timestamped folders so multiple refreshes on the same day can appear as separate data points. Existing `YYYY-MM-DD` snapshot folders remain readable.

## Dashboard denominator logic

The Streamlit sidebar has a toggle called **Include Potential Participants**.

- Toggle ON: denominator includes `Eligibility == "Yes"` and `Eligibility == "Potential Participants"`.
- Toggle OFF: denominator includes only `Eligibility == "Yes"`.
- `Eligibility == "No"` is excluded from denominator in both cases.

## Required dashboard detail columns

The app expects `data/latest/dashboard_detail.csv` or snapshot `detail.csv` to contain these columns:

- `ECHO_ID`
- `staff`
- `Current Staff` (new history-enabled exports)
- `Responsible Staff` (new history-enabled exports)
- `event_short`
- `status_group`
- `Eligibility`
- `Outcome`
- `Completion_Date`
- `eligible_flag`
- `Score`
