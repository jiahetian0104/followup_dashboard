# CHARM Follow-up Dashboard

This folder contains a Streamlit dashboard for monitoring staff-level event progress from the finalized `2026_6_35_month.R` workflow.

## Folder structure

```text
followup_streamlit_app/
├── streamlit_app.py
├── requirements.txt
├── build_dashboard_data_appendix.R
└── data/
    ├── latest/
    │   └── dashboard_detail.csv
    └── snapshots/
        └── YYYY-MM-DD/
            ├── detail.csv
            ├── summary_with_potential.csv
            ├── summary_without_potential.csv
            └── summary.xlsx
```

## Workflow

1. Run the finalized `2026_6_35_month.R` through the step that creates `activity_log_with_staff`.
2. Run or paste `build_dashboard_data_appendix.R` after that object exists.
3. The R appendix saves the latest dashboard CSV and a dated snapshot.
4. Deploy `streamlit_app.py` with `requirements.txt` on Streamlit Community Cloud.

## Dashboard denominator logic

The Streamlit sidebar has a toggle called **Include Potential Participants**.

- Toggle ON: denominator includes `Eligibility == "Yes"` and `Eligibility == "Potential Participants"`.
- Toggle OFF: denominator includes only `Eligibility == "Yes"`.
- `Eligibility == "No"` is excluded from denominator in both cases.

## Required dashboard detail columns

The app expects `data/latest/dashboard_detail.csv` or snapshot `detail.csv` to contain these columns:

- `ECHO_ID`
- `staff`
- `event_short`
- `status_group`
- `Eligibility`
- `Outcome`
- `Completion_Date`
- `eligible_flag`
- `Score`

