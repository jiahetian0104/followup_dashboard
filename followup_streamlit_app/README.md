# Follow-up Dashboard Streamlit App

This is a shareable Streamlit version of the CHARM follow-up dashboard.

## Project structure

```text
followup_streamlit_app/
├── streamlit_app.py
├── requirements.txt
└── data/
    ├── latest/
    │   └── dashboard_detail.csv
    └── snapshots/
        ├── YYYY-MM-DD/
        │   ├── detail.csv
        │   └── summary.csv
        └── ...
```

`summary.csv` is optional for the app because the dashboard recomputes summary metrics from `detail.csv`.

## Required columns

The default `dashboard_detail.csv` should contain at least:

- `eligible_flag`
- `Score`
- `staff`
- `event_short`

Recommended additional columns:

- `status_2026_std`
- `statusId`
- `Outcome`
- `child_echo_id`
- `PIN`
- `age_at_caregiver_completion`
- `source_sheet`

## Run locally

```bash
cd followup_streamlit_app
pip install -r requirements.txt
streamlit run streamlit_app.py
```

## Deploy to Streamlit Community Cloud

1. Push this folder to a GitHub repository.
2. Make sure `requirements.txt` is committed.
3. Add `data/latest/dashboard_detail.csv` if you want the dashboard to open with the latest data by default.
4. Optional: add historical files under `data/snapshots/YYYY-MM-DD/detail.csv`.
5. Go to Streamlit Community Cloud and create a new app.
6. Select the GitHub repository, branch, and `streamlit_app.py` as the main file.
7. Deploy and share the generated link with the internal team.

## Updating data

For routine updates, replace:

```text
data/latest/dashboard_detail.csv
```

For historical tracking, add a new folder:

```text
data/snapshots/YYYY-MM-DD/detail.csv
```

The dashboard will automatically show available snapshot dates in the sidebar.
