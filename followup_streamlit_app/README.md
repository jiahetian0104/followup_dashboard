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
        │   └── summary.xlsx
        ├── YYYY-MM-DD_HHMMSS/
        │   ├── detail.csv
        │   ├── manifest.csv
        │   └── summary.xlsx
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
data/snapshots/YYYY-MM-DD_HHMMSS/detail.csv
```

The R update script creates a unique timestamped folder and `manifest.csv` after
each successful refresh. Older `YYYY-MM-DD` snapshot folders remain supported.

The dashboard automatically shows available snapshots in the sidebar. The
**Historical trends** tab recalculates Progress, Denominator, Numerator, and
Detail records for every snapshot using the same Staff, Event, Status, and
Potential Participant filters as the current-status view. It can show one
overall series or compare Staff and Event series, and the filtered history can
be downloaded as CSV. When comparing by Staff, the trends tab displays its own
Event selector; this local control does not change the Event selected for the
current-status view.
