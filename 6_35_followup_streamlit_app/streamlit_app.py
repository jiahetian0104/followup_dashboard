"""
CHARM follow-up dashboard.

Expected data structure:

6_35_followup_streamlit_app/
├── streamlit_app.py
├── requirements.txt
└── data/
    ├── latest/
    │   └── dashboard_detail.csv
    └── snapshots/
        └── YYYY-MM-DD_HHMMSS/
            ├── detail.csv
            ├── manifest.csv
            └── summary.xlsx

The dashboard can also run by uploading a dashboard_detail.csv file from the sidebar.
"""

from __future__ import annotations

from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd
import plotly.express as px
import streamlit as st


# =========================
# Configuration
# =========================
APP_TITLE = "CHARM Follow-up Dashboard"
POTENTIAL_LABEL = "Potential Participants"
BASE_DIR = Path(__file__).resolve().parent
LATEST_DETAIL_PATH = BASE_DIR / "data" / "latest" / "dashboard_detail.csv"
SNAPSHOT_DIR = BASE_DIR / "data" / "snapshots"


# =========================
# Page settings
# =========================
st.set_page_config(
    page_title=APP_TITLE,
    page_icon="📊",
    layout="wide",
)


# =========================
# Data loading helpers
# =========================
def find_snapshot_detail_files(snapshot_dir: Path = SNAPSHOT_DIR) -> dict[str, Path]:
    """Return available snapshot dates and detail.csv paths."""
    if not snapshot_dir.exists():
        return {}

    snapshots: dict[str, Path] = {}
    for folder in sorted(snapshot_dir.iterdir(), reverse=True):
        if not folder.is_dir():
            continue

        detail_path = folder / "detail.csv"
        dashboard_detail_path = folder / "dashboard_detail.csv"
        if detail_path.exists():
            snapshots[folder.name] = detail_path
        elif dashboard_detail_path.exists():
            snapshots[folder.name] = dashboard_detail_path

    return snapshots


def get_snapshot_timestamp(snapshot_folder: Path) -> pd.Timestamp:
    """Read the recorded refresh time, falling back to the folder name."""
    manifest_path = snapshot_folder / "manifest.csv"
    if manifest_path.exists():
        try:
            manifest = pd.read_csv(manifest_path)
            if "snapshot_time" in manifest.columns and not manifest.empty:
                timestamp = pd.to_datetime(
                    manifest.loc[0, "snapshot_time"], errors="coerce"
                )
                if not pd.isna(timestamp):
                    if timestamp.tzinfo is not None:
                        timestamp = timestamp.tz_convert(
                            "America/Detroit"
                        ).tz_localize(None)
                    return timestamp
        except (OSError, pd.errors.ParserError):
            pass

    for date_format in ("%Y-%m-%d_%H%M%S", "%Y-%m-%d"):
        timestamp = pd.to_datetime(
            snapshot_folder.name,
            format=date_format,
            errors="coerce",
        )
        if not pd.isna(timestamp):
            return timestamp

    return pd.Timestamp(snapshot_folder.stat().st_mtime, unit="s")


@st.cache_data(show_spinner=False)
def load_csv_from_path(path: str, modified_time: int) -> pd.DataFrame:
    """Load a CSV and invalidate the cache when the file changes."""
    return pd.read_csv(path)


@st.cache_data(show_spinner=False)
def load_csv_from_upload(uploaded_file) -> pd.DataFrame:
    """Load CSV uploaded in the Streamlit sidebar."""
    return pd.read_csv(uploaded_file)


@st.cache_data(show_spinner=False)
def load_snapshot_history(
    snapshot_items: tuple[tuple[str, str, int, str], ...],
) -> tuple[pd.DataFrame, tuple[str, ...]]:
    """Load every usable snapshot for the historical trend view."""
    frames: list[pd.DataFrame] = []
    issues: list[str] = []

    for snapshot_id, path, _modified_time, timestamp_iso in snapshot_items:
        try:
            frame = standardize_dashboard_data(pd.read_csv(path))
        except (OSError, pd.errors.ParserError, ValueError) as exc:
            issues.append(f"{snapshot_id}: {exc}")
            continue

        frame["snapshot_id"] = snapshot_id
        frame["snapshot_time"] = pd.Timestamp(timestamp_iso)
        frames.append(frame)

    if not frames:
        return pd.DataFrame(), tuple(issues)

    return pd.concat(frames, ignore_index=True), tuple(issues)


def safe_unique(values: Iterable) -> list[str]:
    """Return sorted non-missing unique values as strings."""
    series = pd.Series(values).dropna()
    return sorted([str(x) for x in series.unique().tolist()])


def standardize_dashboard_data(data: pd.DataFrame) -> pd.DataFrame:
    """Validate and standardize dashboard columns."""
    df = data.copy()

    required_cols = [
        "ECHO_ID",
        "staff",
        "event_short",
        "status_group",
        "Eligibility",
        "Outcome",
        "eligible_flag",
        "Score",
    ]
    missing = [col for col in required_cols if col not in df.columns]
    if missing:
        raise ValueError("Missing required column(s): " + ", ".join(missing))

    df["eligible_flag"] = pd.to_numeric(df["eligible_flag"], errors="coerce").fillna(0)
    df["Score"] = pd.to_numeric(df["Score"], errors="coerce")

    string_cols = [
        "ECHO_ID",
        "staff",
        "Event",
        "event_short",
        "status_group",
        "status_2026_std",
        "participant_status",
        "Eligibility",
        "Age_Window",
        "Outcome",
        "source_sheet",
        "Current Staff",
        "Responsible Staff",
        "Task Stage",
        "Task Age Window",
        "Assignment Source",
    ]
    for col in string_cols:
        if col in df.columns:
            df[col] = df[col].astype("string")

    if "participant_status" not in df.columns:
        df["participant_status"] = df["status_group"].astype("string")

    if "Completion_Date" in df.columns:
        df["Completion_Date"] = pd.to_datetime(df["Completion_Date"], errors="coerce")
    else:
        df["Completion_Date"] = pd.NaT

    df["staff"] = df["staff"].fillna("Unassigned")
    df.loc[df["staff"].astype(str).str.strip().eq(""), "staff"] = "Unassigned"

    return df


def resolve_data_source() -> tuple[pd.DataFrame | None, str]:
    """Resolve data from latest file, snapshot file, or CSV upload."""
    snapshots = find_snapshot_detail_files()

    data_source_options: list[str] = []
    if LATEST_DETAIL_PATH.exists():
        data_source_options.append("Latest")
    if snapshots:
        data_source_options.append("Snapshot")
    data_source_options.append("Upload CSV")

    selected_source = st.sidebar.radio(
        "Data source",
        data_source_options,
        index=0,
        help="Use Latest for current monitoring, Snapshot for historical review, or Upload CSV for quick validation.",
    )

    if selected_source == "Latest":
        return (
            load_csv_from_path(
                str(LATEST_DETAIL_PATH), LATEST_DETAIL_PATH.stat().st_mtime_ns
            ),
            "Latest",
        )

    if selected_source == "Snapshot":
        snapshot_date = st.sidebar.selectbox("Snapshot date", list(snapshots.keys()))
        snapshot_path = snapshots[snapshot_date]
        return (
            load_csv_from_path(str(snapshot_path), snapshot_path.stat().st_mtime_ns),
            f"Snapshot: {snapshot_date}",
        )

    uploaded_file = st.sidebar.file_uploader("Upload dashboard_detail.csv", type=["csv"])
    if uploaded_file is None:
        return None, "No uploaded file"

    return load_csv_from_upload(uploaded_file), "Uploaded CSV"


# =========================
# Metric helpers
# =========================
def apply_filters(
    data: pd.DataFrame,
    include_potential: bool,
    staff_value: str,
    event_value: str,
    status_value: str,
    outcome_value: str,
) -> pd.DataFrame:
    """Apply sidebar filters before calculating dashboard metrics."""
    out = data.copy()

    if include_potential:
        out = out[out["Eligibility"].isin(["Yes", POTENTIAL_LABEL])]
    else:
        out = out[out["Eligibility"].eq("Yes")]

    if staff_value != "All":
        out = out[out["staff"].eq(staff_value)]

    if event_value not in ["All", "Overall"]:
        out = out[out["event_short"].eq(event_value)]

    if status_value != "All":
        out = out[out["status_group"].eq(status_value)]

    if outcome_value != "All":
        out = out[out["Outcome"].eq(outcome_value)]

    return out


def calculate_metrics(data: pd.DataFrame) -> tuple[int, float, float]:
    """Calculate denominator, numerator, and progress."""
    denominator = int(data["eligible_flag"].fillna(0).sum())
    numerator = float(
        np.where(
            data["eligible_flag"].fillna(0).eq(1),
            data["Score"].fillna(0),
            0,
        ).sum()
    )
    progress = np.nan if denominator == 0 else numerator / denominator
    return denominator, numerator, progress


def summarize_dashboard(data: pd.DataFrame, group_cols: list[str]) -> pd.DataFrame:
    """Summarize denominator, numerator, and progress by selected groups."""
    if data.empty:
        return pd.DataFrame(columns=group_cols + ["denominator", "numerator", "progress"])

    summary = (
        data.groupby(group_cols, dropna=False)
        .apply(
            lambda x: pd.Series(
                calculate_metrics(x),
                index=["denominator", "numerator", "progress"],
            )
        )
        .reset_index()
    )
    return summary


def summarize_staff_event(data: pd.DataFrame, add_overall: bool = True) -> pd.DataFrame:
    """Summarize progress by staff and event."""
    by_event = summarize_dashboard(data, ["staff", "event_short"])

    if not add_overall:
        return by_event.sort_values(["staff", "event_short"])

    overall = summarize_dashboard(data, ["staff"])
    overall["event_short"] = "Overall"
    overall = overall[["staff", "event_short", "denominator", "numerator", "progress"]]

    return pd.concat([by_event, overall], ignore_index=True).sort_values(
        ["staff", "event_short"]
    )


def summarize_by_staff(data: pd.DataFrame) -> pd.DataFrame:
    """Summarize overall progress by staff."""
    out = summarize_dashboard(data, ["staff"])
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_by_event(data: pd.DataFrame) -> pd.DataFrame:
    """Summarize overall progress by event."""
    out = summarize_dashboard(data, ["event_short"])
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_by_status(data: pd.DataFrame) -> pd.DataFrame:
    """Summarize overall progress by participant status group."""
    out = summarize_dashboard(data, ["status_group"])
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_history(
    history: pd.DataFrame,
    include_potential: bool,
    staff_value: str,
    event_value: str,
    status_value: str,
    outcome_value: str,
    compare_by: str,
) -> pd.DataFrame:
    """Recalculate metrics independently for each saved refresh."""
    if history.empty:
        return pd.DataFrame()

    rows: list[dict[str, object]] = []
    for (snapshot_id, snapshot_time), snapshot in history.groupby(
        ["snapshot_id", "snapshot_time"], sort=True, dropna=False
    ):
        filtered = apply_filters(
            data=snapshot,
            include_potential=include_potential,
            staff_value=staff_value,
            event_value=event_value,
            status_value=status_value,
            outcome_value=outcome_value,
        )

        if filtered.empty:
            continue

        if compare_by == "Staff":
            groups = filtered.groupby("staff", dropna=False)
        elif compare_by == "Event":
            groups = filtered.groupby("event_short", dropna=False)
        else:
            groups = [("Overall", filtered)]

        for series, group in groups:
            denominator, numerator, progress = calculate_metrics(group)
            rows.append(
                {
                    "snapshot_id": snapshot_id,
                    "snapshot_time": snapshot_time,
                    "series": str(series) if not pd.isna(series) else "Missing",
                    "denominator": denominator,
                    "numerator": numerator,
                    "progress": progress,
                    "detail_records": len(group),
                }
            )

    if not rows:
        return pd.DataFrame(
            columns=[
                "snapshot_id",
                "snapshot_time",
                "series",
                "denominator",
                "numerator",
                "progress",
                "detail_records",
            ]
        )

    return pd.DataFrame(rows).sort_values(["snapshot_time", "series"])


def format_number(x: float) -> str:
    """Format whole numbers without decimals; otherwise one decimal place."""
    if pd.isna(x):
        return "NA"
    x = float(x)
    return f"{int(x):,}" if x.is_integer() else f"{x:,.1f}"


def pct(x: float) -> str:
    """Format numeric value as percentage."""
    if pd.isna(x):
        return "NA"
    return f"{x:.1%}"


def prep_summary_table(data: pd.DataFrame) -> pd.DataFrame:
    """Prepare staff-event summary table for display."""
    table = summarize_staff_event(data, add_overall=True).copy()
    if table.empty:
        return table

    table["denominator"] = table["denominator"].astype("int64")
    table["numerator"] = table["numerator"].map(format_number)
    table["progress"] = table["progress"].map(pct)
    return table


def prep_detail_table(data: pd.DataFrame) -> pd.DataFrame:
    """Order detail columns for display."""
    preferred_cols = [
        "staff",
        "Current Staff",
        "Responsible Staff",
        "ECHO_ID",
        "event_short",
        "Task Age Window",
        "Task Stage",
        "status_group",
        "Eligibility",
        "Outcome",
        "Completion_Date",
        "Age_Window",
        "eligible_flag",
        "Score",
        "Responsibility Start",
        "Assignment Source",
        "source_sheet",
    ]
    cols = [col for col in preferred_cols if col in data.columns]
    remaining = [col for col in data.columns if col not in cols]
    return data[cols + remaining].copy()


def dataframe_to_csv_bytes(data: pd.DataFrame) -> bytes:
    """Convert a data frame to UTF-8 CSV bytes."""
    return data.to_csv(index=False).encode("utf-8")


# =========================
# Chart helpers
# =========================
def make_staff_bar(data: pd.DataFrame):
    """Create staff-level progress chart."""
    summary = summarize_by_staff(data)
    fig = px.bar(
        summary,
        x="staff",
        y="progress",
        hover_data=["denominator", "numerator"],
        title="Overall Progress by Staff",
    )
    fig.update_layout(
        template="plotly_white",
        yaxis_tickformat=".0%",
        xaxis_title="Staff",
        yaxis_title="Progress",
        height=420,
        margin=dict(l=20, r=20, t=60, b=20),
    )
    return fig


def make_event_bar(data: pd.DataFrame):
    """Create event-level progress chart."""
    summary = summarize_by_event(data)
    fig = px.bar(
        summary,
        x="event_short",
        y="progress",
        hover_data=["denominator", "numerator"],
        title="Progress by Event",
    )
    fig.update_layout(
        template="plotly_white",
        yaxis_tickformat=".0%",
        xaxis_title="Event",
        yaxis_title="Progress",
        height=420,
        margin=dict(l=20, r=20, t=60, b=100),
    )
    fig.update_xaxes(tickangle=35)
    return fig


def make_status_bar(data: pd.DataFrame):
    """Create status-group progress chart."""
    summary = summarize_by_status(data)
    fig = px.bar(
        summary,
        x="status_group",
        y="progress",
        hover_data=["denominator", "numerator"],
        title="Progress by Status Group",
    )
    fig.update_layout(
        template="plotly_white",
        yaxis_tickformat=".0%",
        xaxis_title="Status Group",
        yaxis_title="Progress",
        height=420,
        margin=dict(l=20, r=20, t=60, b=100),
    )
    fig.update_xaxes(tickangle=35)
    return fig


def make_staff_event_heatmap(data: pd.DataFrame):
    """Create staff-by-event progress heatmap."""
    summary = summarize_staff_event(data, add_overall=False)
    if summary.empty:
        return px.imshow([[None]], title="Staff × Event Progress")

    pivot = summary.pivot(index="staff", columns="event_short", values="progress")
    fig = px.imshow(
        pivot,
        aspect="auto",
        color_continuous_scale="Blues",
        text_auto=".0%",
        title="Staff × Event Progress",
    )
    fig.update_layout(
        template="plotly_white",
        height=520,
        margin=dict(l=20, r=20, t=60, b=40),
    )
    return fig


def make_trend_chart(data: pd.DataFrame, metric: str):
    """Create a line chart for one historical dashboard metric."""
    metric_config = {
        "Progress": ("progress", "Progress", ".1%"),
        "Denominator": ("denominator", "Denominator", ",.0f"),
        "Numerator": ("numerator", "Numerator", ",.1f"),
        "Detail records": ("detail_records", "Detail records", ",.0f"),
    }
    metric_col, axis_title, tick_format = metric_config[metric]

    fig = px.line(
        data,
        x="snapshot_time",
        y=metric_col,
        color="series",
        markers=True,
        custom_data=[
            "snapshot_id",
            "denominator",
            "numerator",
            "progress",
            "detail_records",
        ],
        title=f"{metric} over time",
    )
    fig.update_traces(
        hovertemplate=(
            "<b>%{fullData.name}</b><br>"
            "Snapshot: %{customdata[0]}<br>"
            "Denominator: %{customdata[1]:,.0f}<br>"
            "Numerator: %{customdata[2]:,.1f}<br>"
            "Progress: %{customdata[3]:.1%}<br>"
            "Detail records: %{customdata[4]:,.0f}<extra></extra>"
        )
    )
    fig.update_layout(
        template="plotly_white",
        xaxis_title="Snapshot date",
        yaxis_title=axis_title,
        yaxis_tickformat=tick_format,
        height=540,
        margin=dict(l=20, r=20, t=70, b=30),
        legend_title_text="Series",
        hovermode="x unified" if data["series"].nunique() > 1 else "closest",
    )
    return fig


# =========================
# App layout
# =========================
st.title(APP_TITLE)
st.caption("Internal dashboard for staff-level follow-up event progress")

raw_df, source_label = resolve_data_source()

if raw_df is None:
    st.info(
        "Upload `dashboard_detail.csv`, or save a default file to "
        "`data/latest/dashboard_detail.csv` in the deployed repository."
    )
    st.stop()

try:
    df = standardize_dashboard_data(raw_df)
except ValueError as exc:
    st.error(str(exc))
    st.stop()

st.sidebar.markdown("---")
st.sidebar.subheader("Filters")

include_potential = st.sidebar.toggle(
    "Include Potential Participants",
    value=True,
    help=(
        "ON: denominator includes Eligibility = Yes and Potential Participants. "
        "OFF: denominator includes only Eligibility = Yes."
    ),
)

staff_value = st.sidebar.selectbox("Staff", ["All"] + safe_unique(df["staff"]))
event_value = st.sidebar.selectbox("Event", ["All", "Overall"] + safe_unique(df["event_short"]))
status_value = st.sidebar.selectbox("Status group", ["All"] + safe_unique(df["status_group"]))
outcome_value = st.sidebar.selectbox("Outcome", ["All"] + safe_unique(df["Outcome"]))

filtered_df = apply_filters(
    data=df,
    include_potential=include_potential,
    staff_value=staff_value,
    event_value=event_value,
    status_value=status_value,
    outcome_value=outcome_value,
)

denominator, numerator, progress = calculate_metrics(filtered_df)
summary_raw = summarize_staff_event(filtered_df, add_overall=True)
summary_table = prep_summary_table(filtered_df)
detail_table = prep_detail_table(filtered_df)

potential_text = "included" if include_potential else "excluded"
st.caption(
    f"Data source: **{source_label}** | Potential Participants are **{potential_text}**. "
    "Progress = completed event rows / eligible event rows after filters."
)

current_tab, trend_tab = st.tabs(["Current status", "Historical trends"])

with current_tab:
    kpi1, kpi2, kpi3, kpi4 = st.columns(4)
    kpi1.metric("Denominator", f"{denominator:,}")
    kpi2.metric("Numerator", format_number(numerator))
    kpi3.metric("Progress", pct(progress))
    kpi4.metric("Detail records", f"{len(filtered_df):,}")

    chart_col1, chart_col2 = st.columns(2)
    with chart_col1:
        st.plotly_chart(make_staff_bar(filtered_df), width="stretch")
    with chart_col2:
        st.plotly_chart(make_event_bar(filtered_df), width="stretch")

    chart_col3, chart_col4 = st.columns(2)
    with chart_col3:
        st.plotly_chart(make_status_bar(filtered_df), width="stretch")
    with chart_col4:
        st.plotly_chart(
            make_staff_event_heatmap(filtered_df), width="stretch"
        )

    st.subheader("Summary Table")
    st.dataframe(summary_table, width="stretch", hide_index=True)
    st.download_button(
        label="Download filtered summary CSV",
        data=dataframe_to_csv_bytes(summary_raw),
        file_name="dashboard_summary_filtered.csv",
        mime="text/csv",
    )

    st.subheader("Detail Data")
    st.dataframe(detail_table, width="stretch", hide_index=True)
    st.download_button(
        label="Download filtered detail CSV",
        data=dataframe_to_csv_bytes(detail_table),
        file_name="dashboard_detail_filtered.csv",
        mime="text/csv",
    )

with trend_tab:
    snapshots = find_snapshot_detail_files()
    snapshot_items = tuple(
        (
            snapshot_id,
            str(path),
            path.stat().st_mtime_ns,
            get_snapshot_timestamp(path.parent).isoformat(),
        )
        for snapshot_id, path in snapshots.items()
    )
    history_df, history_issues = load_snapshot_history(snapshot_items)

    if history_issues:
        st.warning("Some snapshots could not be loaded: " + "; ".join(history_issues))

    if history_df.empty:
        st.info(
            "No historical snapshots are available yet. Run the R update script "
            "to create the first saved data point."
        )
    else:
        control_col1, control_col2 = st.columns(2)
        with control_col1:
            trend_metric = st.selectbox(
                "Metric",
                ["Progress", "Denominator", "Numerator", "Detail records"],
                key="trend_metric",
            )
        with control_col2:
            compare_by = st.selectbox(
                "Compare by",
                ["Overall", "Staff", "Event"],
                key="trend_compare_by",
            )

        trend_event_value = event_value
        if compare_by == "Staff":
            event_control_col, _ = st.columns(2)
            with event_control_col:
                trend_event_value = st.selectbox(
                    "Event",
                    ["Overall"] + safe_unique(history_df["event_short"]),
                    key="trend_event_value",
                    help=(
                        "This Event filter applies only to the historical Staff "
                        "comparison and is independent of the sidebar Event filter."
                    ),
                )

        available_dates = history_df["snapshot_time"].dt.date
        min_snapshot_date = available_dates.min()
        max_snapshot_date = available_dates.max()
        selected_dates = st.date_input(
            "Snapshot date range",
            value=(min_snapshot_date, max_snapshot_date),
            min_value=min_snapshot_date,
            max_value=max_snapshot_date,
            key="trend_date_range",
        )

        if isinstance(selected_dates, (tuple, list)) and len(selected_dates) == 2:
            start_date, end_date = selected_dates
        else:
            start_date = end_date = selected_dates

        history_in_range = history_df[
            history_df["snapshot_time"].dt.date.between(start_date, end_date)
        ]
        trend_df = summarize_history(
            history=history_in_range,
            include_potential=include_potential,
            staff_value=staff_value,
            event_value=trend_event_value,
            status_value=status_value,
            outcome_value=outcome_value,
            compare_by=compare_by,
        )

        if compare_by == "Staff":
            st.caption(
                "Each point is recalculated from that saved refresh. The Event "
                "control above applies only to this Staff comparison; the other "
                "filters come from the sidebar."
            )
        else:
            st.caption(
                "Each point is recalculated from that saved refresh using the "
                "current sidebar filters."
            )

        if trend_df.empty:
            st.info("No historical records match the filters and date range.")
        else:
            st.plotly_chart(
                make_trend_chart(trend_df, trend_metric),
                width="stretch",
            )

            trend_table = trend_df.copy()
            trend_table["snapshot_time"] = trend_table["snapshot_time"].dt.strftime(
                "%Y-%m-%d %H:%M:%S"
            )
            trend_table["progress"] = trend_table["progress"].map(pct)
            trend_table["numerator"] = trend_table["numerator"].map(format_number)

            st.subheader("Historical metric table")
            st.dataframe(trend_table, width="stretch", hide_index=True)
            st.download_button(
                label="Download historical metrics CSV",
                data=dataframe_to_csv_bytes(trend_df),
                file_name="dashboard_history_filtered.csv",
                mime="text/csv",
            )

with st.expander("Data logic notes"):
    st.markdown(
        """
        - `status_group` is the standardized participant status from the R workflow.
        - `staff` is the **Responsible Staff** used for performance credit. **Current Staff** is retained separately for operational follow-up.
        - On the first history-enabled refresh, current Ripple assignments establish the baseline.
        - Each event and age-window task is locked to its responsible staff when it first becomes applicable; later staff changes do not transfer that task.
        - Historical tasks remain visible after a participant moves to the next age window.
        - The **Include Potential Participants** toggle controls whether `Eligibility = Potential Participants` contributes to the denominator.
        - `Eligibility = No` is excluded from denominator in both toggle settings.
        - `Outcome = Complete` contributes 1 point to the numerator; unfinished outcomes contribute 0.
        - New snapshots use `data/snapshots/YYYY-MM-DD_HHMMSS/detail.csv`; older date-only folders remain supported.
        """
    )
