"""
Streamlit version of the CHARM follow-up dashboard.

Expected default data structure:

followup_streamlit_app/
├── streamlit_app.py
├── requirements.txt
└── data/
    ├── latest/
    │   └── dashboard_detail.csv
    └── snapshots/
        ├── 2026-04-28/
        │   ├── detail.csv
        │   └── summary.csv   # optional; app recomputes summary from detail.csv
        └── ...

The app can also run without committed data by using the sidebar CSV uploader.
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
APP_TITLE = "Follow-up Dashboard"
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
# Utility functions
# =========================
def find_snapshot_detail_files(snapshot_dir: Path = SNAPSHOT_DIR) -> dict[str, Path]:
    """Return available snapshot dates and their detail.csv paths."""
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


@st.cache_data(show_spinner=False)
def load_csv_from_path(path: str) -> pd.DataFrame:
    """Load a CSV from a repository path and cache it for faster reruns."""
    return pd.read_csv(path)


@st.cache_data(show_spinner=False)
def load_csv_from_upload(uploaded_file) -> pd.DataFrame:
    """Load a CSV uploaded through the Streamlit sidebar."""
    return pd.read_csv(uploaded_file)


def standardize_dashboard_data(data: pd.DataFrame) -> pd.DataFrame:
    """Standardize columns and validate the minimum required fields."""
    df = data.copy()

    required_cols = ["eligible_flag", "Score", "staff", "event_short"]
    missing = [col for col in required_cols if col not in df.columns]
    if missing:
        raise ValueError(
            "The input data is missing required column(s): " + ", ".join(missing)
        )

    df["eligible_flag"] = pd.to_numeric(df["eligible_flag"], errors="coerce")
    df["Score"] = pd.to_numeric(df["Score"], errors="coerce")

    for col in ["staff", "event_short", "statusId", "Outcome", "status_2026_std"]:
        if col in df.columns:
            df[col] = df[col].astype("string")

    # Prefer call-list standardized status because Potential Participants are
    # defined from the R call-list standardization logic.
    if "status_2026_std" in df.columns:
        df["participant_status"] = df["status_2026_std"].astype("string")
    elif "statusId" in df.columns:
        df["participant_status"] = df["statusId"].astype("string")
    else:
        df["participant_status"] = pd.Series(pd.NA, index=df.index, dtype="string")

    return df


def get_status_filter_col(df: pd.DataFrame) -> str:
    """Use statusId for filtering if available; otherwise use participant_status."""
    return "statusId" if "statusId" in df.columns else "participant_status"


def safe_unique(values: Iterable) -> list[str]:
    """Return sorted non-missing unique values as strings."""
    out = [str(x) for x in pd.Series(values).dropna().unique().tolist()]
    return sorted(out)


def apply_filters(
    data: pd.DataFrame,
    staff_value: str,
    event_value: str,
    status_value: str,
    include_potential: bool,
    status_filter_col: str,
) -> pd.DataFrame:
    """Apply sidebar controls before calculating metrics."""
    out = data.copy()

    if not include_potential:
        out = out[
            out["participant_status"].ne(POTENTIAL_LABEL)
            | out["participant_status"].isna()
        ]

    if staff_value != "All":
        out = out[out["staff"] == staff_value]

    # Event == Overall means no event-level filtering.
    if event_value not in ["All", "Overall"]:
        out = out[out["event_short"] == event_value]

    if status_value != "All":
        out = out[out[status_filter_col] == status_value]

    return out


def calculate_metrics(data: pd.DataFrame) -> tuple[int, float, float]:
    """Return denominator, numerator, and progress for a filtered data frame."""
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
    """Generic summary matching the R denominator/numerator/progress logic."""
    if data.empty:
        return pd.DataFrame(columns=group_cols + ["denominator", "numerator", "progress"])

    out = (
        data.groupby(group_cols, dropna=False)
        .apply(
            lambda x: pd.Series(
                calculate_metrics(x),
                index=["denominator", "numerator", "progress"],
            ),
        )
        .reset_index()
    )
    return out


def summarize_by_staff(data: pd.DataFrame) -> pd.DataFrame:
    """Summarize overall progress by staff."""
    out = summarize_dashboard(data, ["staff"])
    if out.empty:
        return out
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_by_event(data: pd.DataFrame) -> pd.DataFrame:
    """Summarize progress by event."""
    out = summarize_dashboard(data, ["event_short"])
    if out.empty:
        return out
    return out.sort_values("progress", ascending=False, na_position="last")


def summarize_staff_event(data: pd.DataFrame, add_overall: bool = True) -> pd.DataFrame:
    """Summarize progress by staff and event, optionally adding Overall rows."""
    if data.empty:
        return pd.DataFrame(
            columns=["staff", "event_short", "denominator", "numerator", "progress"]
        )

    summary_by_event = summarize_dashboard(data, ["staff", "event_short"])

    if not add_overall:
        return summary_by_event.sort_values(["staff", "event_short"])

    summary_overall = summarize_dashboard(data, ["staff"])
    summary_overall["event_short"] = "Overall"
    summary_overall = summary_overall[
        ["staff", "event_short", "denominator", "numerator", "progress"]
    ]

    out = pd.concat([summary_by_event, summary_overall], ignore_index=True)
    return out.sort_values(["staff", "event_short"])


def pct(x: float) -> str:
    """Format a numeric value as a percentage."""
    if pd.isna(x):
        return "NA"
    return f"{x:.1%}"


def format_number(x: float) -> str:
    """Format whole numbers without decimals, otherwise with one decimal."""
    if pd.isna(x):
        return "NA"
    x = float(x)
    return f"{int(x):,}" if x.is_integer() else f"{x:,.1f}"


def prep_summary_table(data: pd.DataFrame) -> pd.DataFrame:
    """Prepare the summary table for display."""
    sdf = summarize_staff_event(data, add_overall=True).copy()
    if sdf.empty:
        return sdf

    sdf["denominator"] = sdf["denominator"].astype("int64")
    sdf["numerator"] = sdf["numerator"].map(format_number)
    sdf["progress"] = sdf["progress"].map(pct)
    return sdf


def prep_detail_table(data: pd.DataFrame) -> pd.DataFrame:
    """Put the most useful columns first in the detail table."""
    preferred_cols = [
        "staff",
        "child_echo_id",
        "PIN",
        "participant_status",
        "statusId",
        "event_short",
        "eligible_flag",
        "Outcome",
        "Score",
        "age_at_caregiver_completion",
        "source_sheet",
    ]
    cols = [c for c in preferred_cols if c in data.columns]
    remaining_cols = [c for c in data.columns if c not in cols]
    return data[cols + remaining_cols].copy()


def make_bar_staff(data: pd.DataFrame):
    """Create the staff-level progress bar chart."""
    sdf = summarize_by_staff(data)
    fig = px.bar(
        sdf,
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


def make_bar_event(data: pd.DataFrame):
    """Create the event-level progress bar chart."""
    sdf = summarize_by_event(data)
    fig = px.bar(
        sdf,
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
        margin=dict(l=20, r=20, t=60, b=80),
    )
    fig.update_xaxes(tickangle=45)
    return fig


def make_heatmap(data: pd.DataFrame):
    """Create the staff-by-event progress heatmap."""
    sdf = summarize_staff_event(data, add_overall=False)
    if sdf.empty:
        return px.imshow([[None]], text_auto=False, title="Staff × Event Progress")

    pivot = sdf.pivot(index="staff", columns="event_short", values="progress")
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


def dataframe_to_csv_bytes(data: pd.DataFrame) -> bytes:
    """Convert a data frame to UTF-8 CSV bytes for download buttons."""
    return data.to_csv(index=False).encode("utf-8")


def resolve_data_source() -> tuple[pd.DataFrame | None, str]:
    """Resolve input data from latest file, snapshot file, or upload."""
    snapshots = find_snapshot_detail_files()

    data_source_options = []
    if LATEST_DETAIL_PATH.exists():
        data_source_options.append("Latest")
    if snapshots:
        data_source_options.append("Snapshot")
    data_source_options.append("Upload CSV")

    selected_source = st.sidebar.radio(
        "Data source",
        data_source_options,
        index=0,
        help="Use Latest for routine monitoring, Snapshot for historical review, or Upload CSV for quick validation.",
    )

    if selected_source == "Latest":
        raw = load_csv_from_path(str(LATEST_DETAIL_PATH))
        return raw, "Latest"

    if selected_source == "Snapshot":
        snapshot_date = st.sidebar.selectbox("Snapshot date", list(snapshots.keys()))
        raw = load_csv_from_path(str(snapshots[snapshot_date]))
        return raw, f"Snapshot: {snapshot_date}"

    uploaded_file = st.sidebar.file_uploader("Upload dashboard_detail.csv", type=["csv"])
    if uploaded_file is None:
        return None, "No uploaded file"

    raw = load_csv_from_upload(uploaded_file)
    return raw, "Uploaded CSV"


# =========================
# App layout
# =========================
st.title(APP_TITLE)
st.caption("Internal management dashboard for follow-up progress monitoring")

raw_df, source_label = resolve_data_source()

if raw_df is None:
    st.info(
        "Upload `dashboard_detail.csv`, or commit a default file to "
        "`data/latest/dashboard_detail.csv` in the GitHub repository."
    )
    st.stop()

try:
    df = standardize_dashboard_data(raw_df)
except ValueError as exc:
    st.error(str(exc))
    st.stop()

status_filter_col = get_status_filter_col(df)

st.sidebar.markdown("---")
st.sidebar.subheader("Filters")

include_potential = st.sidebar.toggle(
    "Include Potential Participants",
    value=True,
    help="Turn this off to exclude records where participant_status is Potential Participants.",
)

staff_value = st.sidebar.selectbox(
    "Staff",
    ["All"] + safe_unique(df["staff"]),
)

event_value = st.sidebar.selectbox(
    "Event",
    ["All", "Overall"] + safe_unique(df["event_short"]),
)

status_value = st.sidebar.selectbox(
    "Status",
    ["All"] + safe_unique(df[status_filter_col]),
)

filtered_df = apply_filters(
    data=df,
    staff_value=staff_value,
    event_value=event_value,
    status_value=status_value,
    include_potential=include_potential,
    status_filter_col=status_filter_col,
)

denominator, numerator, progress = calculate_metrics(filtered_df)
summary_table = prep_summary_table(filtered_df)
detail_table = prep_detail_table(filtered_df)
summary_raw = summarize_staff_event(filtered_df, add_overall=True)

potential_text = "included" if include_potential else "excluded"
st.caption(
    f"Data source: **{source_label}** | Potential Participants are **{potential_text}**. "
    "Event = Overall calculates metrics across all event rows after other filters are applied."
)

# KPI cards
kpi1, kpi2, kpi3, kpi4 = st.columns(4)
kpi1.metric("Denominator", f"{denominator:,}")
kpi2.metric("Numerator", format_number(numerator))
kpi3.metric("Progress", pct(progress))
kpi4.metric("Detail records", f"{len(filtered_df):,}")

# Charts
chart_col1, chart_col2 = st.columns(2)
with chart_col1:
    st.plotly_chart(make_bar_staff(filtered_df), use_container_width=True)
with chart_col2:
    st.plotly_chart(make_bar_event(filtered_df), use_container_width=True)

st.plotly_chart(make_heatmap(filtered_df), use_container_width=True)

# Tables and downloads
st.subheader("Summary Table")
st.dataframe(summary_table, use_container_width=True, hide_index=True)
st.download_button(
    label="Download summary CSV",
    data=dataframe_to_csv_bytes(summary_raw),
    file_name="dashboard_summary_filtered.csv",
    mime="text/csv",
)

st.subheader("Detail Data")
st.dataframe(detail_table, use_container_width=True, hide_index=True)
st.download_button(
    label="Download detail CSV",
    data=dataframe_to_csv_bytes(detail_table),
    file_name="dashboard_detail_filtered.csv",
    mime="text/csv",
)

with st.expander("Recommended PDF export workflow"):
    st.markdown(
        """
        For leadership updates, use the browser's built-in print workflow:

        1. Open the deployed dashboard link.
        2. Apply the filters you want to report.
        3. Press `Command + P` on Mac or `Ctrl + P` on Windows.
        4. Choose **Save as PDF**.

        This keeps the interactive dashboard simple while still allowing clean static reporting.
        """
    )
