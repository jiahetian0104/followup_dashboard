"""Interactive FTM task-progress dashboard for CHARM IPA follow-up."""

from __future__ import annotations

from datetime import datetime
import importlib
import os
from pathlib import Path

import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

import dashboard_data as dashboard_data_module

# Streamlit Cloud can rerun this main file in an existing process after a Git
# update while retaining the previously imported helper module. Reload it so
# new constants and functions are available during the same hot deployment.
importlib.reload(dashboard_data_module)

from dashboard_data import (
    AGE_GROUP_ORDER,
    OUTCOME_ORDER,
    PARTICIPANT_SCOPE_ORDER,
    DataVersion,
    apply_filters,
    apply_roster_filters,
    build_snapshot_trend,
    calculate_kpis,
    discover_data_versions,
    prepare_detail_table,
    prepare_roster_table,
    read_dashboard_csv,
    read_roster_csv,
    roster_from_task_data,
    safe_unique,
    standardize_dashboard_data,
    standardize_participant_roster,
    summarize_outcomes,
    summarize_progress,
)


APP_TITLE = "CHARM IPA Task Dashboard"
BASE_DIR = Path(__file__).resolve().parent
DATA_DIR = Path(os.getenv("IPA_DASHBOARD_DATA_DIR", BASE_DIR / "data"))
LATEST_PATH = DATA_DIR / "latest" / "task_checklist_long.csv"
SNAPSHOT_DIR = DATA_DIR / "snapshots"

BLUE = "#2F6B9A"
BLUE_DARK = "#234E70"
GOLD = "#D9A441"
ORANGE = "#D97732"
GREY = "#8B929A"
INK = "#25313C"
GRID = "#E7EBEF"
OUTCOME_COLORS = {
    "Complete": BLUE,
    "No-Show": GOLD,
    "Incomplete": ORANGE,
    "No record": GREY,
}
FTM_COLORS = [BLUE_DARK, GOLD, ORANGE, GREY, "#7A5C91", "#6F7D3C", "#3A7D78", "#A65D57"]


st.set_page_config(page_title=APP_TITLE, page_icon="📋", layout="wide")

st.markdown(
    """
    <style>
    .block-container {padding-top: 1.6rem; padding-bottom: 2.5rem;}
    [data-testid="stMetric"] {
        background: #F7F9FB;
        border: 1px solid #E3E8ED;
        border-radius: 10px;
        padding: 0.8rem 1rem;
    }
    [data-testid="stMetricLabel"] {color: #52606D;}
    [data-testid="stMetricValue"] {color: #25313C;}
    </style>
    """,
    unsafe_allow_html=True,
)


@st.cache_data(show_spinner=False)
def load_version(path: str, modified_time_ns: int) -> pd.DataFrame:
    """Load and validate a version; mtime invalidates the cache after updates."""
    del modified_time_ns
    return standardize_dashboard_data(read_dashboard_csv(Path(path)))


@st.cache_data(show_spinner=False)
def load_upload(uploaded_file) -> pd.DataFrame:
    """Load and validate a user-uploaded checklist."""
    return standardize_dashboard_data(pd.read_csv(uploaded_file))


@st.cache_data(show_spinner=False)
def load_roster_version(path: str, modified_time_ns: int) -> pd.DataFrame:
    """Load the complete participant roster; mtime invalidates the cache."""
    del modified_time_ns
    return standardize_participant_roster(read_roster_csv(Path(path)))


def format_percent(value: float) -> str:
    return "—" if pd.isna(value) else f"{value:.1%}"


def format_number(value: float) -> str:
    if pd.isna(value):
        return "—"
    return f"{value:,.2f}".rstrip("0").rstrip(".")


def chart_layout(fig: go.Figure, height: int = 470) -> go.Figure:
    """Apply the shared dashboard visual system."""
    fig.update_layout(
        template="plotly_white",
        height=height,
        font=dict(family="Arial, sans-serif", size=14, color=INK),
        title_font=dict(size=20, color=INK),
        paper_bgcolor="white",
        plot_bgcolor="white",
        margin=dict(l=20, r=25, t=90, b=35),
        legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="left", x=0),
        hoverlabel=dict(font_size=14),
    )
    fig.update_xaxes(gridcolor=GRID, linecolor="#CBD3DA", zeroline=False)
    fig.update_yaxes(gridcolor=GRID, linecolor="#CBD3DA", zeroline=False)
    return fig


def make_task_progress_chart(data: pd.DataFrame) -> go.Figure:
    summary = summarize_progress(data, ["Task"]).sort_values("progress")
    summary["Progress Label"] = summary["progress"].map(format_percent)
    fig = px.bar(
        summary,
        x="progress",
        y="Task",
        orientation="h",
        text="Progress Label",
        custom_data=["participants", "task_rows", "weighted_points"],
        title=(
            "Weighted progress by task"
            "<br><sup>Score earned ÷ applicable participant-task records</sup>"
        ),
    )
    fig.update_traces(
        marker_color=BLUE,
        marker_line_color=BLUE_DARK,
        marker_line_width=0.7,
        textposition="outside",
        cliponaxis=False,
        hovertemplate=(
            "%{y}<br>Progress: %{x:.1%}<br>Participants: %{customdata[0]:,}"
            "<br>Task records: %{customdata[1]:,}<br>Points: %{customdata[2]:.2f}"
            "<extra></extra>"
        ),
    )
    fig.update_xaxes(range=[0, 1.08], tickformat=".0%", title="Progress")
    fig.update_yaxes(title=None)
    return chart_layout(fig, max(430, 46 * len(summary) + 150))


def make_outcome_chart(data: pd.DataFrame) -> go.Figure:
    summary = summarize_outcomes(data, "Task")
    fig = px.bar(
        summary,
        x="share",
        y="Task",
        color="Outcome",
        orientation="h",
        category_orders={"Outcome": OUTCOME_ORDER},
        color_discrete_map=OUTCOME_COLORS,
        custom_data=["records"],
        title=(
            "Outcome mix by task"
            "<br><sup>Share of applicable participant-task records</sup>"
        ),
    )
    fig.update_layout(
        barmode="stack",
        legend=dict(
            orientation="h",
            yanchor="top",
            y=-0.16,
            xanchor="left",
            x=0,
            title=None,
        ),
        margin=dict(l=20, r=25, t=90, b=105),
    )
    fig.update_traces(
        marker_line_color="white",
        marker_line_width=0.5,
        hovertemplate=(
            "%{y}<br>%{fullData.name}: %{x:.1%}<br>Records: %{customdata[0]:,}"
            "<extra></extra>"
        ),
    )
    fig.update_xaxes(range=[0, 1], tickformat=".0%", title="Share")
    fig.update_yaxes(title=None)
    return chart_layout(fig, max(430, 46 * data["Task"].nunique() + 150))


def make_heatmap(data: pd.DataFrame) -> go.Figure:
    summary = summarize_progress(data, ["FTM", "Task"])
    pivot = summary.pivot(index="FTM", columns="Task", values="progress")
    text = pivot.map(lambda value: "" if pd.isna(value) else f"{value:.0%}")
    fig = go.Figure(
        data=go.Heatmap(
            z=pivot.to_numpy(),
            x=pivot.columns.tolist(),
            y=pivot.index.tolist(),
            zmin=0,
            zmax=1,
            colorscale=[
                [0.0, "#EEF4F8"],
                [0.5, "#8DB5D1"],
                [1.0, BLUE_DARK],
            ],
            text=text.to_numpy(),
            texttemplate="%{text}",
            textfont=dict(size=13),
            colorbar=dict(title="Progress", tickformat=".0%"),
            hovertemplate="FTM: %{y}<br>Task: %{x}<br>Progress: %{z:.1%}<extra></extra>",
            hoverongaps=False,
        )
    )
    fig.update_layout(
        title=(
            "FTM × task progress"
            "<br><sup>Blank cells indicate no applicable participant-task records</sup>"
        )
    )
    fig.update_xaxes(title=None, tickangle=35, side="bottom")
    fig.update_yaxes(title=None, autorange="reversed")
    return chart_layout(fig, max(430, 48 * max(len(pivot.index), 4) + 190))


def make_trend_chart(trend: pd.DataFrame, metric: str) -> go.Figure:
    is_percent = metric in {"Weighted Progress", "Completion Rate"}
    fig = px.line(
        trend,
        x="Snapshot Date",
        y=metric,
        color="FTM",
        line_dash="FTM",
        symbol="FTM",
        color_discrete_sequence=FTM_COLORS,
        markers=True,
        custom_data=["FTM", "Participants", "Task Records"],
        title=(
            f"FTM {metric.lower()} across data updates"
            "<br><sup>Each point is one successful data refresh; current filters apply to every point</sup>"
        ),
    )
    hover_value = "%{y:.1%}" if is_percent else "%{y:,.0f}"
    fig.update_traces(
        line=dict(width=2.5),
        marker=dict(size=8, line=dict(color="white", width=1)),
        hovertemplate=(
            "%{customdata[0]}<br>%{x|%b %d, %Y %I:%M %p}<br>"
            + metric
            + ": "
            + hover_value
            + "<br>Participants: %{customdata[1]:,}"
            + "<br>Task records: %{customdata[2]:,}<extra></extra>"
        ),
    )
    if is_percent:
        fig.update_yaxes(range=[0, 1], tickformat=".0%", title=metric)
    else:
        fig.update_yaxes(rangemode="tozero", title=metric)
    fig.update_xaxes(title="Data refresh time")
    fig = chart_layout(fig, 560)
    fig.update_layout(
        legend=dict(
            title="FTM",
            orientation="h",
            yanchor="top",
            y=-0.18,
            xanchor="left",
            x=0,
        ),
        margin=dict(l=20, r=25, t=90, b=125),
    )
    return fig


def csv_bytes(data: pd.DataFrame) -> bytes:
    return data.to_csv(index=False).encode("utf-8-sig")


st.title(APP_TITLE)
st.caption("FTM operational view of participant task completion and IPA follow-up")

versions = discover_data_versions(LATEST_PATH, SNAPSHOT_DIR)
version_lookup = {version.label: version for version in versions}
source_options = list(version_lookup) + ["Upload CSV"]

if not source_options:
    st.error("No dashboard data source is available.")
    st.stop()

with st.sidebar:
    st.header("Dashboard controls")
    source_label = st.selectbox("Data version", source_options, index=0)

    if st.button("Refresh data", width="stretch"):
        st.cache_data.clear()
        st.rerun()

    if source_label == "Upload CSV":
        uploaded = st.file_uploader("Task checklist CSV", type=["csv"])
        if uploaded is None:
            st.info("Choose a participant_task_checklist_long CSV file.")
            st.stop()
        try:
            df = load_upload(uploaded)
        except (ValueError, pd.errors.ParserError) as exc:
            st.error(str(exc))
            st.stop()
        roster = roster_from_task_data(df)
        roster_detail = "Eligible participants reconstructed from uploaded tasks"
        source_detail = "Uploaded CSV"
    else:
        selected_version: DataVersion = version_lookup[source_label]
        try:
            df = load_version(
                str(selected_version.path), selected_version.path.stat().st_mtime_ns
            )
        except (OSError, ValueError, pd.errors.ParserError) as exc:
            st.error(str(exc))
            st.stop()
        roster_path = selected_version.path.parent / "participant_roster.csv"
        if roster_path.exists():
            try:
                roster = load_roster_version(
                    str(roster_path), roster_path.stat().st_mtime_ns
                )
                roster_detail = "Complete participant roster"
            except (OSError, ValueError, pd.errors.ParserError) as exc:
                st.error(str(exc))
                st.stop()
        else:
            roster = roster_from_task_data(df)
            roster_detail = "Eligible participants only; refresh R data to add the complete roster"
        modified = datetime.fromtimestamp(selected_version.path.stat().st_mtime)
        source_detail = f"{selected_version.label} · updated {modified:%b %d, %Y %I:%M %p}"

    st.divider()
    ftm_view_options = {"Task responsibility": "Responsible FTM"}
    if "Current FTM" in df.columns:
        ftm_view_options["Current caseload"] = "Current FTM"
    ftm_view_label = st.radio(
        "FTM view",
        list(ftm_view_options),
        help=(
            "Task responsibility keeps every age-band task with the FTM who "
            "owned it when it first became applicable. Current caseload shows "
            "only the participant's currently applicable tasks."
        ),
    )
    ftm_column = ftm_view_options[ftm_view_label]
    if ftm_column in df.columns:
        df = df.copy()
        df["FTM"] = df[ftm_column].fillna("Unassigned")
    current_tasks_only = ftm_view_label == "Current caseload"
    if current_tasks_only and "Task Stage" in df.columns:
        df = df[df["Task Stage"] == "Current"].copy()

    st.subheader("Filters")
    ftm_options = sorted(set(safe_unique(df["FTM"])) | set(safe_unique(roster["FTM"])))
    selected_ftms = st.multiselect("FTM", ftm_options)
    available_scopes = [
        scope
        for scope in PARTICIPANT_SCOPE_ORDER
        if scope in roster["Eligibility Status"].astype("string").values
    ]
    default_scopes = ["Task eligible"] if "Task eligible" in available_scopes else []
    selected_scopes = st.multiselect(
        "Participant scope",
        available_scopes,
        default=default_scopes,
        help=(
            "Potential participants and 6–11 month participants remain visible "
            "in the roster but do not create task records or enter progress denominators."
        ),
    )
    selected_tasks = st.multiselect("Task", safe_unique(df["Task"]))
    available_ages = [
        age
        for age in AGE_GROUP_ORDER
        if age in set(df["Age Group"].astype("string").values)
        | set(roster["Age Group"].astype("string").values)
    ]
    selected_age_groups = st.multiselect("Age group", available_ages)
    selected_outcomes = st.multiselect("Outcome", OUTCOME_ORDER)
    selected_cohorts = st.multiselect(
        "Cohort",
        sorted(
            set(safe_unique(df["Participant Cohort"]))
            | set(safe_unique(roster["Participant Cohort"]))
        ),
    )
    st.caption("Leave a filter blank to include all values.")

filtered = apply_filters(
    df,
    selected_ftms,
    selected_tasks,
    selected_age_groups,
    selected_outcomes,
    selected_cohorts,
)

# Participant scope controls roster inclusion and whether task rows are eligible
# to appear; it never adds non-applicable rows to the task denominator.
scope_roster = apply_roster_filters(roster, [], [], [], selected_scopes)
scope_keys = scope_roster[["Participant ID", "Participant Cohort"]].drop_duplicates()
filtered = filtered.merge(
    scope_keys,
    on=["Participant ID", "Participant Cohort"],
    how="inner",
)

filtered_roster = apply_roster_filters(
    roster,
    selected_ftms,
    selected_age_groups,
    selected_cohorts,
    selected_scopes,
)

st.caption(
    f"Data source: **{source_detail}** · FTM view: **{ftm_view_label}** · "
    f"Roster: **{roster_detail}**"
)

metrics = calculate_kpis(filtered)
follow_up = metrics["incomplete"] + metrics["no_record"]

kpi_top = st.columns(4)
kpi_top[0].metric("Roster participants", f"{filtered_roster['Participant ID'].nunique():,}")
kpi_top[1].metric("Participants with tasks", f"{metrics['participants']:,}")
kpi_top[2].metric("Applicable task records", f"{metrics['task_rows']:,}")
kpi_top[3].metric("Weighted progress", format_percent(metrics["weighted_progress"]))
kpi_bottom = st.columns(3)
kpi_bottom[0].metric("Complete", f"{metrics['complete']:,}")
kpi_bottom[1].metric("No-Show", f"{metrics['no_show']:,}")
kpi_bottom[2].metric("Needs follow-up", f"{follow_up:,}")

st.caption(
    "Weighted progress uses Complete = 1, No-Show = 0.25, Incomplete = 0, "
    "and No record = 0 in the denominator."
)

overview_tab, trend_tab, roster_tab, participant_tab = st.tabs(
    ["Overview", "Snapshot trend", "Participant roster", "Task details"]
)

with overview_tab:
    if filtered.empty:
        st.info(
            "The selected participant scope has no applicable task records. "
            "Open Participant roster to review these participants."
        )
    else:
        st.plotly_chart(make_task_progress_chart(filtered), width="stretch")
        st.plotly_chart(make_outcome_chart(filtered), width="stretch")

        st.plotly_chart(make_heatmap(filtered), width="stretch")

        summary = summarize_progress(filtered, ["FTM", "Age Group", "Task"])
        summary["Progress"] = summary["progress"].map(format_percent)
        summary["Weighted points"] = summary["weighted_points"].map(format_number)
        summary = summary.rename(
            columns={
                "task_rows": "Task records",
                "participants": "Participants",
            }
        )
        st.subheader("Filtered summary")
        st.dataframe(
            summary[
                [
                    "FTM",
                    "Age Group",
                    "Task",
                    "Participants",
                    "Task records",
                    "Weighted points",
                    "Progress",
                ]
            ],
            width="stretch",
            hide_index=True,
        )

with trend_tab:
    trend_metric = st.selectbox(
        "Trend metric",
        [
            "Weighted Progress",
            "Completion Rate",
            "Complete",
            "No-Show",
            "Needs Follow-up",
            "Participants",
            "Task Records",
        ],
    )
    task_scope_selected = not selected_scopes or "Task eligible" in selected_scopes
    trend = (
        build_snapshot_trend(
            versions,
            selected_ftms,
            selected_tasks,
            selected_age_groups,
            selected_outcomes,
            selected_cohorts,
            ftm_column,
            current_tasks_only,
        )
        if task_scope_selected
        else pd.DataFrame()
    )
    snapshot_count = trend["Snapshot Date"].nunique() if not trend.empty else 0
    if snapshot_count >= 2:
        st.plotly_chart(make_trend_chart(trend, trend_metric), width="stretch")
        st.dataframe(trend, width="stretch", hide_index=True)
    elif not trend.empty:
        st.info(
            "One refresh point is available. The trend line will appear after the "
            "next successful data update."
        )
        trend_display = trend.copy()
        trend_display["Weighted Progress"] = trend_display["Weighted Progress"].map(
            format_percent
        )
        trend_display["Completion Rate"] = trend_display["Completion Rate"].map(
            format_percent
        )
        st.dataframe(trend_display, width="stretch", hide_index=True)
    else:
        st.info(
            "No task trend is available for this selection. Potential and 6–11 "
            "month participants do not enter task-progress denominators."
        )

with roster_tab:
    st.subheader("Filtered participant roster")
    st.caption(
        "Participant scope, FTM, age group, and cohort filters apply here. "
        "Task and outcome filters apply only to task-based views."
    )
    roster_detail_table = prepare_roster_table(filtered_roster)
    if roster_detail_table.empty:
        st.info("No participants match the selected roster filters.")
    else:
        st.dataframe(roster_detail_table, width="stretch", hide_index=True, height=520)
        st.download_button(
            "Download filtered participant roster",
            data=csv_bytes(roster_detail_table),
            file_name="ipa_participant_roster_filtered.csv",
            mime="text/csv",
            width="stretch",
        )

with participant_tab:
    participants = safe_unique(filtered["Participant ID"])
    selected_participant = st.selectbox("Participant", ["All"] + participants)
    participant_rows = (
        filtered
        if selected_participant == "All"
        else filtered[filtered["Participant ID"] == selected_participant]
    )
    detail = prepare_detail_table(participant_rows)
    st.dataframe(detail, width="stretch", hide_index=True, height=520)
    st.download_button(
        "Download filtered participant-task CSV",
        data=csv_bytes(detail),
        file_name="ipa_task_checklist_filtered.csv",
        mime="text/csv",
        width="stretch",
    )

with st.expander("Metric definitions"):
    st.markdown(
        """
        - **Applicable tasks:** participant-task-age-band rows created when the participant enters an eligible age group.
        - **Participant scope:** the complete roster includes task-eligible participants, 6–11 month participants, potential participants, and records needing review.
        - **Non-task participants:** potential and 6–11 month participants are visible in the roster but do not enter task counts, progress, or completion-rate denominators.
        - **Weighted progress:** total task score divided by applicable task rows.
        - **Needs follow-up:** `Incomplete` plus `No record` task rows.
        - **Task responsibility:** every task—including Complete, No-Show, Incomplete, and No record—stays with the FTM responsible when that age-band task first became applicable.
        - **Current caseload:** only currently applicable age-band tasks are grouped under the participant's current FTM.
        - **IPA source:** Ripple completion/scheduling fields plus the latest matching Calendly record for the same participant and age group.
        - **Other task source:** Ripple only.
        """
    )
