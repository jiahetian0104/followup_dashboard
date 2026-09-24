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


@st.cache_data(show_spinner=False)
def load_optional_csv(path: str, modified_time_ns: int) -> pd.DataFrame:
    """Load a dashboard audit export when it is available for this version."""
    del modified_time_ns
    return pd.read_csv(path, dtype={"Participant ID": "string"})


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
    numerator = (
        summary.pivot(index="FTM", columns="Task", values="weighted_points")
        .reindex(index=pivot.index, columns=pivot.columns)
    )
    denominator = (
        summary.pivot(index="FTM", columns="Task", values="task_rows")
        .reindex(index=pivot.index, columns=pivot.columns)
    )
    hover_values = np.stack(
        [numerator.to_numpy(), denominator.to_numpy()], axis=-1
    )
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
            customdata=hover_values,
            texttemplate="%{text}",
            textfont=dict(size=13),
            colorbar=dict(title="Progress", tickformat=".0%"),
            hovertemplate=(
                "FTM: %{y}<br>Task: %{x}<br>Progress: %{z:.1%}"
                "<br>Numerator (weighted points): %{customdata[0]:.2f}"
                "<br>Denominator (task records): %{customdata[1]:,.0f}"
                "<extra></extra>"
            ),
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
        source_dir = None
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
        source_dir = selected_version.path.parent

    assignment_lookup = pd.DataFrame()
    assignment_qc = pd.DataFrame()
    manifest = pd.DataFrame()
    if source_dir is not None:
        assignment_file = (
            "participant_calendly_assignment_lookup.csv"
            if (source_dir / "participant_calendly_assignment_lookup.csv").exists()
            else "calendly_ftm_lookup.csv"
        )
        for file_name, target_name in [
            (assignment_file, "assignment_lookup"),
            ("calendly_ftm_lookup_qc.csv", "assignment_qc"),
            ("manifest.csv", "manifest"),
        ]:
            audit_path = source_dir / file_name
            if audit_path.exists():
                try:
                    loaded = load_optional_csv(
                        str(audit_path), audit_path.stat().st_mtime_ns
                    )
                    if target_name == "assignment_lookup":
                        assignment_lookup = loaded
                    elif target_name == "assignment_qc":
                        assignment_qc = loaded
                    else:
                        manifest = loaded
                except (OSError, pd.errors.ParserError):
                    pass

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

eligible_roster = filtered_roster[filtered_roster["Task Eligible"]].copy()
assigned_roster = eligible_roster[
    eligible_roster["FTM"].astype("string").fillna("Unassigned") != "Unassigned"
]
unassigned_roster = eligible_roster[
    eligible_roster["FTM"].astype("string").fillna("Unassigned") == "Unassigned"
]

metrics = calculate_kpis(filtered)
follow_up = metrics["incomplete"] + metrics["no_record"]

kpi_top = st.columns(4)
kpi_top[0].metric(
    "Current roster participants", f"{filtered_roster['Participant ID'].nunique():,}"
)
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

if not eligible_roster.empty and not unassigned_roster.empty:
    st.warning(
        f"{unassigned_roster['Participant ID'].nunique():,} selected task-eligible "
        "participants have no direct 2026 same-age Calendly host, usable "
        "Invitee name/email match, direct 2026 previous-age host, or direct "
        "2025 Calendly host. Their tasks remain Unassigned."
    )

overview_tab, trend_tab, roster_tab, participant_tab, assignment_tab = st.tabs(
    [
        "Overview",
        "Snapshot trend",
        "Participant roster",
        "Task details",
        "Assignment QA",
    ]
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
    st.caption(
        "FTM trend lines include only snapshots created under the current "
        "Calendly hierarchy, including Invitee name/email and direct 2025 "
        "fallback evidence. Older ownership methods are excluded so the chart "
        "does not mix credit rules."
    )
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

with assignment_tab:
    st.subheader("Calendly assignment coverage")
    coverage_columns = st.columns(4)
    eligible_count = eligible_roster["Participant ID"].nunique()
    assigned_count = assigned_roster["Participant ID"].nunique()
    unassigned_count = unassigned_roster["Participant ID"].nunique()
    coverage = np.nan if eligible_count == 0 else assigned_count / eligible_count
    coverage_columns[0].metric("Task-eligible participants", f"{eligible_count:,}")
    coverage_columns[1].metric("Matched to Calendly host", f"{assigned_count:,}")
    coverage_columns[2].metric("Unassigned", f"{unassigned_count:,}")
    coverage_columns[3].metric("Assignment coverage", format_percent(coverage))
    st.caption(
        "Assignment priority is: direct 2026 Calendly evidence in the same IPA "
        "age band, Invitee name/email evidence, direct 2026 evidence in the "
        "immediately previous age band, then direct 2025 Calendly evidence. "
        "Ripple and the Call List are not used for IPA credit."
    )

    if assignment_lookup.empty:
        st.info(
            "This data version predates the Calendly assignment audit export. "
            "Choose Latest or a newer snapshot to review assignment evidence."
        )
    else:
        lookup_display = assignment_lookup.copy()
        if selected_ftms:
            lookup_display = lookup_display[lookup_display["FTM"].isin(selected_ftms)]
        if selected_age_groups and "IPA Age Band" in lookup_display.columns:
            age_labels = lookup_display["IPA Age Band"].map(
                {
                    "6_11_month": "6–11 months",
                    "12_23_month": "12–23 months",
                    "24_35_month": "24–35 months",
                    "3_5yr": "3–5 years",
                    "6_10yr": "6–10 years",
                    "11_17yr": "11–17 years",
                    "18_20yr": "18–20 years",
                }
            )
            lookup_display = lookup_display[age_labels.isin(selected_age_groups)]
        if selected_cohorts or selected_scopes:
            visible_ids = set(filtered_roster["Participant ID"].dropna().astype(str))
            lookup_display = lookup_display[
                lookup_display["Participant ID"].astype(str).isin(visible_ids)
            ]

        st.markdown("#### Participant assignment decisions")
        lookup_columns = [
            "FTM",
            "Participant ID",
            "IPA Age Band",
            "Calendly FTM Matched Age Band",
            "Calendly Assignment Year",
            "Calendly Assignment Rule",
            "Calendly Appointment Date",
            "Calendly Assignment Date",
            "Calendly Host Raw",
            "Calendly Match Method",
            "Calendly FTM QA Flag",
            "Assignment Source",
        ]
        lookup_columns = [
            column for column in lookup_columns if column in lookup_display.columns
        ]
        st.dataframe(
            lookup_display[lookup_columns], width="stretch", hide_index=True, height=420
        )
        st.download_button(
            "Download filtered assignment lookup",
            data=csv_bytes(lookup_display[lookup_columns]),
            file_name="ipa_calendly_ftm_lookup_filtered.csv",
            mime="text/csv",
            width="stretch",
        )

        st.markdown("#### Calendly records needing review")
        st.caption(
            "These records were not used to assign credit because the participant, "
            "age band, or host could not be resolved confidently."
        )
        if assignment_qc.empty:
            st.success("No unresolved Calendly assignment records in this version.")
        else:
            qc_columns = [
                "Event Start Date",
                "Participant ID",
                "IPA Age Band",
                "Meeting Host",
                "Calendly FTM",
                "Participant Match Method",
                "Calendly QA Flag",
                "Calendly FTM QA Flag",
                "Child Name",
                "Meeting Notes Plain",
            ]
            qc_columns = [
                column for column in qc_columns if column in assignment_qc.columns
            ]
            st.dataframe(
                assignment_qc[qc_columns], width="stretch", hide_index=True, height=420
            )
            st.download_button(
                "Download assignment QA records",
                data=csv_bytes(assignment_qc[qc_columns]),
                file_name="ipa_calendly_assignment_qc.csv",
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
        - **Current IPA owner:** assignment priority is direct 2026 Calendly evidence in the current IPA age band, Invitee name/email evidence, direct 2026 evidence in the immediately preceding age band, and then direct 2025 Calendly evidence. Ripple and Call List owners are not used for IPA credit.
        - **Task responsibility:** every task—including Complete, No-Show, Incomplete, and No record—stays with the Calendly FTM recorded for that participant and age band. A later age band can have a different FTM without moving earlier credit. Lower-priority fallback evidence is upgraded when a higher-priority Calendly match becomes available.
        - **Unassigned:** no confident direct 2026 same-band, Invitee name/email, direct 2026 previous-band, or direct 2025 Calendly host evidence exists. These records stay visible without an FTM credit assignment. Shared-contact evidence remains unassigned unless child information distinguishes one participant.
        - **Current caseload:** only currently applicable age-band tasks are grouped under the participant's current Calendly FTM.
        - **IPA outcome source:** Ripple completion/scheduling fields plus the latest matching 2026 Calendly record for the same participant and age group. A 2025 Calendly record can supply FTM responsibility only; it never changes the 2026 task outcome.
        - **Other task source:** Ripple only.
        """
    )
