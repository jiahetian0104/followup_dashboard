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
    IPA_METRICS,
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
    standardize_ipa_records,
    filter_ipa_records,
    summarize_ipa_activity,
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


def make_ipa_heatmap(summary: pd.DataFrame) -> go.Figure:
    """FTM names horizontally; four absolute activity counts vertically."""
    values = summary.set_index("FTM")[IPA_METRICS].T
    z = values.apply(pd.to_numeric, errors="coerce").to_numpy(dtype=float, na_value=np.nan)
    fig = go.Figure(go.Heatmap(
        x=values.columns.tolist(),
        y=["Complete", "No-show", "Cancel", "Reschedule"],
        z=z, zmin=0,
        colorscale=[[0, "#EEF4F8"], [0.5, "#8DB5D1"], [1, BLUE_DARK]],
        text=np.where(np.isnan(z), "", np.char.mod("%.0f", z)),
        texttemplate="%{text}", textfont=dict(size=16),
        colorbar=dict(title="Count"), hoverongaps=False,
        hovertemplate="FTM: %{x}<br>%{y}: %{z:,.0f}<extra></extra>",
    ))
    fig.update_layout(title="IPA completion and appointment activity by FTM")
    fig.update_xaxes(title=None, tickangle=30 if len(values.columns) > 7 else 0)
    fig.update_yaxes(title=None, autorange="reversed")
    return chart_layout(fig, 390)


def make_ipa_performance_chart(summary: pd.DataFrame) -> go.Figure:
    """Weighted outcomes per distinct participant-age-band served by each FTM."""
    scores = summary.dropna(subset=["Weighted IPA Performance"]).copy()
    fig = go.Figure(go.Bar(
        x=scores["FTM"], y=scores["Weighted IPA Performance"], marker_color=BLUE,
        text=scores["Weighted IPA Performance"].map(format_percent), textposition="outside",
        cliponaxis=False,
        customdata=scores[["Complete Count", "No-show Count", "Weighted Points",
                           "Participant Age Bands"]].to_numpy(),
        hovertemplate=(
            "FTM: %{x}<br>Weighted IPA performance: %{y:.1%}"
            "<br>Complete: %{customdata[0]:,.0f}<br>No-show: %{customdata[1]:,.0f}"
            "<br>Weighted points: %{customdata[2]:.2f}"
            "<br>Participant age bands: %{customdata[3]:,.0f}<extra></extra>"
        ),
    ))
    maximum = scores["Weighted IPA Performance"].max() if not scores.empty else 0
    fig.update_layout(title="Weighted IPA performance by FTM")
    fig.update_xaxes(title=None, tickangle=30 if len(scores) > 7 else 0)
    # Repeat no-shows can legitimately push this score above 100%; do not cap.
    fig.update_yaxes(title="Weighted IPA performance", tickformat=".0%",
                     range=[0, max(1.05, float(maximum) * 1.15)])
    return chart_layout(fig, 410)


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
            "was assigned when it first became applicable. Current caseload shows "
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

    # IPA activity has its own event-based filters in the IPA tab. The other
    # tabs continue to use task responsibility and current-caseload filters.
    df = df[df["Task"] != "In-Person Assessments"].copy()
    st.subheader("Other task / roster filters")
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
    f"Data source: **{source_detail}**"
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

ipa_tab, overview_tab, trend_tab, roster_tab, participant_tab, assignment_tab = st.tabs(
    [
        "IPA activity",
        "Other tasks",
        "Other task trend",
        "Participant roster",
        "Task details",
        "Assignment QA",
    ]
)

with ipa_tab:
    records_path = source_dir / "participant_ipa_records.csv" if source_dir else None
    ftm_records_path = source_dir / "participant_ipa_ftm_records.csv" if source_dir else None
    if not records_path or not records_path.exists() or not ftm_records_path.exists():
        st.info(
            "IPA appointment history is not available for this data version. "
            "Run the updated IPA Data.R to export participant IPA records and FTM activity. "
            "Latest-task records cannot reconstruct historical cancellations or reschedules."
        )
    else:
        try:
            ipa_records = standardize_ipa_records(load_optional_csv(
                str(records_path), records_path.stat().st_mtime_ns
            ))
            ipa_ftm_records = standardize_ipa_records(load_optional_csv(
                str(ftm_records_path), ftm_records_path.stat().st_mtime_ns
            ), ftm_records=True)
        except (OSError, ValueError, pd.errors.ParserError) as exc:
            st.error(f"Cannot load IPA activity: {exc}")
        else:
            st.caption(
                "Appointment-year scope: 2026. All recorded age bands are included. "
                "IPA filters below are independent of the sidebar's other-task and roster filters."
            )
            controls = st.columns(3)
            ipa_ftms = controls[0].multiselect(
                "IPA FTM", safe_unique(ipa_ftm_records["FTM"]), key="ipa_ftms"
            )
            ipa_ages = controls[1].multiselect(
                "IPA age band", [age for age in AGE_GROUP_ORDER
                                 if age in set(ipa_records["Age Group"])], key="ipa_ages"
            )
            ipa_cohorts = controls[2].multiselect(
                "IPA cohort", safe_unique(ipa_records["Participant Cohort"]), key="ipa_cohorts"
            )
            activity = filter_ipa_records(ipa_ftm_records, ipa_ftms, ipa_ages, ipa_cohorts)
            participant_records = ipa_records.copy()
            if ipa_ages:
                participant_records = participant_records[participant_records["Age Group"].isin(ipa_ages)]
            if ipa_cohorts:
                participant_records = participant_records[participant_records["Participant Cohort"].isin(ipa_cohorts)]
            if ipa_ftms:
                keys = ["Participant ID", "Participant Cohort", "IPA Age Band"]
                participant_records = participant_records.merge(activity[keys].drop_duplicates(), on=keys)
            if activity.empty:
                st.info("No IPA activity matches these filters.")
            else:
                ipa_summary = summarize_ipa_activity(activity)
                columns = st.columns(4)
                for column, metric, label in zip(columns, IPA_METRICS,
                                                ["Complete", "No-show", "Cancel", "Reschedule"]):
                    total = activity[metric].sum() if activity[metric].notna().all() else np.nan
                    column.metric(label, format_number(total))
                st.plotly_chart(make_ipa_heatmap(ipa_summary), width="stretch")
                st.caption(
                    "Cancel excludes reschedules. Reschedule counts actions on the old booking. "
                    "Appointment activity stays with that booking's host. Complete is credited once "
                    "per participant and age band to the single eligible host on the Ripple completion date. "
                    "Cohort differences do not block completion matching. Bookings from 2025 and 2026 can supply "
                    "completion credit. Prefer the same age band; if it has no bookings, use the immediately "
                    "preceding age band, preferring a completion-date match and then the latest booking. "
                    "The latest booking supplies "
                    "fallback completion credit, regardless of booking cancellation/no-show status. "
                    "A booking covering multiple children counts once for each child. "
                    "Blank counts mean missing evidence; darker cells mean more activity."
                )
                pending = activity.loc[activity["FTM"] == "Pending verification", "Complete Count"].sum()
                if pending:
                    st.warning(f"{pending:,} completed IPA records need completion-host verification.")
                if activity["FTM"].str.startswith("Shared hosts:").any():
                    st.info("Bookings with multiple hosts stay together under Shared hosts; they are not duplicated across FTMs.")
                st.subheader("FTM IPA work summary")
                summary_display = ipa_summary.copy()
                summary_display["Weighted IPA Performance"] = summary_display["Weighted IPA Performance"].map(format_percent)
                st.dataframe(summary_display, hide_index=True, width="stretch")
                st.download_button("Download FTM IPA summary", csv_bytes(ipa_summary),
                                   "ftm_ipa_summary.csv", "text/csv", key="ipa_summary_download")
                if ipa_summary["Weighted IPA Performance"].notna().any():
                    st.plotly_chart(make_ipa_performance_chart(ipa_summary), width="stretch")
                else:
                    st.info("No individual FTM has a defined weighted IPA performance for these filters.")
                st.caption(
                    "Weighted IPA performance = (Complete + 0.25 × No-show) ÷ participant age bands. "
                    "Each participant and age band counts once within each FTM's attributed activity, "
                    "including records with only cancellations or reschedules. Counts and denominator "
                    "use the same filters. Repeated no-shows can produce a score above 100%; this is "
                    "a weighted performance score, not a completion rate. Pending verification, "
                    "Unassigned, and Shared hosts do not receive an individual FTM percentage."
                )
                fallback_keys = activity.loc[
                    (activity["Complete Count"] > 0) &
                    (activity["Completion Attribution"].str.contains("fallback|Previous-age-band", na=False)),
                    ["Participant ID", "Participant Cohort", "IPA Age Band"]
                ].drop_duplicates()
                if not fallback_keys.empty:
                    st.caption(f"{len(fallback_keys):,} completions use fallback booking evidence or a previous-age-band match.")
                with st.expander("Activity attributed to each FTM"):
                    st.dataframe(activity, hide_index=True, width="stretch")
                    st.download_button("Download participant × age band × FTM activity", csv_bytes(activity),
                                       "participant_ipa_ftm_records.csv", "text/csv", key="ipa_ftm_download")

            st.subheader("Participant IPA records")
            st.caption(
                "One row per participant and recorded age band. Counts include the entire appointment "
                "history for the selected age bands and cohorts, including bookings with other FTMs."
            )
            participant_records = participant_records.drop(columns=["firstName", "lastName"], errors="ignore")
            record_order = ["Participant ID", "Age Group", "IPA Complete", "No-show Count",
                            "Cancel Count", "Reschedule Count", "Completion FTM",
                            "Completion Date", "Completion Attribution", "Completion Fallback Appointment Date", "Appointment Count",
                            "Participant Cohort", "IPA Age Band", "IPA Year"]
            record_order = [column for column in record_order if column in participant_records]
            record_order += [column for column in participant_records if column not in record_order]
            st.dataframe(participant_records, hide_index=True, width="stretch", column_order=record_order)
            st.download_button("Download participant IPA records", csv_bytes(participant_records),
                               "participant_ipa_records.csv", "text/csv", key="ipa_records_download")
            history_path = source_dir / "ipa_appointment_history.csv"
            if history_path.exists():
                history = load_optional_csv(str(history_path), history_path.stat().st_mtime_ns)
                unresolved = history[["Participant ID", "Participant Cohort", "IPA Age Band", "Invitee UUID"]].isna().any(axis=1)
                if unresolved.any():
                    st.caption(
                        f"{unresolved.sum():,} appointment rows in this version have unresolved identity "
                        "or age-band information and are excluded from participant and FTM totals."
                    )
                history["Age Group"] = history["IPA Age Band"].map(dashboard_data_module.AGE_GROUP_LABELS)
                visible_history = filter_ipa_records(history, ipa_ftms, ipa_ages, ipa_cohorts)
                with st.expander("Appointment history and reschedule links"):
                    st.dataframe(visible_history, hide_index=True, width="stretch")
                    st.download_button("Download IPA appointment history", csv_bytes(visible_history),
                                       "ipa_appointment_history.csv", "text/csv", key="ipa_history_download")

with overview_tab:
    st.caption("IPA completion and appointment activity are reported separately in IPA activity.")
    st.caption(f"FTM view: {ftm_view_label} · Roster: {roster_detail}")
    if not eligible_roster.empty and not unassigned_roster.empty:
        st.warning(
            f"{unassigned_roster['Participant ID'].nunique():,} selected task-eligible "
            "participants have no resolved owner. Their other tasks remain Unassigned."
        )
    kpi_top = st.columns(4)
    kpi_top[0].metric("Participants with other tasks", f"{metrics['participants']:,}")
    kpi_top[1].metric("Other task records", f"{metrics['task_rows']:,}")
    kpi_top[2].metric("Complete", f"{metrics['complete']:,}")
    kpi_top[3].metric("Weighted progress", format_percent(metrics["weighted_progress"]))
    st.caption("Other-task progress: Complete = 1; Incomplete and No record = 0.")
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
        "fallback evidence, plus the last-resort staff-verified recent IPA FTM. "
        "Older ownership methods are excluded so the chart does not mix credit "
        "rules."
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
            selected_tasks or safe_unique(df["Task"]),
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
    st.subheader("IPA FTM assignment coverage")
    coverage_columns = st.columns(4)
    eligible_count = eligible_roster["Participant ID"].nunique()
    assigned_count = assigned_roster["Participant ID"].nunique()
    unassigned_count = unassigned_roster["Participant ID"].nunique()
    coverage = np.nan if eligible_count == 0 else assigned_count / eligible_count
    coverage_columns[0].metric("Task-eligible participants", f"{eligible_count:,}")
    coverage_columns[1].metric("Assigned IPA FTM", f"{assigned_count:,}")
    coverage_columns[2].metric("Unassigned", f"{unassigned_count:,}")
    coverage_columns[3].metric("Assignment coverage", format_percent(coverage))
    st.caption(
        "Assignment priority is: direct 2026 Calendly evidence in the same IPA "
        "age band, Invitee name/email evidence, direct 2026 evidence in the "
        "immediately previous age band, direct 2025 Calendly evidence, then a "
        "staff-verified recent IPA FTM fallback. Ripple and the Call List are "
        "not used for IPA credit."
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
        - **Weighted progress:** total task score divided by applicable other-task rows; IPA is excluded.
        - **Needs follow-up:** `Incomplete` plus `No record` task rows.
        - **Current IPA owner:** assignment priority is direct 2026 Calendly evidence in the current IPA age band, Invitee name/email evidence, direct 2026 evidence in the immediately preceding age band, direct 2025 Calendly evidence, and finally a staff-verified recent IPA FTM fallback. Ripple and Call List owners are not used for IPA credit.
        - **Other-task responsibility:** the existing responsibility ledger is used for non-IPA tasks. It does not determine IPA appointment activity or completion credit.
        - **Unassigned:** no confident direct 2026 same-band, Invitee name/email, direct 2026 previous-band, direct 2025 Calendly host, or staff-verified recent IPA FTM evidence exists. These records stay visible without an FTM credit assignment. Shared-contact evidence remains unassigned unless child information distinguishes one participant.
        - **Current caseload:** only currently applicable age-band tasks are grouped under the participant's current Calendly FTM.
        - **IPA records:** one row per participant and recorded age band, using all 2026 Calendly appointments and Ripple completion/scheduling evidence. Historical counts remain after completion.
        - **IPA No-show:** distinct participant/invitee bookings marked No Show.
        - **IPA Cancel:** canceled bookings with Rescheduled = FALSE. Rescheduled cancellations are excluded; missing rescheduling evidence stays unknown.
        - **IPA Reschedule:** each old booking marked Rescheduled = TRUE counts as one action, including intermediate bookings in a reschedule chain.
        - **IPA completion credit:** Ripple proves completion. Prefer the single eligible Calendly host on the completion date (not canceled or no-show). Ignore cohort differences when matching completion hosts. Use 2025 and 2026 bookings, preferring the same age band: completion-date match first, then latest booking. Only if the same age band has no bookings, try the immediately preceding age band with the same date-first priority. Latest-booking fallbacks may include canceled/no-show bookings and are labeled. Latest is ordered by appointment time then booking creation time. Conflicting latest hosts, missing hosts, or no usable same/previous-band booking remain Pending verification. Older bookings supply completion evidence only; appointment workload remains in the reporting year.
        - **Weighted IPA performance:** (Complete + 0.25 × No-show) divided by distinct participant/cohort/age-band records attributed to each FTM. Cancel/reschedule-only records enter the denominator. Repeated no-shows can push the score above 100%; the score is not a completion rate.
        - **Shared IPA hosts:** appointment activity is retained once under a combined Shared hosts label, without duplicating counts across individual FTMs.
        - **Other task source:** Ripple only.
        """
    )
