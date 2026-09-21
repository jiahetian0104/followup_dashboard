"""Data loading, validation, filtering, and summaries for the IPA dashboard."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import numpy as np
import pandas as pd


REQUIRED_COLUMNS = {
    "FTM",
    "Participant ID",
    "Participant Cohort",
    "IPA Age Band",
    "Task",
    "Outcome",
    "Score",
}

ROSTER_REQUIRED_COLUMNS = {
    "FTM",
    "Participant ID",
    "Participant Cohort",
    "statusId",
    "Age Group",
    "Task Eligible",
    "Eligibility Status",
}

AGE_GROUP_LABELS = {
    "6_11_month": "6–11 months",
    "12_23_month": "12–23 months",
    "24_35_month": "24–35 months",
    "3_5yr": "3–5 years",
    "6_10yr": "6–10 years",
    "11_17yr": "11–17 years",
    "18_20yr": "18–20 years",
    "potential": "Potential participants",
    "needs_review": "Needs review",
}

AGE_GROUP_ORDER = list(AGE_GROUP_LABELS.values())
OUTCOME_ORDER = ["Complete", "No-Show", "Incomplete", "No record"]
PARTICIPANT_SCOPE_ORDER = [
    "Task eligible",
    "6-11 months",
    "Potential participants",
    "Needs review",
]
TREND_COLUMNS = [
    "Snapshot Date",
    "FTM",
    "Weighted Progress",
    "Completion Rate",
    "Complete",
    "No-Show",
    "Needs Follow-up",
    "Participants",
    "Task Records",
]


@dataclass(frozen=True)
class DataVersion:
    """A selectable dashboard dataset."""

    label: str
    path: Path
    snapshot_date: str | None
    is_latest: bool = False


def discover_data_versions(latest_path: Path, snapshot_dir: Path) -> list[DataVersion]:
    """Return Latest followed by available dated snapshots, newest first."""
    versions: list[DataVersion] = []
    if latest_path.exists():
        versions.append(DataVersion("Latest", latest_path, None, True))

    if snapshot_dir.exists():
        for folder in sorted(snapshot_dir.iterdir(), reverse=True):
            if not folder.is_dir():
                continue
            candidates = [
                folder / "task_checklist_long.csv",
                folder / "participant_task_checklist_long.csv",
                folder / "detail.csv",
            ]
            file_path = next((path for path in candidates if path.exists()), None)
            if file_path is not None:
                snapshot_value = folder.name
                manifest_path = folder / "manifest.csv"
                if manifest_path.exists():
                    try:
                        manifest = pd.read_csv(manifest_path, nrows=1)
                        generated_at = str(manifest.loc[0, "generated_at"])
                        # R appends a timezone abbreviation; the dashboard already
                        # runs in the same local timezone, so keep the wall time.
                        snapshot_value = generated_at.rsplit(" ", 1)[0]
                    except (OSError, KeyError, IndexError, pd.errors.ParserError):
                        snapshot_value = folder.name
                versions.append(
                    DataVersion(
                        label=f"Snapshot · {format_snapshot_label(snapshot_value)}",
                        path=file_path,
                        snapshot_date=snapshot_value,
                    )
                )
    return versions


def parse_snapshot_datetime(snapshot_id: str) -> pd.Timestamp:
    """Parse legacy daily and new per-refresh snapshot folder names."""
    for date_format in ("%Y-%m-%d_%H%M%S", "%Y-%m-%d"):
        try:
            return pd.to_datetime(snapshot_id, format=date_format)
        except ValueError:
            continue
    return pd.to_datetime(snapshot_id, errors="coerce")


def format_snapshot_label(snapshot_id: str) -> str:
    """Return a concise human-readable snapshot label."""
    timestamp = parse_snapshot_datetime(snapshot_id)
    if pd.isna(timestamp):
        return snapshot_id
    if "_" in snapshot_id or len(snapshot_id) > 10:
        return timestamp.strftime("%b %d, %Y · %I:%M:%S %p")
    return timestamp.strftime("%b %d, %Y")


def read_dashboard_csv(path: Path) -> pd.DataFrame:
    """Read a task checklist CSV without coercing participant IDs."""
    return pd.read_csv(path, dtype={"Participant ID": "string", "PIN": "string"})


def read_roster_csv(path: Path) -> pd.DataFrame:
    """Read the participant roster without coercing participant identifiers."""
    return pd.read_csv(path, dtype={"Participant ID": "string", "PIN": "string"})


def standardize_dashboard_data(data: pd.DataFrame) -> pd.DataFrame:
    """Validate and standardize the participant-task long table."""
    df = data.copy()
    missing = sorted(REQUIRED_COLUMNS - set(df.columns))
    if missing:
        raise ValueError("Missing required column(s): " + ", ".join(missing))

    text_columns = [
        "FTM",
        "Current FTM",
        "Responsible FTM",
        "Task Stage",
        "Participant ID",
        "firstName",
        "lastName",
        "Participant Cohort",
        "statusId",
        "IPA Age Band",
        "Task",
        "Outcome",
        "Source",
        "Previous Roster FTM",
        "Assignment Source",
        "Calendly FTM Matched Age Band",
        "Calendly Assignment Year",
        "Calendly Assignment Rule",
        "Calendly Host Raw",
        "Calendly Match Method",
        "Calendly FTM QA Flag",
    ]
    for column in text_columns:
        if column in df.columns:
            df[column] = df[column].astype("string").str.strip()

    df["FTM"] = df["FTM"].fillna("Unassigned")
    df["Score"] = pd.to_numeric(df["Score"], errors="coerce")
    df["Age Group"] = df["IPA Age Band"].map(AGE_GROUP_LABELS).fillna(
        df["IPA Age Band"]
    )
    df["Age Group"] = pd.Categorical(
        df["Age Group"], categories=AGE_GROUP_ORDER, ordered=True
    )
    df["Outcome"] = pd.Categorical(
        df["Outcome"], categories=OUTCOME_ORDER, ordered=True
    )

    for column in [
        "birthday",
        "Outcome_Date",
        "Calendly_Appointment_Date",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
    ]:
        if column in df.columns:
            df[column] = pd.to_datetime(df[column], errors="coerce")

    duplicate_key = ["Participant ID", "Participant Cohort", "IPA Age Band", "Task"]
    duplicates = df.duplicated(duplicate_key, keep=False)
    if duplicates.any():
        examples = (
            df.loc[duplicates, duplicate_key]
            .drop_duplicates()
            .head(5)
            .astype(str)
            .agg(" / ".join, axis=1)
            .tolist()
        )
        raise ValueError(
            "Participant-task rows are not unique. Examples: " + "; ".join(examples)
        )

    return df


def standardize_participant_roster(data: pd.DataFrame) -> pd.DataFrame:
    """Validate the complete roster, including participants without tasks."""
    df = data.copy()
    missing = sorted(ROSTER_REQUIRED_COLUMNS - set(df.columns))
    if missing:
        raise ValueError("Missing roster column(s): " + ", ".join(missing))

    text_columns = [
        "FTM",
        "Previous Roster FTM",
        "Participant ID",
        "firstName",
        "lastName",
        "Participant Cohort",
        "statusId",
        "Age Group",
        "IPA Age Band",
        "Eligibility Status",
        "Assignment Source",
        "Calendly FTM Matched Age Band",
        "Calendly Assignment Year",
        "Calendly Assignment Rule",
        "Calendly Host Raw",
        "Calendly Match Method",
        "Calendly FTM QA Flag",
    ]
    for column in text_columns:
        if column in df.columns:
            df[column] = df[column].astype("string").str.strip()

    df["FTM"] = df["FTM"].fillna("Unassigned")
    df["Task Eligible"] = (
        df["Task Eligible"]
        .astype("string")
        .str.lower()
        .isin(["true", "1", "yes", "y"])
    )
    df["Age Group"] = df["Age Group"].map(AGE_GROUP_LABELS).fillna(df["Age Group"])
    df["Age Group"] = pd.Categorical(
        df["Age Group"], categories=AGE_GROUP_ORDER, ordered=True
    )
    for column in [
        "birthday",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
    ]:
        if column in df.columns:
            df[column] = pd.to_datetime(df[column], errors="coerce")

    duplicate_key = ["Participant ID", "Participant Cohort"]
    duplicates = df.duplicated(duplicate_key, keep=False)
    if duplicates.any():
        examples = (
            df.loc[duplicates, duplicate_key]
            .drop_duplicates()
            .head(5)
            .astype(str)
            .agg(" / ".join, axis=1)
            .tolist()
        )
        raise ValueError(
            "Participant roster rows are not unique. Examples: " + "; ".join(examples)
        )
    return df


def roster_from_task_data(data: pd.DataFrame) -> pd.DataFrame:
    """Create an eligible-only roster for legacy snapshots without a roster file."""
    preferred = [
        "FTM",
        "Participant ID",
        "firstName",
        "lastName",
        "birthday",
        "Participant Cohort",
        "statusId",
        "Age Group",
    ]
    roster = data[[column for column in preferred if column in data.columns]].copy()
    roster = roster.drop_duplicates(["Participant ID", "Participant Cohort"])
    roster["Task Eligible"] = True
    roster["Eligibility Status"] = "Task eligible"
    return roster


def safe_unique(values: Iterable) -> list[str]:
    """Return sorted non-missing values as strings."""
    return sorted(str(value) for value in pd.Series(values).dropna().unique())


def apply_filters(
    data: pd.DataFrame,
    ftms: list[str],
    tasks: list[str],
    age_groups: list[str],
    outcomes: list[str],
    cohorts: list[str],
) -> pd.DataFrame:
    """Apply all dashboard-wide filters."""
    df = data.copy()
    if ftms:
        df = df[df["FTM"].isin(ftms)]
    if tasks:
        df = df[df["Task"].isin(tasks)]
    if age_groups:
        df = df[df["Age Group"].astype("string").isin(age_groups)]
    if outcomes:
        df = df[df["Outcome"].astype("string").isin(outcomes)]
    if cohorts:
        df = df[df["Participant Cohort"].isin(cohorts)]
    return df


def apply_roster_filters(
    data: pd.DataFrame,
    ftms: list[str],
    age_groups: list[str],
    cohorts: list[str],
    participant_scopes: list[str],
) -> pd.DataFrame:
    """Apply roster-level filters without changing task denominators."""
    df = data.copy()
    if ftms:
        df = df[df["FTM"].isin(ftms)]
    if age_groups:
        df = df[df["Age Group"].astype("string").isin(age_groups)]
    if cohorts:
        df = df[df["Participant Cohort"].isin(cohorts)]
    if participant_scopes:
        df = df[df["Eligibility Status"].isin(participant_scopes)]
    return df


def calculate_kpis(data: pd.DataFrame) -> dict[str, float | int]:
    """Calculate dashboard headline metrics at participant-task grain."""
    task_rows = len(data)
    participants = data["Participant ID"].nunique()
    weighted_points = float(data["Score"].fillna(0).sum())
    weighted_progress = np.nan if task_rows == 0 else weighted_points / task_rows
    complete = int((data["Outcome"] == "Complete").sum())
    no_show = int((data["Outcome"] == "No-Show").sum())
    incomplete = int((data["Outcome"] == "Incomplete").sum())
    no_record = int((data["Outcome"] == "No record").sum())
    return {
        "participants": participants,
        "task_rows": task_rows,
        "weighted_points": weighted_points,
        "weighted_progress": weighted_progress,
        "complete": complete,
        "no_show": no_show,
        "incomplete": incomplete,
        "no_record": no_record,
    }


def summarize_progress(data: pd.DataFrame, group_columns: list[str]) -> pd.DataFrame:
    """Weighted task progress by the requested dimensions."""
    if data.empty:
        return pd.DataFrame(
            columns=group_columns
            + ["task_rows", "participants", "weighted_points", "progress"]
        )

    summary = (
        data.assign(_score=data["Score"].fillna(0))
        .groupby(group_columns, observed=True, dropna=False)
        .agg(
            task_rows=("Task", "size"),
            participants=("Participant ID", "nunique"),
            weighted_points=("_score", "sum"),
        )
        .reset_index()
    )
    summary["progress"] = summary["weighted_points"] / summary["task_rows"]
    return summary


def summarize_outcomes(data: pd.DataFrame, group_column: str) -> pd.DataFrame:
    """Outcome counts and within-group shares."""
    if data.empty:
        return pd.DataFrame(columns=[group_column, "Outcome", "records", "share"])

    summary = (
        data.groupby([group_column, "Outcome"], observed=True, dropna=False)
        .size()
        .rename("records")
        .reset_index()
    )
    totals = summary.groupby(group_column, observed=True)["records"].transform("sum")
    summary["share"] = summary["records"] / totals
    return summary


def build_snapshot_trend(
    versions: list[DataVersion],
    ftms: list[str],
    tasks: list[str],
    age_groups: list[str],
    outcomes: list[str],
    cohorts: list[str],
    ftm_column: str = "FTM",
    current_tasks_only: bool = False,
    calendly_assignment_only: bool = True,
) -> pd.DataFrame:
    """Apply filters to comparable snapshots and calculate one row per FTM.

    FTM attribution changed from Ripple/Call List to Calendly host in September
    2026 and later added a 2025 Calendly fallback. By default, snapshots without
    the current assignment-year field are excluded so a trend line never mixes
    different ownership definitions.
    """
    records: list[dict[str, object]] = []
    for version in versions:
        if version.is_latest or version.snapshot_date is None:
            continue
        try:
            snapshot = standardize_dashboard_data(read_dashboard_csv(version.path))
        except (OSError, ValueError, pd.errors.ParserError):
            continue
        if calendly_assignment_only:
            if "Assignment Source" not in snapshot.columns:
                continue
            if "Calendly Assignment Year" not in snapshot.columns:
                continue
            assignment_sources = (
                snapshot["Assignment Source"].astype("string").dropna().str.strip()
            )
            if not assignment_sources.str.startswith("Calendly").any():
                continue
        if ftm_column in snapshot.columns:
            snapshot["FTM"] = snapshot[ftm_column].fillna("Unassigned")
        if current_tasks_only and "Task Stage" in snapshot.columns:
            snapshot = snapshot[snapshot["Task Stage"] == "Current"]
        filtered = apply_filters(snapshot, ftms, tasks, age_groups, outcomes, cohorts)
        snapshot_time = parse_snapshot_datetime(version.snapshot_date)
        for ftm, ftm_rows in filtered.groupby("FTM", dropna=False, observed=True):
            metrics = calculate_kpis(ftm_rows)
            records.append(
                {
                    "Snapshot Date": snapshot_time,
                    "FTM": str(ftm),
                    "Weighted Progress": metrics["weighted_progress"],
                    "Completion Rate": (
                        np.nan
                        if metrics["task_rows"] == 0
                        else metrics["complete"] / metrics["task_rows"]
                    ),
                    "Complete": metrics["complete"],
                    "No-Show": metrics["no_show"],
                    "Needs Follow-up": metrics["incomplete"] + metrics["no_record"],
                    "Participants": metrics["participants"],
                    "Task Records": metrics["task_rows"],
                }
            )
    if not records:
        return pd.DataFrame(columns=TREND_COLUMNS)
    return pd.DataFrame(records).dropna(subset=["Snapshot Date"]).sort_values(
        ["Snapshot Date", "FTM"]
    )


def prepare_detail_table(data: pd.DataFrame) -> pd.DataFrame:
    """Return a concise operational detail table."""
    preferred = [
        "FTM",
        "Current FTM",
        "Responsible FTM",
        "Task Stage",
        "Participant ID",
        "firstName",
        "lastName",
        "Age Group",
        "Task",
        "Outcome",
        "Score",
        "Outcome_Date",
        "Participant Cohort",
        "statusId",
        "Source",
        "Assignment Source",
        "Calendly FTM Matched Age Band",
        "Calendly Assignment Year",
        "Calendly Assignment Rule",
        "Calendly Host Raw",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
        "Calendly Match Method",
        "Calendly FTM QA Flag",
        "Calendly_Status",
        "Calendly_Appointment_Date",
    ]
    columns = [column for column in preferred if column in data.columns]
    result = data[columns].copy()
    for column in [
        "Outcome_Date",
        "Calendly_Appointment_Date",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
    ]:
        if column in result.columns:
            result[column] = result[column].dt.strftime("%Y-%m-%d").fillna("")
    return result


def prepare_roster_table(data: pd.DataFrame) -> pd.DataFrame:
    """Return the complete filtered participant roster for FTM review."""
    preferred = [
        "FTM",
        "Participant ID",
        "firstName",
        "lastName",
        "birthday",
        "Participant Cohort",
        "statusId",
        "Age Group",
        "Eligibility Status",
        "Task Eligible",
        "Assignment Source",
        "Calendly FTM Matched Age Band",
        "Calendly Assignment Year",
        "Calendly Assignment Rule",
        "Calendly Host Raw",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
        "Calendly Match Method",
        "Calendly FTM QA Flag",
    ]
    columns = [column for column in preferred if column in data.columns]
    result = data[columns].copy()
    for column in [
        "birthday",
        "Calendly Appointment Date",
        "Calendly Assignment Date",
    ]:
        if column in result.columns:
            values = pd.to_datetime(result[column], errors="coerce")
            result[column] = values.dt.strftime("%Y-%m-%d").fillna("")
    return result.sort_values(
        [column for column in ["FTM", "Participant Cohort", "Age Group", "Participant ID"] if column in result.columns]
    )
