"""2026 IPA dashboard data contract. No API calls or prior-output state."""
from __future__ import annotations
from dataclasses import dataclass
from pathlib import Path
import pandas as pd

SCHEMA_VERSION = "ipa-2026-v2"
AGE_GROUP_LABELS = {
    "6_11_month": "6–11 months", "12_23_month": "12–23 months",
    "24_35_month": "24–35 months", "3_5yr": "3–5 years", "6_10yr": "6–10 years",
    "11_17yr": "11–17 years", "18_20yr": "18–20 years", "needs_review": "Needs review",
}
AGE_GROUP_ORDER = list(AGE_GROUP_LABELS.values())
MONTH_NAMES = ["January", "February", "March", "April", "May", "June", "July",
               "August", "September", "October", "November", "December"]

WORKLOAD_COUNTS = ["Appointment Count", "No-show Count", "Cancel Count", "Reschedule Count"]
FILES = {
    "completions": "ipa_survey_completions.csv", "submissions": "ipa_survey_submissions.csv",
    "appointments": "ipa_appointment_history.csv", "workload": "participant_ipa_ftm_records.csv",
    "appointment_qc": "ipa_appointment_history_qc.csv", "comparison": "ipa_survey_event_comparison.csv",
    "event_qc": "ipa_survey_event_qc.csv", "specimens": "biospecimen_collections.csv",
    "specimen_qc": "biospecimen_qc.csv", "roster": "participant_roster.csv",
}

@dataclass(frozen=True)
class DataVersion:
    label: str
    path: Path
    schema: str
    timestamp: str


def read_csv(path: Path) -> pd.DataFrame:
    """Preserve identifiers; convert only documented numeric/date/boolean columns."""
    data = pd.read_csv(path, dtype="string").drop(columns=["firstName", "lastName"], errors="ignore")
    for column in ["Completed at", "Survey Assignment Time", "First Completion Time", "Latest Completion Time"]:
        if column in data:
            # readr serializes POSIXct as UTC; restore study-local calendar dates/months.
            data[column] = pd.to_datetime(data[column], errors="coerce", utc=True, format="mixed").dt.tz_convert("America/New_York").dt.tz_localize(None)
    for column in ["Completion Date", "Event Start Date", "Event Completion Date",
                   "Collection Date", "Calendly Assignment Date"]:
        if column in data:
            data[column] = pd.to_datetime(data[column], errors="coerce", format="mixed")
    for column in [*WORKLOAD_COUNTS, "Completion year", "Submission index", "Submission Count", "Submission Order", "Extra Submissions", "Date Difference Days"]:
        if column in data:
            data[column] = pd.to_numeric(data[column], errors="raise").astype("Int64")
    for column in ["Survey Complete", "Event Complete", "Event Complete 2026", "QC Relevant", "Counted in IPA", "Canceled", "True Cancel", "No Show", "Rescheduled"]:
        if column in data:
            data[column] = data[column].str.lower().map({"true": True, "false": False}).astype("boolean")
    if "IPA Age Band" in data:
        data["Age Group"] = data["IPA Age Band"].map(AGE_GROUP_LABELS).fillna(data["IPA Age Band"])
    for source, label in [("Survey Age Band", "Survey Age Group"), ("Event Age Band", "Event Age Group")]:
        if source in data:
            data[label] = data[source].map(AGE_GROUP_LABELS).fillna(data[source])
    return data


def discover_data_versions(data_dir: Path) -> list[DataVersion]:
    result = []
    folders = [data_dir / "latest"]
    snapshots = data_dir / "snapshots"
    if snapshots.exists():
        folders += sorted((p for p in snapshots.iterdir() if p.is_dir()), reverse=True)
    for folder in folders:
        manifest_path = folder / "manifest.csv"
        if not manifest_path.exists():
            continue
        try:
            manifest = pd.read_csv(manifest_path, dtype="string").iloc[0]
        except (OSError, ValueError, IndexError, pd.errors.ParserError):
            continue
        schema = str(manifest.get("schema_version", "legacy"))
        timestamp = str(manifest.get("generated_at", folder.name))
        prefix = "Latest" if folder.name == "latest" else "Snapshot"
        suffix = " · previous methodology" if schema != SCHEMA_VERSION else ""
        result.append(DataVersion(f"{prefix} · {timestamp}{suffix}", folder, schema, timestamp))
    return result


def filter_records(data: pd.DataFrame, ages=(), cohorts=(), ftms=(), ftm_column="FTM") -> pd.DataFrame:
    result = data.copy()
    for column, selected in [("Age Group", ages), ("Participant Cohort", cohorts), (ftm_column, ftms)]:
        if selected and column in result:
            result = result[result[column].isin(selected)]
    return result


def unknown_sum(values: pd.Series):
    return pd.NA if values.isna().any() else int(values.sum())


def summarize_workload(data: pd.DataFrame) -> pd.DataFrame:
    if data.empty:
        return pd.DataFrame(columns=["FTM", *WORKLOAD_COUNTS, "Participants", "Participant Age Bands"])
    grouped = data.groupby("FTM", dropna=False, observed=True)
    result = grouped[WORKLOAD_COUNTS].agg(unknown_sum)
    result["Participants"] = grouped["Participant ID"].nunique()
    result["Participant Age Bands"] = grouped.size()
    return result.reset_index()


def validate_batch(tables: dict[str, pd.DataFrame]) -> None:
    completions = tables["completions"]
    keys = ["Participant ID", "IPA Age Band"]
    if completions[keys].isna().any().any() or completions.duplicated(keys).any():
        raise ValueError("Survey completions must be unique by participant and age band.")
    if completions["Completed at"].isna().any() or not completions["Completed at"].dt.year.eq(2026).all():
        raise ValueError("Completion timestamps must be in 2026.")
    specimens = tables["specimens"]
    if specimens["Collection Date"].isna().any() or not specimens["Collection Date"].dt.year.eq(2026).all():
        raise ValueError("Biospecimen collections need a valid 2026 collection date.")
    workload = tables["workload"]
    if workload.duplicated([*keys, "Participant Cohort", "FTM"]).any():
        raise ValueError("Workload keys are duplicated.")
    matched = tables["appointments"]["Match Status"].eq("Matched").sum()
    if workload["Appointment Count"].sum() != matched:
        raise ValueError("Appointment workload does not reconcile with matched bookings.")
    if any(workload[column].dropna().lt(0).any() for column in WORKLOAD_COUNTS):
        raise ValueError("Negative activity counts.")


def snapshot_trend(versions: list[DataVersion], ages=(), cohorts=(), ftms=()) -> pd.DataFrame:
    rows = []
    for version in versions:
        if version.path.name == "latest" or version.path.name.startswith("replaced_") or version.schema != SCHEMA_VERSION:
            continue
        try:
            completions = filter_records(read_csv(version.path / FILES["completions"]), ages, cohorts, ftms, "Attributed FTM")
            specimens = filter_records(read_csv(version.path / FILES["specimens"]), ages, cohorts, ftms)
            timestamp = pd.to_datetime(version.timestamp.rsplit(" ", 1)[0])
        except (OSError, ValueError, KeyError, pd.errors.ParserError):
            continue
        rows.append({"Refresh": timestamp, "IPA completions": len(completions), "Biospecimen collections": len(specimens)})
    return pd.DataFrame(rows).sort_values("Refresh") if rows else pd.DataFrame()


def appointment_count_table(workload: pd.DataFrame) -> pd.DataFrame:
    """Booking totals by host; the grand-total distinct counts cross host boundaries."""
    columns = ["FTM", "Appointment Count", "Participants", "Participant Age Bands"]
    result = summarize_workload(workload)[columns]
    total = pd.DataFrame([{"FTM": "Total", "Appointment Count": int(workload["Appointment Count"].sum()),
        "Participants": workload["Participant ID"].nunique(),
        "Participant Age Bands": len(workload.drop_duplicates(["Participant ID", "Participant Cohort", "IPA Age Band"]))}])
    return pd.concat([result, total], ignore_index=True)


def monthly_collection_table(specimens: pd.DataFrame) -> pd.DataFrame:
    """Month rows and specimen-task columns, with row/column totals from filtered collections."""
    if specimens.empty:
        return pd.DataFrame(columns=["Month", "Total"])
    counts = (specimens.assign(_month=specimens["Collection Date"].dt.month)
              .groupby(["_month", "Task"], observed=True).size().unstack(fill_value=0).sort_index())
    counts["Total"] = counts.sum(axis=1)
    counts.index = [MONTH_NAMES[int(month)-1] for month in counts.index]
    counts.loc["Total"] = counts.sum(axis=0)
    counts.index.name = "Month"
    counts.columns.name = None
    return counts.reset_index()
