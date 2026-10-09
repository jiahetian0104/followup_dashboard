"""Reviewed prenatal CSV loading and calculations shared by the app and checks."""
from pathlib import Path
import calendar
import re

import pandas as pd

SITES = ["Detroit", "Flint", "Traverse City", "Unknown"]
SITE_COLORS = {"Detroit": "#1689EE", "Flint": "#2837A8", "Traverse City": "#EB773D", "Unknown": "#929AA6"}
ELEMENTS = ["Prenatal 1 Survey", "Prenatal 1 Anthro", "Prenatal 1 Blood", "Prenatal 1 Urine",
            "Actigraphy", "Prenatal 2 Survey", "Prenatal 2 Anthro", "Prenatal 2 Blood",
            "Prenatal 2 Urine", "Water", "Cord Blood", "Maternal Medical Record Abstraction"]
DATE_FIELDS = ["consent_date", "child_dob", "Complete Date", "estduedate", "report_as_of", "edd_month_start"]
BOOL_FIELDS = ["Eligible", "Has Completion Record", "is_biospecimen", "projected_birth"]


def read_progress(path: Path) -> pd.DataFrame:
    df = pd.read_csv(path, dtype={name: "string" for name in
        ["participant_key", "globalId", "customId", "echo_id", "PIN"]})
    required = {"participant_key", "site", "event_code", "Data Element Name", "Eligible",
                "Has Completion Record", "is_biospecimen", "projected_birth", "report_year", *DATE_FIELDS}
    missing = required - set(df)
    if missing:
        raise ValueError("The progress export needs refreshing; missing fields: " + ", ".join(sorted(missing)))
    if df["participant_key"].isna().any() or df.duplicated(["participant_key", "event_code"]).any():
        raise ValueError("Progress rows must have a unique participant/event pair.")
    for col in DATE_FIELDS:
        parsed = pd.to_datetime(df[col], errors="coerce")
        if (df[col].notna() & parsed.isna()).any():
            raise ValueError(f"Invalid date values in {col}.")
        df[col] = parsed
    for col in BOOL_FIELDS:
        values = df[col].astype("string").str.lower()
        mapped = values.map({"true": True, "false": False, "1": True, "0": False}).astype("boolean")
        if (df[col].notna() & mapped.isna()).any():
            raise ValueError(f"Invalid boolean values in {col}.")
        df[col] = mapped
    if df["report_year"].nunique() != 1 or df["report_as_of"].nunique() != 1:
        raise ValueError("The progress file must contain one reporting year and snapshot date.")
    roster = df.drop_duplicates("participant_key")
    for col in ["site", "consent_date", "child_dob", "estduedate", "projected_birth"]:
        if df.groupby("participant_key")[col].nunique(dropna=False).gt(1).any():
            raise ValueError(f"Conflicting participant values in {col}.")
    if roster.empty:
        raise ValueError("The progress export contains no participants.")
    return df


def read_specimens(path: Path, progress: pd.DataFrame) -> pd.DataFrame:
    """Include prenatal and 0–5 month events, repairing legacy ID placement."""
    df = pd.read_csv(path, dtype={"globalId": "string", "customId": "string", "participant_key": "string"})
    needed = {"is_biospecimen", "event_date", "event_code"}
    if not needed.issubset(df):
        raise ValueError("The full event export lacks required specimen fields.")
    specimen = df["is_biospecimen"].astype("string").str.lower()
    df = df.loc[specimen.eq("true")].copy()
    df["event_date"] = pd.to_datetime(df["event_date"], errors="raise")
    roster = progress.drop_duplicates("participant_key")
    local_lookup = dict(zip(roster["customId"].dropna(), roster.loc[roster["customId"].notna(), "participant_key"]))
    global_lookup = dict(zip(roster["globalId"].dropna(), roster.loc[roster["globalId"].notna(), "participant_key"]))
    if roster["customId"].dropna().duplicated().any():
        raise ValueError("Duplicate local IDs prevent specimen identity matching.")
    site_lookup = dict(zip(roster["participant_key"], roster["site"]))

    def resolve(row):
        if "participant_key" in row and row["participant_key"] in site_lookup:
            return row["participant_key"]
        # Older exports may have the local and ECHO identifiers reversed.
        for field in ["customId", "globalId"]:
            value = row.get(field)
            if pd.isna(value):
                continue
            value = str(value).strip().upper()
            if re.fullmatch(r"\d{4}[PF]\d+", value) and value in local_lookup:
                return local_lookup[value]
            value = re.sub(r"\s*\(\s*(\d+)\s*\)$", r" (\1)", value)
            if value in global_lookup:
                return global_lookup[value]
        return pd.NA

    df["participant_key"] = df.apply(resolve, axis=1) if len(df) else pd.Series(dtype="string")
    if df["participant_key"].isna().any():
        raise ValueError("Some specimen events cannot be matched to the progress roster; refresh both exports.")
    if df.duplicated(["participant_key", "event_code"]).any():
        raise ValueError("Duplicate specimen completions found; refresh and check the exports.")
    df["site"] = df["participant_key"].map(site_lookup)
    if "report_as_of" in df and len(df):
        dates = pd.to_datetime(df["report_as_of"], errors="raise").dropna().unique()
        if len(dates) != 1 or pd.Timestamp(dates[0]) != progress["report_as_of"].iloc[0]:
            raise ValueError("Specimen and progress exports have different snapshot dates; refresh both.")
    return df


def annual_counts(progress: pd.DataFrame, date_field: str, year: int, sites: list[str]) -> pd.DataFrame:
    roster = progress.drop_duplicates("participant_key")
    counts = roster.loc[roster[date_field].dt.year.eq(year)].groupby("site").size()
    return counts.reindex(sites, fill_value=0).rename("Participants").reset_index()


def month_cards(progress: pd.DataFrame, specimens: pd.DataFrame | None, year: int, month: int) -> dict:
    roster = progress.drop_duplicates("participant_key")
    in_month = lambda dates: dates.dt.year.eq(year) & dates.dt.month.eq(month)
    return {"Enrollments": int(in_month(roster["consent_date"]).sum()),
            "Births": int(in_month(roster["child_dob"]).sum()),
            "Biospecimens collected": None if specimens is None else int(in_month(specimens["event_date"]).sum())}


def event_summary(progress: pd.DataFrame, year: int) -> pd.DataFrame:
    rows = []
    for name in ELEMENTS:
        group = progress.loc[progress["Data Element Name"].eq(name)]
        eligible = group["Eligible"].fillna(False)
        complete = group["Has Completion Record"].fillna(False)
        count = int(eligible.sum())
        done = int((eligible & complete).sum())
        known = group["Eligible"].notna().any()
        rows.append({"Data element": name,
            "Completed in year": int((complete & group["Complete Date"].dt.year.eq(year)).sum()),
            "Eligible": count if known else None,
            "Eligible complete": done if known else None,
            "Eligible incomplete": count - done if known else None,
            "Not eligible": int((~group["Eligible"].fillna(True)).sum()),
            "Eligibility unknown": int(group["Eligible"].isna().sum()),
            "Completion rate": done / count if count else None})
    return pd.DataFrame(rows)


def projected_counts(progress: pd.DataFrame, year: int, sites: list[str]) -> pd.DataFrame:
    roster = progress.drop_duplicates("participant_key")
    pending = roster.loc[roster["projected_birth"].fillna(False) & roster["estduedate"].dt.year.eq(year)].copy()
    pending["Month number"] = pending["estduedate"].dt.month
    as_of = progress["report_as_of"].iloc[0] if len(progress) else pd.Timestamp(year, 1, 1)
    first = as_of.month if as_of.year == year else 1
    grid = pd.MultiIndex.from_product([sites, range(first, 13)], names=["site", "Month number"])
    counts = pending.groupby(["site", "Month number"]).size().reindex(grid, fill_value=0)
    result = counts.rename("Projected births").reset_index()
    result["Month"] = result["Month number"].map(lambda month: calendar.month_name[month])
    return result
