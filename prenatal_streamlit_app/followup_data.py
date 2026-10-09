"""0–5 month roster, long-table validation and filter-aware measurements."""
from pathlib import Path
import pandas as pd

FOLLOWUP_ELEMENTS = ["Postnatal Consent", "Survey", "Child Anthro", "Mom Anthro",
                     "Visit Scheduled", "Diaper Sent", "VIB Request", "Child Urine",
                     "Child Stool", "Mom Hair", "Mom Bloodspot", "Breast Milk", "Partner Contact"]
EVENT_FIELDS = ["postnatal_consent", "survey", "child_anthro", "mom_anthro", "visit_scheduled",
                "diaper_sent", "vib_request", "child_urine", "child_stool", "mom_hair", "mom_bs", "breast_milk", "partner_contact"]


def read_followup(data_dir: Path):
    roster = pd.read_csv(data_dir / "month0to5_participants.csv", dtype={name: "string" for name in
        ["participant_key", "globalId", "customId", "echo_id", "PIN"]})
    long = pd.read_csv(data_dir / "month0to5_progress.csv", dtype={name: "string" for name in
        ["participant_key", "globalId", "customId"]})
    required_roster = {"participant_key", "site", "customId", "dateCreated", "Eligible", "birth_date", "birth_date_source",
                       "age_months", "age_out_date", "days_to_age_out", "aging_out", "report_year", "report_as_of", *EVENT_FIELDS}
    required_long = {"participant_key", "site", "dateCreated", "Eligible", "event_code", "Data Element Name",
                     "Complete Date", "Has Completion Record", "is_biospecimen", "report_year", "report_as_of"}
    for frame, required in [(roster, required_roster), (long, required_long)]:
        missing = required - set(frame)
        if missing:
            raise ValueError("Refresh the 0–5 month exports; missing fields: " + ", ".join(sorted(missing)))
        if frame["participant_key"].isna().any():
            raise ValueError("Missing participant identifiers in the 0–5 month exports.")
        for field in ["dateCreated", "report_as_of", "birth_date", "age_out_date", "Complete Date", *EVENT_FIELDS]:
            if field not in frame:
                continue
            parsed = pd.to_datetime(frame[field], errors="coerce")
            if (frame[field].notna() & parsed.isna()).any():
                raise ValueError(f"Invalid 0–5 month dates in {field}.")
            frame[field] = parsed
        for field in ["Eligible", "aging_out", "Has Completion Record", "is_biospecimen"]:
            if field not in frame:
                continue
            mapped = frame[field].astype("string").str.lower().map({"true": True, "false": False}).astype("boolean")
            if (frame[field].notna() & mapped.isna()).any():
                raise ValueError(f"Invalid boolean values in {field}.")
            frame[field] = mapped
        if frame["report_year"].nunique() != 1 or frame["report_as_of"].nunique() != 1:
            raise ValueError("0–5 month exports require one reporting year and snapshot date.")
    if roster.empty or roster["participant_key"].duplicated().any() or long.duplicated(["participant_key", "event_code"]).any():
        raise ValueError("The 0–5 month roster and event table must have unique records.")
    if set(roster["participant_key"]) != set(long["participant_key"]) or len(long) != len(roster) * len(EVENT_FIELDS):
        raise ValueError("Roster and long-table populations do not reconcile; refresh both exports.")
    for field in ["site", "dateCreated", "Eligible", "report_as_of", "report_year"]:
        if long.groupby("participant_key")[field].nunique(dropna=False).gt(1).any():
            raise ValueError(f"Inconsistent participant metadata in {field}.")
        left = roster.set_index("participant_key")[field].sort_index()
        right = long.drop_duplicates("participant_key").set_index("participant_key")[field].sort_index()
        if not left.equals(right):
            raise ValueError(f"Roster and event metadata disagree in {field}.")
    if not roster["customId"].str.fullmatch(r"[0-9]{4}F[0-9]+").fillna(False).all():
        raise ValueError("0–5 month roster must contain F-category local IDs only.")
    expected = roster["dateCreated"].dt.year.eq(int(roster["report_year"].iloc[0])).astype("boolean")
    expected.loc[roster["dateCreated"].isna()] = pd.NA
    if not expected.equals(roster["Eligible"]):
        raise ValueError("Eligibility must equal the dateCreated reporting-year rule.")
    return roster, long


def filter_followup(roster, long, sites, eligible_only=False):
    selected = roster.loc[roster["site"].isin(sites)].copy()
    if eligible_only:
        selected = selected.loc[selected["Eligible"].fillna(False)].copy()
    events = long.loc[long["participant_key"].isin(selected["participant_key"])].copy()
    return selected, events


def followup_summary(events, year):
    rows = []
    for element in FOLLOWUP_ELEMENTS:
        data = events.loc[events["Data Element Name"].eq(element)]
        eligible = data["Eligible"].fillna(False)
        done = data["Has Completion Record"].fillna(False)
        denominator = int(eligible.sum())
        complete = int((eligible & done).sum())
        known = data["Eligible"].notna().any()
        rows.append({"Data element": element, "Completed in year": int(data["Complete Date"].dt.year.eq(year).sum()),
                     "Eligible": denominator if known else None, "Eligible complete": complete if known else None,
                     "Eligible incomplete": denominator - complete if known else None,
                     "Not eligible": int((~data["Eligible"].fillna(True)).sum()),
                     "Eligibility unknown": int(data["Eligible"].isna().sum()),
                     "Completion rate": complete / denominator if denominator else None})
    return pd.DataFrame(rows)


def followup_cards(events, year, month):
    dates = events["Complete Date"]
    current = events.loc[dates.dt.year.eq(year) & dates.dt.month.eq(month)]
    return {"Surveys": int(current["event_code"].eq("survey").sum()),
            "Biospecimens": int(current["is_biospecimen"].fillna(False).sum()),
            "Visits scheduled": int(current["event_code"].eq("visit_scheduled").sum())}


def followup_workflow(roster, year, linked=False):
    survey = roster["survey"].dt.year.eq(year)
    visit = roster["visit_scheduled"].dt.year.eq(year)
    diaper = roster["diaper_sent"].dt.year.eq(year)
    if linked:
        visit = survey & visit
        diaper = visit & diaper
    counts = [int(survey.sum()), int(visit.sum()), int(diaper.sum())]
    return pd.DataFrame({"Stage": ["Surveys completed", "Visits scheduled", "Diapers sent"], "Participants": counts})


def aging_out_list(roster, outstanding_only=False):
    aging = roster.loc[roster["aging_out"].fillna(False)].copy()
    if outstanding_only:
        aging = aging.loc[aging["survey"].isna() | aging["visit_scheduled"].isna()].copy()
    aging["Survey"] = aging["survey"].notna().map({True: "Complete", False: "Incomplete"})
    aging["Visit scheduled"] = aging["visit_scheduled"].notna().map({True: "Complete", False: "Incomplete"})
    return aging.sort_values(["age_out_date", "site", "customId"])
