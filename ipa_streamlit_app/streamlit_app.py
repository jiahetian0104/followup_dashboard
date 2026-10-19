"""CHARM 2026 IPA completions, booking workload and biospecimen collections."""
from pathlib import Path
import os
import importlib
import dashboard_data as dashboard_data_module
importlib.reload(dashboard_data_module)
import pandas as pd
import plotly.express as px
import streamlit as st
from dashboard_data import (SCHEMA_VERSION, AGE_GROUP_ORDER, AGE_GROUP_LABELS, MONTH_NAMES, FILES, WORKLOAD_COUNTS, appointment_count_table, monthly_collection_table,
    discover_data_versions, read_csv, filter_records, summarize_workload, unknown_sum,
    validate_batch, snapshot_trend)

st.set_page_config(page_title="CHARM · IPA 2026", page_icon="◷", layout="wide")
st.markdown("""<style>
.block-container {padding-top:2rem; max-width:1500px}
[data-testid="stMetric"] {background:#f3f6fa; padding:16px; border-radius:10px}
h1 {color:#234e70} [data-testid="stSidebar"] {background:#f6f8fb}
</style>""", unsafe_allow_html=True)
st.title("CHARM IPA · 2026")
DATA_DIR = Path(os.getenv("IPA_DASHBOARD_DATA_DIR", Path(__file__).resolve().parent / "data"))

@st.cache_data
def load_table(path: str, mtime: int):
    return read_csv(Path(path))

def load(path):
    return load_table(str(path), path.stat().st_mtime_ns)

def table(data, name, label="Records"):
    date_config = {column: st.column_config.DateColumn(format="MM/DD/YYYY")
                   for column in ["Completion Date", "Event Completion Date", "Collection Date", "Event Start Date"]
                   if column in data}
    st.dataframe(data, hide_index=True, width="stretch", column_config=date_config)
    st.download_button(f"Download {label.lower()}", data.to_csv(index=False).encode("utf-8-sig"),
                       file_name=name, mime="text/csv", key=f"download_{name}")

def figure(fig):
    fig.update_layout(template="plotly_white", margin=dict(l=10,r=10,t=40,b=20),
                      font=dict(color="#25313c"), legend_title_text="")
    st.plotly_chart(fig, width="stretch")

def options(data, column):
    return sorted(data[column].dropna().astype(str).unique()) if column in data else []

def number(value):
    return "Unknown" if pd.isna(value) else f"{int(value):,}"

versions = discover_data_versions(DATA_DIR)
if not versions:
    st.info("No completed refresh is available. Run IPA Data.R to create the first snapshot.")
    st.stop()
with st.sidebar:
    st.header("Data snapshot")
    label = st.selectbox("Data version", [v.label for v in versions])
    if st.button("Reload saved data", width="stretch"):
        st.cache_data.clear()
        st.rerun()
    st.caption("Reload reads saved files. API data is refreshed by IPA Data.R.")
version = next(v for v in versions if v.label == label)
manifest = load(version.path / "manifest.csv")
if version.schema != SCHEMA_VERSION:
    st.warning("This snapshot uses the previous event-based methodology. It is retained for reference and is excluded from the new survey-based trends.")
    candidates = [name for name in ["participant_ipa_records.csv", "ipa_appointment_history.csv", "task_checklist_long.csv"] if (version.path / name).exists()]
    if candidates:
        chosen = st.selectbox("Previous snapshot records", candidates)
        table(load(version.path / chosen), chosen, "previous snapshot records")
    st.stop()
try:
    tables = {key: load(version.path / filename) for key, filename in FILES.items()}
    validate_batch(tables)
except (OSError, ValueError, KeyError, pd.errors.ParserError) as exc:
    st.error(f"This saved batch could not be validated: {exc}")
    st.stop()
info = manifest.iloc[0]
st.caption(f"Ripple data: {info.get('ripple_fetched_at', info['fetched_at'])} · Calendly data: {info.get('calendly_fetched_at', info['fetched_at'])} · Calculated: {info['generated_at']} · {info['source_mode']}")
all_ages = set().union(*(set(t.get("Age Group", pd.Series(dtype=str)).dropna()) for t in tables.values()))
all_cohorts = set().union(*(set(t.get("Participant Cohort", pd.Series(dtype=str)).dropna()) for t in tables.values()))
with st.sidebar:
    st.divider()
    st.header("Age & cohort")
    ages = st.multiselect("Age group", [age for age in AGE_GROUP_ORDER if age in all_ages])
    cohorts = st.multiselect("Participant cohort", sorted(all_cohorts))
    st.caption("These filters apply across tabs. FTM filters belong to each tab's attribution rule.")
completion_tab, booking_tab, specimen_tab, qc_tab, snapshot_tab = st.tabs(
    ["IPA completions", "Appointment workload", "Biospecimens", "Data QC", "Snapshot trend"])
with completion_tab:
    st.subheader("Completed IPAs")
    st.caption("One completion per child and age band: the first completed survey within 2026. FTM, site and month follow that submission.")
    ftms = st.multiselect("Completion FTM", options(tables["completions"], "Attributed FTM"))
    completed = filter_records(tables["completions"], ages, cohorts, ftms, "Attributed FTM")
    metrics = st.columns(3)
    metrics[0].metric("IPA completions", f"{len(completed):,}")
    metrics[1].metric("Participants", f"{completed['Participant ID'].nunique():,}")
    unresolved = completed["Attributed FTM"].str.startswith("Unconfirmed:", na=True).sum()
    metrics[2].metric("FTM to verify", f"{unresolved:,}")
    if completed.empty:
        st.info("No survey completions match these filters.")
    else:
        left,right = st.columns([1.1,1])
        with left:
            by_ftm = completed.groupby(["Attributed FTM","Age Group"], dropna=False).size().rename("Completions").reset_index()
            ftm_chart = px.bar(by_ftm, x="Completions", y="Attributed FTM", color="Age Group", orientation="h",
                              title="Completions by FTM and age group", category_orders={"Age Group":AGE_GROUP_ORDER})
            ftm_totals = by_ftm.groupby("Attributed FTM", dropna=False)["Completions"].sum()
            for ftm, total in ftm_totals.items():
                ftm_chart.add_annotation(x=int(total), y=ftm, text=f"{total:,}", showarrow=False,
                                         xanchor="left", yanchor="middle", xshift=6,
                                         font=dict(size=12, color="#25313c"))
            largest_ftm_total = int(ftm_totals.max())
            ftm_chart.update_xaxes(range=[0, largest_ftm_total + max(3, largest_ftm_total * 0.14)])
            figure(ftm_chart)
        with right:
            monthly = completed.assign(Month=completed["Completed at"].dt.month_name()).groupby(
                ["Month","Attributed FTM"]).size().rename("Completions").reset_index()
            chart = px.bar(monthly,x="Month",y="Completions",color="Attributed FTM",title="Completion month",
                           category_orders={"Month":MONTH_NAMES})
            chart.update_xaxes(type="category")
            chart.update_traces(hovertemplate="Month: %{x}<br>Completions: %{y}<extra>%{fullData.name}</extra>")
            month_totals = monthly.groupby("Month")["Completions"].sum()
            for month, total in month_totals.items():
                chart.add_annotation(x=month, y=int(total), text=f"{total:,}", showarrow=False,
                                     xanchor="center", yanchor="bottom", yshift=5,
                                     font=dict(size=12, color="#25313c"))
            largest_month_total = int(month_totals.max())
            chart.update_yaxes(range=[0, largest_month_total + max(2, largest_month_total * 0.14)])
            figure(chart)
        site_counts = completed.assign(Site=completed["Site"].fillna("Not recorded")).groupby("Site").size().rename("Completions").reset_index()
        figure(px.bar(site_counts,x="Site",y="Completions",title="Completions by site",color_discrete_sequence=["#2f6b9a"]))
    with st.expander("Completion records"):
        table(completed, "ipa_survey_completions.csv", "completion records")
    with st.expander("All original survey submissions"):
        st.caption("Includes excluded repeat submissions and other years returned by the same export. These rows do not each count as a completion.")
        history = filter_records(tables["submissions"], ages, cohorts, ftms, "Attributed FTM")
        table(history, "ipa_survey_submissions.csv", "survey submissions")
with booking_tab:
    st.subheader("2026 appointment workload")
    st.caption("Actions stay with each Calendly booking host. A booking covering multiple children counts once per child. Completion credit is shown separately in IPA completions.")
    hosts = st.multiselect("Booking host", options(tables["appointments"], "FTM"))
    workload = filter_records(tables["workload"], ages, cohorts, hosts)
    history = filter_records(tables["appointments"], ages, cohorts, hosts)
    summary = summarize_workload(workload)
    st.subheader("Appointment counts")
    st.caption("Booking totals include appointments marked no-show, canceled or rescheduled. Each child-booking counts once in the total.")
    table(appointment_count_table(workload)[["FTM", "Appointment Count"]], "ipa_appointment_counts.csv", "appointment counts")
    st.subheader("Appointment actions")
    action_counts = ["No-show Count", "Cancel Count", "Reschedule Count"]
    columns = st.columns(3)
    for col,metric in zip(columns,action_counts):
        col.metric(metric.replace(" Count", ""), number(unknown_sum(workload[metric])))
    unmatched = history["Match Status"].ne("Matched").sum()
    if unmatched:
        st.info(f"{unmatched:,} child-booking rows have unresolved participant, age band or booking ID and are excluded from the matched workload counts above. They remain in appointment records and QC.")
    if not summary.empty:
        plot = summary.melt(id_vars="FTM",value_vars=action_counts,var_name="Action",value_name="Count")
        plot["Count"] = pd.to_numeric(plot["Count"],errors="coerce").astype(float)
        figure(px.bar(plot,x="FTM",y="Count",color="Action",barmode="group",title="Booking actions by host"))
        st.caption("Cancel excludes reschedules. Each old booking in a reschedule chain counts as an action. These flags describe appointments and should not be added to the appointment total. Missing status evidence stays unknown.")
    with st.expander("Action summary"):
        table(summary[["FTM", *action_counts]], "ftm_ipa_actions.csv", "action summary")
    with st.expander("Appointment records"):
        table(history,"ipa_appointment_history.csv","appointment records")
with specimen_tab:
    st.subheader("Biospecimen collections in 2026")
    st.caption("Completed collection flag and a valid 2026 completion date are required. Includes children who aged into a new group or have since withdrawn.")
    with st.expander("How the biospecimen FTM is matched", expanded=True):
        st.markdown("""All matches use the **same child, same age band and 2026 records**, in this order:

1. FTM from the **latest completed Survey**.
2. If no completed Survey exists, the host of the **latest Calendly appointment**.
3. If neither source identifies an FTM, **Unassigned**.

IPA completion counts use the first completed Survey; biospecimen FTM assignment uses the latest. Collection records retain the assignment source and dates for review.""")
    owners = st.multiselect("Biospecimen FTM",options(tables["specimens"],"FTM"))
    specimens = filter_records(tables["specimens"],ages,cohorts,owners)
    tasks = st.multiselect("Biospecimen task",options(tables["specimens"],"Task"))
    if tasks:
        specimens = specimens[specimens["Task"].isin(tasks)]
    cols = st.columns(3)
    cols[0].metric("Collections",f"{len(specimens):,}")
    cols[1].metric("Participants",f"{specimens['Participant ID'].nunique():,}")
    cols[2].metric("Unassigned",f"{specimens['FTM'].eq('Unassigned').sum():,}")
    if not specimens.empty:
        counts = specimens.groupby(["Task","FTM"],dropna=False).size().rename("Collections").reset_index()
        figure(px.bar(counts,x="Collections",y="Task",color="FTM",orientation="h",title="Collections by task and FTM"))
        st.subheader("Collection month")
        st.caption("Monthly collection counts by specimen task, using the selected FTM, task, age and cohort filters. Only months with recorded collections are shown.")
        table(monthly_collection_table(specimens), "biospecimen_monthly_collections.csv", "monthly collections")
    with st.expander("Collection records"):
        table(specimens,"biospecimen_collections.csv","collection records")
with qc_tab:
    st.subheader("Survey duplicates · same age band")
    st.caption("Participants with more than one completed Survey in the same age band within 2026. Only the first completed submission counts toward IPA completions; all repeated submissions remain available for review.")
    duplicate_summary_path = version.path / "ipa_survey_duplicate_summary.csv"
    duplicate_detail_path = version.path / "ipa_survey_duplicate_qc.csv"
    if duplicate_summary_path.exists() and duplicate_detail_path.exists():
        duplicate_summary = filter_records(load(duplicate_summary_path), ages, cohorts)
        duplicate_detail = filter_records(load(duplicate_detail_path), ages, cohorts)
        duplicate_metrics = st.columns(3)
        duplicate_metrics[0].metric("Participants with duplicates", f"{duplicate_summary['Participant ID'].nunique():,}")
        duplicate_metrics[1].metric("Participant age bands with duplicates", f"{len(duplicate_summary):,}")
        duplicate_metrics[2].metric("Extra completed submissions", f"{duplicate_summary['Extra Submissions'].sum():,}")
        if duplicate_summary.empty:
            st.info("No duplicate completed Surveys match these age and cohort filters.")
        else:
            summary_columns = ["Participant ID", "Age Group", "Submission Count", "Extra Submissions",
                "First Completion Time", "Latest Completion Time", "First FTM", "Latest FTM", "FTMs Recorded"]
            table(duplicate_summary[summary_columns], "ipa_survey_duplicate_summary.csv", "duplicate survey summary")
        with st.expander("All submissions in duplicate groups"):
            detail_columns = ["Participant ID", "Age Group", "Submission index", "Submission Order", "Completed at",
                "Attributed FTM", "Site", "Counted in IPA", "Submission Count", "Participant Cohort", "QC Issue"]
            table(duplicate_detail[detail_columns], "ipa_survey_duplicate_qc.csv", "duplicate survey submissions")
    else:
        st.info("Duplicate Survey QC was not exported in this snapshot. Run the updated IPA Data.R to add it to a new snapshot.")
    st.subheader("Survey and event cross-check")
    st.caption("Compare completion presence per child and age band. Survey controls IPA completion counts. Event flags remain independent evidence; missing dates and date differences are retained for review.")
    comparison = filter_records(tables["comparison"],ages,cohorts)
    totals = st.columns(2)
    totals[0].metric("Survey completions", f"{comparison['Survey Complete'].sum():,}")
    totals[1].metric("Event completion flags · 2026", f"{comparison['Event Complete 2026'].sum():,}")
    relevant = comparison.get("QC Relevant", comparison["Survey Complete"] | comparison["Event Complete 2026"]).fillna(False)
    review = comparison[relevant].copy()
    cols = st.columns(5)
    for col,status in zip(cols[:3],["Both complete","Survey only","Event only"]):
        col.metric(status,f"{review['QC Status'].eq(status).sum():,}")
    conflict_rows = review[review["QC Status"].eq("Age-band conflict")]
    cols[3].metric("Age-band conflicts · participants", f"{conflict_rows['Participant ID'].nunique():,}")
    date_diff = review[review["QC Status"].eq("Both complete") & review["Date Difference Days"].fillna(0).ne(0)]
    cols[4].metric("Completion date differences", f"{len(date_diff):,}")
    st.caption("Same-day Survey/event records in different age bands are paired as an age-band conflict. Records with only an event outside 2026 and no 2026 Survey are excluded from the review queue.")
    conflict_path = version.path / "ipa_age_band_conflicts.csv"
    if conflict_path.exists():
        conflicts = load(conflict_path)
        if cohorts:
            conflicts = conflicts[conflicts["Participant Cohort"].isin(cohorts)]
        if ages:
            conflicts = conflicts[conflicts["Survey Age Band"].map(AGE_GROUP_LABELS).isin(ages) |
                                  conflicts["Event Age Band"].map(AGE_GROUP_LABELS).isin(ages)]
        if not conflicts.empty:
            st.subheader("Age-band conflicts")
            st.caption("Both sources show completion on the same date, but the recorded age bands differ. Source records have not been corrected automatically.")
            first_columns = ["Participant ID", "Completion Date", "Survey Age Group", "Event Age Group", "Attributed FTM", "QC Issue"]
            ordered = [column for column in first_columns if column in conflicts]
            table(conflicts[ordered], "ipa_age_band_conflicts.csv", "age-band conflicts")
    statuses = st.multiselect("QC status", options(review,"QC Status"),
        default=[s for s in ["Age-band conflict","Survey only","Event only"] if s in options(review,"QC Status")])
    visible = review[review["QC Status"].isin(statuses)] if statuses else review
    if statuses:
        table(visible,"ipa_survey_event_comparison.csv","survey/event comparison")
    else:
        st.caption("No status restriction: all 2026 survey/event evidence is shown.")
        table(visible,"ipa_survey_event_comparison.csv","survey/event comparison")
    with st.expander("Completion date differences"):
        st.caption("Date Difference Days = Event completion date minus Survey completion date. A negative value means the event date is earlier.")
        table(date_diff, "ipa_completion_date_qc.csv", "completion date differences")
    with st.expander("All 2026 survey/event exceptions"):
        event_exceptions = filter_records(tables["event_qc"],ages,cohorts)
        if "QC Relevant" in event_exceptions:
            event_exceptions = event_exceptions[event_exceptions["QC Relevant"].fillna(False)]
        else:
            event_exceptions = event_exceptions[event_exceptions["Survey Complete"] | event_exceptions["Event Complete 2026"]]
        table(event_exceptions,"ipa_survey_event_qc.csv","survey/event exceptions")
    with st.expander("Appointment matching and status QC"):
        table(filter_records(tables["appointment_qc"],ages,cohorts),"ipa_appointment_history_qc.csv","appointment QC")
    with st.expander("Biospecimen dates and collection flags QC"):
        st.caption("Completed flags without a valid date are excluded from collection totals. These records remain visible here.")
        table(filter_records(tables["specimen_qc"],ages,cohorts),"biospecimen_qc.csv","biospecimen QC")
with snapshot_tab:
    st.subheader("Saved refreshes")
    st.caption("Each successful refresh preserves an immutable snapshot. These lines show changes between saved data refreshes, not the month in which assessments or collections occurred. Previous-methodology snapshots are excluded.")
    trend_ftms = st.multiselect("Trend FTM",sorted(set(options(tables["completions"],"Attributed FTM")) | set(options(tables["specimens"],"FTM"))))
    trend = snapshot_trend(versions,ages,cohorts,trend_ftms)
    if len(trend) >= 2:
        figure(px.line(trend,x="Refresh",y=["IPA completions","Biospecimen collections"],markers=True,title="Counts across comparable refreshes"))
    elif len(trend) == 1:
        st.info("The first survey-based snapshot is ready. A trend will appear after another successful refresh.")
    else:
        st.info("No comparable survey-based snapshots are available.")
    if not trend.empty:
        table(trend,"snapshot_trend.csv","snapshot counts")
    with st.expander("Refresh manifest"):
        table(manifest.drop(columns=["raw_run"],errors="ignore"),"manifest.csv","refresh manifest")
