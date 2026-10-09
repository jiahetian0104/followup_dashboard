"""Local prenatal dashboard backed by PRENATAL_2026.R exports."""
import calendar
import os
from pathlib import Path

import pandas as pd
import plotly.express as px
import streamlit as st

from dashboard_data import (SITES, SITE_COLORS, ELEMENTS, read_progress, read_specimens,
                            month_cards, annual_counts, event_summary, projected_counts)

st.set_page_config(page_title="CHARM Follow-up Reports", page_icon="📊", layout="wide")
st.markdown("""<style>
.block-container {padding-top:2rem; padding-bottom:2rem; max-width:1600px;}
h1 {font-weight:650 !important; letter-spacing:-.04em;}
h3 {font-size:1.2rem !important; font-weight:600 !important;}
[data-testid="stMetric"] {border-radius:15px; padding:20px 24px; background:#edf5ff; border:1px solid #dce9f8;}
[data-testid="stMetricValue"] {font-size:2.7rem; font-weight:650;}
[data-testid="stMetricLabel"] {font-size:.95rem;}
[data-testid="stVerticalBlockBorderWrapper"] > div {border-radius:16px;}
[data-testid="stTabs"] button {font-size:1rem;}
.block-container {container-type:inline-size;}
@container (max-width:950px) {
  [data-testid="stHorizontalBlock"]:has([data-testid="stPlotlyChart"]) {flex-direction:column;}
  [data-testid="stHorizontalBlock"]:has([data-testid="stPlotlyChart"]) > [data-testid="stColumn"] {width:100% !important; flex:1 1 100% !important;}
}
@media (max-width:1050px) {
  [data-testid="stHorizontalBlock"]:has([data-testid="stPlotlyChart"]) {flex-direction:column;}
  [data-testid="stHorizontalBlock"]:has([data-testid="stPlotlyChart"]) > [data-testid="stColumn"] {width:100% !important; flex:1 1 100% !important;}
}

</style>""", unsafe_allow_html=True)

DATA_DIR = Path(os.getenv("PRENATAL_DASHBOARD_DATA_DIR", Path(__file__).resolve().parent.parent / "powerbi" / "latest"))


@st.cache_data(show_spinner=False)
def load_exports(progress_path, progress_version, event_path, event_version):
    del progress_version, event_version  # File mtimes invalidate the cached snapshot.
    progress = read_progress(Path(progress_path))
    try:
        specimens = read_specimens(Path(event_path), progress)
        specimen_error = None
    except (FileNotFoundError, ValueError, KeyError) as error:
        specimens, specimen_error = None, str(error)
    return progress, specimens, specimen_error


def style_chart(fig, height, *, legend=False):
    fig.update_layout(height=height, margin=dict(l=42, r=24, t=24, b=44),
                      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
                      font=dict(family="Arial, sans-serif", color="#35445A", size=13),
                      showlegend=legend, legend=dict(orientation="h", y=1.12, x=0),
                      hoverlabel=dict(bgcolor="white", font_size=13))
    fig.update_xaxes(showgrid=False, zeroline=False)
    fig.update_yaxes(gridcolor="#E8EDF4", zeroline=False)
    return fig


def site_bar(frame, height):
    fig = px.bar(frame, x="site", y="Participants", color="site", text="Participants",
                 color_discrete_map=SITE_COLORS, category_orders={"site": SITES})
    fig.update_traces(width=.55, textposition="outside", cliponaxis=False,
                      hovertemplate="%{x}<br>%{y} participants<extra></extra>")
    fig.update_xaxes(title=None)
    fig.update_yaxes(title=None, rangemode="tozero", dtick=10,
                     range=[0, max(5, frame["Participants"].max() * 1.2)])
    return style_chart(fig, height)


def reset_filters():
    st.session_state["selected_sites"] = st.session_state["available_sites"]
    st.session_state["selected_month"] = st.session_state["default_month"]


report_page = st.sidebar.radio("Report", ["Prenatal", "0–5 Month"], key="report_page")
# Reserve the Site position directly below Report, then choose the source before loading.
site_control = st.sidebar.container()
snapshot_root = DATA_DIR.parent / "snapshots"
required_exports = ("prenatal_progress.csv", "prenatal_events.csv",
                    "month0to5_participants.csv", "month0to5_progress.csv")
versions = {"Latest": DATA_DIR}
if snapshot_root.is_dir():
    for folder in sorted(snapshot_root.iterdir(), reverse=True):
        if folder.is_dir() and all((folder / name).is_file() for name in required_exports):
            versions[f"Snapshot · {folder.name}"] = folder
if st.session_state.get("data_version", "Latest") not in versions:
    st.session_state["data_version"] = "Latest"
selected_version = st.sidebar.selectbox("Data version", list(versions), key="data_version",
    help="Latest shows the most recent export. Snapshots show the data and eligibility as recorded in that export.")
DATA_DIR = versions[selected_version]
PROGRESS_FILE = DATA_DIR / "prenatal_progress.csv"
EVENT_FILE = DATA_DIR / "prenatal_events.csv"
if report_page == "0–5 Month":
    import runpy
    runpy.run_path(str(Path(__file__).with_name("followup_page.py")),
                   init_globals={"DATA_DIR": DATA_DIR, "style_chart": style_chart, "SITE_CONTROL": site_control})
    st.stop()

if not PROGRESS_FILE.exists():
    st.title("Prenatal Report")
    st.error("The prenatal progress export is missing. Run PRENATAL_2026.R to create it.")
    st.stop()
try:
    progress_all, specimens_all, specimen_error = load_exports(
        str(PROGRESS_FILE), PROGRESS_FILE.stat().st_mtime_ns,
        str(EVENT_FILE), EVENT_FILE.stat().st_mtime_ns if EVENT_FILE.exists() else None)
except (ValueError, KeyError, pd.errors.ParserError) as error:
    st.error(str(error))
    st.stop()

year = int(progress_all["report_year"].iloc[0])
as_of = progress_all["report_as_of"].iloc[0]
available = [site for site in SITES if site in progress_all["site"].unique()]
default_month = as_of.month if as_of.year == year else 12
st.session_state["available_sites"] = available
st.session_state["default_month"] = default_month
if "selected_sites" not in st.session_state:
    st.session_state["selected_sites"] = available

with st.sidebar:
    selected_sites = site_control.multiselect("Site", available, key="selected_sites")
    st.button("Reset filters", on_click=reset_filters, key="reset_filters", width="stretch")
    st.divider()
    st.subheader("Report settings")
    st.button("Reload CSV data", on_click=load_exports.clear, width="stretch")
    st.caption("Reload reads the local exports. Run PRENATAL_2026.R to fetch new Ripple data.")
    st.divider()
    st.caption("Snapshot date")
    st.write(as_of.strftime("%B %d, %Y"))
    with st.expander("Definitions & sources"):
        st.markdown("""**Actual completions:** dated completions in the reporting year, regardless of enrollment year or eligibility.

**Enrollment / births:** unique participants, using their consent / actual birth date.

**Eligibility:** Prenatal 1 and Actigraphy use enrollment year. Prenatal 2 uses the year of estimated due date minus 84 days. Water requires enrollment in the reporting year and on or after March 1, 2026. Cord blood and medical record eligibility are pending.

**Eligible completion rate:** recorded completions among eligible participants divided by eligible participants. Completion dates can be outside the reporting year. Unknown eligibility is excluded.

**Projected births:** currently pending; no recorded birth/loss, not in Pregnancy Loss or the excluded 0–5 month statuses, and estimated due date on or after the snapshot date.

**Sources:** prenatal_progress.csv; prenatal_events.csv for the complete biospecimen scope. These are local snapshots, not a live Ripple connection.""")

st.title(f"{year} Prenatal Report")
st.caption(f"Snapshot: {as_of:%B %d, %Y}")
if not selected_sites:
    st.info("Select at least one site to view the report.")
    st.stop()
progress = progress_all.loc[progress_all["site"].isin(selected_sites)].copy()
specimens = None if specimens_all is None else specimens_all.loc[specimens_all["site"].isin(selected_sites)].copy()
overview, eligibility_tab = st.tabs(["Overview", "Eligibility & progress"])
summary = event_summary(progress, year)
with overview:
    with st.container(border=True):
        month = st.selectbox("Monthly cards", range(1, 13), index=default_month - 1,
                             format_func=lambda value: calendar.month_name[value], key="selected_month")
    values = month_cards(progress, specimens, year, month)
    c1, c2, c3 = st.columns(3)
    c1.metric(f"{calendar.month_name[month]} enrollments", values["Enrollments"],
              help="Unique participants by enrollment date; not event-row counts.")
    c2.metric(f"{calendar.month_name[month]} biospecimens",
              "—" if values["Biospecimens collected"] is None else values["Biospecimens collected"],
              help="Completed participant × sample events from prenatal and 0–5 month collections.")
    c3.metric(f"{calendar.month_name[month]} births", values["Births"], help="Unique participants by actual birth date.")
    if specimens is None:
        st.warning("The biospecimen export is unavailable or inconsistent. Reload the exports after running PRENATAL_2026.R.")
        with st.expander("Biospecimen source issue"):
            st.write(specimen_error)
    st.caption("The month selection applies to the three cards; the charts below show the full reporting year.")

    left, right = st.columns([2.2, 1], gap="large")
    with left, st.container(border=True):
        st.subheader(f"{year} completed data elements")
        ordered = summary.sort_values("Completed in year", ascending=True, kind="stable")
        fig = px.bar(ordered, x="Completed in year", y="Data element", orientation="h",
                     text="Completed in year", color_discrete_sequence=["#1689EE"])
        fig.update_traces(textposition="outside", cliponaxis=False,
                          hovertemplate="%{y}<br>%{x} completed in the year<extra></extra>")
        fig.update_xaxes(title="Completed events", rangemode="tozero", showgrid=True, gridcolor="#E8EDF4",
                         range=[0, max(5, summary["Completed in year"].max() * 1.16)])
        fig.update_yaxes(title=None, showgrid=False, categoryorder="array", categoryarray=ordered["Data element"].tolist())
        fig = style_chart(fig, 550)
        fig.update_layout(margin=dict(l=235, r=28, t=20, b=50))
        st.plotly_chart(fig, width="stretch", key="completed_chart", theme=None)
        st.caption("Actual dated completions, including participants outside this year's eligibility cohort.")
    with right:
        with st.container(border=True):
            st.subheader(f"{year} prenatal enrollment")
            enrollment = annual_counts(progress, "consent_date", year, selected_sites)
            st.plotly_chart(site_bar(enrollment, 235), width="stretch", theme=None, key="enrollment_chart")
        with st.container(border=True):
            st.subheader(f"{year} births")
            births = annual_counts(progress, "child_dob", year, selected_sites)
            st.plotly_chart(site_bar(births, 235), width="stretch", theme=None, key="birth_chart")

    with st.container(border=True):
        st.subheader(f"{year} projected births")
        projection = projected_counts(progress, year, selected_sites)
        fig = px.line(projection, x="Month", y="Projected births", color="site", markers=True,
                      color_discrete_map=SITE_COLORS, category_orders={"site": SITES,
                      "Month": projection.drop_duplicates("Month number").sort_values("Month number")["Month"].tolist()})
        fig.update_traces(line_width=3, marker_size=8,
                          hovertemplate="%{x}<br>%{y} pending births<extra>%{fullData.name}</extra>")
        fig.update_xaxes(title=None)
        fig.update_yaxes(title="Pending births", rangemode="tozero", dtick=1)
        st.plotly_chart(style_chart(fig, 290, legend=True), width="stretch", theme=None, key="projection_chart")
        st.caption(f"Pending births as of {as_of:%B %d, %Y}, grouped by estimated due month. Overdue records are excluded.")
        st.download_button("Download projected births", projection.to_csv(index=False).encode(),
                           "prenatal_projected_births_filtered.csv", "text/csv")
    with st.expander("Event counts & comparison export"):
        st.dataframe(summary[["Data element", "Completed in year"]], hide_index=True, width="stretch")
        st.download_button("Download completed event counts", summary[["Data element", "Completed in year"]].to_csv(index=False).encode(),
                           "prenatal_completed_events_filtered.csv", "text/csv")

with eligibility_tab:
    st.subheader("Completion among eligible participants")
    st.caption("Prenatal 1 + Actigraphy: enrollment in the reporting year. Prenatal 2: estimated due date minus 12 weeks starts in the reporting year. Water: enrolled on or after March 1, 2026, within the reporting year.")
    chosen_elements = st.multiselect("Data elements", ELEMENTS, default=ELEMENTS, key="progress_elements")
    view = summary.loc[summary["Data element"].isin(chosen_elements)].copy()
    known = view.loc[view["Eligible"].notna()].copy()
    if not chosen_elements:
        st.info("Select at least one data element.")
    elif known.empty:
        st.info("Eligibility rules are pending for the selected elements. Actual completions remain available in Overview.")
    else:
        stacked = known.melt(id_vars=["Data element", "Eligible", "Completion rate"],
                              value_vars=["Eligible complete", "Eligible incomplete"], var_name="Status", value_name="Participants")
        fig = px.bar(stacked, y="Data element", x="Participants", color="Status", orientation="h",
                     color_discrete_map={"Eligible complete": "#1689EE", "Eligible incomplete": "#DDE7F3"},
                     category_orders={"Data element": ELEMENTS}, custom_data=["Eligible", "Completion rate"])
        fig.update_traces(hovertemplate="%{y}<br>%{x} participants<br>Eligible: %{customdata[0]:.0f}<br>Completion rate: %{customdata[1]:.1%}<extra>%{fullData.name}</extra>")
        fig.update_xaxes(title="Eligible participants", rangemode="tozero")
        fig.update_yaxes(title=None, showgrid=False)
        fig = style_chart(fig, max(280, len(known) * 42 + 80), legend=True)
        fig.update_layout(margin=dict(l=175, r=24, t=55, b=50))
        st.plotly_chart(fig, width="stretch", theme=None, key="progress_chart")
    if chosen_elements:
        st.caption("Unknown eligibility is excluded from completion rates. Cord blood and medical record rules are pending. Blank rates mean the denominator is unknown or zero.")
        table = view.drop(columns="Completed in year")
        display_table = table.copy()
        for column in ["Eligible", "Eligible complete", "Eligible incomplete"]:
            display_table[column] = display_table[column].map(lambda value: "—" if pd.isna(value) else f"{int(value):,}")
        display_table["Completion rate"] = display_table["Completion rate"].map(
            lambda value: "—" if pd.isna(value) else f"{value:.1%}")
        st.dataframe(display_table, hide_index=True, width="stretch")
        st.download_button("Download eligibility summary", table.to_csv(index=False).encode(),
                           "prenatal_eligibility_filtered.csv", "text/csv")
        late = progress.loc[progress["Data Element Name"].isin(chosen_elements) & progress["Complete Date"].gt(as_of)]
        if len(late):
            st.warning(f"{len(late)} recorded completion date(s) are after the snapshot date. The current progress calculation retains these records; review their dates.")
