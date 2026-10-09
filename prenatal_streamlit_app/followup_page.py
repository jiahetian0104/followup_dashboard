"""Executed inside the existing Streamlit app; uses the shared theme/chart style."""
import calendar
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import streamlit as st

from dashboard_data import SITES
from followup_data import (FOLLOWUP_ELEMENTS, read_followup, filter_followup, followup_summary,
                           followup_cards, followup_workflow, aging_out_list)


@st.cache_data(show_spinner=False)
def load_followup(directory, roster_version, progress_version):
    del roster_version, progress_version
    return read_followup(directory)


def reset_followup():
    st.session_state["followup_sites"] = st.session_state["followup_available"]
    st.session_state["followup_month"] = st.session_state["followup_default_month"]
    st.session_state["followup_eligible_only"] = False


roster_path = DATA_DIR / "month0to5_participants.csv"
long_path = DATA_DIR / "month0to5_progress.csv"
if not roster_path.exists() or not long_path.exists():
    st.title("0–5 Month Report")
    st.warning("Run PRENATAL_2026.R to create the 0–5 month roster and event exports.")
    st.stop()
try:
    roster_all, events_all = load_followup(DATA_DIR, roster_path.stat().st_mtime_ns, long_path.stat().st_mtime_ns)
except (ValueError, KeyError, pd.errors.ParserError) as error:
    st.error(str(error))
    st.stop()

year = int(roster_all["report_year"].iloc[0])
as_of = roster_all["report_as_of"].iloc[0]
available = [site for site in SITES if site in roster_all["site"].unique()]
st.session_state["followup_available"] = available
st.session_state["followup_default_month"] = as_of.month if as_of.year == year else 12
if "followup_sites" not in st.session_state:
    st.session_state["followup_sites"] = available

with st.sidebar:
    sites = SITE_CONTROL.multiselect("Site", available, key="followup_sites")
    st.button("Reset filters", on_click=reset_followup, key="reset_followup", width="stretch")
    st.divider()
    st.subheader("0–5 month settings")
    eligible_only = st.checkbox("Restrict activity to eligible participants", key="followup_eligible_only",
        help="Off: all actual completions in the reporting year. On: participants created in the reporting year only.")
    st.button("Reload CSV data", on_click=load_followup.clear, key="reload_followup", width="stretch")
    st.caption("Run PRENATAL_2026.R to retrieve new Ripple data, then reload the exports.")
    st.divider()
    st.caption("Snapshot date")
    st.write(as_of.strftime("%B %d, %Y"))
    with st.expander("Definitions & sources"):
        st.markdown("""**Population:** F-category local IDs only.

**Eligibility:** dateCreated in the reporting year. A missing creation date leaves eligibility unknown.

**Actual completions:** each participant × event with a completion date in the reporting year. The activity checkbox controls whether only eligible participants are included.

**Biospecimens:** child urine, child stool, maternal hair, maternal bloodspot and breast milk. Counts are completed sample events, not tubes.

**Workflow:** Annual event counts compares independent milestone counts. Linked participant coverage starts from this year's completed surveys, then counts the same participants with visit-scheduling and diaper records in the reporting year.

**Aging out:** eligible participants aged at least 4 and under 6 completed calendar months at the snapshot date. Uses child_dob, otherwise birthday. Six-month deadlines clip to month end when needed.

**IPA vs. VIB:** completed visit-scheduling events versus submitted VIB requests, rather than visits attended.

**Sources:** month0to5_participants.csv and month0to5_progress.csv, exported from PRENATAL_2026.R.""")

st.title(f"{year} 0–5 Month Report")
st.caption(f"Snapshot: {as_of:%B %d, %Y}")
if not sites:
    st.info("Select at least one site to view the report.")
    st.stop()
site_roster, site_events = filter_followup(roster_all, events_all, sites)
roster, events = filter_followup(roster_all, events_all, sites, eligible_only)
eligible_count = int(site_roster["Eligible"].fillna(False).sum())
unknown_count = int(site_roster["Eligible"].isna().sum())
st.caption(f"{eligible_count:,} eligible participants · F-category records created in {year} · {unknown_count:,} with unknown eligibility")
if roster.empty:
    st.info("No participants match the selected activity population. Change the site selection or eligibility restriction.")
    st.stop()

overview, progress_tab = st.tabs(["Overview", "Eligibility & progress"])
summary = followup_summary(events, year)
with overview:
    with st.container(border=True):
        month = st.selectbox("Monthly cards", range(1, 13), index=st.session_state["followup_default_month"] - 1,
            format_func=lambda value: calendar.month_name[value], key="followup_month")
    cards = followup_cards(events, year, month)
    c1, c2, c3 = st.columns(3)
    c1.metric(f"{calendar.month_name[month]} surveys", cards["Surveys"])
    c2.metric(f"{calendar.month_name[month]} biospecimens", cards["Biospecimens"],
              help="Child urine/stool, maternal hair/bloodspot, and breast milk completion events.")
    c3.metric(f"{calendar.month_name[month]} visits scheduled", cards["Visits scheduled"])
    st.caption("The month selection applies to the three cards. Annual charts use the full reporting year.")
    left, right = st.columns([2.1, 1], gap="large")
    with left, st.container(border=True):
        st.subheader(f"{year} completed data elements")
        ordered = summary.sort_values("Completed in year", ascending=True, kind="stable")
        fig = px.bar(ordered, x="Completed in year", y="Data element", orientation="h", text="Completed in year",
                     color_discrete_sequence=["#1689EE"])
        fig.update_traces(textposition="outside", cliponaxis=False,
            hovertemplate="%{y}<br>%{x} completed events<extra></extra>")
        fig.update_xaxes(title="Completed events", rangemode="tozero", range=[0, max(5, ordered["Completed in year"].max() * 1.18)])
        fig.update_yaxes(title=None, categoryorder="array", categoryarray=ordered["Data element"].tolist(), showgrid=False)
        fig = style_chart(fig, 535)
        fig.update_layout(margin=dict(l=145, r=28, t=20, b=50))
        st.plotly_chart(fig, width="stretch", theme=None, key="followup_elements_chart")
    with right, st.container(border=True):
        st.subheader("Survey to visit workflow")
        basis = st.radio("Count basis", ["Annual event counts", "Linked participant coverage"],
                         key="workflow_basis", label_visibility="collapsed")
        workflow = followup_workflow(roster, year, linked=basis == "Linked participant coverage")
        fig = go.Figure(go.Funnel(y=workflow["Stage"], x=workflow["Participants"],
            texttemplate="%{value}<br>%{percentInitial:.1%}" if workflow["Participants"].iloc[0] > 0 else "%{value}",
            marker=dict(color=["#1689EE", "#397DD1", "#8AB7E7"])))
        fig = style_chart(fig, 330)
        fig.update_layout(margin=dict(l=140, r=18, t=24, b=24))
        st.plotly_chart(fig, width="stretch", theme=None, key="followup_workflow_chart")
        st.caption("Stages are independent annual counts; participants may differ." if basis == "Annual event counts"
                   else "Stages show the intersection of the same participants with the required records in the reporting year.")
        if workflow["Participants"].iloc[0] == 0:
            st.caption("Stage ratios are unavailable because no completed surveys are recorded.")
        st.download_button("Download workflow", workflow.to_csv(index=False).encode(), "month0to5_workflow_filtered.csv", "text/csv")
    aging_column, request_column = st.columns([1.2, 1.8], gap="large")
    with aging_column, st.container(border=True):
        st.subheader("Aging out list")
        outstanding = st.checkbox("Only outstanding survey or visit", key="aging_out_outstanding")
        aging = aging_out_list(site_roster, outstanding)
        st.caption("Eligible participants aged 4–5 months, ordered by their six-month deadline.")
        if aging.empty:
            st.info("No participants match the aging-out criteria.")
        else:
            view = aging[["customId", "site", "age_months", "age_out_date", "days_to_age_out", "Survey", "Visit scheduled"]].rename(
                columns={"customId": "Local ID", "site": "Site", "age_months": "Age (months)",
                         "age_out_date": "Six-month deadline", "days_to_age_out": "Days remaining"})
            st.dataframe(view, hide_index=True, width="stretch", height=320,
                         column_config={"Age (months)": st.column_config.NumberColumn(format="%d"),
                                        "Six-month deadline": st.column_config.DateColumn(format="MMM DD, YYYY")})
            st.download_button("Download aging out list", view.to_csv(index=False).encode(), "month0to5_aging_out_filtered.csv", "text/csv")
    with request_column, st.container(border=True):
        st.subheader("IPA vs. VIB requests")
        requests = pd.DataFrame({"Milestone": ["Visits scheduled", "VIB requests"],
            "Participants": [int(roster["visit_scheduled"].dt.year.eq(year).sum()), int(roster["vib_request"].dt.year.eq(year).sum())]})
        fig = px.bar(requests, x="Participants", y="Milestone", color="Milestone", orientation="h", text="Participants",
                     color_discrete_map={"Visits scheduled": "#EB773D", "VIB requests": "#1689EE"})
        fig.update_traces(textposition="outside", cliponaxis=False)
        fig.update_xaxes(title="Participants", rangemode="tozero", range=[0, max(5, requests["Participants"].max() * 1.2)])
        fig.update_yaxes(title=None, showgrid=False)
        fig = style_chart(fig, 280)
        fig.update_layout(margin=dict(l=130, r=28, t=24, b=50))
        st.plotly_chart(fig, width="stretch", theme=None, key="followup_requests_chart")
        st.caption("In-person visit scheduling versus submitted remote-assessment requests. A participant can have both milestones.")
    with st.expander("Event counts & Power BI comparison"):
        st.dataframe(summary, hide_index=True, width="stretch")
        st.download_button("Download event summary", summary.to_csv(index=False).encode(), "month0to5_event_summary_filtered.csv", "text/csv")

with progress_tab:
    st.subheader("Completion among eligible participants")
    st.caption(f"Eligibility: dateCreated in {year}. Completion rate uses all recorded completions among eligible participants.")
    chosen = st.multiselect("Data elements", FOLLOWUP_ELEMENTS, default=FOLLOWUP_ELEMENTS, key="followup_elements")
    # Eligibility progress always uses the eligible roster for the selected sites.
    table = followup_summary(site_events, year)
    table = table.loc[table["Data element"].isin(chosen)].copy()
    known = table.loc[table["Eligible"].notna()].copy()
    if not chosen:
        st.info("Select at least one data element.")
    elif known.empty or eligible_count == 0:
        st.info("No confirmed eligible participants are available for the selected sites.")
    else:
        stacked = known.melt(id_vars=["Data element", "Eligible", "Completion rate"],
                             value_vars=["Eligible complete", "Eligible incomplete"], var_name="Status", value_name="Participants")
        fig = px.bar(stacked, x="Participants", y="Data element", color="Status", orientation="h",
                     color_discrete_map={"Eligible complete": "#1689EE", "Eligible incomplete": "#DDE7F3"},
                     category_orders={"Data element": FOLLOWUP_ELEMENTS}, custom_data=["Eligible", "Completion rate"])
        fig.update_traces(hovertemplate="%{y}<br>%{x} participants<br>Eligible: %{customdata[0]:.0f}<br>Completion rate: %{customdata[1]:.1%}<extra>%{fullData.name}</extra>")
        fig.update_xaxes(title="Eligible participants", rangemode="tozero")
        fig.update_yaxes(title=None, showgrid=False)
        fig = style_chart(fig, max(320, len(known) * 36 + 90), legend=True)
        fig.update_layout(margin=dict(l=150, r=24, t=55, b=50))
        st.plotly_chart(fig, width="stretch", theme=None, key="followup_progress_chart")
    if chosen:
        display = table.copy()
        for column in ["Eligible", "Eligible complete", "Eligible incomplete"]:
            display[column] = display[column].map(lambda value: "—" if pd.isna(value) else f"{int(value):,}")
        display["Completion rate"] = display["Completion rate"].map(lambda value: "—" if pd.isna(value) else f"{value:.1%}")
        st.dataframe(display, hide_index=True, width="stretch")
        st.download_button("Download eligibility summary", table.to_csv(index=False).encode(), "month0to5_eligibility_filtered.csv", "text/csv")
    review_count = int(events.loc[events["Data Element Name"].isin(chosen), "Complete Date"].gt(as_of).sum())
    if review_count:
        st.warning(f"{review_count} recorded completion dates are after the snapshot date. These records are retained for review.")
