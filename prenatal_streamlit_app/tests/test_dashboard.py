import sys
import tempfile
import unittest
from pathlib import Path

import pandas as pd
from streamlit.testing.v1 import AppTest

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))
from dashboard_data import read_progress, read_specimens, annual_counts, month_cards, event_summary, projected_counts


class MetricTests(unittest.TestCase):
    def setUp(self):
        # Same participant repeats for two events; enrollment/birth must count once.
        self.frame = pd.DataFrame({
            "participant_key": ["a", "a", "b", "b"],
            "globalId": ["ABC123-01-0 (001)"] * 2 + ["DEF456-01-0 (2)"] * 2,
            "customId": ["4101P1"] * 2 + ["4401P1"] * 2,
            "site": ["Detroit"] * 2 + ["Flint"] * 2,
            "event_code": ["pn1_survey", "water"] * 2,
            "Data Element Name": ["Prenatal 1 Survey", "Water"] * 2,
            "Eligible": pd.Series([True, True, False, False], dtype="boolean"),
            "Has Completion Record": pd.Series([True, False, True, False], dtype="boolean"),
            "is_biospecimen": pd.Series([False, True, False, True], dtype="boolean"),
            "projected_birth": pd.Series([False, False, True, True], dtype="boolean"),
            "consent_date": pd.to_datetime(["2026-10-01"] * 2 + ["2025-12-01"] * 2),
            "child_dob": pd.to_datetime(["2026-10-03"] * 2 + [None] * 2),
            "Complete Date": pd.to_datetime(["2026-10-01", None, "2026-10-02", None]),
            "estduedate": pd.to_datetime(["2026-10-01"] * 2 + ["2026-11-05"] * 2),
            "edd_month_start": pd.to_datetime(["2026-10-01"] * 2 + ["2026-11-01"] * 2),
            "report_as_of": pd.to_datetime(["2026-10-07"] * 4), "report_year": [2026] * 4,
        })

    def test_deduplication_and_cross_year_births(self):
        births = annual_counts(self.frame, "child_dob", 2026, ["Detroit", "Flint"])
        self.assertEqual(births["Participants"].tolist(), [1, 0])
        enrolled = annual_counts(self.frame, "consent_date", 2026, ["Detroit", "Flint"])
        self.assertEqual(enrolled["Participants"].tolist(), [1, 0])
        self.assertEqual(month_cards(self.frame, None, 2026, 10),
                         {"Enrollments": 1, "Births": 1, "Biospecimens collected": None})
        self.assertEqual(month_cards(self.frame, pd.DataFrame({"event_date": pd.to_datetime([])}), 2026, 10)["Biospecimens collected"], 0)

    def test_actuals_do_not_drop_not_eligible_completions(self):
        summary = event_summary(self.frame, 2026).set_index("Data element")
        row = summary.loc["Prenatal 1 Survey"]
        self.assertEqual(row["Completed in year"], 2)
        self.assertEqual(row["Eligible"], 1)
        self.assertEqual(row["Eligible complete"], 1)
        self.assertEqual(row["Not eligible"], 1)
        self.assertEqual(row["Completion rate"], 1)
        self.assertEqual(summary.loc["Water", "Eligible"], 1)
        self.assertEqual(summary.loc["Water", "Eligible incomplete"], 1)
        self.assertEqual(summary.loc["Water", "Completion rate"], 0)
        unknown = self.frame.copy()
        unknown.loc[unknown["Data Element Name"].eq("Water"), "Eligible"] = pd.NA
        pending = event_summary(unknown, 2026).set_index("Data element")
        self.assertTrue(pd.isna(pending.loc["Water", "Eligible"]))
        self.assertTrue(pd.isna(pending.loc["Water", "Completion rate"]))

    def test_projection_zeros_and_unique_people(self):
        counts = projected_counts(self.frame, 2026, ["Detroit", "Flint"])
        self.assertEqual(counts["Projected births"].sum(), 1)
        self.assertEqual(len(counts), 6)
        self.assertEqual(counts.loc[(counts.site == "Flint") & (counts["Month number"] == 11), "Projected births"].iloc[0], 1)
        self.assertEqual(annual_counts(self.frame[self.frame.site == "Flint"], "child_dob", 2026, ["Flint"])["Participants"].iloc[0], 0)

    def test_legacy_specimen_id_swap_and_leading_zero(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "events.csv"
            pd.DataFrame({"globalId": ["4101P1"], "customId": ["ABC123-01-0 (001)"],
                          "is_biospecimen": [True], "event_code": ["pn1_blood"],
                          "event_date": ["2026-10-01"], "report_as_of": ["2026-10-07"]}).to_csv(path, index=False)
            result = read_specimens(path, self.frame)
            self.assertEqual(result.participant_key.tolist(), ["a"])
            self.assertEqual(result.site.tolist(), ["Detroit"])
            self.assertEqual(month_cards(self.frame, result, 2026, 10)["Biospecimens collected"], 1)

    def test_rejects_duplicate_progress(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "progress.csv"
            pd.concat([self.frame, self.frame.iloc[[0]]]).to_csv(path, index=False)
            with self.assertRaisesRegex(ValueError, "unique participant/event"):
                read_progress(path)


class StreamlitTests(unittest.TestCase):
    def test_snapshot_selection_persists_between_reports(self):
        app = AppTest.from_file(str(APP / "streamlit_app.py"), default_timeout=20).run()
        selector = app.selectbox(key="data_version")
        self.assertEqual(selector.value, "Latest")
        historical = next(option for option in selector.options if option.startswith("Snapshot"))
        selector.set_value(historical).run()
        self.assertEqual(len(app.exception), 0)
        self.assertGreater(len(app.metric), 0)
        app.radio(key="report_page").set_value("0–5 Month").run()
        self.assertEqual(app.selectbox(key="data_version").value, historical)
        self.assertEqual(len(app.exception), 0)
        app.selectbox(key="data_version").set_value("Latest").run()
        self.assertEqual(len(app.exception), 0)
        self.assertGreater(len(app.metric), 0)

    def test_default_filter_reset_and_empty_selection(self):
        app = AppTest.from_file(str(APP / "streamlit_app.py"), default_timeout=20).run()
        self.assertEqual(len(app.exception), 0)
        baseline = [metric.value for metric in app.metric]
        app.session_state["selected_sites"] = ["Detroit"]
        app.run()
        self.assertEqual(len(app.exception), 0)
        frame = read_progress(APP.parent / "powerbi" / "latest" / "prenatal_progress.csv")
        subset = frame[frame.site == "Detroit"]
        expected = month_cards(subset, None, int(frame.report_year.iloc[0]), int(app.session_state["selected_month"]))
        self.assertEqual(app.metric[0].value, str(expected["Enrollments"]))
        self.assertEqual(app.metric[2].value, str(expected["Births"]))
        app.selectbox(key="selected_month").set_value(1).run()
        self.assertTrue(app.metric[0].label.startswith("January"))
        app.button(key="reset_filters").click().run()
        self.assertEqual([metric.value for metric in app.metric], baseline)
        app.session_state["selected_sites"] = []
        app.run()
        self.assertEqual(len(app.metric), 0)
        self.assertIn("Select at least one site", app.info[0].value)


if __name__ == "__main__":
    unittest.main()
