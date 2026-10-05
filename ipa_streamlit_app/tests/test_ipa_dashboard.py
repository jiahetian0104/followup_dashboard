"""Checks for IPA action counts, filters, and the rendered Streamlit model."""
from pathlib import Path
import sys
import unittest

import pandas as pd

APP_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP_DIR))
from dashboard_data import (standardize_ipa_records, filter_ipa_records,
                            summarize_ipa_activity, IPA_COUNTS)


class IpaActivityTests(unittest.TestCase):
    def setUp(self):
        self.data = standardize_ipa_records(pd.DataFrame([
            ["A", "cohort", "3_5yr", 2026, False, "FTM 1", 2, 0, 0, 1, 1],
            ["B", "cohort", "3_5yr", 2026, True, "FTM 2", 1, 1, 0, 0, 0],
            ["C", "cohort", "6_10yr", 2026, True, "FTM 1", 1, 0, 1, 0, 0],
            ["C", "cohort", "6_10yr", 2026, True, "FTM 2", 1, 1, 0, 0, 0],
        ], columns=["Participant ID", "Participant Cohort", "IPA Age Band", "IPA Year",
                    "IPA Complete", "FTM", *IPA_COUNTS]), ftm_records=True)

    def test_participant_names_are_removed_from_legacy_exports(self):
        named = self.data.assign(firstName="Private", lastName="Name")
        cleaned = standardize_ipa_records(named, ftm_records=True)
        self.assertNotIn("firstName", cleaned.columns)
        self.assertNotIn("lastName", cleaned.columns)

    def test_actions_stay_with_host_and_complete_is_credited_once(self):
        summary = summarize_ipa_activity(self.data).set_index("FTM")
        self.assertEqual(summary.loc["FTM 1", "Complete Count"], 0)
        self.assertEqual(summary.loc["FTM 1", "Cancel Count"], 1)
        self.assertEqual(summary.loc["FTM 1", "Reschedule Count"], 1)
        self.assertEqual(summary.loc["FTM 2", "Complete Count"], 2)
        self.assertEqual(summary["Appointment Count"].sum(), 5)
        self.assertEqual(summary.loc["FTM 1", "Participant Age Bands"], 2)
        self.assertEqual(summary.loc["FTM 1", "Weighted Points"], 0.25)
        self.assertAlmostEqual(summary.loc["FTM 1", "Weighted IPA Performance"], 0.125)
        self.assertEqual(summary.loc["FTM 2", "Weighted IPA Performance"], 1)

    def test_filters_and_reset_reconcile(self):
        selected = filter_ipa_records(self.data, ["FTM 1"], ["3–5 years"], [])
        self.assertEqual(len(selected), 1)
        self.assertEqual(summarize_ipa_activity(selected)["Appointment Count"].sum(), 2)
        reset = filter_ipa_records(self.data, [], [], [])
        self.assertEqual(summarize_ipa_activity(reset)["Complete Count"].sum(), 2)

    def test_unknown_counts_are_preserved(self):
        self.data.loc[0, "Cancel Count"] = pd.NA
        summary = summarize_ipa_activity(self.data).set_index("FTM")
        self.assertTrue(pd.isna(summary.loc["FTM 1", "Cancel Count"]))

    def test_duplicate_records_are_rejected(self):
        with self.assertRaises(ValueError):
            standardize_ipa_records(pd.concat([self.data, self.data.iloc[:1]]), ftm_records=True)

    def test_empty_selection(self):
        selected = filter_ipa_records(self.data, ["no such host"], [], [])
        self.assertTrue(summarize_ipa_activity(selected).empty)

    def test_repeated_no_shows_are_not_capped(self):
        record = self.data.iloc[[1]].copy()
        record["No-show Count"] = 2
        summary = summarize_ipa_activity(record)
        self.assertEqual(summary["Weighted IPA Performance"].iloc[0], 1.5)

    def test_unresolved_credit_has_no_individual_percentage(self):
        record = self.data.iloc[[1]].copy()
        record["FTM"] = "Pending verification"
        self.assertTrue(pd.isna(summarize_ipa_activity(record)["Weighted IPA Performance"].iloc[0]))


class StreamlitModelTests(unittest.TestCase):
    def test_app_loads_and_separates_ipa(self):
        from streamlit.testing.v1 import AppTest
        app = AppTest.from_file(str(APP_DIR / "streamlit_app.py"), default_timeout=30).run()
        self.assertFalse(app.exception, [exc.message for exc in app.exception])
        self.assertEqual(app.tabs[0].label, "IPA activity")
        task_control = next(widget for widget in app.multiselect if widget.label == "Task")
        self.assertNotIn("In-Person Assessments", task_control.options)
        ipa_control = next((widget for widget in app.multiselect if widget.label == "IPA FTM"), None)
        if ipa_control is not None and ipa_control.options:
            ipa_control.set_value([ipa_control.options[0]])
            app.run()
            self.assertFalse(app.exception, [exc.message for exc in app.exception])
            next(widget for widget in app.multiselect if widget.label == "IPA FTM").set_value([])
            app.run()
            self.assertFalse(app.exception, [exc.message for exc in app.exception])


if __name__ == "__main__":
    unittest.main()
