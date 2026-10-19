"""Contract, filtering, snapshot compatibility and Streamlit interaction checks."""
from pathlib import Path
import os
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import pandas as pd

APP_DIR = Path(__file__).resolve().parents[1]
ROOT = APP_DIR.parents[1]
sys.path.insert(0,str(APP_DIR))
from dashboard_data import (FILES, read_csv, validate_batch, summarize_workload,
    filter_records, discover_data_versions, snapshot_trend)

class DashboardTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp = tempfile.TemporaryDirectory(prefix="ipa_dashboard_test_")
        cls.data_dir = Path(cls.temp.name)
        env = os.environ.copy()
        env["IPA_TEST_OUTPUT_DIR"] = str(cls.data_dir)
        subprocess.run(["Rscript",str(APP_DIR/"tests/test_ipa_activity.R")],cwd=ROOT,env=env,check=True,capture_output=True)
        cls.tables = {key:read_csv(cls.data_dir/"latest"/filename) for key,filename in FILES.items()}

    @classmethod
    def tearDownClass(cls):
        cls.temp.cleanup()

    def test_contract_and_first_vs_latest(self):
        validate_batch(self.tables)
        completed = self.tables["completions"].iloc[0]
        self.assertEqual(completed["Attributed FTM"],"Anna")
        self.assertEqual(completed["Completed at"].month,2)
        specimen = self.tables["specimens"].query('`Participant ID` == "A"').iloc[0]
        self.assertEqual(specimen["FTM"],"Nicole")
        self.assertEqual(specimen["statusId"],"Withdrawn")

    def test_unknown_workload_and_filters(self):
        data = self.tables["workload"].copy()
        selected = filter_records(data,[],[],["Jo"])
        self.assertEqual(summarize_workload(selected)["Appointment Count"].sum(),2)
        data.loc[data["FTM"]=="Jo","No-show Count"] = pd.NA
        summary = summarize_workload(data).set_index("FTM")
        self.assertTrue(pd.isna(summary.loc["Jo","No-show Count"]))
        self.assertFalse(any("Weighted" in col for col in summary.columns))

    def test_study_timezone_preserves_completion_month(self):
        path = self.data_dir / "timezone.csv"
        pd.DataFrame({"Completed at":["2026-02-01T04:30:00Z", "2027-01-01T04:30:00Z"]}).to_csv(path,index=False)
        dates = read_csv(path)["Completed at"]
        self.assertEqual(dates.iloc[0], pd.Timestamp("2026-01-31 23:30:00"))
        self.assertEqual(dates.iloc[1].year,2026)

    def test_duplicate_and_wrong_year_fail(self):
        tables = {key:value.copy() for key,value in self.tables.items()}
        tables["completions"] = pd.concat([tables["completions"]]*2)
        with self.assertRaises(ValueError):validate_batch(tables)
        tables["completions"] = self.tables["completions"].copy()
        tables["specimens"]["Collection Date"] = pd.Timestamp("2025-01-01")
        with self.assertRaises(ValueError):validate_batch(tables)

    def test_snapshots_only_compare_same_method(self):
        import shutil
        snapshots = self.data_dir/"snapshots"
        snapshots.mkdir(exist_ok=True)
        new = snapshots/"2026-10-19_100000"
        if not new.exists():shutil.copytree(self.data_dir/"latest",new)
        old = snapshots/"2026-10-05_100000"
        old.mkdir(exist_ok=True)
        pd.DataFrame([{"generated_at":"2026-10-05 10:00:00 EDT"}]).to_csv(old/"manifest.csv",index=False)
        versions=discover_data_versions(self.data_dir)
        self.assertTrue(any(v.schema=="legacy" for v in versions))
        trend=snapshot_trend(versions)
        self.assertEqual(len(trend),1)
        self.assertEqual(trend.iloc[0]["IPA completions"],1)
        self.assertEqual(snapshot_trend(versions,ftms=["Nicole"]).iloc[0]["IPA completions"],0)

    def test_app_and_filters(self):
        from streamlit.testing.v1 import AppTest
        with patch.dict(os.environ,{"IPA_DASHBOARD_DATA_DIR":str(self.data_dir)}):
            app=AppTest.from_file(str(APP_DIR/"streamlit_app.py"),default_timeout=30).run()
            self.assertFalse(app.exception,[e.message for e in app.exception])
            self.assertEqual([t.label for t in app.tabs],["IPA completions","Appointment workload","Biospecimens","Data QC","Snapshot trend"])
            owner=next(w for w in app.multiselect if w.label=="Completion FTM")
            owner.set_value(["Anna"]);app.run()
            self.assertFalse(app.exception,[e.message for e in app.exception])
            host=next(w for w in app.multiselect if w.label=="Booking host")
            host.set_value(["Jo"]);app.run()
            self.assertFalse(app.exception,[e.message for e in app.exception])
            age=next(w for w in app.multiselect if w.label=="Age group")
            age.set_value(["12–23 months"]);app.run()
            self.assertFalse(app.exception,[e.message for e in app.exception])
            owner=next(w for w in app.multiselect if w.label=="Completion FTM")
            owner.set_value([]);app.run()
            self.assertFalse(app.exception,[e.message for e in app.exception])

if __name__=="__main__":unittest.main()
