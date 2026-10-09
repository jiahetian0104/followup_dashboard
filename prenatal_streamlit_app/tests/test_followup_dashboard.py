import sys
import unittest
from pathlib import Path
import pandas as pd
from streamlit.testing.v1 import AppTest

APP = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(APP))
from followup_data import (read_followup, filter_followup, followup_cards, followup_summary,
                           followup_workflow, aging_out_list)


class FollowupTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.roster, cls.events = read_followup(APP.parent / 'powerbi' / 'latest')
        cls.year = int(cls.roster.report_year.iloc[0])

    def test_source_reconciliation_and_denominator(self):
        summary = followup_summary(self.events, self.year)
        eligible = int(self.roster.Eligible.fillna(False).sum())
        self.assertTrue(summary.Eligible.eq(eligible).all())
        self.assertTrue((summary['Eligible complete'] + summary['Eligible incomplete']).eq(eligible).all())
        expected = self.events['Complete Date'].dt.year.eq(self.year).sum()
        self.assertEqual(summary['Completed in year'].sum(), expected)

    def test_site_and_eligible_filters(self):
        roster, events = filter_followup(self.roster, self.events, ['Detroit'], True)
        self.assertTrue(roster.site.eq('Detroit').all())
        self.assertTrue(roster.Eligible.all())
        self.assertEqual(len(events), len(roster) * 13)
        age = aging_out_list(self.roster)
        self.assertTrue(age.Eligible.all())
        self.assertTrue(age.age_months.between(4, 5).all())
        pending = aging_out_list(self.roster, True)
        self.assertTrue((pending.survey.isna() | pending.visit_scheduled.isna()).all())

    def test_workflow_intersections_and_independent_counts(self):
        frame = pd.DataFrame({'survey': pd.to_datetime(['2026-01-02','2026-02-02',None]),
                              'visit_scheduled': pd.to_datetime(['2026-01-03','2026-02-01','2026-03-01']),
                              'diaper_sent': pd.to_datetime(['2026-01-04','2026-02-03','2026-03-02'])})
        self.assertEqual(followup_workflow(frame,2026)['Participants'].tolist(), [2,3,3])
        self.assertEqual(followup_workflow(frame,2026,True)['Participants'].tolist(), [2,2,2])

    def test_page_navigation_filters_and_empty_state(self):
        app = AppTest.from_file(str(APP/'streamlit_app.py'), default_timeout=20).run()
        app.radio(key='report_page').set_value('0–5 Month').run()
        self.assertEqual(len(app.exception),0)
        baseline = [metric.value for metric in app.metric]
        app.session_state['followup_sites'] = ['Detroit']
        app.run()
        expected = followup_cards(self.events[self.events.site=='Detroit'],self.year,int(app.session_state['followup_month']))
        self.assertEqual([metric.value for metric in app.metric], [str(value) for value in expected.values()])
        app.checkbox(key='followup_eligible_only').check().run()
        self.assertEqual(len(app.exception),0)
        app.radio(key='workflow_basis').set_value('Linked participant coverage').run()
        self.assertEqual(len(app.exception),0)
        app.button(key='reset_followup').click().run()
        self.assertEqual([metric.value for metric in app.metric],baseline)
        app.session_state['followup_sites'] = []
        app.run()
        self.assertEqual(len(app.metric),0)
        self.assertIn('Select at least one site',app.info[0].value)
        app.radio(key='report_page').set_value('Prenatal').run()
        self.assertEqual(len(app.exception),0)
        self.assertTrue(app.metric[0].label.endswith('enrollments'))


if __name__ == '__main__':
    unittest.main()
