# CHARM IPA Task Dashboard

This Streamlit app gives FTM staff a filterable participant-level view of IPA and Ripple task progress.

IPA activity is a separate tab from biospecimens and other Ripple tasks. Its
heatmap places FTM names horizontally and Complete, No-show, Cancel, and
Reschedule counts vertically. Below the work summary, a percentage bar chart
shows `(Complete + 0.25 * No-show) / participant age bands` for each individual
FTM. Participant age bands are unique within each FTM, include cancel/reschedule-only
records, and use the same filters as the numerator. Transferred tasks can enter
more than one FTM's denominator. Repeated no-shows can yield a score above 100%:
this weighted performance score is not a completion rate and is not capped.
Unassigned, Pending verification and Shared hosts do not receive individual
performance percentages. The other-task charts, metrics, details and
trends exclude In-Person Assessments. IPA controls are independent of the
sidebar's roster/current-caseload/task filters and include all recorded age bands.

## Data flow

Running `../IPA Data.R` writes dashboard-ready files to:

- `data/latest/`: replaced after every refresh
- `data/snapshots/YYYY-MM-DD_HHMMSS/`: one immutable time point per successful refresh

The dashboard uses `task_checklist_long.csv` for task metrics and
`participant_roster.csv` for the complete current roster. The roster retains
6–11 month and potential participants even though they do not yet have
applicable IPA task rows. The wide checklist and FTM summary are also exported
for downstream use.

The refresh also maintains two persistent audit tables:

- `data/latest/ipa_appointment_history.csv`: all 2026 assessment bookings, with participant/age-band matching, booking host, canceled/no-show/rescheduled flags and old/new invitee links. Repeated reschedules are counted on each old booking, not on the new booking.
- `data/latest/ipa_appointment_history_qc.csv`: unresolved participant/age-band bookings, missing status evidence, missing hosts and shared-host bookings.
- `data/latest/participant_ipa_records.csv`: one row per participant/cohort/recorded age band, with completion, appointment counts, historical no-show/true cancellation/reschedule counts and completion attribution.
- `data/latest/participant_ipa_ftm_records.csv`: participant × age band × FTM activity. Each booking stays with its host; completion credit can belong to a different FTM.
- `data/latest/ftm_ipa_summary.csv`: FTM activity totals, separate from `ftm_task_summary.csv` (other tasks only).
- `data/latest/ipa_completion_evidence.csv`: retained Ripple completion/date evidence for replaying attribution rules without a new API refresh.

- `data/latest/calendly_ftm_lookup.csv`: one latest host/inviter record per participant, appointment year, and IPA age band for 2025–2026, with the source appointment and matching method retained for audit.
- `data/latest/participant_calendly_assignment_lookup.csv`: the current assignment decision for every participant-age-band row. Priority is direct 2026 same-age evidence, Invitee name/email evidence, direct 2026 previous-age evidence, direct 2025 Calendly evidence, then the staff-verified recent IPA FTM fallback.
- `data/latest/calendly_ftm_lookup_qc.csv`: Calendly records whose participant, age band, or host could not be resolved confidently.
- `data/latest/calendly_participant_match_audit.csv`: every retained Calendly participant match with the invitee, child-name answer, matching method, age band, host, and QA result.
- `data/latest/participant_contact_name_aliases.csv`: active Ripple Participant/Mother/Alternate name and email relationships used for Invitee fallback matching.
- `data/latest/calendly_invitee_fallback_assignments.csv`: participant-age-band assignments supplied by the Invitee name/email evidence tier.
- `data/latest/manual_verified_fallback_assignments.csv`: auditable last-resort participant-age-band assignments confirmed by project staff when no usable Calendly evidence exists.
- `data/latest/ftm_assignment_history.csv`: effective-dated IPA assignment history by participant and age band. Calendly is the authoritative source; Ripple and the Call List remain available only as comparison fields.
- `data/latest/task_responsibility_ledger.csv`: locks every participant-age-band-task row to the assigned IPA FTM for that age band, regardless of outcome. A lower-priority fallback is upgraded when higher-priority Calendly evidence becomes available.

The September 20, 2026 refresh is the Calendly-assignment baseline. A participant can have a different Calendly host in a later age band; prior Complete, No-Show, Incomplete, and No record rows remain with the FTM recorded for the earlier age band. Participant identity is resolved using explicit IDs, child-name answers, Invitee child names, and active Ripple Participant/Mother/Alternate name or email relationships. Email can bridge contact surname changes. If one contact maps to siblings, a child first name can distinguish the participant; otherwise the event remains unresolved rather than assigning credit arbitrarily. Current assignment priority is direct 2026 same-age evidence, Invitee name/email evidence, direct 2026 previous-age evidence, direct 2025 Calendly evidence, then the staff-verified recent IPA FTM fallback. If none exists, the task remains `Unassigned`; Ripple and Call List owners are never used for IPA credit. The 2025 and staff-verified fallbacks supply responsibility only and never change a 2026 IPA outcome. Any later higher-priority Calendly evidence automatically replaces the staff-verified fallback.

## Run locally

From this directory:

```bash
python3 -m pip install -r requirements.txt
streamlit run streamlit_app.py
```

The sidebar lets users choose Latest, a timestamped snapshot, or a manually uploaded checklist CSV. `Task responsibility` includes current and historical age-band tasks under their locked responsible FTM. `Current caseload` shows only currently applicable tasks under the participant's current Calendly FTM. Filters are available for participant scope, FTM, task, age group, outcome, and participant cohort. The Participant roster tab lets FTMs explicitly open Potential participants or 6–11 month participants without adding them to task-progress denominators. The Assignment QA tab shows host matching coverage, source appointments, and unresolved records. The Snapshot trend tab starts with the Calendly-assignment baseline and excludes older Ripple/Call List snapshots so the series does not mix ownership definitions.

## Metric definitions

- **Complete:** score 1
- **No-Show:** score 0.25
- **Incomplete:** score 0
- **No record:** no source record; displayed separately and counted as 0 in weighted progress
- **Weighted progress:** sum of task scores divided by the number of applicable participant-task rows

IPA activity uses every matching appointment, rather than the legacy checklist's
latest appointment. Cancel counts only `Canceled = TRUE` with `Rescheduled = FALSE`;
Reschedule counts each `Rescheduled = TRUE` old booking, including repeated actions
in a chain. New/old invitee links are exported for tracing the chain. A successor
link is also accepted as positive rescheduling evidence. Missing flags remain
unknown, including in summaries, rather than becoming zero.

Ripple completion is counted once per participant/age band. Completion credit first
goes to a single distinct individual Calendly host on the Ripple completion date,
from a booking that is neither canceled nor no-show. If that host cannot be resolved,
credit falls back to the latest booking in the same age band. Cohort differences
are ignored for completion attribution; the credited task retains its Ripple cohort.
Bookings in 2025 and 2026 may supply completion evidence. Only when no same-band
booking exists, try the immediately preceding age band, preferring an eligible
completion-date host and then the latest booking. Latest fallbacks include canceled
or no-show bookings and use appointment time, then booking creation time.
`Completion Attribution` and `Completion Fallback Appointment Date` identify
inferred credit. Missing/shared hosts or conflicting latest hosts remain
**Pending verification**. Older bookings do not enter the 2026 workload counts.
`ipa_completion_booking_evidence.csv` retains both years for attribution replay.
Participant IPA exports, displays and downloads exclude first and last names,
including displays loaded from older snapshots. Shared-host bookings are counted
once under a combined label. Dated 2025 completion evidence is excluded.
Undated completion flags remain visible for verification. Unmatched participant or
age-band appointments remain in the history QA export and do not get FTM credit.

The full legacy checklist remains available for responsibility audit and historical
completion evidence; its latest-record IPA outcome is not used by the new IPA
heatmap. Older snapshots without the new exports explicitly show that IPA activity
history is unavailable. They are not reconstructed from latest-task records.

Run the focused calculation checks from the workspace root:

```bash
Rscript followup_dashboard/ipa_streamlit_app/tests/test_ipa_activity.R
python3 -m unittest discover -s followup_dashboard/ipa_streamlit_app/tests
```

## Optional path override

If the R script is run from another location, set `IPA_DASHBOARD_APP_DIR` to this app directory before running it.
