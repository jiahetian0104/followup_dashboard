# CHARM IPA Task Dashboard

This Streamlit app gives FTM staff a filterable participant-level view of IPA and Ripple task progress.

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

- `data/latest/calendly_ftm_lookup.csv`: one latest host/inviter record per participant, appointment year, and IPA age band for 2025–2026, with the source appointment and matching method retained for audit.
- `data/latest/participant_calendly_assignment_lookup.csv`: the current assignment decision for every participant-age-band row. Priority is direct 2026 same-age evidence, Invitee name/email evidence, direct 2026 previous-age evidence, then direct 2025 Calendly evidence.
- `data/latest/calendly_ftm_lookup_qc.csv`: Calendly records whose participant, age band, or host could not be resolved confidently.
- `data/latest/calendly_participant_match_audit.csv`: every retained Calendly participant match with the invitee, child-name answer, matching method, age band, host, and QA result.
- `data/latest/participant_contact_name_aliases.csv`: active Ripple Participant/Mother/Alternate name and email relationships used for Invitee fallback matching.
- `data/latest/calendly_invitee_fallback_assignments.csv`: participant-age-band assignments supplied by the Invitee name/email evidence tier.
- `data/latest/ftm_assignment_history.csv`: effective-dated IPA assignment history by participant and age band. Calendly is the authoritative source; Ripple and the Call List remain available only as comparison fields.
- `data/latest/task_responsibility_ledger.csv`: locks every participant-age-band-task row to the matched Calendly FTM for that age band, regardless of outcome. A lower-priority fallback is upgraded when higher-priority Calendly evidence becomes available.

The September 20, 2026 refresh is the Calendly-assignment baseline. A participant can have a different Calendly host in a later age band; prior Complete, No-Show, Incomplete, and No record rows remain with the FTM recorded for the earlier age band. Participant identity is resolved using explicit IDs, child-name answers, Invitee child names, and active Ripple Participant/Mother/Alternate name or email relationships. Email can bridge contact surname changes. If one contact maps to siblings, a child first name can distinguish the participant; otherwise the event remains unresolved rather than assigning credit arbitrarily. Current assignment priority is direct 2026 same-age evidence, Invitee name/email evidence, direct 2026 previous-age evidence, then direct 2025 Calendly evidence. If none exists, the task remains `Unassigned`; Ripple and Call List owners are never used for IPA credit. The 2025 Calendly fallback supplies responsibility only and never changes a 2026 IPA outcome.

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

For IPA tasks, a participant's latest Calendly appointment within the same IPA age band controls the outcome. A completed latest appointment remains Complete even if an earlier appointment was a no-show. Non-IPA task outcomes come from Ripple.

## Optional path override

If the R script is run from another location, set `IPA_DASHBOARD_APP_DIR` to this app directory before running it.
