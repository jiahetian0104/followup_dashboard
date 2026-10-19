# Ripple event fields and applicable age bands.
ipa_event_map <- tribble(
  ~age_band, ~scheduled_col, ~complete_col, ~scheduled_date_col, ~complete_date_col,
  "12_23_month",
  "event.12_23mo_ipa_scheduled.completed",
  "event.12_23mo_ipa_complete.completed",
  "event.12_23mo_ipa_scheduled.completedDate",
  "event.12_23mo_ipa_complete.completedDate",
  "24_35_month",
  "event.24_35mo_ipa_scheduled.completed",
  "event.24_35mo_ipa_complete.completed",
  "event.24_35mo_ipa_scheduled.completedDate",
  "event.24_35mo_ipa_complete.completedDate",
  "3_5yr",
  "event.2026_3_5yr_ipa_scheduled.completed",
  "event.2026_3_5yr_ipa_complete.completed",
  "event.2026_3_5yr_ipa_scheduled.completedDate",
  "event.2026_3_5yr_ipa_complete.completedDate",
  "6_10yr",
  "event.2026_6_10yr_ipa_scheduled.completed",
  "event.2026_6_10yr_ipa_complete.completed",
  "event.2026_6_10yr_ipa_scheduled.completedDate",
  "event.2026_6_10yr_ipa_complete.completedDate",
  "11_17yr",
  "event.2026_11_17yr_ipa_scheduled.completed",
  "event.2026_11_17yr_ipa_complete.completed",
  "event.2026_11_17yr_ipa_scheduled.completedDate",
  "event.2026_11_17yr_ipa_complete.completedDate",
  "18_20yr",
  "event.2026_18_20yr_ipa_scheduled.completed",
  "event.2026_18_20yr_ipa_complete.completed",
  "event.2026_18_20yr_ipa_scheduled.completedDate",
  "event.2026_18_20yr_ipa_complete.completedDate"
)


ripple_task_map <- tibble(
  Task = c(
    "12-23mo Child Urine",
    "12-23mo Child Bloodspot",
    "12-23mo Mom Bloodspot",
    "12-23mo Hair",
    "24-35mo Mom Urine",
    "3-5yr Child Saliva",
    "11-17yr Child Hair",
    "3yrs+ Tooth 1",
    "3yrs+ Tooth 2",
    "3yrs+ Tooth 3"
  ),
  age_bands = list(
    "12_23_month",
    "12_23_month",
    "12_23_month",
    "12_23_month",
    "24_35_month",
    "3_5yr",
    "11_17yr",
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr"),
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr"),
    c("3_5yr", "6_10yr", "11_17yr", "18_20yr")
  ),
  completed_col = c(
    "event.12_23mo_child_urine_diaper_.completed",
    "event.12_23mo_child_bloodspot.completed",
    "event.12_23mo_mom_bloodspot.completed",
    "event.12_23mo_child_hair.completed",
    "event.24_35mo_cg_urine.completed",
    "event.2026_3_5yr_child_saliva.completed",
    "event.2026_11_17yr_child_hair.completed",
    "event.2026_tooth_1_kit_received.completed",
    "event.2026_tooth_2_kit_received.completed",
    "event.2026_tooth_3_kit_received.completed"
  ),
  completed_date_col = c(
    "event.12_23mo_child_urine_diaper_.completedDate",
    "event.12_23mo_child_bloodspot.completedDate",
    "event.12_23mo_mom_bloodspot.completedDate",
    "event.12_23mo_child_hair.completedDate",
    "event.24_35mo_cg_urine.completedDate",
    "event.2026_3_5yr_child_saliva.completedDate",
    "event.2026_11_17yr_child_hair.completedDate",
    "event.2026_tooth_1_kit_received.completedDate",
    "event.2026_tooth_2_kit_received.completedDate",
    "event.2026_tooth_3_kit_received.completedDate"
  )
)


