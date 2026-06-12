# ============================================================
# Build dashboard data for Streamlit deployment
# ============================================================
# Use this section after `activity_log_with_staff` has been created
# in 2026_6_35_month.R.
#
# Expected input object:
#   activity_log_with_staff
#
# Expected columns:
#   ECHO_ID, Event, status_group, Eligibility, Age_Window,
#   Completion, Completion_Date, staff
# ============================================================

library(tidyverse)
library(readr)
library(openxlsx)
library(fs)

# -----------------------------
# 1. Set output folder
# -----------------------------

APP_DIR <- file.path(
  "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard",
  "followup_streamlit_app"
)

latest_dir <- file.path(APP_DIR, "data", "latest")
snapshot_date <- format(Sys.Date(), "%Y-%m-%d")
snapshot_dir <- file.path(APP_DIR, "data", "snapshots", snapshot_date)

dir_create(latest_dir)
dir_create(snapshot_dir)

# -----------------------------
# 2. Standardize dashboard detail data
# -----------------------------

dashboard_detail <- activity_log_with_staff %>%
  mutate(
    # Use a display-friendly event name while keeping the original event name
    event_short = Event,
    
    # Fill missing staff assignment for dashboard grouping
    staff = if_else(is.na(staff) | str_trim(staff) == "", "Unassigned", staff),
    
    # Keep status_group as the standardized participant status for dashboard filters
    status_2026_std = status_group,
    participant_status = status_group,
    
    # Streamlit denominator logic:
    # Yes and Potential Participants are controlled by the app toggle.
    # No records are never included in the denominator.
    eligible_flag = case_when(
      Eligibility %in% c("Yes", "Potential Participants") ~ 1,
      TRUE ~ 0
    ),
    
    # Streamlit numerator logic:
    # Complete is counted as 1; all unfinished states are counted as 0.
    Score = case_when(
      Completion == "Complete" ~ 1,
      Completion %in% c("Incomplete", "No record", "Other") ~ 0,
      TRUE ~ NA_real_
    ),
    
    Outcome = Completion,
    source_sheet = "2026_6_35_month.R"
  ) %>%
  select(
    ECHO_ID,
    staff,
    Event,
    event_short,
    status_group,
    status_2026_std,
    participant_status,
    Eligibility,
    Age_Window,
    Outcome,
    Completion_Date,
    eligible_flag,
    Score,
    source_sheet
  ) %>%
  arrange(staff, Event, status_group, ECHO_ID)

# -----------------------------
# 3. Summary helper
# -----------------------------

summarise_dashboard <- function(data) {
  summary_by_event <- data %>%
    group_by(staff, event_short) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, coalesce(Score, 0), 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    )
  
  summary_overall <- data %>%
    group_by(staff) %>%
    summarise(
      denominator = sum(eligible_flag, na.rm = TRUE),
      numerator = sum(if_else(eligible_flag == 1, coalesce(Score, 0), 0), na.rm = TRUE),
      progress = if_else(denominator == 0, NA_real_, numerator / denominator),
      .groups = "drop"
    ) %>%
    mutate(event_short = "Overall") %>%
    relocate(event_short, .after = staff)
  
  bind_rows(summary_by_event, summary_overall) %>%
    arrange(staff, event_short)
}

# -----------------------------
# 4. Create two summary versions
# -----------------------------

# Version 1: include both eligible participants and Potential Participants
dashboard_summary_with_potential <- dashboard_detail %>%
  filter(Eligibility %in% c("Yes", "Potential Participants")) %>%
  summarise_dashboard()

# Version 2: only include currently eligible participants
dashboard_summary_without_potential <- dashboard_detail %>%
  filter(Eligibility == "Yes") %>%
  mutate(
    eligible_flag = 1
  ) %>%
  summarise_dashboard()

# -----------------------------
# 5. Save latest and snapshot outputs
# -----------------------------

write_csv(
  dashboard_detail,
  file.path(latest_dir, "dashboard_detail.csv"),
  na = ""
)

write_csv(
  dashboard_detail,
  file.path(snapshot_dir, "detail.csv"),
  na = ""
)

write_csv(
  dashboard_summary_with_potential,
  file.path(snapshot_dir, "summary_with_potential.csv"),
  na = ""
)

write_csv(
  dashboard_summary_without_potential,
  file.path(snapshot_dir, "summary_without_potential.csv"),
  na = ""
)

summary_wb <- createWorkbook()

addWorksheet(summary_wb, "With Potential")
writeData(summary_wb, "With Potential", dashboard_summary_with_potential)

addWorksheet(summary_wb, "Without Potential")
writeData(summary_wb, "Without Potential", dashboard_summary_without_potential)

addWorksheet(summary_wb, "Detail")
writeData(summary_wb, "Detail", dashboard_detail)

saveWorkbook(
  summary_wb,
  file.path(snapshot_dir, "summary.xlsx"),
  overwrite = TRUE
)

message("Dashboard data saved to: ", APP_DIR)
message("Latest detail: ", file.path(latest_dir, "dashboard_detail.csv"))
message("Snapshot folder: ", snapshot_dir)
