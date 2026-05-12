
# 1. Load Packages --------------------------------------------------------

library(tidyverse)


# 2. Import Data ----------------------------------------------------------

file_path <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard/2026_6_35_month_export.csv"

df <- read_csv(file_path, show_col_types = FALSE)


# 3. Check Calculated Age window and statusId ----------------------------

df_age <- df %>%
  mutate(
    birthday_date = mdy(birthday),
    age_months = interval(birthday_date, Sys.Date()) %/% months(1),
    age_window = case_when(
      age_months >= 6  & age_months <= 11 ~ "6_11_month",
      age_months >= 12 & age_months <= 23 ~ "12_23_month",
      age_months >= 24 & age_months <= 35 ~ "24_35_month",
      TRUE ~ NA_character_
    )
  )

df_age_check <- df_age %>%
  mutate(
    status_age_window = case_when(
      str_detect(statusId, regex("^6-11 Month", ignore_case = TRUE)) ~ "6_11_month",
      str_detect(statusId, regex("^12-23 Month", ignore_case = TRUE)) ~ "12_23_month",
      str_detect(statusId, regex("^24-35 Month", ignore_case = TRUE)) ~ "24_35_month",
      TRUE ~ NA_character_
    ),
    
    age_window_match = case_when(
      age_window == status_age_window ~ "Match",
      !is.na(age_window) & !is.na(status_age_window) & age_window != status_age_window ~ "Mismatch",
      !is.na(age_window) & is.na(status_age_window) ~ "Birthday in target range, status not target age status",
      is.na(age_window) & !is.na(status_age_window) ~ "Status target age status, birthday outside target range",
      TRUE ~ "Both missing / outside target range"
    )
  ) %>%
  filter(!is.na(status_age_window))

df_age_check %>%
  count(age_window, status_age_window, age_window_match, sort = TRUE)

# even there are some mismatch, but use statusId to identify age window for our follow-up dashboard.

# Caregiver survey related events across age windows
cg_event_map <- tribble(
  ~age_window,    ~event_prefix,                         ~staff_by_age,
  "6_11_month",   "event.6_11_mo_cg_survey",              "Anna",
  "12_23_month",  "event.12_23mo_caregiver_survey",        "Jody",
  "24_35_month",  "event.24_35mo_caregiver_survey",        "Cassie"
) %>%
  mutate(
    completed_col      = paste0(event_prefix, ".completed"),
    completed_date_col = paste0(event_prefix, ".completedDate"),
    missed_col         = paste0(event_prefix, ".missed"),
    missed_date_col    = paste0(event_prefix, ".missedDate"),
    scheduled_date_col = paste0(event_prefix, ".scheduledDate")
  )

participant_staff_base <- df_age_check %>%
  left_join(
    cg_event_map,
    by = c("status_age_window" = "age_window")
  ) %>%
  mutate(row_id = row_number())


to_logical <- function(x) {
  case_when(
    x %in% c(TRUE, "TRUE", "True", "true", "1", 1) ~ TRUE,
    x %in% c(FALSE, "FALSE", "False", "false", "0", 0) ~ FALSE,
    TRUE ~ NA
  )
}

participant_staff <- participant_staff_base %>%
  mutate(
    tags = if_else(is.na(tags) | str_trim(tags) == "", "", tags),
    
    tag_staff = str_extract(
      tags,
      regex("\\b(Anna|Jody|Cassie)\\b", ignore_case = TRUE)
    ),
    
    tag_staff = str_to_title(tag_staff),
    
    staff = coalesce(tag_staff, staff_by_age),
    
    cg_completed = map2_lgl(
      completed_col,
      row_id,
      ~ to_logical(participant_staff_base[[.x]][.y])
    ),
    
    cg_missed = map2_lgl(
      missed_col,
      row_id,
      ~ to_logical(participant_staff_base[[.x]][.y])
    ),
    
    cg_completedDate = map2_chr(
      completed_date_col,
      row_id,
      ~ as.character(participant_staff_base[[.x]][.y])
    ),
    
    cg_missedDate = map2_chr(
      missed_date_col,
      row_id,
      ~ as.character(participant_staff_base[[.x]][.y])
    ),
    
    cg_scheduledDate = map2_chr(
      scheduled_date_col,
      row_id,
      ~ as.character(participant_staff_base[[.x]][.y])
    ),
    
    cg_status = case_when(
      cg_completed == TRUE ~ "Completed",
      cg_missed == TRUE ~ "Missed",
      cg_completed == FALSE ~ "Not completed",
      is.na(cg_completed) ~ "No record",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    globalId,
    statusId,
    birthday,
    age_months,
    birthday_age_window = age_window,
    dashboard_age_window = status_age_window,
    staff,
    cg_status,
    cg_completed,
    cg_completedDate,
    cg_missed,
    cg_missedDate,
    cg_scheduledDate,
    tags
  )

staff_progress <- participant_staff %>%
  group_by(staff) %>%
  summarise(
    total_assigned = n_distinct(globalId),
    completed = n_distinct(globalId[cg_completed == TRUE]),
    not_completed = n_distinct(globalId[cg_completed == FALSE]),
    missed = n_distinct(globalId[cg_missed == TRUE]),
    no_record = n_distinct(globalId[is.na(cg_completed)]),
    completion_rate = completed / total_assigned,
    .groups = "drop"
  ) %>%
  arrange(desc(total_assigned), staff)


staff_progress




# 可选：画每个工作人员完成率
ggplot(staff_progress, aes(x = reorder(staff, completion_rate), y = completion_rate)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Staff",
    y = "Caregiver survey completion rate",
    title = "6-11 Month Caregiver Survey Progress by Staff"
  ) +
  theme_minimal()


# 5. Save the data --------------------------------------------------------

library(openxlsx)

output_folder <- "/Users/tianjiah/Library/CloudStorage/OneDrive-MichiganStateUniversity/Data Manager/followup_dashboard"

output_path <- file.path(
  output_folder,
  "caregiver_survey_staff_progress.xlsx"
)

notes_df <- tibble::tibble(
  Note = c(
    "1. The participant_staff list excludes ECHO 2 Refusal, Potential Participants, and Withdrawn participants.",
    "2. dashboard_age_window is derived from statusId. birthday_age_window is calculated from birthday.",
    "3. Staff assignment is based primarily on tags. If staff tags are missing, staff is assigned based on the age window derived from statusId.",
    "4. staff_progress uses the number of participants assigned to each staff member as the denominator and calculates caregiver survey completion progress."
  )
)

wb <- createWorkbook()

addWorksheet(wb, "participant_staff")
writeData(wb, "participant_staff", participant_staff)

addWorksheet(wb, "staff_progress")
writeData(wb, "staff_progress", staff_progress)

addWorksheet(wb, "Notes")
writeData(wb, "Notes", notes_df)

# Optional: make columns easier to read
setColWidths(wb, "participant_staff", cols = 1:ncol(participant_staff), widths = "auto")
setColWidths(wb, "staff_progress", cols = 1:ncol(staff_progress), widths = "auto")
setColWidths(wb, "Notes", cols = 1:ncol(notes_df), widths = "auto")

saveWorkbook(wb, output_path, overwrite = TRUE)




