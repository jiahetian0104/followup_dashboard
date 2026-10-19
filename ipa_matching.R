# Participant identity and appointment age rules, preserved from the existing pipeline.
normalize_person_name <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    iconv(from = "", to = "ASCII//TRANSLIT") %>%
    # Apostrophes inside a name do not create a new token: A'Mya = Amya.
    str_replace_all("['’`]", "") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    na_if("")
}


normalize_email <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    str_squish() %>%
    na_if("")
}


# Split a person name into normalized first and last tokens. Calendly invitee
# names and Ripple contact names are both free text, so middle names are
# ignored and common generational suffixes are removed before matching.
person_name_parts <- function(full_name) {
  if (is.na(full_name) || str_squish(as.character(full_name)) == "") {
    return(list(first = NA_character_, last = NA_character_))
  }

  cleaned <- as.character(full_name) %>%
    str_squish() %>%
    str_remove(regex("\\s+(Jr|Sr|II|III|IV)\\.?$", ignore_case = TRUE))
  parts <- str_split(cleaned, "\\s+", simplify = FALSE)[[1]]
  parts <- parts[parts != ""]

  if (length(parts) == 0) {
    return(list(first = NA_character_, last = NA_character_))
  }
  if (length(parts) < 2) {
    return(list(
      first = normalize_person_name(parts[[1]] %||% NA_character_),
      last = NA_character_
    ))
  }

  list(
    first = normalize_person_name(parts[[1]]),
    last = normalize_person_name(parts[[length(parts)]])
  )
}


# Create the participant-to-contact relationship needed for Calendly matching.
# Active Participant, Mother, and Alternate names and emails are retained;
# phone and address fields are never imported into the IPA pipeline.
extract_ripple_contact_name_aliases <- function(data) {
  contact_slots <- names(data) %>%
    str_subset("^contact\\.[0-9]+\\.contactName$") %>%
    str_extract("(?<=^contact\\.)[0-9]+(?=\\.contactName$)") %>%
    as.integer() %>%
    unique() %>%
    sort()

  if (length(contact_slots) == 0) {
    return(tibble(
      child_echo_id = character(),
      contact_role = character(),
      contact_name = character(),
      contact_first_key = character(),
      contact_last_key = character(),
      contact_email = character(),
      contact_email_key = character()
    ))
  }

  row_value <- function(row, field) {
    if (!field %in% names(row)) return(NA_character_)
    value <- as.character(row[[field]][[1]]) %>% str_squish()
    if (is.na(value) || value == "") NA_character_ else value
  }

  is_true_text <- function(value) {
    !is.na(value) && str_to_lower(value) %in% c("true", "1", "yes")
  }

  map_dfr(seq_len(nrow(data)), function(row_number) {
    row <- data[row_number, , drop = FALSE]
    child_echo_id <- row_value(row, "globalId") %>%
      str_remove("\\s*\\([^)]*\\)$") %>%
      str_squish()
    if (is.na(child_echo_id) || child_echo_id == "") return(tibble())


    map_dfr(contact_slots, function(slot) {
      prefix <- paste0("contact.", slot, ".")
      archived <- row_value(row, paste0(prefix, "archived"))
      archived_at <- row_value(row, paste0(prefix, "archivedAt"))
      if (is_true_text(archived) || !is.na(archived_at)) return(tibble())

      title <- row_value(row, paste0(prefix, "contactTitle"))
      title_key <- str_to_lower(title %||% "") %>% str_squish()
      contact_role <- case_when(
        title_key == "participant" ~ "Participant",
        title_key %in% c(
          "biological mother", "biological mom", "mother", "mom"
        ) ~ "Mother",
        str_detect(title_key, "^alternate") ~ "Alternate",
        TRUE ~ NA_character_
      )
      if (is.na(contact_role)) return(tibble())

      contact_name <- row_value(row, paste0(prefix, "contactName"))
      if (is.na(contact_name) && contact_role == "Participant") {
        contact_name <- str_c(
          row_value(row, "firstName"), row_value(row, "lastName"),
          sep = " "
        ) %>% str_squish() %>% na_if("")
      }

      name_parts <- person_name_parts(contact_name)
      info_slots <- names(row) %>%
        str_subset(paste0(
          "^contact\\.", slot, "\\.infos\\.[0-9]+\\.contactType$"
        )) %>%
        str_extract(paste0(
          "(?<=^contact\\.", slot, "\\.infos\\.)[0-9]+",
          "(?=\\.contactType$)"
        )) %>%
        as.integer() %>%
        unique() %>%
        sort()

      contact_emails <- map_chr(info_slots, function(info_slot) {
        info_prefix <- paste0(prefix, "infos.", info_slot, ".")
        info_archived <- row_value(row, paste0(info_prefix, "archived"))
        info_archived_at <- row_value(row, paste0(info_prefix, "archivedAt"))
        if (is_true_text(info_archived) || !is.na(info_archived_at)) {
          return(NA_character_)
        }
        contact_type <- row_value(row, paste0(info_prefix, "contactType"))
        if (is.na(contact_type) || str_to_lower(contact_type) != "email") {
          return(NA_character_)
        }
        row_value(row, paste0(info_prefix, "information"))
      }) %>%
        discard(is.na) %>%
        unique()

      if (length(contact_emails) == 0) contact_emails <- NA_character_
      if (
        (is.na(name_parts$first) || is.na(name_parts$last)) &&
          all(is.na(contact_emails))
      ) return(tibble())

      tibble(
        child_echo_id,
        contact_role,
        contact_name,
        contact_first_key = name_parts$first,
        contact_last_key = name_parts$last,
        contact_email = contact_emails,
        contact_email_key = normalize_email(contact_emails)
      )
    })
  }) %>%
    distinct()
}


# Extract all confirmed IDs (customId, familyId, child_echo_id) that appear
# in a free-text note, verified against the catalog. Returns a long tibble
# with one row per matched ID.
extract_note_ids <- function(note, catalog) {
  if (is.na(note) || trimws(note) == "") {
    return(tibble(
      id_type = character(),
      matched_id = character(),
      child_echo_id = character()
    ))
  }
  tokens <- str_extract_all(note, "[A-Za-z0-9]+")[[1]]
  tokens_upper <- toupper(tokens)
  echo_ids <- toupper(str_extract_all(
    note, "[A-Za-z]{2,4}[0-9]{3,4}-[0-9]{2}-[A-Za-z]")[[1]])

  bind_rows(
    catalog %>%
      filter(
        !is.na(child_custom_id) &
          toupper(child_custom_id) %in% tokens_upper
      ) %>%
      transmute(
        id_type = "child_custom_id",
        matched_id = child_custom_id,
        child_echo_id
      ),
    catalog %>%
      filter(!is.na(child_id) & toupper(child_id) %in% tokens_upper) %>%
      transmute(
        id_type = "crosswalk_child_id",
        matched_id = child_id,
        child_echo_id
      ),
    catalog %>%
      filter(!is.na(child_echo_id) & toupper(child_echo_id) %in% echo_ids) %>%
      transmute(id_type = "child_echo_id", matched_id = child_echo_id, child_echo_id),
    catalog %>%
      filter(!is.na(mom_id) & toupper(mom_id) %in% tokens_upper) %>%
      transmute(id_type = "mom_id", matched_id = mom_id, child_echo_id),
    catalog %>%
      filter(!is.na(mom_echo_id) & toupper(mom_echo_id) %in% echo_ids) %>%
      transmute(id_type = "mom_echo_id", matched_id = mom_echo_id, child_echo_id),
    catalog %>%
      filter(
        !is.na(familyId) &
          toupper(as.character(familyId)) %in% tokens_upper
      ) %>%
      transmute(id_type = "familyId", matched_id = as.character(familyId), child_echo_id)
  ) %>%
    distinct()
}


# Match one Calendly row to participants. Explicit child ECHO/custom IDs in
# meeting notes are tried first and may return multiple children. A mother or
# family ID is only used together with the child's full name. When no usable
# ID is found, a unique full-name match across the catalog is the fallback.
match_cal_row_to_cohort <- function(
    note,
    cal_fn,
    cal_ln,
    invitee_fn,
    invitee_ln,
    invitee_email,
    catalog
) {
  empty <- tibble(
    res_fn = NA_character_,
    res_ln = NA_character_,
    matched_by = NA_character_,
    matched_child_echo_id = NA_character_,
    matched_custom_id = NA_character_,
    matched_family_id = NA_character_,
    matched_birthday = NA_character_,
    matched_cohort = NA_character_,
    participant_match_qa = NA_character_
  )
  if (nrow(catalog) == 0) {
    return(empty)
  }

  # Older cohort-specific calls do not contain the Crosswalk aliases. Add
  # empty versions so the same matching function remains reusable.
  if (!"child_custom_id" %in% names(catalog)) {
    catalog$child_custom_id <- as.character(catalog$customId)
  }
  if (!"mom_id" %in% names(catalog)) catalog$mom_id <- NA_character_
  if (!"mom_echo_id" %in% names(catalog)) catalog$mom_echo_id <- NA_character_
  if (!"child_id" %in% names(catalog)) catalog$child_id <- NA_character_
  if (!"contact_role" %in% names(catalog)) catalog$contact_role <- NA_character_
  if (!"contact_first_key" %in% names(catalog)) {
    catalog$contact_first_key <- NA_character_
  }
  if (!"contact_last_key" %in% names(catalog)) {
    catalog$contact_last_key <- NA_character_
  }
  if (!"contact_email_key" %in% names(catalog)) {
    catalog$contact_email_key <- NA_character_
  }

  catalog <- catalog %>%
    mutate(across(
      any_of(c(
        "child_echo_id", "child_custom_id", "customId", "familyId",
        "child_id", "mom_id", "mom_echo_id"
      )),
      as.character
    ))

  format_matches <- function(candidates, method) {
    candidates %>%
      filter(!is.na(child_echo_id)) %>%
      distinct(child_echo_id, .keep_all = TRUE) %>%
      transmute(
        res_fn = as.character(firstName),
        res_ln = as.character(lastName),
        matched_by = method,
        matched_child_echo_id = as.character(child_echo_id),
        matched_custom_id = as.character(child_custom_id),
        matched_family_id = as.character(familyId),
        matched_birthday = as.character(birthday),
        matched_cohort = as.character(cohort),
        participant_match_qa = NA_character_
      )
  }

  hits <- extract_note_ids(note, catalog)

  if (nrow(hits) > 0) {
    # Explicit child IDs may intentionally return multiple children when
    # several IDs are written in one note. Mother/family IDs only narrow the
    # family and must be disambiguated by the child's normalized full name.
    for (type in c(
      "child_echo_id", "child_custom_id", "crosswalk_child_id",
      "mom_id", "mom_echo_id", "familyId"
    )) {
      ids <- unique(hits$matched_id[hits$id_type == type])
      if (length(ids) == 0) next
      if (type == "child_echo_id") {
        cand <- catalog %>% filter(!is.na(child_echo_id) & child_echo_id %in% ids)
      } else if (type == "child_custom_id") {
        cand <- catalog %>%
          filter(!is.na(child_custom_id) & child_custom_id %in% ids)
      } else if (type == "crosswalk_child_id") {
        cand <- catalog %>% filter(!is.na(child_id) & child_id %in% ids)
      } else if (type == "mom_id") {
        cand <- catalog %>% filter(!is.na(mom_id) & mom_id %in% ids)
      } else if (type == "mom_echo_id") {
        cand <- catalog %>%
          filter(!is.na(mom_echo_id) & mom_echo_id %in% ids)
      } else {
        cand <- catalog %>%
          filter(!is.na(familyId) & as.character(familyId) %in% ids)
      }
      if (nrow(cand) == 0) next

      if (type %in% c("mom_id", "mom_echo_id", "familyId")) {
        # Parent/family identifiers never identify a child by themselves.
        # Require the Calendly child name and never expand all siblings.
        by_name <- cand %>%
          filter(
            normalize_person_name(firstName) == cal_fn,
            normalize_person_name(lastName) == cal_ln
          ) %>%
          distinct(child_echo_id, .keep_all = TRUE)
        if (nrow(by_name) == 1) {
          return(format_matches(by_name, paste0(type, "+name")))
        }
      } else {
        matched <- format_matches(cand, type)
        if (nrow(matched) > 0) {
          return(matched)
        }
      }
    }
  }

  hit <- catalog %>%
    filter(
      normalize_person_name(firstName) == cal_fn,
      normalize_person_name(lastName) == cal_ln
    ) %>%
    distinct(child_echo_id, .keep_all = TRUE)
  if (nrow(hit) == 1) {
    return(format_matches(hit, "child_name"))
  }

  invitee_email <- normalize_email(invitee_email)
  invitee_email_hit <- catalog %>%
    filter(
      !is.na(invitee_email),
      !is.na(contact_email_key),
      contact_email_key == invitee_email
    ) %>%
    distinct(child_echo_id, .keep_all = TRUE)

  # A shared family email cannot identify one child without child information.
  # Shared family contacts require child information to resolve siblings.
  if (nrow(invitee_email_hit) > 1 && is.na(cal_fn)) {
    return(empty %>% mutate(
      participant_match_qa =
        "Invitee email maps to multiple participants and child name is missing"
    ))
  }

  # When the booking did not supply a usable child ID, the invitee may be the
  # participant. Require a unique full-name match and honor the shared-email
  # safeguard above.
  if (!is.na(invitee_fn) && !is.na(invitee_ln)) {
    invitee_child_hit <- catalog %>%
      filter(
        normalize_person_name(firstName) == invitee_fn,
        normalize_person_name(lastName) == invitee_ln
      ) %>%
      distinct(child_echo_id, .keep_all = TRUE)
    if (nrow(invitee_child_hit) == 1) {
      return(format_matches(invitee_child_hit, "invitee_child_name"))
    }
  }

  # Email is stable when a contact's surname changes. If the email belongs to
  # siblings, the child first name from the booking question may safely select
  # one child within that already identified family.
  if (nrow(invitee_email_hit) > 0) {
    email_child_hit <- invitee_email_hit
    if (nrow(email_child_hit) > 1 && !is.na(cal_fn)) {
      email_child_hit <- email_child_hit %>%
        filter(normalize_person_name(firstName) == cal_fn) %>%
        distinct(child_echo_id, .keep_all = TRUE)
    }
    if (nrow(email_child_hit) == 1) {
      match_method <- if_else(
        nrow(invitee_email_hit) > 1,
        "invitee_email+child_first_name",
        "invitee_email"
      )
      return(format_matches(email_child_hit, match_method))
    }
    return(empty %>% mutate(
      participant_match_qa =
        "Invitee email maps to multiple participants; child first name did not resolve one"
    ))
  }

  if (!is.na(invitee_fn) && !is.na(invitee_ln)) {
    # Otherwise the invitee may be a Participant, Mother, or Alternate contact
    # stored in Ripple. Within a shared family contact, child first name may
    # disambiguate siblings even when the supplied child surname has changed.
    invitee_contact_hit <- catalog %>%
      filter(
        !is.na(contact_role),
        contact_first_key == invitee_fn,
        contact_last_key == invitee_ln
      ) %>%
      arrange(factor(
        contact_role,
        levels = c("Participant", "Mother", "Alternate")
      )) %>%
      distinct(child_echo_id, .keep_all = TRUE)
    original_contact_hit_count <- nrow(invitee_contact_hit)
    if (nrow(invitee_contact_hit) > 1 && !is.na(cal_fn)) {
      invitee_contact_hit <- invitee_contact_hit %>%
        filter(normalize_person_name(firstName) == cal_fn) %>%
        distinct(child_echo_id, .keep_all = TRUE)
    }
    if (nrow(invitee_contact_hit) == 1) {
      match_method <- paste0(
        "invitee_",
        str_to_lower(invitee_contact_hit$contact_role[[1]]),
        "_name",
        if_else(
          original_contact_hit_count > 1,
          "+child_first_name",
          ""
        )
      )
      return(format_matches(invitee_contact_hit, match_method))
    }
    if (nrow(invitee_contact_hit) > 1) {
      return(empty %>% mutate(
        participant_match_qa =
          "Invitee contact name maps to multiple participants; child first name did not resolve one"
      ))
    }
  }

  empty
}


# Resolve every Calendly row against a participant catalog and expand rows
# when one note explicitly contains multiple child IDs.
resolve_calendly_cohort <- function(cal_data, catalog) {
  cal_data %>%
    mutate(
      cal_fn = normalize_person_name(`Child First Name`),
      cal_ln = normalize_person_name(`Child Last Name`),
      invitee_name_parts = map(`Invitee Name`, person_name_parts),
      invitee_fn = map_chr(invitee_name_parts, "first"),
      invitee_ln = map_chr(invitee_name_parts, "last"),
      invitee_email = normalize_email(`Invitee Email`)
    ) %>%
    rowwise() %>%
    mutate(res = list(match_cal_row_to_cohort(
      `Meeting Notes Plain`, cal_fn, cal_ln, invitee_fn, invitee_ln,
      invitee_email, catalog
    ))) %>%
    ungroup() %>%
    unnest(res) %>%
    mutate(
      join_fn = coalesce(res_fn, `Child First Name`),
      join_ln = coalesce(res_ln, `Child Last Name`)
    ) %>%
    select(
      -cal_fn, -cal_ln, -invitee_name_parts, -invitee_fn, -invitee_ln,
      -invitee_email,
      -res_fn, -res_ln
    )
}



get_booking_answer <- function(questions_and_answers, question_pattern) {
  for (qa in questions_and_answers) {
    if (grepl(question_pattern, trimws(qa$question), ignore.case = TRUE)) {
      return(qa$answer)
    }
  }
  NA_character_
}


# Split a free-text child name answer into first and last name: the first
# token is kept as the first name and the last token as the last name.
# Trailing generational suffixes (Jr, Sr, II, III, IV) are dropped first.
# A single-token answer is kept as the first name with an NA last name.
parse_child_name <- function(child_name) {
  if (is.na(child_name)) {
    return(list(first = NA_character_, last = NA_character_))
  }
  
  name <- gsub("\\s+", " ", trimws(child_name))
  name <- gsub("\\s+(Jr|Sr|II|III|IV)\\.?$", "", name, ignore.case = TRUE)
  
  parts <- strsplit(name, " ", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  
  if (length(parts) >= 2) {
    list(
      first = parts[1],
      last = parts[length(parts)]
    )
  } else if (length(parts) == 1) {
    list(
      first = parts[1],
      last = NA_character_
    )
  } else {
    list(
      first = NA_character_,
      last = NA_character_
    )
  }
}


# Name of the Calendly user who owns/hosts the scheduled event (the staff
# member who set up the invite). Multiple members are joined with "; ".
get_event_host_name <- function(event) {
  host_names <- sapply(
    event$event_memberships,
    function(m) m$user_name
  )
  if (length(host_names) == 0) {
    NA_character_
  } else {
    paste(host_names, collapse = "; ")
  }
}


# Convert the Calendly host/inviter name into the stable short FTM labels used
# by the dashboard. The patterns cover the full Calendly display names as well
# as existing short labels, so one staff member is not split across two rows.
# An unrecognized host is retained as written for review.
ipa_ftm_name_patterns <- c(
  Amy = "\\bAmy\\b",
  Andrew = "\\bAndrew\\b",
  Anna = "\\bAnna\\b",
  Breanna = "\\bBreanna\\b",
  Cassie = "\\bCassie\\b",
  Jo = "\\b(?:Jo|Jolyn)\\b",
  Jody = "\\bJody\\b",
  Kowlini = "\\bKowlini\\b",
  Nicole = "\\bNicole\\b",
  Toye = "\\b(?:Toye|Shetoye)\\b"
)

normalize_calendly_ftm <- function(host_name) {
  if (is.na(host_name) || str_squish(host_name) == "") {
    return(NA_character_)
  }

  host_members <- str_split(host_name, ";", simplify = FALSE)[[1]] %>%
    str_squish() %>%
    discard(~ .x == "")

  normalized_members <- map_chr(host_members, function(member) {
    known_hits <- names(ipa_ftm_name_patterns)[
      map_lgl(
        ipa_ftm_name_patterns,
        ~ str_detect(member, regex(.x, ignore_case = TRUE))
      )
    ]

    if (length(known_hits) == 1) {
      known_hits[[1]]
    } else {
      member
    }
  })

  paste(unique(normalized_members), collapse = "; ") %>% na_if("")
}


parse_child_age_months <- function(age_text, reference_months = NA_real_,
                                   cohort = NA_character_) {
  if (is.na(age_text) || str_squish(age_text) == "") {
    return(NA_real_)
  }

  age <- age_text %>%
    str_to_lower() %>%
    str_replace_all("one and a half", "1.5") %>%
    str_replace_all("\\bone\\b", "1") %>%
    str_replace_all("'", "") %>%
    str_squish()

  year_match <- str_match(
    age,
    "([0-9]+(?:\\.[0-9]+)?)\\s*(?:years?|yrs?|yr|y)\\b"
  )[, 2]
  month_match <- str_match(
    age,
    "([0-9]+(?:\\.[0-9]+)?)\\s*(?:months?|mos?|mo|mths?)\\b"
  )[, 2]

  years <- suppressWarnings(as.numeric(year_match))
  months <- suppressWarnings(as.numeric(month_match))

  if (!is.na(years) || !is.na(months)) {
    return(coalesce(years, 0) * 12 + coalesce(months, 0))
  }

  value <- suppressWarnings(parse_number(age, na = c("", "NA")))
  if (is.na(value)) {
    return(NA_real_)
  }

  # Values above 20 cannot be study age in years and are treated as months.
  if (value > 20) {
    return(value)
  }

  month_candidate <- value
  year_candidate <- value * 12

  if (!is.na(reference_months)) {
    candidates <- c(month_candidate, year_candidate)
    return(candidates[which.min(abs(candidates - reference_months))])
  }

  if (!is.na(cohort) && cohort == "6-35 month") {
    return(month_candidate)
  }
  if (!is.na(cohort) && cohort == "3-20 year") {
    return(year_candidate)
  }

  # Without participant context, decimals and values up to 20 are most
  # plausibly years in this study; retain a QA flag downstream.
  year_candidate
}


parse_calendly_datetime <- function(x) {
  parse_date_time(
    x,
    orders = c("ymd HMSz", "ymd HMS", "mdy HMS", "mdy HM"),
    tz = "UTC",
    quiet = TRUE
  )
}


parse_participant_birthday <- function(x) {
  as_date(parse_date_time(
    x,
    orders = c("ymd", "mdy", "dmy"),
    quiet = TRUE
  ))
}


calculate_age_months <- function(birthday, appointment_datetime) {
  if (is.na(birthday) || is.na(appointment_datetime) ||
      as_date(appointment_datetime) < birthday) {
    return(NA_real_)
  }
  floor(time_length(
    interval(birthday, as_date(appointment_datetime)),
    unit = "month"
  ))
}


assign_ipa_age_band <- function(age_months) {
  case_when(
    age_months >= 6 & age_months < 12 ~ "6_11_month",
    age_months >= 12 & age_months < 24 ~ "12_23_month",
    age_months >= 24 & age_months < 36 ~ "24_35_month",
    age_months >= 36 & age_months < 72 ~ "3_5yr",
    age_months >= 72 & age_months < 132 ~ "6_10yr",
    age_months >= 132 & age_months < 216 ~ "11_17yr",
    age_months >= 216 & age_months < 252 ~ "18_20yr",
    TRUE ~ NA_character_
  )
}

