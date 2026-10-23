# Methods: read the original exports; never open participant images.
questionnaire_files <- c(intro = "01_Data/initial_survey.csv",
  daily = "01_Data/daily_survey.csv", outro = "01_Data/closing_survey.csv")
questionnaires <- map(questionnaire_files, ~ read_delim(.x, delim = ";",
  col_types = cols(.default = "c"), trim_ws = TRUE, show_col_types = FALSE) %>%
  mutate(participant = clean_text(personalParticipantCode)))
intro_raw <- questionnaires$intro
upload_raw <- questionnaires$daily
outro_raw <- questionnaires$outro
stopifnot(!anyNA(intro_raw$participant), !anyDuplicated(intro_raw$participant),
  !anyNA(outro_raw$participant), !anyDuplicated(outro_raw$participant))

# Local input lists retain the original filenames and their documented exclusions.
technical_exclusions <- read_csv("06_Coding/upload_exclusions.csv", show_col_types = FALSE)
duplicate_uploads <- read_csv("06_Coding/duplicate_uploads.csv", show_col_types = FALSE)
upload_records <- upload_raw %>%
  select(participant, scheduled, matches("^daily_[0-9]+_screenshot$")) %>%
  pivot_longer(matches("^daily_[0-9]+_screenshot$"), values_to = "filename") %>%
  mutate(filename = clean_text(filename)) %>% filter(!is.na(filename)) %>%
  distinct(participant, filename, .keep_all = TRUE) %>%
  anti_join(technical_exclusions, by = c("participant", "filename")) %>%
  anti_join(duplicate_uploads, by = c("participant", "filename"))

# Preregistered seven-upload threshold; no minimum number of upload days.
sample_audit <- intro_raw %>%
  transmute(participant, intro_complete = !is.na(clean_text(committed)),
    eligible = str_to_lower(str_squish(intro_stop_age)) %in% c("true", "t", "1", "yes", "y", "ja", "j") &
      str_to_lower(str_squish(intro_stop_usage)) %in% c("true", "t", "1", "yes", "y", "ja", "j")) %>%
  left_join(count(upload_records, participant, name = "n_uploads"), by = "participant") %>%
  left_join(outro_raw %>% transmute(participant,
    outro_complete = !is.na(clean_text(committed))), by = "participant") %>%
  mutate(n_uploads = replace_na(n_uploads, 0L),
    outro_complete = replace_na(outro_complete, FALSE),
    included = eligible & intro_complete & outro_complete & n_uploads >= 7,
    Exclusion = case_when(!eligible ~ "Screening criteria not met",
      !intro_complete ~ "Initial survey incomplete", !outro_complete ~ "Closing survey incomplete",
      n_uploads < 7 ~ "Fewer than seven distinct uploads", TRUE ~ "Included"))
included_ids <- sample_audit %>% filter(included) %>% pull(participant)
stopifnot(length(included_ids) > 0)

daily_raw <- read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Coding") %>%
  mutate(across(c(participant, screenshot_id), clean_text),
    across(c(public_rel_coded, advertisement_coded, topic_coded, source_coded,
      media_format, incidentality_code, locality_code, situation_code), as.numeric))
codebook <- read.xlsx("06_Coding/coding_sheet.xlsx", sheet = "Codebook")
expected_codes <- list(public_rel_coded = c(1, 0, 99, 98, 97),
  advertisement_coded = c(1, 0, 99), topic_coded = c(1:14, 99),
  source_coded = c(1:10, 99), media_format = c(1:3, 99))
for (variable in names(expected_codes)) {
  codes <- codebook %>% filter(Variable == variable, str_detect(as.character(Code), "^[0-9]+$"))
  stopifnot(!anyDuplicated(codes$Code), !anyNA(codes$Category),
    setequal(as.numeric(codes$Code), expected_codes[[variable]]),
    all(na.omit(daily_raw[[variable]]) %in% expected_codes[[variable]]))
}
stopifnot(!anyNA(daily_raw$participant), !anyNA(daily_raw$screenshot_id),
  !anyDuplicated(daily_raw$screenshot_id))
marked_duplicates <- str_detect(str_to_lower(daily_raw$notes), "duplikat|duplicate|doppelt") &
  !str_detect(str_to_lower(daily_raw$notes), "doppelt.*teilgenommen|doppelte.*teilnahme")
stopifnot(!any(marked_duplicates, na.rm = TRUE))

intro <- intro_raw %>% filter(participant %in% included_ids) %>%
  mutate(age = na_if(as.numeric(intro_age_num), -1),
    gender_code = as.numeric(intro_gender), education_code = na_if(as.numeric(intro_education), -1))
outro <- outro_raw %>% filter(participant %in% included_ids)
daily <- daily_raw %>% filter(participant %in% included_ids)
daily_public <- daily %>% filter(public_rel_coded == 1) %>% decode_practices() %>%
  mutate(platform = coalesce(clean_text(platform_coded), clean_text(platform_reported)),
    encounter = factor(incidentality_code, levels = 1:3,
      labels = c("Deliberate search", "Followed, no search", "Chance")))
stopifnot(nrow(daily_public) > 0, all(is.na(intro$age) | intro$age >= 60),
  all(na.omit(intro$gender_code) %in% 1:3), all(na.omit(intro$education_code) %in% 1:8),
  setequal(included_ids, unique(daily$participant)))
for (variable in c("incidentality_code", "locality_code", "situation_code")) {
  stopifnot(all(na.omit(daily_public[[variable]]) %in% 1:3))
}

sample_flow <- tibble(Stage = c("Initial survey records", "Screening criteria met",
  "Screening met and both surveys complete", "At least seven distinct uploads"),
  N = c(nrow(sample_audit), sum(sample_audit$eligible),
    sum(with(sample_audit, eligible & intro_complete & outro_complete)), length(included_ids)))
sample_age <- describe_numeric(intro$age, "Age (years)")
sample_gender <- frequency_table(factor(intro$gender_code, 1:3, c("Female", "Male", "Diverse")))
sample_education <- frequency_table(factor(intro$education_code, 1:8,
  c("No qualification", "Lower secondary school", "Intermediate secondary school",
    "Polytechnic secondary school", "University of applied sciences entrance qualification",
    "University entrance qualification", "University degree", "Other qualification")))
participation <- upload_records %>% filter(participant %in% included_ids) %>%
  group_by(participant) %>% summarise(Uploads = n(),
    Questionnaire_days = n_distinct(substr(scheduled, 1, 10), na.rm = TRUE), .groups = "drop")
participation_summary <- bind_rows(describe_numeric(participation$Uploads, "Distinct uploads per person"),
  describe_numeric(participation$Questionnaire_days, "Scheduled questionnaire days with uploads"))
pretest_reliability <- read.xlsx("07_Pretest/pretest_reliability.xlsx", sheet = "Reliability") %>%
  transmute(Variable = variable, Assessable = n_pairable_units, Complete = n_complete_units,
    Alpha = kripp_alpha_nominal, CI_lower = ci95_low, CI_upper = ci95_high)
