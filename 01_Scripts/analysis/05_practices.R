# RQ2c: selection and engagement practices --------------------------------------
# Responses can include several practices. "None of the above" is exclusive.

# 1. Describe practices across all valid posts ---------------------------------

practice_long <- daily_public %>%
  select(participant, read, research, engage, practice_none) %>%
  pivot_longer(
    -participant,
    names_to = "Practice",
    values_to = "Value"
  )

practice_summary <- practice_long %>%
  group_by(Practice) %>%
  summarise(
    Yes = sum(Value == 1, na.rm = TRUE),
    Valid_N = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Percent = 100 * mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Give each person equal weight --------------------------------------------
# First calculate each person's share, then average these shares.

practice_person <- practice_long %>%
  filter(!is.na(Value)) %>%
  group_by(participant, Practice) %>%
  summarise(Percent = 100 * mean(Value), .groups = "drop")

practice_person_summary <- practice_person %>%
  group_by(Practice) %>%
  summarise(
    N = n(),
    Percent = mean(Percent),
    .groups = "drop"
  )

# 3. Compare practices across encounter routes ---------------------------------
# Exploratory link between RQ2b and RQ2c. Groups can overlap and differ in size.

practice_route_long <- daily_public %>%
  select(participant, encounter, read, research, engage, practice_none) %>%
  filter(!is.na(encounter)) %>%
  pivot_longer(
    c(read, research, engage, practice_none),
    names_to = "Practice",
    values_to = "Value"
  )

practice_post_routes <- practice_route_long %>%
  group_by(encounter, Practice) %>%
  summarise(
    Valid_posts = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Post_percent = 100 * mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

# A person without valid posts for a route contributes no share for that route.
practice_person_routes <- practice_route_long %>%
  filter(!is.na(Value)) %>%
  group_by(participant, encounter, Practice) %>%
  summarise(Share = mean(Value), .groups = "drop") %>%
  group_by(encounter, Practice) %>%
  summarise(
    Participants = n(),
    Person_percent = 100 * mean(Share),
    .groups = "drop"
  )

practices_by_route <- practice_post_routes %>%
  left_join(practice_person_routes, by = c("encounter", "Practice"))
