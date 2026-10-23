# RQ2c: multiple responses, including an explicit "None of the above".
practice_summary <- daily_public %>% select(read, research, engage, practice_none) %>%
  pivot_longer(everything(), names_to = "Practice", values_to = "Value") %>%
  group_by(Practice) %>% summarise(Yes = sum(Value == 1, na.rm = TRUE),
    Valid_N = sum(!is.na(Value)), Missing = sum(is.na(Value)),
    Percent = 100 * mean(Value, na.rm = TRUE), .groups = "drop")
practice_person <- daily_public %>% select(participant, read, research, engage, practice_none) %>%
  pivot_longer(-participant, names_to = "Practice", values_to = "Value") %>%
  filter(!is.na(Value)) %>% group_by(participant, Practice) %>%
  summarise(Percent = 100 * mean(Value), .groups = "drop")
practice_person_summary <- practice_person %>% group_by(Practice) %>%
  summarise(N = n(), Percent = mean(Percent), .groups = "drop")

# Exploratory link between RQ2b and RQ2c. Descriptive groups can overlap and differ.
practice_route_long <- daily_public %>% select(participant, encounter, read, research, engage, practice_none) %>%
  filter(!is.na(encounter)) %>% pivot_longer(c(read, research, engage, practice_none),
    names_to = "Practice", values_to = "Value")
practices_by_route <- practice_route_long %>% group_by(encounter, Practice) %>%
  summarise(Valid_posts = sum(!is.na(Value)), Missing = sum(is.na(Value)),
    Post_percent = 100 * mean(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(practice_route_long %>% filter(!is.na(Value)) %>%
    group_by(participant, encounter, Practice) %>% summarise(Share = mean(Value), .groups = "drop") %>%
    group_by(encounter, Practice) %>% summarise(Participants = n(),
      Person_percent = 100 * mean(Share), .groups = "drop"), by = c("encounter", "Practice"))
