# RQ1: content distributions ----------------------------------------------------

# 1. Attach English labels to the original coding categories -------------------
# Residual codes remain valid categories, including when their count is zero.

topic_names <- c(
  `1` = "Politics, state and elections",
  `2` = "International affairs, war and security",
  `3` = "Economy, work, finance and consumers",
  `4` = "Society, social affairs, migration and religion",
  `5` = "Education, science and technology",
  `6` = "Health and care",
  `7` = "Climate, environment and energy",
  `8` = "Crime, accidents and justice",
  `9` = "Transport, infrastructure and housing",
  `10` = "Weather and natural events",
  `11` = "Culture, media and entertainment",
  `12` = "History and remembrance",
  `13` = "Sport",
  `14` = "Events and public service",
  `99` = "Other, mixed or unclear topic"
)

source_names <- c(
  `1` = "Journalistic media outlet",
  `2` = "Alternative or partisan media outlet",
  `3` = "Political party or politician",
  `4` = "State or public institution",
  `5` = "NGO, association, initiative or movement",
  `6` = "Science, expert or fact-checker",
  `7` = "Company or brand",
  `8` = "Individual journalist",
  `9` = "Creator, influencer or other public figure",
  `10` = "Private individual",
  `99` = "Other or unidentifiable account"
)

format_names <- c(
  `1` = "Text/link-based",
  `2` = "Static visual",
  `3` = "Moving audiovisual",
  `99` = "Not determinable"
)

daily_public <- daily_public %>%
  mutate(
    topic = factor(topic_coded, levels = names(topic_names), labels = topic_names),
    source = factor(source_coded, levels = names(source_names), labels = source_names),
    format = factor(media_format, levels = names(format_names), labels = format_names)
  )

# 2. Describe the corpus and its content ----------------------------------------

relevance <- frequency_table(factor(
  daily$public_rel_coded,
  levels = c(1, 0, 99, 98, 97),
  labels = c(
    "Publicly relevant",
    "Not publicly relevant",
    "Unclear relevance",
    "Not a social media post",
    "Technically unreadable"
  )
))

topics <- frequency_table(daily_public$topic)
sources <- frequency_table(daily_public$source)
formats <- frequency_table(daily_public$format)

# Participants can contribute on several platforms; counts therefore overlap.
platforms <- daily_public %>%
  group_by(platform) %>%
  summarise(
    Posts = n(),
    Participants = n_distinct(participant),
    .groups = "drop"
  ) %>%
  mutate(Percent = 100 * Posts / sum(Posts))

advertising <- frequency_table(factor(
  daily_public$advertisement_coded,
  levels = c(1, 0, 99),
  labels = c("Advertising", "No advertising", "Unclear")
))

# 3. Tables A1-A2: sources within each topic ------------------------------------
# Exploratory RQ1 description. Both tables share the same topic denominator.

topic_source <- daily_public %>%
  filter(!is.na(topic), !is.na(source)) %>%
  count(topic, source, name = "n", .drop = FALSE) %>%
  group_by(topic) %>%
  mutate(
    Topic_N = sum(n),
    Percent = if_else(Topic_N > 0, 100 * n / Topic_N, NA_real_)
  ) %>%
  ungroup()

topic_source_wide <- topic_source %>%
  mutate(Cell = sprintf("%s (%.1f%%)", n, Percent)) %>%
  select(Topic = topic, Topic_N, source, Cell) %>%
  pivot_wider(names_from = source, values_from = Cell)

table_a1 <- topic_source_wide %>%
  select(Topic, Topic_N, all_of(unname(source_names[1:5])))

table_a2 <- topic_source_wide %>%
  select(Topic, Topic_N, all_of(unname(source_names[6:11])))
