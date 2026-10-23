# RQ1: descriptive content distributions; residual codes stay in the denominator.
topic_names <- c("Politics, state and elections", "International affairs, war and security",
  "Economy, work, finance and consumers", "Society, social affairs, migration and religion",
  "Education, science and technology", "Health and care", "Climate, environment and energy",
  "Crime, accidents and justice", "Transport, infrastructure and housing", "Weather and natural events",
  "Culture, media and entertainment", "History and remembrance", "Sport", "Events and public service",
  "Other, mixed or unclear topic")
names(topic_names) <- c(1:14, 99)
source_names <- c("Journalistic media outlet", "Alternative or partisan media outlet",
  "Political party or politician", "State or public institution", "NGO, association, initiative or movement",
  "Science, expert or fact-checker", "Company or brand", "Individual journalist",
  "Creator, influencer or other public figure", "Private individual", "Other or unidentifiable account")
names(source_names) <- c(1:10, 99)
format_names <- c("Text/link-based", "Static visual", "Moving audiovisual", "Not determinable")
names(format_names) <- c(1:3, 99)
daily_public <- daily_public %>% mutate(
  topic = factor(topic_coded, names(topic_names), topic_names),
  source = factor(source_coded, names(source_names), source_names),
  format = factor(media_format, names(format_names), format_names))
relevance <- frequency_table(factor(daily$public_rel_coded, c(1, 0, 99, 98, 97),
  c("Publicly relevant", "Not publicly relevant", "Unclear relevance",
    "Not a social media post", "Technically unreadable")))
topics <- frequency_table(daily_public$topic)
sources <- frequency_table(daily_public$source)
formats <- frequency_table(daily_public$format)
platforms <- daily_public %>% group_by(platform) %>%
  summarise(Posts = n(), Participants = n_distinct(participant), .groups = "drop") %>%
  mutate(Percent = 100 * Posts / sum(Posts))
advertising <- frequency_table(factor(daily_public$advertisement_coded, c(1, 0, 99),
  c("Advertising", "No advertising", "Unclear")))

# Tables A1-A2: exploratory RQ1 cross-tabulation; percentages are within topic.
topic_source <- daily_public %>% filter(!is.na(topic), !is.na(source)) %>%
  count(topic, source, name = "n", .drop = FALSE) %>% group_by(topic) %>%
  mutate(Topic_N = sum(n), Percent = if_else(Topic_N > 0, 100 * n / Topic_N, NA_real_)) %>% ungroup()
topic_source_wide <- topic_source %>% mutate(Cell = sprintf("%s (%.1f%%)", n, Percent)) %>%
  select(Topic = topic, Topic_N, source, Cell) %>% pivot_wider(names_from = source, values_from = Cell)
table_a1 <- topic_source_wide %>% select(Topic, Topic_N, all_of(unname(source_names[1:5])))
table_a2 <- topic_source_wide %>% select(Topic, Topic_N, all_of(unname(source_names[6:11])))
