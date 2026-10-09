# ------------------------------------------------------------------------------
# Data
# ------------------------------------------------------------------------------

# ----- Select and Filter data
descriptive_data <- trial_data %>% filter(participant %in% participant_list) %>% filter(setting == 'save') 

descriptive_data <- descriptive_data %>% mutate(checkpoint = checkpoint/4) %>% select(participant, trialNumber, pipesPassed, checkpoint, attemptIndex, condition, difficulty, completed, usedEasierSettings) 

descriptive_data <- descriptive_data %>%
  mutate(uniqueId = paste0(participant, '_', trialNumber,  '_', attemptIndex),
         pipes_end_index = (checkpoint*4) + pipesPassed
  ) %>% relocate(participant, uniqueId, trialNumber, attemptIndex)

descriptive_data <- descriptive_data %>%
  mutate(
    levelComplete = case_when(
      condition == "ShortCond" & pipes_end_index > 12 ~ "complete",
      condition == "ShortCond" & pipes_end_index <= 12 ~ "error",
      condition == "LongCond"  & pipes_end_index > 24 ~ "complete",
      condition == "LongCond"  & pipes_end_index <= 24 ~ "error",
      TRUE ~ NA_character_
    ),
    pipes_fail_index = if_else(levelComplete == "complete", NA_real_, pipes_end_index + 1)
  )

descriptive_data <- descriptive_data %>% relocate(participant:attemptIndex, condition:difficulty, checkpoint, pipesPassed, pipes_end_index, levelComplete, pipes_fail_index)

descriptive_data <- descriptive_data %>% mutate(end_cp = floor((pipes_end_index)/4))

descriptive_data <- descriptive_data %>% mutate(group_skill = ifelse(participant %in% skilled_participants, 'Skilled', 'unSkilled'))

# ------------------------------------------------------------------------------
# Descriptive plots
# ------------------------------------------------------------------------------

# ----- Number of attempt end at each CP (How many times participant end at the different checkpoints, including the end) ----
descriptive_data %>%
  group_by(participant, group_skill, condition, end_cp) %>%
  summarise(
    n_reached = n(),
    .groups = "drop"
  ) %>% group_by(condition, end_cp, group_skill) %>%
  summarise(
    mean_reached = mean(n_reached, na.rm = TRUE),
    sd_reached = sd(n_reached, na.rm = TRUE),
    .groups = "drop"
  ) %>% group_by(condition, group_skill) %>%
  complete(
    end_cp = 0:6,
    fill = list(mean_reached = 0)
  ) %>% ungroup %>% ggplot(aes(x = end_cp, y = mean_reached, fill = condition)) +
  facet_wrap(group_skill~.) +
  geom_errorbar(aes(ymin = mean_reached, ymax=mean_reached+sd_reached, group = condition),  position = position_dodge(-0.5), width = 0.4) +
  geom_bar(stat = 'identity', position = position_dodge(-0.5), color = '#533846') +
  labs(
    # title = "Avg. Number of attempts ending at each CP for both conditions",
    x = "CP (crash or complete)",
    y = expression("Avg." ~ italic(n) ~ "of attempts"),
    fill = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c('#E93609', '#79CB6C'), labels = c('Long (6 cp)', 'Short (3 cp)')) +
  scale_x_continuous(breaks= 0:6, labels = c('Start', paste0('CP', 1:2), 'End | CP3', paste0('CP', 4:5), 'End')) +
  theme(legend.position = c(0.9, 0.83),
  )

# ----- Number of GUs ----
game_data_filtered %>% filter(response == "easier") %>%
  mutate(end_cp = ifelse(condition == "LongCond", 6 - end_distance_to_goal, 3 - end_distance_to_goal)) %>%
  group_by(subject_nr, group_skill, condition, end_cp) %>%
  summarise(
    n_gu = n(),
    .groups = "drop"
  ) %>% group_by(condition, end_cp, group_skill) %>%
  summarise(
    mean_gu = mean(n_gu, na.rm = TRUE),
    sd_gu = sd(n_gu, na.rm = TRUE),
    .groups = "drop"
  ) %>% group_by(condition, group_skill) %>%
  complete(
    end_cp = 0:6,
    fill = list(mean_gu = 0)
  ) %>% ungroup %>% ggplot(aes(x = end_cp, y = mean_gu, fill = condition)) +
  facet_wrap(group_skill~.) +
  geom_errorbar(aes(ymin = mean_gu, ymax=mean_gu+sd_gu, group = condition),  position = position_dodge(-0.5), width = 0.4) +
  geom_bar(stat = 'identity', position = position_dodge(-0.5), color = '#533846', show.legend = FALSE) +
  labs(
    x = "CP",
    y = expression("Avg." ~ italic(n) ~ "of GU"),
    fill = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c('#E93609', '#79CB6C'), labels = c('Long (6 cp)', 'Short (3 cp)')) +
  scale_x_continuous(breaks= 0:6, labels = c('Start', paste0('CP', 1:2), 'End | CP3', paste0('CP', 4:5), 'End')) +
  theme(legend.position = c(0.9, 0.83),
  )

# ----- Who reached CP ? -----
descriptive_data %>%
  group_by(participant, group_skill, condition) %>%
  summarise(
    reached_cp = unique(floor(end_cp)),
    .groups = "drop"
  ) %>%
  unnest(reached_cp) %>%
  distinct(participant, group_skill, condition, reached_cp) %>%
  group_by(condition, group_skill, reached_cp) %>%
  summarise(
    n_reached = n_distinct(participant),
    .groups = "drop"
  ) %>%
  group_by(condition, group_skill) %>%
  mutate(
    total_participants = n_distinct(descriptive_data$participant[descriptive_data$group_skill == unique(group_skill) &
                                                                   descriptive_data$condition == unique(condition)]),
    prop_reached = n_reached / total_participants
  ) %>%
  complete(
    reached_cp = 0:6,
    fill = list(n_reached = 0, prop_reached = 0)
  ) %>%
  ungroup() %>% ggplot(aes(x = reached_cp, y = prop_reached, fill = condition)) +
  facet_wrap(group_skill ~ .) +
  geom_col(position = position_dodge(-0.5), color = "#533846", show.legend = FALSE) +
  labs(
    x = "CP (end)",
    y = expression("% of participants"),
    fill = ""
  ) +
  theme_conan() +
  scale_fill_manual(
    values = c("#E93609", "#79CB6C"),
    labels = c("Long (6 CP)", "Short (3 CP)")
  ) +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("Start", paste0("CP", 1:2), "End | CP3", paste0("CP", 4:5), "End")
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme(legend.position = c(0.9, 0.83))

# ----- Average progress based on the starting checkpoint ----- 
descriptive_data %>% filter(uniqueId != "Participant 10_29_2") %>%
  group_by(condition, group_skill, checkpoint) %>%
  summarise(
    mean_end = mean(end_cp, na.rm = TRUE),
    sd_end = sd(end_cp, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>% mutate(condition = ifelse(condition == "ShortCond", "Short (3 cp)", "Long (6 cp)")) %>% 
  ggplot(aes(x = checkpoint, y = mean_end, fill = group_skill)) +
  facet_wrap(condition~.) +
  geom_line(aes(color = group_skill), size = 0.9, show.legend = FALSE) +
  geom_ribbon(aes(ymin = mean_end - sd_end, ymax = mean_end + sd_end, fill = group_skill),
              alpha = 0.4, color = NA, show.legend = FALSE) +
  geom_point(size = 3, shape = 21, color = '#533846') +
  labs(
    x = "Starting checkpoint",
    y = "Avg. Ending checkpoint",
    color = "",
    fill = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_color_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_x_continuous(
    breaks = 0:6,
    labels = c("Start", paste0("CP", 1:2), "End | CP3", paste0("CP", 4:5), "End")
  ) +
  scale_y_continuous(
    breaks = 0:6,
    labels = c("Start", paste0("CP", 1:2), "End | CP3", paste0("CP", 4:5), "End")
  ) +
  theme(legend.position = c(0.8, 0.87), axis.text.x = element_text(angle = 30, hjust = 0.8))

# ---- Transition between CP heatmap ----

# Build transitions between checkpoints reached
transitions <- descriptive_data %>% filter(uniqueId != "Participant 10_29_2") %>%
  mutate(from = checkpoint,
         to = end_cp) %>%
  group_by(condition, group_skill, from, to) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition, from) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

transitions_participant <- descriptive_data %>% filter(uniqueId != "Participant 10_29_2") %>%
  mutate(from = checkpoint, to = end_cp) %>%
  group_by(participant, condition, group_skill, from, to) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(participant, condition, group_skill, from) %>%
  mutate(prob = n / sum(n)) %>%   # within-participant probabilities
  ungroup() %>% group_by(condition, group_skill, from, to) %>%
  summarise(prob = mean(prob, na.rm = TRUE), .groups = "drop")

# Heatmap des transitions
transitions_participant %>% filter(condition == "ShortCond") %>% complete(
  group_skill,
  from = 0:5,
  to = 0:6,
  fill = list(prob = 0, n = 0)
) %>% 
  mutate(condition = ifelse(condition == "ShortCond", "Short (3 cp)", "Long (6 cp)")) %>%
  ggplot(aes(x = factor(from), y = factor(to), fill = prob)) +
  geom_tile(color = "white") +
  facet_wrap(group_skill~.) +
  scale_fill_gradient(low = "white", high = "#79CB6C", limits = c(0, 1)) +
  labs(
    x = "From CP",
    y = "To CP",
    fill = "P(Transition)"
  ) +
  theme_conan() +
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 30, hjust = 0.8), axis.text = element_text(size = 8)) + 
  scale_x_discrete(
    breaks = 0:5,
    labels = c("Start", paste0("CP", 1:2), " ", " ", " ")
  ) + scale_y_discrete(
    breaks = 0:6,
    labels = c("Start", paste0("CP", 1:2), "End", " ", " ", " ")
  ) 

transitions_participant %>% filter(condition == "LongCond") %>%
  mutate(condition = ifelse(condition == "ShortCond", "Short (3 cp)", "Long (6 cp)")) %>%
  ggplot(aes(x = factor(from), y = factor(to), fill = prob)) +
  geom_tile(color = "white") +
  facet_wrap(group_skill~.) +
  scale_fill_gradient(low = "white", high = "#E93609", limits = c(0, 1)) +
  labs(
    x = "From CP",
    y = "To CP",
    fill = "P(Transition)"
  ) +
  theme_conan() +
  theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 30, hjust = 0.8), axis.text = element_text(size = 8)) + 
  scale_x_discrete(
    breaks = 0:5,
    labels = c("Start", paste0("CP", 1:5))
  ) + scale_y_discrete(
    breaks = 0:6,
    labels = c("Start", paste0("CP", 1:5), "End")
  ) 