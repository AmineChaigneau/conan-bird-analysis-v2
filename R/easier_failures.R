# ------------------------------------------------------------------------------
# Failure in Easier Settings
# ------------------------------------------------------------------------------

easier_data <- trial_data %>% filter(setting == 'easier')

# ------ Easier failure and Trial Number
failures_easier <- easier_data %>% select(trialNumber, participant_id, attemptIndex, condition, difficulty, checkpoint, pipesPassed) %>% 
  group_by(participant_id, condition, difficulty, trialNumber) %>% summarise(
    n_attempts = n(),
    n_failures = n_attempts - 1,
    .groups = "drop"
  )

failures_easier %>%
  group_by(condition, trialNumber, difficulty) %>%
  summarise(
    mean_failures = mean(n_failures),
    se = sd(n_failures) / sqrt(n()),  # optionnel: erreur standard
    .groups = "drop"
  ) %>%
  ggplot(aes(x = trialNumber, y = mean_failures, fill = condition, color = condition)) +
  facet_grid(condition~difficulty) +
  geom_line() +
  labs(
    x = "Difficulty",
    y = "Mean number of failures",
    fill = "Condition"
  ) +
  theme_minimal()

# Correlation

# ------ Easier failure and Attempt Index before Disengagement
corr_data_group <- easier_data %>%
  select(trialNumber, participant_id, attemptIndex, condition, difficulty) %>%
  group_by(participant_id, condition, difficulty, trialNumber) %>%
  summarise(
    normal_failures = min(attemptIndex, na.rm = TRUE) - 1,
    easier_failures = n() - 1,
    .groups = "drop"
  ) %>% filter(easier_failures < 100) %>% filter(normal_failures < 100)

corr_data <- easier_data %>%
  select(trialNumber, participant_id, attemptIndex) %>%
  group_by(participant_id , trialNumber) %>%
  summarise(
    normal_failures = min(attemptIndex, na.rm = TRUE) - 1,
    easier_failures = n() - 1,
    .groups = "drop"
  ) %>% filter(easier_failures < 100) %>% filter(normal_failures < 100)

# Plot 
corr_data_group %>% 
  ggplot(aes(x = normal_failures, y = easier_failures)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(difficulty ~ condition) +
  labs(
    x = "Number of failures in normal setting",
    y = "Number of failures in easier setting",
    title = "Relation between normal-setting failures and easier-setting failures"
  ) +
  theme_minimal()

corr_data %>% 
  ggplot(aes(x = normal_failures, y = easier_failures)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Number of failures in normal setting",
    y = "Number of failures in easier setting",
    title = "Relation between normal-setting failures and easier-setting failures"
  ) +
  theme_minimal()

corr_data %>% 
  ggplot(aes(x = trialNumber, y = easier_failures)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Number of failures in normal setting",
    y = "Number of failures in easier setting",
    title = "Relation between normal-setting failures and easier-setting failures"
  ) +
  theme_minimal()


# Correlation
cor.test(corr_data$normal_failures, corr_data$easier_failures)

cor.test(corr_data$trialNumber, corr_data$easier_failures)
