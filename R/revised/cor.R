# ------------------------------------------------------------------------------
# Attempts and GU probs correlate at different CP
# ------------------------------------------------------------------------------

# ------ Aggregate at CP level
game_data_filtered$response_binomial

cp_attempts <- game_data_filtered %>%
  mutate(attempt_number = attemptIndex + 1) %>%
  group_by(subject_nr, condition, trialId, end_distance_to_goal_f) %>%
  summarise(
    attempts_at_cp = n(),   # number of attempts at this CP
    gave_up = max(response_binomial),  # whether GU occurred at that CP
    .groups = "drop"
  )

cp_subject_summary <- cp_attempts %>%
  group_by(subject_nr, end_distance_to_goal_f, condition) %>%
  summarise(
    mean_attempts_cp = mean(attempts_at_cp, na.rm = TRUE),
    gu_prob = mean(gave_up, na.rm = TRUE),
    .groups = "drop"
  )

subject_summary <- cp_attempts %>%
  group_by(subject_nr, condition) %>%
  summarise(
    mean_attempts_cp = mean(attempts_at_cp, na.rm = TRUE),
    gu_prob = mean(gave_up, na.rm = TRUE),
    .groups = "drop"
  )

# ------ Correlate across CPs
cp_subject_summary %>%
  group_by(condition) %>%
  summarise(
    cor = cor(mean_attempts_cp, gu_prob, use = "complete.obs")
  )

subject_summary %>%
  group_by(condition) %>%
  summarise(
    cor = cor(mean_attempts_cp, gu_prob, use = "complete.obs")
  )


cp_subject_summary %>%
ggplot(aes(x = mean_attempts_cp, y = gu_prob, color = end_distance_to_goal_f)) +
  geom_point(size = 2) +
  # geom_text(nudge_y = 0.01) +
  facet_wrap(~condition) +
  theme_minimal()

# ------ Simple model

# Does attempts explain GU once progress is accounted for?

model_attempt_gu <- lm(gu_prob ~ mean_attempts_cp + end_distance_to_goal_f + condition, data = cp_subject_summary)
summary(model_attempt_gu)

lm(GU ~ attempts + distance_to_goal)