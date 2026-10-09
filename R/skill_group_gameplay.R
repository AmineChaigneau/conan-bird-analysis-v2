# ---------------------------------------------------------------------------
# Skill groups show distinct progression profiles during gameplay
# ---------------------------------------------------------------------------

reach_data <- descriptive_data %>%
  group_by(participant, group_skill, condition) %>%
  summarise(max_cp = max(end_cp), .groups = "drop") %>%
  mutate(max_cp = floor(max_cp)) %>%
  rowwise() %>%
  mutate(cp_list = list(
    if(condition == "ShortCond") 0:3 else 0:6
  )) %>%
  unnest(cp_list) %>%
  rename(cp = cp_list) %>%
  mutate(reached = as.integer(max_cp >= cp))

model_reach <- glmer(
  reached ~ group_skill * cp * condition +
    (1 | participant),
  data = reach_data,
  family = binomial,
  control = glmerControl(optimizer="bobyqa")
)

summary(model_reach)

# Progress
descriptive_data <- descriptive_data %>%
  mutate(progress = end_cp - checkpoint)

model_progress <- lmer(
  progress ~ group_skill * checkpoint * condition +
    difficulty +
    (1 | participant),
  data = descriptive_data
)

summary(model_progress)
