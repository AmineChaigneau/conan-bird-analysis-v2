# ------------------------------------------------------------------------------
# Process Game Data for Poisson GLMM
# ------------------------------------------------------------------------------

aggregate_game_data_withCond <- game_data_filtered %>% group_by(subject_nr, trialId, condition, difficulty, trialNumber, group_skill) %>% summarise(
  attempts = n()
) %>% arrange(subject_nr, trialNumber) %>% ungroup %>% mutate(
  trialNumber_unscale = trialNumber,
  trialNumber = scale(trialNumber)
)

# ------------------------------------------------------------------------------
# Predict Number of Attempt
# ------------------------------------------------------------------------------

# ----- Poisson GLMM ------ #
attempts_m0 <- glmer(
  attempts ~ condition + difficulty + group_skill + trialNumber + (1 | subject_nr), 
  data = aggregate_game_data_withCond,
  family = poisson(link = "log")
)

car::vif(attempts_m0)
summary(attempts_m0) # 11574.7

# Candidate curves (use orthogonal polys for stability)
m_trial2 <- update(attempts_m0, . ~ . - trialNumber + poly(trialNumber, 2))
m_trial2xskill <- update(attempts_m0, . ~ . - trialNumber - group_skill + poly(trialNumber, 2)*group_skill)
m_condxskill <- update(attempts_m0, . ~ . - condition - group_skill + condition*group_skill)
m_diffxskill <- update(attempts_m0, . ~ . - difficulty - group_skill + difficulty*group_skill)
m_condxdiffxskill <- update(attempts_m0, . ~ . - condition - difficulty - group_skill + condition*difficulty*group_skill)

anova(attempts_m0, m_trial2)     # 
anova(attempts_m0, m_trial2xskill) #
anova(attempts_m0, m_condxskill)   # 
anova(attempts_m0, m_diffxskill)   # 
anova(attempts_m0, m_condxdiffxskill)   # 

fit_attempts_main <- glmer(
  attempts ~ condition*difficulty*group_skill + poly(trialNumber, 2)*group_skill + (1 | subject_nr), 
  data = aggregate_game_data_withCond,
  family = poisson(link = "log")
)

car::vif(fit_attempts_main)
summary(fit_attempts_main) # 11183.2 

DHARMa::testDispersion(fit_attempts_main) # Overdispersion < .05

fit_attempts_nb <- glmmTMB::glmmTMB(
  attempts ~ condition*difficulty*group_skill + poly(trialNumber, 2, raw = TRUE)*group_skill + (1 | subject_nr), 
  data = aggregate_game_data_withCond,
  family = nbinom2(link = "log")  # binomial négatif variance quadratique
)

summary(fit_attempts_nb)$AIC # 8386.3

ci_attempt_nb <- confint(fit_attempts_nb, method = "Wald")

# ------------------------------------------------------------------------------
# Marginal Means
# ------------------------------------------------------------------------------

trial_grid <- seq(
  min(aggregate_game_data_withCond$trialNumber),
  max(aggregate_game_data_withCond$trialNumber),
  length.out = 30
)

emm_trial <- emmeans(
  fit_attempts_nb,
  ~ trialNumber * condition * group_skill,
  at = list(
    trialNumber = trial_grid,
    condition = c("ShortCond", "LongCond"),
    group_skill = c("Skilled", "unSkilled"),
    difficulty = c("Low", "High")
  ),
  type = "response"
)

emm_trial_df <- as.data.frame(emm_trial)

emm_diff_cond <- emmeans(
  fit_attempts_nb,
  ~ difficulty * condition * group_skill,
  at = list(
    trialNumber = mean(aggregate_game_data_withCond$trialNumber)
  ),
  type = "response"
)

contrast(
  emm_diff_cond,
  method = "pairwise",
  by = c("group_skill"),
  simple = "condition",
  adjust = "holm"
)

emm_diff_cond <- as.data.frame(emm_diff_cond) %>% mutate(condition = ifelse(condition == "LongCond", "Long (6 cp)", "Short (3 cp)"))

# ------------------------------------------------------------------------------
# Visualization (predicted values)
# ------------------------------------------------------------------------------

# ---- trial index 
points <- aggregate_game_data_withCond %>% group_by(trialNumber_unscale, condition, group_skill) %>% summarise(
  mean_attempts = mean(attempts),
  sd = sd (attempts)
) %>% mutate(condition = ifelse(condition == 'LongCond', 'Long (6 cp)', 'Short (3 cp)'))

mean_trial <- attr(scale(aggregate_game_data_withCond$trialNumber_unscale), "scaled:center")
sd_trial   <- attr(scale(aggregate_game_data_withCond$trialNumber_unscale), "scaled:scale")

emm_trial_df %>%  mutate(
  trialNumber_unscaled = trialNumber * sd_trial + mean_trial,
  condition = ifelse(condition == 'LongCond', 'Long (6 cp)', 'Short (3 cp)')) %>%
  ggplot(aes(x = trialNumber_unscaled, y = response, color = group_skill, fill = group_skill)) +
    facet_wrap(~ condition) +
    geom_line(linewidth = 1) +
    geom_point(data = points, aes(x = trialNumber_unscale, y = mean_attempts, fill = group_skill, shape = group_skill), alpha = 0.9, color = '#533846', show.legend = FALSE) + 
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL ), alpha = 0.4, color = NA) +
    labs(
      x = "Trial (index)",
      y = "Predicted number of attempts",
      color = "",
      fill = "",
      shape = ""
    ) +
    scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
    scale_color_manual(values = c('#F9B734', '#4EC0CA')) +
    scale_shape_manual(values = c(21, 23)) +
    theme_conan() +
    theme(legend.position = "top") + scale_y_continuous(limits = c(2, 14), breaks = c(2, 5, 8, 11, 14), labels = c("2.0", "5.0", "8.0", "11.0", "14.0"))

# ---- difficulty / condition / skill
emm_diff_cond %>%
  ggplot(aes(x = condition, y = response)) +
  facet_wrap(difficulty~.) +
  # geom_line(aes(group = interaction(difficulty, group_skill)), position = position_dodge(0.5)) +
  # geom_point(aes(fill = group_skill, shape = group_skill), color = '#533846', size = 3, position = position_dodge(0.5)) + 
  geom_bar(aes(fill = group_skill), stat = 'identity', position = position_dodge(width = 0.9), color = '#533846', alpha = 0.8, show.legend = FALSE) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL, group = interaction(group_skill, difficulty)), width = 0.2, color = '#533846', position = position_dodge(0.9), show.legend = FALSE) +
  labs(
    x = "",
    y = "Predicted number of attempts",
    title = "",
    fill = "",
    shape = ""
  ) +
  scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_color_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_shape_manual(values = c(21, 22)) +
  theme_conan() +
  theme(
    legend.position = "top",
    legend.margin=margin(-20, 0, -5, 0),
  ) + ylim(0, 12)

