source(file = "mt_data_processing.R")
source(file = "mt_lmm_nGu")

# ------------------------------------------------------------------------------
# Scale
# ------------------------------------------------------------------------------
mouse_features_full <- mouse_features_full %>% mutate(condition = as.factor(ifelse(condition == 'LongCond', 'Long (6 cp)', 'Short (3 cp)')),
                                                      group_skill = as.factor(group_skill))

mouse_features_full <- mouse_features_full %>% mutate(log_MAD = log(max_dev + 1e-6))

mouse_features_full %>% left_join(game_data_filtered)

# ------ Overall scaling ------ #
mouse_features_full_scaled <- mouse_features_full %>% mutate(
  trialNumber_factor = as.factor(trialNumber),
  trialNumber = scale(trialNumber),
  jumpCount = scale(jumpCount),
  reward = scale(reward),
  start_distance_to_goal = scale(start_distance_to_goal),
  end_distance_to_goal = scale(end_distance_to_goal),
  goal_proximity_end = scale(goal_proximity_end),
  goal_proximity_start = scale(goal_proximity_start),
  level = as.factor(level)
)

# ------ Within-participant scaling ------ #
mouse_features_full_scaled <- mouse_features_full_scaled %>% group_by(subject_nr) %>% mutate(attemptIndex_unscale = attemptIndex) %>% mutate(
  attemptIndex = scale(attemptIndex),
  pipesPassed = scale(pipesPassed),
  cumPipesPassed = scale(cumPipesPassed),
  cumJumpCount = scale(cumJumpCount),
  all_cumPipesPassed = scale(all_cumPipesPassed),
) %>% ungroup

# --------------------------------------------
# Maximal Deviation - Persist vs. Give Up
# --------------------------------------------

md_m0_full <- lmer(log_MAD ~ response * end_distance_to_goal + group_skill + difficulty + attemptIndex + trialNumber + pipesPassed +
                     (1 | subject_nr), data = mouse_features_full_scaled, REML = TRUE)

summary(md_m0_full)

# Refit base under ML for fair comparisons
fit_md_m0 <- update(md_m0_full, REML = FALSE)

# Candidate curves (use orthogonal polys for stability)
m_trial2 <- update(fit_md_m0, . ~ . - trialNumber + poly(trialNumber, 2))
m_attempt2 <- update(fit_md_m0, . ~ . - attemptIndex + poly(attemptIndex, 2))
m_pipes2 <- update(fit_md_m0, . ~ . - pipesPassed + poly(pipesPassed, 2))

anova(fit_md_m0, m_trial2)     # ns
anova(fit_md_m0, m_attempt2)   # ns
anova(fit_md_m0, m_pipes2) #ns

# --- main model
fit_md_main <- lmerTest::lmer(log_MAD ~ response * end_distance_to_goal + group_skill + difficulty + attemptIndex + trialNumber + pipesPassed +
                                (1 | subject_nr), data = mouse_features_full_scaled, REML = TRUE)

par(mfrow = c(1, 2))
plot(fit_md_main, which = 1)      # résidus vs fitted
qqnorm(resid(fit_md_main)) 
qqline(resid(fit_md_main))

summary(fit_md_main)
summary(fit_md_main)$AIC # 17496.82 

ci_md <- confint(fit_md_main, method = "Wald")

# ----- emmeans 

emm_md <- emmeans(
  fit_md_main,
  ~ end_distance_to_goal | response,
  at = list(end_distance_to_goal = seq(
    min(mouse_features_full_scaled$end_distance_to_goal),
    max(mouse_features_full_scaled$end_distance_to_goal),
    length = 20,
  )), pbkrtest.limit = 8244
)

emm_md_df <- as.data.frame(emm_md)
center_edg <- attr(mouse_features_full_scaled$end_distance_to_goal, "scaled:center")
scale_edg  <- attr(mouse_features_full_scaled$end_distance_to_goal, "scaled:scale")

emm_md_df %>% 
  ggplot(aes(x = end_distance_to_goal * scale_edg + center_edg, y = exp(emmean), color = response, linetype = response)) + 
    geom_line(linewidth = 0.9) +
    geom_ribbon(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL), fill = response), alpha = .2, colour = NA) +
    labs(
      x = "CP from the goal",
      y = "Predicted MAD",
      color = "",
      fill = ""
    ) + scale_color_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
    scale_fill_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
    # scale_x_reverse() +
    scale_x_continuous(trans = "reverse", breaks = 6:1, labels = function(x) paste0(x, "CP")) +
    scale_linetype_manual(values = c('dotdash', 'solid')) +
    theme_conan() + guides(color = FALSE, fill = FALSE, linetype = FALSE) 

mouse_features_full %>%
  mutate(end_distance_to_goal_f = factor(end_distance_to_goal_f, rev(levels(end_distance_to_goal_f)))) %>%
  group_by(subject_nr, response, end_distance_to_goal_f) %>%
  summarise(
    mad = mean(max_dev, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = end_distance_to_goal_f, y = mad, fill = response, pattern = response)) + 
  geom_boxplot_pattern(outliers = FALSE, alpha = .7, position = position_dodge(width = 0.75), pattern_fill = "#DB9502") +
  geom_point(shape = 21, alpha = .7, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) +
  scale_color_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
  scale_fill_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
  labs(
    x = "CP from the goal",
    y = "MAD",
    color = "",
    fill = ""
  ) +
  theme_conan() + guides(color = FALSE, fill = FALSE, pattern = FALSE)

mouse_features_full %>%
  mutate(end_distance_to_goal_f = factor(end_distance_to_goal_f, rev(levels(end_distance_to_goal_f)))) %>%
  ggplot(aes(x = end_distance_to_goal_f, y = max_dev, fill = response, pattern = response)) + 
  facet_wrap(condition~., ncol = 1) +
  geom_boxplot_pattern(outliers = FALSE, alpha = .7, position = position_dodge(width = 0.75), pattern_fill = "#DB9502", pattern_size = 0.5, pattern_spacing = 0.1) +
  # geom_point(shape = 21, alpha = .7, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) +
  scale_color_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
  scale_fill_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Resart")) + 
  labs(
    x = "CP from the goal",
    y = "MAD",
    color = "",
    fill = ""
  ) +
  theme_conan() + guides(color = FALSE, fill = FALSE, pattern = FALSE)

# --------------------------------------------
# Maximal Deviation - nGu
# --------------------------------------------

mouse_features_ngu <- mouse_features_full %>% left_join(game_data_filtered_nGu %>% select(uniqueId, nGu), by = "uniqueId") %>% drop_na(nGu)


fit_md_nGu <- lmerTest::lmer(log_MAD ~ nGu * group_skill +
                                (1 | subject_nr), data = mouse_features_ngu, REML = TRUE)

par(mfrow = c(1, 2))
plot(fit_md_nGu, which = 1)      # résidus vs fitted
qqnorm(resid(fit_md_nGu)) 
qqline(resid(fit_md_nGu))

summary(fit_md_nGu)
summary(fit_md_nGu)$AIC # 17496.82 

