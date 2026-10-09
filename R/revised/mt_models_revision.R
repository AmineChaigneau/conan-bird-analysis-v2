# ==============================================================================
# Mouse-tracking (MAD) skill checks -- Reviewer 1, point 5
# ==============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# dir.create("model_outputs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# STEP 0 -- Data sufficiency: trajectory counts by skill x response [x CP]
# ------------------------------------------------------------------------------
counts_skill_response <- mouse_features_full_scaled %>%
  count(group_skill, response) %>%
  pivot_wider(names_from = response, values_from = n, values_fill = 0)
counts_skill_response

counts_skill_response_cp <- mouse_features_full_scaled %>%
  count(group_skill, response, end_distance_to_goal_f) %>%
  pivot_wider(names_from = response, values_from = n, values_fill = 0) %>%
  arrange(end_distance_to_goal_f, group_skill)
counts_skill_response_cp

# ------------------------------------------------------------------
# STEP 1 -- Random-effects structure (REML, same fixed effects)
# ------------------------------------------------------------------
ctrl_lmm <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

fit_md_main <- lmerTest::lmer(
  log_MAD ~ response * end_distance_to_goal + group_skill + difficulty +
    attemptIndex + trialNumber + pipesPassed + (1 | subject_nr),
  data = mouse_features_full_scaled, REML = TRUE, control = ctrl_lmm
)
nobs(fit_md_main)

# fit_md_main <- update(fit_md_main, control = ctrl)   # intercept-only baseline
nobs(fit_md_main)                                    # N to report

# Factors: correlated slope replaces the intercept term
md_resp <- update(fit_md_main, . ~ . - (1 | subject_nr) + (1 + response   | subject_nr))
md_diff <- update(fit_md_main, . ~ . - (1 | subject_nr) + (1 + difficulty | subject_nr))

# Numeric predictors: uncorrelated slopes, as before
md_dist  <- update(fit_md_main, . ~ . + (0 + end_distance_to_goal || subject_nr))
md_att   <- update(fit_md_main, . ~ . + (0 + attemptIndex || subject_nr))
md_trial <- update(fit_md_main, . ~ . + (0 + trialNumber  || subject_nr))
md_pipes <- update(fit_md_main, . ~ . + (0 + pipesPassed  || subject_nr))

cands <- list(response = md_resp, difficulty = md_diff, distance = md_dist,
              attempt = md_att, trial = md_trial, pipes = md_pipes)
lapply(cands, function(m) anova(fit_md_main, m, refit = FALSE))
sapply(cands, isSingular)

md_a <- update(md_resp, . ~ . + (0 + trialNumber || subject_nr))
anova(md_resp, md_a, refit = FALSE); isSingular(md_a)

md_b <- update(md_a, . ~ . + (0 + end_distance_to_goal || subject_nr))
anova(md_a, md_b, refit = FALSE); isSingular(md_b)

md_c <- update(md_a, . ~ . + (0 + attemptIndex || subject_nr))
anova(md_a, md_c, refit = FALSE); isSingular(md_c)

md_d <- update(md_a, . ~ . + (0 + pipesPassed || subject_nr))
anova(md_a, md_d, refit = FALSE); isSingular(md_d)

md_e <- update(md_d, . ~ . + (0 + end_distance_to_goal || subject_nr))
anova(md_d, md_e, refit = FALSE); isSingular(md_e)

md_f <- update(md_d, . ~ . + (0 + attemptIndex || subject_nr))
anova(md_d, md_f, refit = FALSE); isSingular(md_f)

md_g <- update(md_e, . ~ . + (0 + attemptIndex || subject_nr))
anova(md_e, md_g, refit = FALSE); isSingular(md_g)

md_re_final <- md_e
summary(md_re_final)

# Only if a convergence warning remains:
# summary(allFit(md_re_final))$fixef

# ------------------------------------------------------------------
# STEP 2 -- Skill main effect, from the FINAL model
# ------------------------------------------------------------------
summary(md_re_final)$coefficients["group_skillunSkilled", ]

# ------------------------------------------------------------------
# STEP 3 -- Skill interactions (ML, fixed part changes)
# ------------------------------------------------------------------
md_ML <- update(md_re_final, REML = FALSE)
anova(md_ML, update(md_ML, . ~ . + response:group_skill))
anova(md_ML, update(md_ML, . ~ . + end_distance_to_goal:group_skill))
anova(md_ML, update(md_ML, . ~ . + response:group_skill + end_distance_to_goal:group_skill))

# ------------------------------------------------------------------
# STEP 4 -- Simple slopes of distance for each decision
# ------------------------------------------------------------------
emmeans::emtrends(md_re_final, ~ response, var = "end_distance_to_goal",
                  lmer.df = "asymptotic") |> emmeans::test()

emmeans::emtrends(fit_md_main, ~ response, var = "end_distance_to_goal",
                  lmer.df = "asymptotic") |> emmeans::test()

# ------------------------------------------------------------------------------
# STEP 5 -- Supplementary figure: predicted MAD by CP, split by skill
# ------------------------------------------------------------------------------

md_ML <- update(md_re_final, REML = FALSE)          # md_re_final = md_e
m_skill_both <- update(md_ML, . ~ . + response:group_skill +
                         end_distance_to_goal:group_skill)

emm_md_skill <- emmeans(
  m_skill_both,
  ~ end_distance_to_goal | response * group_skill,
  at = list(end_distance_to_goal = seq(
    min(mouse_features_full_scaled$end_distance_to_goal),
    max(mouse_features_full_scaled$end_distance_to_goal),
    length = 20)),
  lmer.df = "asymptotic"
)

emm_md_skill_df <- as.data.frame(emm_md_skill)

center_edg <- attr(mouse_features_full_scaled$end_distance_to_goal, "scaled:center")
scale_edg  <- attr(mouse_features_full_scaled$end_distance_to_goal, "scaled:scale")

emm_md_skill_df %>%
  ggplot(aes(x = end_distance_to_goal * scale_edg + center_edg, y = exp(emmean), color = response, linetype = response)) +
  facet_wrap(~ group_skill) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL), fill = response), alpha = .2, colour = NA) +
  labs(x = "CP from the goal", y = "Predicted MAD", color = "", fill = "") +
  scale_color_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Restart")) +
  scale_fill_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Restart")) +
  scale_x_continuous(trans = "reverse", breaks = 6:1, labels = function(x) paste0(x, "CP")) +
  scale_linetype_manual(values = c('dotdash', 'solid')) +
  theme_conan() + guides(linetype = FALSE)

ggsave("model_outputs/mad_by_cp_skill_split.png", width = 8, height = 4)

emm_md <- emmeans(
  md_re_final,                       # = md_e
  ~ end_distance_to_goal | response,
  at = list(end_distance_to_goal = seq(
    min(mouse_features_full_scaled$end_distance_to_goal),
    max(mouse_features_full_scaled$end_distance_to_goal),
    length = 20)),
  lmer.df = "asymptotic"
)

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

# ------ Raw-data version, matching original boxplot style, split by skill ------ #
mouse_features_full %>%
  mutate(end_distance_to_goal_f = factor(end_distance_to_goal_f, rev(levels(end_distance_to_goal_f)))) %>%
  group_by(subject_nr, group_skill, response, end_distance_to_goal_f) %>%
  summarise(mad = mean(max_dev, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = end_distance_to_goal_f, y = mad, fill = response, pattern = response)) +
  facet_wrap(~ group_skill) +
  geom_boxplot_pattern(outliers = FALSE, alpha = .7, position = position_dodge(width = 0.75), pattern_fill = "#DB9502") +
  geom_point(shape = 21, alpha = .7, position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75)) +
  scale_fill_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Restart")) +
  labs(x = "CP from the goal", y = "MAD", fill = "") +
  theme_conan() + guides(pattern = FALSE)

ggsave("model_outputs/mad_by_cp_skill_split_raw.png", width = 8, height = 4)

# ------------------------------------------------------------------------------
# STEP 5 -- Supplementary figure: predicted MAD by CP, split by skill
# ------------------------------------------------------------------------------

