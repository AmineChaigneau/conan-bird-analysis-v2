# game_data_scaled <- game_data_scaled %>% filter(trialId != "11_Participant 2")

# ------------------------------------------------------------------------------
# Response Models (GLMM, scaled fixed effects)
# ------------------------------------------------------------------------------

# ------ Model 1 ------ #
model_001 <- glmer(
  response_binomial ~ end_distance_to_goal + group_skill + difficulty + poly(attemptIndex, 2) + poly(pipesPassed, 2) + poly(trialNumber, 2) + (1 | subject_nr),
  data = game_data_scaled, family = binomial, control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
)
summary(model_001)
summary(model_001)$AIC # 3470.341

# ------ Adding interaction ------ #
model_002 <- update(model_001, . ~ . + end_distance_to_goal:group_skill)
anova(model_001, model_002) # ns

model_003 <- update(model_001, . ~ . + poly(attemptIndex, 2):group_skill)
anova(model_001, model_003) # Significant *** - AIC 3416.5

model_004 <- update(model_001, . ~ . + poly(pipesPassed, 2):group_skill)
anova(model_001, model_004) # ns

model_005 <- update(model_001, . ~ . + poly(trialNumber, 2):group_skill)
anova(model_001, model_005) # Significant *** - AIC 3429.0

model_006 <- update(model_001, . ~ . + end_distance_to_goal:difficulty)
anova(model_001, model_006) # ns

# ----- multiple interaction from model_003
model_007 <- update(model_003, . ~ . + end_distance_to_goal:group_skill)
anova(model_003, model_007) # Significant * - AIC 3414.4

model_008 <- update(model_007, . ~ . + poly(trialNumber, 2):group_skill)
anova(model_007, model_008) # Significant *** - AIC 3377.9

# ------ Final Model Comparison LRT ------ #
anova(model_001, model_002, model_003, model_004, model_005, model_006, model_007, model_008, test = "LRT")

# ------------------------------------------------------------------------------
# Final Model
# ------------------------------------------------------------------------------
model_final <- model_008

model_final_unested <- glmer(
  response_binomial ~ end_distance_to_goal + group_skill + difficulty + poly(attemptIndex, 2, raw = TRUE) + poly(pipesPassed, 2, raw = TRUE) + poly(trialNumber, 2, raw = TRUE) + poly(attemptIndex, 2, raw = TRUE):group_skill + end_distance_to_goal:group_skill +  poly(trialNumber, 2, raw = TRUE):group_skill + (1 | subject_nr),
  data = game_data_scaled, family = binomial, control = glmerControl(optimizer = "bobyqa",  optCtrl = list(maxfun = 2e5))
)

# ------ Results ------ #
car::vif(model_final)
summary(model_final)
summary(model_final)$AIC

ci_model_final <- confint(model_final, method = "Wald")

# --- Overdispersion and Residuals
overdisp_fun(model_008)
res <- simulateResiduals(model_008, n = 1000)
plot(res)

# --- Marginal Means
attempt_grid <- list(
  attemptIndex = seq(
    from = min(game_data_scaled$attemptIndex),
    to = max(game_data_scaled$attemptIndex),
    length.out = 100
  )
)

emm_attempt <- emmeans(
  model_final_unested,
  ~ attemptIndex * group_skill,
  at = attempt_grid,
  type = "response"   # gives predicted probabilities
)

trial_grid <- list(
  trialNumber = seq(
    from = min(game_data_scaled$trialNumber),
    to = max(game_data_scaled$trialNumber),
    length.out = 100
  )
)

emm_trial_probs <- emmeans(
  model_final_unested,
  ~ trialNumber * group_skill,
  at = trial_grid,
  type = "response"
)

# ------------------------------------------------------------------------------
# Auxiliary models
# ------------------------------------------------------------------------------

# ------ Trial index x Skill ------ #
model_trialxskill <- glmer(
  response_binomial ~ group_skill*poly(trialNumber, 2) + (1 | subject_nr),
  data = game_data_scaled, family = binomial, control = glmerControl(optimizer = "bobyqa")
)

summary(model_trialxskill)

# ------------------------------------------------------------------------------
# Alternatives models (Condition, Block order)
# ------------------------------------------------------------------------------

# ------  Separate Model with Slope For Level ------ #
model_level_slope <- update(model_001, . ~ . + (1 | level))
summary(model_level_slope)$AIC # 3363.339 convergence code

# ------ Model Blocks ------ #
model_block <- update(model_001, . ~ . + group_block)
summary(model_block)
summary(model_block)$AIC # 3375.181

# ------ Model with Condition ------ #
model_condition <- update(model_001, . ~ . - end_distance_to_goal + condition)
summary(model_condition)$AIC # 3570.363 convergence code

# ------------------------------------------------------------------------------
#  Alternative Model Distance by Condition
# ------------------------------------------------------------------------------

# ------ Alternative Model ------ #
game_data_scaled <- game_data_scaled %>% mutate(CP_by_condition = paste0(condition, "_", end_distance_to_goal_f)) 

model_CP_by_Condition <- glmer(
  response_binomial ~ CP_by_condition + CP_by_condition:group_skill + (1 | subject_nr),
  data = game_data_scaled, family = binomial, control = glmerControl(optimizer = "bobyqa")
)

car::vif(model_CP_by_Condition)
summary(model_CP_by_Condition)
summary(model_CP_by_Condition)$AIC

# ------ Distance by Condition ------ #
emm_dtg <- emmeans(model_CP_by_Condition, ~ CP_by_condition, type = "response")

contrast_SL <- contrast(
  emm_dtg,
  method = list(
    "Short vs Long (CP3)" = c(0, 0, -1, 0, 0, 0, 0, 0, 1),  # Long3 (3) vs Short3 (9)
    "Short vs Long (CP2)" = c(0, -1, 0, 0, 0, 0, 0, 1, 0),  # Long2 (2) vs Short2 (8)
    "Short vs Long (CP1)" = c(-1, 0, 0, 0, 0, 0, 1, 0, 0)   # Long1 (1) vs Short1 (7)
  ),
  adjust = "holm"
)

contrast_SL_df <- contrast_SL %>% as.data.frame() %>% mutate(
    CP = stringr::str_extract(contrast, "\\(CP\\d\\)"),
    CP = gsub("[()]", "", CP),
    CP = stringr::str_replace(CP, "CP(\\d)", "\\1CP"),
    label = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

contrast_SL_prosp <- contrast(
  emm_dtg,
  method = list(
    "Short3 vs Long6" = c(0, 0, 0, 0, 0, -1, 0, 0, 1),
    "Short2 vs Long5" = c(0, 0, 0, 0, -1, 0, 0, 1, 0),
    "Short1 vs Long4" = c(0, 0, 0, -1, 0, 0, 1, 0, 0)
  ),
  adjust = "holm"
)

contrast_SL_prosp_df <- contrast_SL_prosp %>%
  as.data.frame() %>%
  mutate(
    from_start = c(0, 1, 2),
    label = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

contrast_Long <- contrast(
  emm_dtg,
  method = list(
    "Long6 vs Long5" = c(0, 0, 0, 0, -1, 1, 0, 0, 0),
    "Long5 vs Long4" = c(0, 0, 0, -1, 1, 0, 0, 0, 0),
    "Long4 vs Long3" = c(0, 0, -1, 1, 0, 0, 0, 0, 0),
    "Long3 vs Long2" = c(0, -1, 1, 0, 0, 0, 0, 0, 0),
    "Long2 vs Long1" = c(-1, 1, 0, 0, 0, 0, 0, 0, 0)
  ),
  adjust = "holm"
)
contrast_Long <- contrast_Long %>% as.data.frame() %>% mutate(condition = "Long (6cp)")

contrast_Short <- contrast(
  emm_dtg,
  method = list(
    "Short3 vs Short2" = c(0, 0, 0, 0, 0, 0, 0, -1, 1),
    "Short2 vs Short1" = c(0, 0, 0, 0, 0, 0, -1, 1, 0)
  ),
  adjust = "holm"
)
contrast_Short <- contrast_Short %>% as.data.frame() %>% mutate(condition = "Short (3cp)")

# ------ Distance by Condition x Skill ------ #
emm_dtg_skill <- emmeans(model_CP_by_Condition, ~ CP_by_condition*group_skill, type = "response", nesting = NULL)

contrast_skill_specific <- contrast(
  emm_dtg_skill,
  method = list(
    # Skilled only (first 9 rows)
    "Short vs Long (CP3) - Skilled" = c(0,0,-1,0,0,0,0,0,1,  0,0,0,0,0,0,0,0,0),
    "Short vs Long (CP2) - Skilled" = c(0,-1,0,0,0,0,0,1,0,  0,0,0,0,0,0,0,0,0),
    "Short vs Long (CP1) - Skilled" = c(-1,0,0,0,0,0,1,0,0,  0,0,0,0,0,0,0,0,0),
    
    # Unskilled only (last 9 rows)
    "Short vs Long (CP3) - Unskilled" = c(0,0,0,0,0,0,0,0,0,  0,0,-1,0,0,0,0,0,1),
    "Short vs Long (CP2) - Unskilled" = c(0,0,0,0,0,0,0,0,0,  0,-1,0,0,0,0,0,1,0),
    "Short vs Long (CP1) - Unskilled" = c(0,0,0,0,0,0,0,0,0,  -1,0,0,0,0,0,1,0,0)
  ),
  adjust = "holm"
)

contrast_skill_specific_df <- contrast_skill_specific %>%
  as.data.frame() %>%
  mutate(
    CP = stringr::str_extract(contrast, "CP\\d"),
    CP = stringr::str_replace(CP, "CP(\\d)", "\\1CP"),
    group_skill = case_when(
      grepl("Unskilled", contrast) ~ "unSkilled",
      grepl("Skilled", contrast) ~ "Skilled"
    ),
    label = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

contrast_skill_specific_prosp <- contrast(
  emm_dtg_skill,
  method = list(
    # Skilled only (first 9 rows)
    "Short3 vs Long6 - Skilled" = c(0,0,0,0,0,-1,0,0,1,  0,0,0,0,0,0,0,0,0),
    "Short2 vs Long5 - Skilled" = c(0,0,0,0,-1,0,0,1,0,  0,0,0,0,0,0,0,0,0),
    "Short1 vs Long4 - Skilled" = c(0,0,0,-1,0,0,1,0,0,  0,0,0,0,0,0,0,0,0),
    
    # Unskilled only (last 9 rows)
    "Short3 vs Long6 - Unskilled" = c(0,0,0,0,0,0,0,0,0,  0,0,0,0,0,-1,0,0,1),
    "Short2 vs Long5 - Unskilled" = c(0,0,0,0,0,0,0,0,0,  0,0,0,0,-1,0,0,1,0),
    "Short1 vs Long4 - Unskilled" = c(0,0,0,0,0,0,0,0,0,  0,0,0,-1,0,0,1,0,0)
  ),
  adjust = "holm"
)

contrast_skill_specific_prosp_df <- contrast_skill_specific_prosp %>%
  as.data.frame() %>%
  mutate(
    from_start = case_when(
      grepl("Short3 vs Long6", contrast) ~ 0,
      grepl("Short2 vs Long5", contrast) ~ 1,
      grepl("Short1 vs Long4", contrast) ~ 2
    ),
    group_skill = case_when(
      grepl("Unskilled", contrast) ~ "unSkilled",
      grepl("Skilled", contrast) ~ "Skilled"
    ),
    label = case_when(
      p.value < .001 ~ "***",
      p.value < .01 ~ "**",
      p.value < .05 ~ "*",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(label))

# ------ Plots ------ # 
emm_dtg_df <- as.data.frame(emm_dtg)

emm_dtg_df <- emm_dtg_df %>%
  tidyr::separate(CP_by_condition, into = c("condition", "CP"), sep = "\\_") %>%
  mutate(
    CP = factor(CP, levels = sort((unique(CP)))),         # order CP numerically
    condition = factor(condition, levels = c("ShortCond", "LongCond")) # adjust levels if needed
  )

ann_df <- emm_dtg_df %>%
  group_by(CP) %>%
  summarise(
    y = max(prob + SE, na.rm = TRUE) + 0.01,
    .groups = "drop"
  ) %>%
  left_join(contrast_SL_df, by = "CP") %>%
  filter(!is.na(label)) %>%
  mutate(
    x = match(CP, rev(levels(emm_dtg_df$CP))),
    x_left = x - 0.15,
    x_right = x + 0.15,
    y_text = y + 0.005
  )

# Retrospective Plot
emm_dtg_df %>% 
  mutate(condition = ifelse(condition == "LongCond", "Long (6 cp)", "Short (3 cp)")) %>%
  mutate(CP = factor(CP, levels = rev(levels(CP)))) %>%
  ggplot(aes(x = CP, y = prob, fill = condition, shape = condition)) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, color = "#533846", position = position_dodge(width = 0.3)) +
  geom_line(aes(group = condition), position = position_dodge(width = 0.3), color = "#533846") +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_segment(data = ann_df, aes(x = x_left, xend = x_right, y = y, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_segment(data = ann_df, aes(x = x_left, xend = x_left, y = y - 0.004, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_segment(data = ann_df, aes(x = x_right, xend = x_right, y = y - 0.004, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_text(data = ann_df, aes(x = x, y = y_text, label = label), inherit.aes = FALSE, color = "#533846", size = 5) +
  labs(
    x = "CP from the goal",
    y = "Predicted Probability of GU",
    fill = "",
    shape = ""
  ) + 
  theme_conan() +
  scale_fill_manual(values = c("#E93609", "#79CB6C", "#F9B734", "#4EC0CA")) +
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.85, 0.83)) +
  ylim(0, 0.2)

# Prospective Plot
emm_dtg_plot_prospe <- emm_dtg_df %>%
  mutate(
    cp_num = as.numeric(str_remove(CP, "CP")),
    total_cp = ifelse(condition == "ShortCond", 3, 6),
    from_start = total_cp - cp_num,   # 0 = start
    condition_lab = ifelse(condition == "LongCond", "Long (6 cp)", "Short (3 cp)")
  )

ann_df_prosp <- emm_dtg_plot_prospe %>%
  group_by(from_start) %>%
  summarise(
    y = max(prob + SE, na.rm = TRUE) + 0.01,
    .groups = "drop"
  ) %>%
  left_join(contrast_SL_prosp_df, by = "from_start") %>%
  filter(!is.na(label)) %>%
  mutate(
    x = from_start,
    x_left = x - 0.12,
    x_right = x + 0.12,
    y_text = y + 0.005
  )

emm_dtg_plot_prospe %>%
  ggplot(aes(x = from_start, y = prob, fill = condition_lab, shape = condition_lab)) + 
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.15, color = "#533846", position = position_dodge(width = 0.25)) +
  geom_line(aes(group = condition_lab), color = "#533846", position = position_dodge(width = 0.25)) +
  geom_point(size = 3, position = position_dodge(width = 0.25)) +
  geom_segment(data = ann_df_prosp, aes(x = x_left, xend = x_right, y = y, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_segment(data = ann_df_prosp, aes(x = x_left, xend = x_left, y = y - 0.004, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_segment(data = ann_df_prosp, aes(x = x_right, xend = x_right, y = y - 0.004, yend = y), inherit.aes = FALSE, color = "#533846") +
  geom_text(data = ann_df_prosp, aes(x = x, y = y_text, label = label), inherit.aes = FALSE, color = "#533846", size = 5) +
  scale_x_continuous(breaks = 0:5, labels = c("Start", "CP1", "CP2", "CP3", "CP4", "CP5")) +
  labs(
    x = "CP from the start",
    y = "Predicted Probability of GU",
    fill = "",
    shape = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c("#E93609", "#79CB6C")) +
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.85, 0.83), panel.grid.minor.x = element_blank()) + guides(fill = FALSE, shape = FALSE) +
  ylim(0, 0.2)

# Skills
# Retrospective Plot
emm_dtg_skill_df <- as.data.frame(emm_dtg_skill)

emm_dtg_skill_df <- emm_dtg_skill_df %>%
  tidyr::separate(CP_by_condition, into = c("condition", "CP"), sep = "\\_") %>%
  mutate(
    CP = factor(CP, levels = sort(unique(CP))),         # order CP numerically
    condition = factor(condition, levels = c("ShortCond", "LongCond")) # adjust levels if needed
  )

ann_df <- emm_dtg_skill_df %>%
  group_by(group_skill, CP) %>%
  summarise(
    y = max(prob + SE, na.rm = TRUE) + 0.015,
    .groups = "drop"
  ) %>%
  left_join(
    contrast_skill_specific_df,
    by = c("group_skill", "CP")
  ) %>%
  filter(!is.na(label)) %>%
  mutate(
    x = match(CP, rev(levels(emm_dtg_skill_df$CP))),
    x_left = x - 0.15,
    x_right = x + 0.15,
    y_text = y + 0.008
  )

emm_dtg_skill_df %>% mutate(condition = ifelse(condition == 'LongCond', 'Long (6 cp)', 'Short (3 cp)')) %>%
  mutate(CP = factor(CP, levels = rev(levels(CP)))) %>%
  ggplot(aes(x = CP, y = prob, fill = condition, shape = condition)) +
  facet_wrap(group_skill~.) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.2, position = position_dodge(width = 0.3)) +
  geom_line(aes(group = condition), position = position_dodge(width = 0.3)) +
  geom_point(size = 3, position = position_dodge(width = 0.3), show.legend = FALSE) +
  geom_segment(data = ann_df, aes(x = x_left, xend = x_right, y = y, yend = y), inherit.aes = FALSE) +
  geom_segment(data = ann_df, aes(x = x_left, xend = x_left, y = y - 0.006, yend = y), inherit.aes = FALSE) +
  geom_segment(data = ann_df, aes(x = x_right, xend = x_right, y = y - 0.006, yend = y), inherit.aes = FALSE) +
  geom_text(data = ann_df, aes(x = x, y = y_text, label = label), inherit.aes = FALSE, size = 5) +
  labs(
    x = "CP from the goal",
    y = "Pred. Probs. of GU",
    # title = "Model-predicted GU probability by CP and Condition",
    fill = "",
    shape = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c('#E93609', '#79CB6C', '#F9B734', '#4EC0CA')) +
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.9, 0.83)) + scale_y_continuous(limits = c(-0.02, 0.35), breaks = c(0, 0.10, 0.20, 0.30), labels = c("0.00", "0.10", "0.20", "0.30"))

# Prospective Plot
ann_df_prosp <- emm_dtg_skill_df %>%
  mutate(
    cp_num = as.numeric(stringr::str_remove(CP, "CP")),
    total_cp = ifelse(condition == "ShortCond", 3, 6),
    from_start = total_cp - cp_num
  ) %>%
  group_by(group_skill, from_start) %>%
  summarise(
    y = max(prob + SE, na.rm = TRUE) + 0.015,
    .groups = "drop"
  ) %>%
  left_join(
    contrast_skill_specific_prosp_df,
    by = c("group_skill", "from_start")
  ) %>%
  filter(!is.na(label)) %>%
  mutate(
    x = from_start,
    x_left = x - 0.12,
    x_right = x + 0.12,
    y_text = y + 0.008
  )

emm_dtg_skill_df %>%
  mutate(cp_num = as.numeric(stringr::str_remove(CP, "CP")), total_cp = ifelse(condition == "ShortCond", 3, 6), from_start = total_cp - cp_num, condition_lab = ifelse(condition == "LongCond", "Long (6 cp)", "Short (3 cp)")
  ) %>%
  ggplot(aes(x = from_start, y = prob, fill = condition_lab, shape = condition_lab)) +
  facet_wrap(group_skill ~ .) +
  geom_errorbar(aes(ymin = prob - SE, ymax = prob + SE), width = 0.15, position = position_dodge(width = 0.25)) +
  geom_line(aes(group = condition_lab), position = position_dodge(width = 0.25)) +
  geom_point(size = 3, position = position_dodge(width = 0.25), show.legend = FALSE) +
  geom_segment(data = ann_df_prosp, aes(x = x_left, xend = x_right, y = y, yend = y), inherit.aes = FALSE) +
  geom_segment(data = ann_df_prosp, aes(x = x_left, xend = x_left, y = y - 0.006, yend = y), inherit.aes = FALSE) +
  geom_segment(data = ann_df_prosp, aes(x = x_right, xend = x_right, y = y - 0.006, yend = y), inherit.aes = FALSE) +
  geom_text(data = ann_df_prosp,aes(x = x, y = y_text, label = label), inherit.aes = FALSE, size = 5) +
  scale_x_continuous(breaks = 0:5, labels = c("Start", "CP1", "CP2", "CP3", "CP4", "CP5")) +
  labs(
    x = "CP from the start",
    y = "Pred. Probs. of GU",
    fill = "",
    shape = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c("#E93609", "#79CB6C")) +
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.9, 0.83), panel.grid.minor.x = element_blank()) +
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks = c(0, 0.10, 0.20, 0.30),
    labels = c("0.00", "0.10", "0.20", "0.30")
  )

# ------------------------------------------------------------------------------
#  Visualisation Main Model (Figure 3)
# ------------------------------------------------------------------------------
emm_attempt_df <- as.data.frame(emm_attempt)

obs_means <- game_data_scaled %>%
  mutate(bin_attempt = cut(attemptIndex, breaks = 30)) %>%
  group_by(group_skill, bin_attempt) %>%
  summarise(
    attempt_mid = mean(attemptIndex),
    mean_prob = mean(response_binomial),
    n = n(),
    .groups = "drop"
  )

emm_attempt_df %>% mutate(attemptIndex_unscaled = attemptIndex * sd_trial + mean_trial) %>%
  ggplot(aes(x = attemptIndex, y = prob, color = group_skill, fill = group_skill)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA) +
    geom_point(data = obs_means, aes(x = attempt_mid, y = mean_prob, fill = group_skill, shape = group_skill), size = 1.5, alpha = 0.9, color = '#533846', show.legend = FALSE) +
    labs(
      x = "Attempt Index (scaled)",
      y = "Predicted Probability of Giving Up",
      color = "",
      fill = ""
    ) +
  theme_conan() +
  scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_color_manual(values = c('#F9B734', '#4EC0CA')) + 
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.2, 0.9))

unique_trials <- sort(unique(game_data_scaled$trialNumber))

emm_trial <- emmeans(
  model_final_unested,
  ~ trialNumber * group_skill,
  at = list(trialNumber = unique_trials),
  type = "response"
)

# ----- trial index
emm_trial_probs_df <- as.data.frame(emm_trial_probs)

mean_trial <- attr(scale(game_data_filtered$trialNumber), "scaled:center")
sd_trial   <- attr(scale(game_data_filtered$trialNumber), "scaled:scale")

obs_means <- game_data_filtered %>%
  mutate(bin_trial = cut(trialNumber, breaks = 30)) %>%
  group_by(group_skill, condition, bin_trial) %>%
  summarise(
    trial_mid = mean(trialNumber),
    mean_prob = mean(response_binomial),
    n = n(),
    .groups = "drop"
  )

emm_trial_probs_df %>% mutate(trialNumber_unscaled = trialNumber * sd_trial + mean_trial) %>%
  ggplot(aes(x = trialNumber_unscaled, y = prob, color = group_skill, fill = group_skill)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_point(data = obs_means, aes(x = trial_mid, y = mean_prob, fill = group_skill, shape = group_skill), size = 1.5, alpha = 0.9, color = '#533846', show.legend = FALSE) +
  labs(
    x = "Trial (index)",
    y = "Predicted Probability of Giving Up",
    color = "",
    fill = ""
  ) +
  theme_conan() +
  scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_color_manual(values = c('#F9B734', '#4EC0CA')) + 
  scale_shape_manual(values = c(21, 22)) +
  theme(legend.position = c(0.2, 0.9)) + scale_y_continuous(limits = c(0, 0.4), breaks = c(0, 0.1, 0.2, 0.3, 0.4), labels = c("0.00", "0.10", "0.20", "0.30", "0.40"))
