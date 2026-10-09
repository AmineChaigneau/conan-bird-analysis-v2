# --------------------------------------------
# Ngu -3, -2, -1
# --------------------------------------------
source(file = "mt_data_processing.R")

attempt_level <- game_data_filtered %>%
  group_by(uniqueId, trialId, subject_nr, attemptIndex, trialNumber) %>% 
  summarise(response = first(response)) %>% arrange(subject_nr, trialNumber, attemptIndex) %>%
  ungroup()

attempt_level <- attempt_level %>%
  group_by(trialId) %>%
  mutate(
    gu_attemptIndex = attemptIndex[response == "easier"][1],
    nGu = attemptIndex - gu_attemptIndex,
    nGu = ifelse(nGu > 0, NA, nGu)
  ) %>%
  ungroup() %>%
  select(-gu_attemptIndex)

attempt_before_gu <- attempt_level %>% drop_na(nGu) %>% select(uniqueId, nGu)

game_data_filtered_nGu <- game_data_filtered %>% left_join(attempt_before_gu, by = "uniqueId") %>% filter(nGu >= -3)

library(ordinal)

game_data_filtered_nGu$nGu <- factor(game_data_filtered_nGu$nGu, levels = c(-3, -2, -1), ordered = TRUE)

game_data_filtered_nGu <- game_data_filtered_nGu %>% drop_na(nGu) # remove 0

game_data_filtered_nGu <- game_data_filtered_nGu %>% mutate(condition = ifelse(condition == "LongCond", "Long (6 cp)", "Short (3 cp)"))

# --------------------------------------------
# About Performance: Pipes Passed
# --------------------------------------------

# ----- Poisson
m_pipesPassed_pois <- glmer(
  pipesPassed ~ nGu * group_skill * condition +
    (1 | subject_nr),
  family = poisson(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ),
  data = game_data_filtered_nGu
)

par(mfrow = c(1, 2))
plot(m_pipesPassed_pois, which = 1)      # résidus vs fitted
qqnorm(resid(m_pipesPassed_pois)) 
qqline(resid(m_pipesPassed_pois))

summary(m_pipesPassed_pois) 

# ----- Exclude interactions 
m_pipesPassed_pois_1 <- glmer(
  pipesPassed ~ nGu + nGu:group_skill + nGu:condition + # nGu:trialNumber +
    (1 | subject_nr),
  family = poisson(link = "log"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  ),
  data = game_data_filtered_nGu
)

par(mfrow = c(1, 2))
plot(m_pipesPassed_pois_1, which = 1)      # résidus vs fitted
qqnorm(resid(m_pipesPassed_pois_1)) 
qqline(resid(m_pipesPassed_pois_1))

summary(m_pipesPassed_pois_1) #1724.5

# ----- emmeans

emm <- emmeans(
  m_pipesPassed_pois,
  ~ nGu | group_skill * condition,
  type = "response"   # very important for Poisson
)

emm_df <- as.data.frame(emm)

# ----- Visualization
ggplot(emm_df, aes(x = nGu, y = rate, fill = group_skill)) +
  geom_line(aes(group = group_skill),
            position = position_dodge(0.22),
            linewidth = 0.6) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL,
                    group = group_skill),
                width = 0.12,
                linewidth = 0.4,
                position = position_dodge(0.22)) +
  geom_point(aes(shape = group_skill),
             position = position_dodge(0.22),
             size = 2.5) +
  facet_wrap(~ condition, ncol = 2) +
  labs(
    x = expression(italic(n)~"Gu"),
    y = "Predicted Pipes Passed",
    fill = "",
    shape = ""
  ) +
  scale_color_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_fill_manual(values = c('#F9B734', '#4EC0CA')) +
  scale_shape_manual(values = c(21, 22)) +
  theme_conan() + theme(legend.position = c(0.9, 0.83))
