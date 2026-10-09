# ==============================================================================
# R2 point 3 -- Failure likelihood as a predictor of disengagement
# ==============================================================================

library(tidyverse)
library(lme4)

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# ------------------------------------------------------------------------------
# STEP 0 -- Obstacle-level hazard data (crashes + completions)
# ------------------------------------------------------------------------------
skill_lookup <- game_data_filtered %>%
  group_by(subject_nr) %>%
  summarise(mean_pipes = mean(pipesPassed, na.rm = TRUE), .groups = "drop") %>%
  mutate(skill_cont = as.numeric(scale(mean_pipes))) %>%
  left_join(game_data_filtered %>% distinct(subject_nr, group_skill), by = "subject_nr")

hazard_data <- game_data_all %>%
  filter(trialId %!in% trial_to_filter, subject_nr %in% skill_lookup$subject_nr) %>%
  group_by(trialId) %>%
  arrange(attemptIndex, .by_group = TRUE) %>%
  mutate(is_last = row_number() == n()) %>%
  ungroup() %>%
  # mid-trial "none" = unrecorded choice; further setting=='save' attempts exist afterward,
  # which is only possible if that choice was "save" (a disengage would drop the trial from
  # this filtered-to-'save' dataset from that point on) -- recode accordingly, not dropped
  mutate(response_fixed = if_else(response == "none" & !is_last, "save", as.character(response)),
         outcome = factor(case_when(response_fixed == "none"  & is_last ~ "completed",
                                    response_fixed == "save"             ~ "persist",
                                    response_fixed == "easier"           ~ "disengage"),
                          levels = c("persist", "disengage", "completed")),
         crash = as.integer(outcome != "completed"),
         difficulty = factor(difficulty, levels = levels(game_data_scaled$difficulty))) %>%
  left_join(skill_lookup %>% select(subject_nr, skill_cont, group_skill), by = "subject_nr")

table(hazard_data$outcome, useNA = "ifany")   # no NA expected

# ------------------------------------------------------------------------------
# STEP 1 -- Stage 1: per-obstacle failure probability ~ skill x difficulty
# ------------------------------------------------------------------------------
stage1_00 <- glmer(cbind(crash, pipesPassed) ~ group_skill + difficulty + (1 | subject_nr),
                   data = hazard_data, family = binomial, control = ctrl)
summary(stage1_00)

stage1_01 <- update(stage1_00, . ~ . + group_skill:difficulty)
anova(stage1_00, stage1_01)

stage1_final <- stage1_01
summary(stage1_final)

# ------------------------------------------------------------------------------
# STEP 2 -- Predicted failure likelihood per attempt (fixed effects only)
# ------------------------------------------------------------------------------
game_data_scaled$fail_logit <- predict(stage1_final, newdata = game_data_scaled, re.form = NA, type = "link")
game_data_scaled$fail_z     <- as.numeric(scale(game_data_scaled$fail_logit))

# need re_terms and cov_terms from response_models_revision.R

fit_glmm <- function(rhs) {
  glmer(as.formula(paste("response_binomial ~", rhs)),
        data = game_data_scaled, family = binomial, control = ctrl)
}

# ------------------------------------------------------------------------------
# STEP 3 -- Nested test: does failure likelihood summarize skill + difficulty?
# ------------------------------------------------------------------------------
# Main-effects-only versions: fail_z is an exact linear combination of the
# group_skill/difficulty terms here (no interactions added), so m_fail is
# nested in m_sep -- this is the one valid likelihood-ratio test in this script.
m_sep  <- fit_glmm(paste(cov_terms, "+ group_skill * difficulty +", re_terms))
m_fail <- fit_glmm(paste(cov_terms, "+ fail_z +", re_terms))

isSingular(m_sep)
isSingular(m_fail)
anova(m_fail, m_sep)

# ------------------------------------------------------------------------------
# STEP 4 -- Descriptive comparison: full models, with attempt/trial interactions
# ------------------------------------------------------------------------------
# NOT nested (fail_z's interactions are freely estimated, not derived from
# stage 1) -- AIC/BIC comparison only, no anova()/LRT/p-value here.
m_ref_full  <- fit_glmm(paste(cov_terms, "+ group_skill + difficulty + group_skill:poly(attemptIndex, 2) + end_distance_to_goal:group_skill + group_skill:poly(trialNumber, 2) +", re_terms))
m_fail_full <- fit_glmm(paste(cov_terms, "+ fail_z + fail_z:poly(attemptIndex, 2) + end_distance_to_goal:fail_z + fail_z:poly(trialNumber, 2) +", re_terms))

AIC(m_ref_full)   # should match model_r_final, 3100.3
AIC(m_fail_full) 
isSingular(m_ref_full)
isSingular(m_fail_full)

tibble(model = c("skill + difficulty (full)", "failure likelihood (full)"),
       npar  = c(attr(logLik(m_ref_full), "df"), attr(logLik(m_fail_full), "df")),
       AIC   = c(AIC(m_ref_full), AIC(m_fail_full)),
       BIC   = c(BIC(m_ref_full), BIC(m_fail_full)))
