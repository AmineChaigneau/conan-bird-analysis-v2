# ==============================================================================
# REVISION ANALYSIS -- Reviewer-requested checks on the disengagement GLMM
# Motivation Science submission -- response to Reviewer 1 (pt. 1, 2) & Reviewer 2 (pt. 2)
# ==============================================================================

library(tidyverse)
library(lme4)
library(emmeans)
library(car)
library(DHARMa)
library(broom.mixed)

set.seed(1234)

if (!exists("overdisp_fun")) {
  overdisp_fun <- function(model) {
    rdf <- df.residual(model)
    rp <- residuals(model, type = "pearson")
    Pearson.chisq <- sum(rp^2)
    prat <- Pearson.chisq / rdf
    pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
    c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
  }
}

dir.create("model_outputs", showWarnings = FALSE)

# ------------------------------------------------------------------------------
# STEP 0 -- Continuous skill variable
# ------------------------------------------------------------------------------
skill_lookup <- game_data_filtered %>%
  group_by(subject_nr) %>%
  summarise(mean_pipes_passed = mean(pipesPassed, na.rm = TRUE), .groups = "drop") %>%
  mutate(skill_cont = as.numeric(scale(mean_pipes_passed)))

skill_lookup <- skill_lookup %>%
  left_join(game_data_filtered %>% distinct(subject_nr, group_skill), by = "subject_nr")

t.test(mean_pipes_passed ~ group_skill, data = skill_lookup)
cor.test(skill_lookup$skill_cont, as.numeric(as.factor(skill_lookup$group_skill)))

ggplot(skill_lookup, aes(x = group_skill, y = mean_pipes_passed)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  labs(x = "", y = "Mean obstacles passed / attempt (raw)") +
  theme_conan()
# ggsave("model_outputs/skill_split_vs_continuous_sanity_check.png", width = 5, height = 4)

game_data_scaled <- game_data_scaled %>%
  left_join(skill_lookup %>% select(subject_nr, skill_cont), by = "subject_nr")

stopifnot(sum(is.na(game_data_scaled$skill_cont)) == 0)

# ------------------------------------------------------------------------------
# STEP 1 -- Random-effects structure (binary skill, published fixed effects)
# ------------------------------------------------------------------------------

# ------ Baseline (= submitted model_final) ------ #
model_r00 <- glmer(
  response_binomial ~ end_distance_to_goal + group_skill + difficulty +
    poly(attemptIndex, 2) + poly(pipesPassed, 2) + poly(trialNumber, 2) +
    poly(attemptIndex, 2):group_skill + end_distance_to_goal:group_skill +
    poly(trialNumber, 2):group_skill +
    (1 | subject_nr),
  data = game_data_scaled, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model_r00)$AIC
isSingular(model_r00)

# ------ Candidate slopes (added one at a time, zero-correlation) ------ #
model_r01 <- update(model_r00, . ~ . + (0 + end_distance_to_goal || subject_nr))
anova(model_r00, model_r01)
isSingular(model_r01)

model_r02 <- update(model_r00, . ~ . + (0 + attemptIndex || subject_nr))
anova(model_r00, model_r02)
isSingular(model_r02)

model_r03 <- update(model_r00, . ~ . + (0 + difficulty || subject_nr))
anova(model_r00, model_r03)
isSingular(model_r03)

model_r04 <- update(model_r00, . ~ . + (0 + pipesPassed || subject_nr))
anova(model_r00, model_r04)
isSingular(model_r04)

model_r05 <- update(model_r00, . ~ . + (0 + trialNumber || subject_nr))
anova(model_r00, model_r05)
isSingular(model_r05)

# ------ Combine end_distance_to_goal + attemptIndex (uncorrelated) ------ #
model_r06 <- update(
  model_r00,
  . ~ . + (0 + end_distance_to_goal || subject_nr) + (0 + attemptIndex || subject_nr)
)
anova(model_r00, model_r06)
isSingular(model_r06)
summary(rePCA(model_r06))

# ------ Allow those slopes to correlate with intercept ------ #
model_r07 <- update(
  model_r06,
  . ~ . - (1 | subject_nr) - (0 + end_distance_to_goal || subject_nr) - (0 + attemptIndex || subject_nr)
  + (1 + end_distance_to_goal + attemptIndex | subject_nr)
)
anova(model_r06, model_r07)
isSingular(model_r07)

# ------ Confirmatory check: trialNumber on top of r07 ------ #
model_r12 <- update(model_r07, . ~ . + (0 + trialNumber || subject_nr))
anova(model_r07, model_r12)
isSingular(model_r12)

# ------ Re-check remaining candidates against the combined structure ------ #
model_r13 <- update(model_r12, . ~ . + (0 + pipesPassed || subject_nr))
anova(model_r12, model_r13)
isSingular(model_r13)
summary(rePCA(model_r13))

model_r14 <- update(model_r12, . ~ . + (0 + difficulty || subject_nr))
anova(model_r12, model_r14)
isSingular(model_r14)

# ------ Final random-effects structure (binary skill) ------ #
model_r_final <- model_r13

car::vif(model_r_final)
overdisp_fun(model_r_final)
res_r_final <- simulateResiduals(model_r_final, n = 1000)
plot(res_r_final)
summary(model_r_final) # Final model reported in the paper 
confint(model_r_final, method = "Wald")

re_terms <- "(1 + end_distance_to_goal + attemptIndex | subject_nr) + (0 + trialNumber || subject_nr) + (0 + pipesPassed || subject_nr)"
cov_terms <- "end_distance_to_goal + poly(attemptIndex, 2) + poly(pipesPassed, 2) + poly(trialNumber, 2)"

# ------------------------------------------------------------------------------
# STEP 2 -- Continuous skill, original random-effects structure
# ------------------------------------------------------------------------------

# ------ Model 1 (continuous) ------ #
model_c001 <- glmer(
  response_binomial ~ end_distance_to_goal + skill_cont + difficulty +
    poly(attemptIndex, 2) + poly(pipesPassed, 2) + poly(trialNumber, 2) + (1 | subject_nr),
  data = game_data_scaled, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
summary(model_c001)$AIC

# ------ Adding interactions (each tested against model_c001) ------ #
model_c002 <- update(model_c001, . ~ . + end_distance_to_goal:skill_cont)
anova(model_c001, model_c002)

model_c003 <- update(model_c001, . ~ . + poly(attemptIndex, 2):skill_cont)
anova(model_c001, model_c003)

model_c004 <- update(model_c001, . ~ . + poly(pipesPassed, 2):skill_cont)
anova(model_c001, model_c004)

model_c005 <- update(model_c001, . ~ . + poly(trialNumber, 2):skill_cont)
anova(model_c001, model_c005)

# ------ Multi-interaction chain (each tested against its true parent) ------ #
model_c006 <- update(model_c003, . ~ . + end_distance_to_goal:skill_cont)
anova(model_c003, model_c006)

model_c007 <- update(model_c006, . ~ . + poly(trialNumber, 2):skill_cont)
anova(model_c006, model_c007)

# ------ Leaner alternative: skip the ns distance x skill term ------ #
model_c008 <- update(model_c003, . ~ . + poly(trialNumber, 2):skill_cont)
anova(model_c003, model_c008)
AIC(model_c007, model_c008)

# ------ Final model (continuous skill, RI only) ------ #
model_c_final <- model_c008

car::vif(model_c_final)
overdisp_fun(model_c_final)
res_c_final <- simulateResiduals(model_c_final, n = 1000)
plot(res_c_final)
summary(model_c_final)
confint(model_c_final, method = "Wald")

# ------------------------------------------------------------------------------
# STEP 3 -- Continuous skill + revised random-effects structure
# ------------------------------------------------------------------------------
# Random part must match model_r_final from Step 1.

model_rc_final <- update(
  model_c_final,
  . ~ . - (1 | subject_nr) + (1 + end_distance_to_goal + attemptIndex | subject_nr)
  + (0 + trialNumber || subject_nr) + (0 + pipesPassed || subject_nr)
)
isSingular(model_rc_final)
summary(rePCA(model_rc_final))

car::vif(model_rc_final)
overdisp_fun(model_rc_final)
res_rc_final <- simulateResiduals(model_rc_final, n = 1000)
plot(res_rc_final)
summary(model_rc_final)
confint(model_rc_final, method = "Wald")

# ------------------------------------------------------------------------------
# Summary table -- every candidate model (send this back)
# ------------------------------------------------------------------------------
all_models <- list(
  r00 = model_r00, r01 = model_r01, r02 = model_r02, r03 = model_r03,
  r04 = model_r04, r05 = model_r05, r06 = model_r06, r07 = model_r07, r12 = model_r12, r13 = model_r13, r14 = model_r14,
  c001 = model_c001, c002 = model_c002, c003 = model_c003, c004 = model_c004,
  c005 = model_c005, c006 = model_c006, c007 = model_c007, c008 = model_c008,
  rc_final = model_rc_final
)

fit_summary_all <- tibble(
  model    = names(all_models),
  npar     = map_dbl(all_models, ~ attr(logLik(.x), "df")),
  AIC      = map_dbl(all_models, AIC),
  BIC      = map_dbl(all_models, BIC),
  logLik   = map_dbl(all_models, ~ as.numeric(logLik(.x))),
  singular = map_lgl(all_models, isSingular)
)
fit_summary_all

# ------ Final models only (published vs. the three revised specs) ------ #
model_list <- list(
  "Published (binary skill, RI only)"        = model_final,
  "Step 1: binary skill + random slopes"     = model_r_final,
  "Step 2: continuous skill, RI only"        = model_c_final,
  "Step 3: continuous skill + random slopes" = model_rc_final
)

fit_summary <- tibble(
  model    = names(model_list),
  AIC      = map_dbl(model_list, AIC),
  BIC      = map_dbl(model_list, BIC),
  logLik   = map_dbl(model_list, ~ as.numeric(logLik(.x))),
  n_obs    = map_dbl(model_list, nobs),
  singular = map_lgl(model_list, isSingular)
)
fit_summary

fixed_effects_all <- map_dfr(
  model_list, broom.mixed::tidy, effects = "fixed", conf.int = TRUE, .id = "model"
)
fixed_effects_all

# ------------------------------------------------------------------------------
# Marginal means for plotting
# ------------------------------------------------------------------------------
attempt_grid <- list(
  attemptIndex = seq(min(game_data_scaled$attemptIndex), max(game_data_scaled$attemptIndex), length.out = 100)
)
emm_attempt_r <- emmeans(model_r_final, ~ attemptIndex * group_skill, at = attempt_grid, type = "response")

trial_grid <- list(
  trialNumber = seq(min(game_data_scaled$trialNumber), max(game_data_scaled$trialNumber), length.out = 100)
)
emm_trial_r <- emmeans(model_r_final, ~ trialNumber * group_skill, at = trial_grid, type = "response")

skill_grid <- list(skill_cont = c(-1, 0, 1))
emm_attempt_rc <- emmeans(model_rc_final, ~ attemptIndex * skill_cont,
                          at = c(attempt_grid, skill_grid), type = "response")
emm_trial_rc   <- emmeans(model_rc_final, ~ trialNumber * skill_cont,
                          at = c(trial_grid, skill_grid), type = "response")

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------

saveRDS(all_models, "model_outputs/all_candidate_models.rds")
saveRDS(model_list, "model_outputs/final_models_list.rds")
saveRDS(list(emm_attempt_r = emm_attempt_r, emm_trial_r = emm_trial_r,
             emm_attempt_rc = emm_attempt_rc, emm_trial_rc = emm_trial_rc),
        "model_outputs/emmeans_for_plots.rds")

write_csv(fit_summary_all, "model_outputs/fit_summary_all_candidates.csv")
write_csv(fit_summary, "model_outputs/fit_summary_final_models.csv")
write_csv(fixed_effects_all, "model_outputs/fixed_effects_final_models.csv")
write_csv(skill_lookup, "model_outputs/skill_continuous_vs_split_lookup.csv")

# ------------------------------------------------------------------------------
# STEP 4 -- Refit Alternative Model Distance by Condition
# ------------------------------------------------------------------------------

# ------ Baseline: CP_by_condition without the skill interaction ------ #
model_cpA00 <- glmer(
  response_binomial ~ CP_by_condition + CP_by_condition:group_skill + (1 | subject_nr),
  data = game_data_scaled, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)

summary(model_cpA00)$AIC
isSingular(model_cpA00)

# slope for distance from goal (parallels the main model)
model_cpB1 <- update(model_cpA00, . ~ . - (1 | subject_nr) +
                       (1 + end_distance_to_goal | subject_nr))

anova(model_cpA00, model_cpB1)

# add a slope for level length (replace `condition` with your variable name)
model_cpB2 <- update(model_cpA00, . ~ . - (1 | subject_nr) +
                       (1 + end_distance_to_goal + condition | subject_nr))

anova(model_cpA00, model_cpB2)

anova(model_cpA00, model_cpB1, model_cpB2)
isSingular(model_cpB1); isSingular(model_cpB2)

# ------ Summary ------ #
cpA_candidates <- list(baseline = model_cpA00, distance = model_cpB1,
                       distanceCondition = model_cpB2)

# Contrast
model_CP_by_Condition <- model_cpB2

# ==============================================================================
# R1 point 4 -- learning vs. motivation account of the first-checkpoint effect
# ==============================================================================

# ------ Block position: was this condition's block encountered first or second? ------ #
game_data_scaled <- game_data_scaled %>%
  mutate(block_position = factor(case_when(
    condition == "LongCond"  & group_block == "Long-first"  ~ "first",
    condition == "ShortCond" & group_block == "Short-first" ~ "first",
    TRUE ~ "second"
  )))

# ------ Restrict to the two CP1-transition checkpoints per condition ------ #
long_cp1 <- game_data_scaled %>%
  filter(CP_by_condition %in% c("LongCond_6CP", "LongCond_5CP")) %>%
  mutate(CP1 = droplevels(factor(CP_by_condition)))

short_cp1 <- game_data_scaled %>%
  filter(CP_by_condition %in% c("ShortCond_3CP", "ShortCond_2CP")) %>%
  mutate(CP1 = droplevels(factor(CP_by_condition)))

# ------ Sparsity check ------ #
bind_rows(long_cp1, short_cp1) %>%
  count(CP_by_condition, block_position, response_binomial) %>%
  pivot_wider(names_from = response_binomial, values_from = n, values_fill = 0)

# ------ Fit: does block position moderate the CP1 transition? ------ #
fit_cp1 <- function(d) {
  m1 <- glmer(response_binomial ~ CP1 + block_position + difficulty + (1 | subject_nr),
              data = d, family = binomial, control = glmerControl(optimizer = "bobyqa"))
  m2 <- update(m1, . ~ . + CP1:block_position)
  list(main = m1, interaction = m2, lrt = anova(m1, m2))
}

long_fit  <- fit_cp1(long_cp1)
short_fit <- fit_cp1(short_cp1)

isSingular(long_fit$interaction)
isSingular(short_fit$interaction)
long_fit$lrt
short_fit$lrt

p.adjust(c(long = long_fit$lrt$`Pr(>Chisq)`[2], short = short_fit$lrt$`Pr(>Chisq)`[2]), method = "holm")

# ------ Predicted probabilities by block position ------ #
emmeans(long_fit$interaction,  ~ CP1 * block_position, type = "response")
emmeans(short_fit$interaction, ~ CP1 * block_position, type = "response")

# ------ Complementary check: does performance improve across the experiment? ------ #
perf_trend <- glmer(
  pipesPassed ~ poly(trialNumber, 2) + condition + difficulty + (1 | subject_nr),
  data = game_data_filtered, family = poisson,
  control = glmerControl(optimizer = "bobyqa")
)
summary(perf_trend)


