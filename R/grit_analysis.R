library(psych)

# ------ Item count / reliability check ------ #
grit_items <- grit_data %>%
  separate(responses, into = paste0("item", 1:12), sep = ",", convert = TRUE)

alpha(select(grit_items, starts_with("item")))

# ------ Restrict to analytic sample (post-exclusion N) ------ #

analytic_ids <- game_data_filtered %>% distinct(subject_nr) %>% pull(subject_nr)

grit_final <- grit_data %>% filter(participant %in% analytic_ids) %>% distinct(participant, .keep_all = TRUE)
setdiff(analytic_ids, grit_final$participant) 

# ------ Participant-level outcomes ------ #
disengage_rate <- game_data_filtered %>%
  group_by(subject_nr) %>%
  summarise(disengage_rate = mean(response_binomial), .groups = "drop")

skill_lookup <- game_data_filtered %>%
  group_by(subject_nr) %>%
  summarise(mean_pipes = mean(pipesPassed, na.rm = TRUE), .groups = "drop") %>%
  mutate(skill_cont = as.numeric(scale(mean_pipes)))

grit_merged <- grit_final %>%
  rename(subject_nr = participant) %>%
  left_join(disengage_rate, by = "subject_nr") %>%
  left_join(skill_lookup, by = "subject_nr")

stopifnot(sum(is.na(grit_merged$disengage_rate)) == 0)

# ------ Correlations (Spearman primary, Pearson secondary) ------ #
run_cor <- function(x, y, n) {
  s <- cor.test(x, y, method = "spearman", exact = FALSE)
  p <- cor.test(x, y, method = "pearson")
  ci <- r.con(unname(s$estimate), n)
  tibble(n = n, rho = unname(s$estimate), rho_lwr = ci[1], rho_upr = ci[2],
         p_spearman = s$p.value, r_pearson = unname(p$estimate), p_pearson = p$p.value)
}

res <- bind_rows(
  disengagement = run_cor(grit_merged$gritScore, grit_merged$disengage_rate, nrow(grit_merged)),
  skill         = run_cor(grit_merged$gritScore, grit_merged$skill_cont, nrow(grit_merged)),
  .id = "outcome"
)
res$p_holm <- p.adjust(res$p_spearman, method = "holm")
res
