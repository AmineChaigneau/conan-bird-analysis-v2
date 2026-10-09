# ------------------------------------------------------------------------------
# Extract Raw Data from the raw data folder
# ------------------------------------------------------------------------------

# ------ Get all participant folders and filter participant folders by "mode" in utils_data.json ------ #
folder_name <- 'participants_002'
participant_folders <- paste0("data/raw/", folder_name,"/", list.files('./data/raw/participants_002/'))
mode_list <- c('pre_experiment_prolific_2', 'pre_experiment_prolific', 'experiment_1', 'experiment_2')

{
  # Read utils data (demographics)
  utils <- participant_folders %>%
    map_dfr(function(participant_path) {
      utils_data_path <- file.path(participant_path, "utils_data.json")
      if (file.exists(utils_data_path)) {
        utils_data <- fromJSON(utils_data_path)
        tibble(participant_id = basename(participant_path)) %>%
          bind_cols(as_tibble(utils_data))
      }
    }) %>% select(-levelsOrderShort, -levelsOrderLong) %>% distinct() %>%
    filter(mode %in% mode_list) %>% filter(pseudo %!in% c("AmineTest", "Aurélien"))
  
  filtered_participant_folders <- utils$participant_id %>%
    map_chr(~ file.path(paste0("data/raw/", folder_name), .))
  
  # Read trial data
  trial_data <- filtered_participant_folders %>%
    map_dfr(function(participant_path) {
      participant_id <- basename(participant_path)
      print(participant_id)
      trials_path <- file.path(participant_path, "trials")
      if (dir.exists(trials_path)) {
        read_trial_data(trials_path) %>%
          mutate(participant_id = participant_id)
      }
    })
  
  # Read grit data
  grit_data <- filtered_participant_folders %>%
    map_dfr(function(participant_path) {
      grit_path <- file.path(participant_path, "/scales/grit.json")
      if (file.exists(grit_path)) {
        read_grit_data(participant_path) %>%
          mutate(participant_id = basename(participant_path))
      }
    })
  
  # Transformation
  trial_data <- trial_data %>% mutate(choice_binomial = ifelse(choice == "save", 0, 1))
  # trial_data <- trial_data %>% mutate(participant_id = as.factor(participant_id))
  
  trial_data <- trial_data %>% mutate(participant = as.factor(paste0('Participant ', as.numeric(as.factor(participant_id)))))
  utils <- utils %>% mutate(participant = as.factor(paste0('Participant ', as.numeric(as.factor(participant_id)))))
  
  p <- data.frame(participant = utils$participant, participant_id = utils$participant_id)
  grit_data <- grit_data %>% left_join(p, by = 'participant_id') %>% select(-participantId, -file_path)
  rm(p)
  
  trial_data <- trial_data %>% mutate(difficulty = ifelse(difficulty == 'Easy', 'Low', 'High'))
  # write_csv(trial_data, "./output/trial_data.csv")
}

# ------------------------------------------------------------------------------
# Filtered Dataset
# ------------------------------------------------------------------------------

# ------ Remove all attempts after an easier choice (not interesting) ------ #
filtered_data <- trial_data %>% filter(setting == 'save')
filtered_data <- filtered_data %>% mutate(participant = as.factor(participant), condition = as.factor(condition), difficulty = as.factor(difficulty))

# Add reward by trial 
reward <- filtered_data %>% filter(attemptIndex == 0) %>% group_by(participant) %>% arrange(trialNumber) %>%
  summarise(trialNumber = trialNumber, reward = cumsum(rewardEarned)) %>% ungroup

filtered_data <- filtered_data %>% left_join(reward, by = c('participant', 'trialNumber'))
rm(reward)

# ------------------------------------------------------------------------------
# Filtered participants
# ------------------------------------------------------------------------------

participant_list <- filtered_data %>% group_by(participant, choice) %>% summarise(n = n()) %>% filter(choice == "easier" & n <= 30) %>% select(participant) %>% pull
filtered_data %>% group_by(participant) %>% summarise(n = n()) %>%
  ggplot() + 
  geom_density(aes(x = n), color = '#E93609', fill = '#E93609', alpha = 0.5) +
  geom_vline(aes(xintercept = 60)) +
  theme_conan() + scale_x_continuous(breaks = seq(0, 750, 50)) +
  labs(title = "Distriution n of observation (attempts)",
       x = "Density",
       y = "n of observation")
# ggsave('./output/plots/observation_density.png', height = 3, width = 7)

participant_list <- filtered_data %>% group_by(participant) %>% summarise(n = n()) %>% filter(n > 60) %>% select(participant) %>% pull
participants_to_remove <- filtered_data %>% distinct(participant) %>% filter(!participant %in% participant_list) %>% pull(participant)

removed_data <- filtered_data %>% filter(participant %in% participants_to_remove)
filtered_data <- filtered_data %>% filter(participant %in% participant_list)







