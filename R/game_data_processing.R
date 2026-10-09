# ---------------------------------------------------------------------------
# Game Data Processing
# ---------------------------------------------------------------------------
game_data <- filtered_data %>% rename(subject_nr = participant, response = choice, response_binomial = choice_binomial) %>% 
  select(trialNumber, condition, difficulty, attemptIndex, checkpoint, pipesPassed, jumpCount, time, subject_nr, response_binomial, response, reward, level)

# Create uniqueIds
game_data <- game_data %>% mutate(
  trialId = as.factor(paste0(trialNumber, '_', subject_nr)),
) %>% relocate(trialId, subject_nr)

# Compute experiment cumulative var
game_data <- game_data %>% group_by(subject_nr) %>% mutate(
  cumJumpCount = cumsum(jumpCount),
  all_cumPipesPassed = cumsum(pipesPassed),
) %>% ungroup

table(game_data$response)
game_data_all <- game_data
game_data <- game_data %>% filter(response != 'none')

# ------ Distance to the Goal ------ #
game_data <- game_data %>% mutate(checkpoint = checkpoint/4)

game_data <- game_data %>%
  group_by(subject_nr, trialId) %>%
  arrange(attemptIndex, .by_group = TRUE) %>%
  mutate(
    max_checkpoint = ifelse(condition == "ShortCond", 3, 6),
    start_distance_to_goal = max_checkpoint - checkpoint,
    next_checkpoint = lead(checkpoint),
    end_distance_to_goal = case_when(
      !is.na(next_checkpoint) ~ max_checkpoint - next_checkpoint,
      TRUE ~ max_checkpoint - checkpoint  # If no next attempt, stay with current
    ),
  ) %>%
  select(-next_checkpoint, -max_checkpoint) %>%
  ungroup()

game_data <- game_data %>% relocate(trialId:attemptIndex, pipesPassed:time, checkpoint, start_distance_to_goal, end_distance_to_goal, reward, response_binomial, response)

# ------ Proportional progression ------ #
game_data <- game_data %>% mutate(
  goal_proximity_start = ifelse(condition == "ShortCond", start_distance_to_goal / 3, start_distance_to_goal / 6),
  goal_proximity_end = ifelse(condition == "ShortCond", end_distance_to_goal / 3, end_distance_to_goal / 6)
)

# ------ Cumulative Pipes and Trial Output ------ #
game_data <- game_data %>% group_by(trialId) %>%
  mutate(
    final_output = if_else(last(response) == "easier", "give_up", "completed"),
    cumPipesPassed = cumsum(pipesPassed)
  ) %>% ungroup

game_data <- game_data %>% relocate(trialId:end_distance_to_goal, cumPipesPassed, cumJumpCount, goal_proximity_start:final_output, response, response_binomial)

# ------ Skill Group ------ #
median_pipes_passed <- game_data %>% group_by(subject_nr) %>% summarise(avg_pipes = mean(pipesPassed, na.rm = TRUE)) %>% ungroup %>% summarise(median = median(avg_pipes)) %>% pull

skilled_participants <- game_data %>% group_by(subject_nr) %>% summarise(avg_pipes = mean(pipesPassed, na.rm = TRUE)) %>% ungroup %>% filter(avg_pipes > median_pipes_passed) %>% select(subject_nr) %>% pull
unskilled_participants <- game_data %>% group_by(subject_nr) %>% summarise(avg_pipes = mean(pipesPassed, na.rm = TRUE)) %>% ungroup %>% filter(avg_pipes < median_pipes_passed) %>% select(subject_nr) %>% pull

game_data <- game_data %>% mutate(group_skill = ifelse(subject_nr %in% skilled_participants, 'Skilled', 'unSkilled'))

# ------ block Group ------ #
block_order <- game_data %>% group_by(subject_nr) %>% select(subject_nr, trialNumber, condition) %>% filter(trialNumber == 1) %>% distinct() %>%
  rename(group_block = condition) %>% mutate(group_block = ifelse(group_block == "LongCond", 'Long-first', 'Short-first')) %>% select(-trialNumber)

game_data <- game_data %>% left_join(block_order, by = 'subject_nr')

# ------------------------------------------------------------------------------
# Convert and Rename Factors
# ------------------------------------------------------------------------------
game_data <- game_data %>%
  mutate(
    end_distance_to_goal_f = as.factor(paste0(end_distance_to_goal, "CP")),
    start_distance_to_goal_f = as.factor(paste0(start_distance_to_goal, "CP")),
    checkpoint_f = as.factor(paste0(checkpoint, "CP")),
    condition = as.factor(condition),
    difficulty = as.factor(difficulty)
  )

# ------------------------------------------------------------------------------
# Game Data Outliers Filter
# ------------------------------------------------------------------------------

# ----- Filter ------ #
trial_to_filter <- game_data %>% summarise_attempts() %>% trial_to_removed 
game_data_filtered <- game_data %>% filter(trialId %!in% trial_to_filter) 

game_data_filtered <- game_data_filtered %>% mutate(uniqueId = paste0(trialId, '_', attemptIndex))

# ------------------------------------------------------------------------------
# Game Data Scale
# ------------------------------------------------------------------------------
# ------ Overall scaling ------ #
game_data_scaled <- game_data_filtered %>% mutate(
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
game_data_scaled <- game_data_scaled %>% group_by(subject_nr) %>% mutate(
  attemptIndex = scale(attemptIndex),
  pipesPassed = scale(pipesPassed),
  cumPipesPassed = scale(cumPipesPassed),
  cumJumpCount = scale(cumJumpCount),
  all_cumPipesPassed = scale(all_cumPipesPassed),
) %>% ungroup

# ------------------------------------------------------------------------------
# Export  
# ------------------------------------------------------------------------------
# write.csv(x = game_data_scale_filtered, './output/game_data_processed_scaled.csv')
# write.csv(x = game_data_filtered, './output/game_data_processed.csv')
