# --------------------------------------------
# Game Area Constants
# --------------------------------------------
height_clickArea <- 65
width_clickArea <- 435

width_gameArea <- 435
height_gameArea <- 770 + height_clickArea

buttons <- data.frame(
  id = c("retry", "make it easier"),
  x = c(-(width_clickArea / 2), (width_clickArea / 2) - 96),        # Top-left x positions
  y = c(height_gameArea - 84, height_gameArea - 84), # Top-left y positions
  width = c(96, 96),
  height = c(84, 84)
)

start_button <- data.frame(
  x = -(107.75 / 2),
  y = 0,
  height = height_clickArea,
  width = 107.75
)

ggplot() +
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "white", color = "black", inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "#FC1717", color = "black") + 
  ylim(0, height_gameArea) + xlim(-width_gameArea/2, width_gameArea/2) + theme_minimal()

# --------------------------------------------
# Data Transformation
# --------------------------------------------

# Create uniqueIds
mt_filter_data <- filtered_data %>% rename(subject_nr = participant) %>% mutate(
  trialId = as.factor(paste0(trialNumber, '_', subject_nr)),
) %>% select(trialId, attemptIndex, trackingData)

mouseTrap_data <- game_data_filtered %>% left_join(mt_filter_data, by = c('trialId', 'attemptIndex')) %>% select(-time)

# --------------------------------------------
# Step 0: Filtering based on Attempts outliers and Data collection issues
# --------------------------------------------

# Filter based on Attempt Number
mouseTrap_data_filtered_attempt <- mouseTrap_data %>% filter(trialId %!in% trial_to_filter)

# Check original counts
original_attempt_count <- nrow(mouseTrap_data_filtered_attempt)
original_participant_count <- n_distinct(mouseTrap_data_filtered_attempt$subject_nr)

# First pass filtering: Remove invalid tracking data
mouseTrap_data_filtered <- mouseTrap_data %>%
  mutate(
    trackingData = map(trackingData, ~ if(is.list(.x)) as.data.frame(.x) else .x),
    is_valid = map_lgl(trackingData, ~ 
                         is.data.frame(.x) && 
                         nrow(.x) >= 10 && 
                         all(c("x", "y", "time") %in% names(.x)))) 

# Track removed attempts
removed_attempts <- mouseTrap_data_filtered %>% 
  filter(!is_valid) %>%
  count(subject_nr, name = "removed_attempts")

removed_attempts %>% summarise(sum(removed_attempts))

# Apply first filter
mouseTrap_data_filtered_mt <- mouseTrap_data_filtered %>% 
  filter(is_valid) %>%
  select(-is_valid)

# --------------------------------------------
# Participant-level Filtering (20%+ removed)
# Remove participant with not enough choice observation
# --------------------------------------------

# Calculate removal rates
participant_removal_stats <- mouseTrap_data_filtered_mt %>%
  group_by(subject_nr) %>%
  summarise(
    remaining_attempts = n(),
    .groups = "drop"
  ) %>%
  left_join(removed_attempts, by = "subject_nr") %>%
  mutate(
    removed_attempts = replace_na(removed_attempts, 0),
    total_attempts = remaining_attempts + removed_attempts,
    removal_rate = removed_attempts / total_attempts
  )

# --------------------------------------------
# Reporting Removals
# --------------------------------------------
# Calculate final counts
final_attempt_count <- nrow(mouseTrap_data_filtered)
final_participant_count <- n_distinct(mouseTrap_data_filtered$subject_nr)

participants_to_remove # 7

nrow(mouseTrap_data) - nrow(mouseTrap_data_filtered) # 17.8%
nrow(mouseTrap_data) - nrow(mouseTrap_data_filtered_mt)
nrow(mouseTrap_data_filtered_mt) - nrow(mouseTrap_data_filtered)

# --------------------------------------------
# Step 1: Preprocess Data
# --------------------------------------------

mouseTrap_data_unnested <- mouseTrap_data_filtered %>% filter(!subject_nr %in% participants_to_remove) %>%
  mutate(trackingData = map(trackingData, ~ if (length(.) == 0) NULL else .)) %>%
  filter(!map_lgl(trackingData, is.null)) %>%
  unnest(trackingData, names_sep = "_") %>%
  mutate(
    trackingData_x = trackingData_x - (width_gameArea / 2), # X-axis: shift to range -200 to 200
    trackingData_y = trackingData_y       # Y-axis remains 0 to 800
  ) %>% group_by(trialId, attemptIndex) %>% filter(!(row_number() == 1 & trackingData_y > 75)) #error record (clean if first pos is outside of the jump button)

mouseTrap_data_unnested <- mouseTrap_data_unnested %>% mutate(uniqueId = paste0(trialId, '_', attemptIndex)) %>% relocate(trialId, uniqueId)

# --------------------------------------------
# Button Position and check
# --------------------------------------------

# ----- Check Button Position ----- #
mouseTrap_data_unnested %>% 
  ggplot(aes(x = trackingData_x, y = trackingData_y, group = uniqueId)) + 
  geom_path() +
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "white", color = "black", inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "#FC1717", color = "black", inherit.aes = FALSE) + 
  ylim(0, height_gameArea) + xlim(-width_gameArea/2, width_gameArea/2) + theme_minimal()

mouseTrap_data_unnested %>% filter(subject_nr == "Participant 1") %>%  
  arrange(uniqueId, trackingData_time) %>%
  ggplot(aes(x = trackingData_x, y = trackingData_y, group = uniqueId, color = response )) + 
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "white", color = "black", inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "#FC1717", color = "black", inherit.aes = FALSE) + 
  geom_path() +
  ylim(0, height_gameArea) + xlim(-width_gameArea/2, width_gameArea/2) + theme_minimal()

# ----- Check Trajectories Modality ----- #
{mouseTrap_data_unnested %>%
  ggplot(aes(x = trackingData_x, y = trackingData_y)) +
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE) +
  scale_fill_gradientn(
    colors = c("white", viridis::viridis(5, option = "rocket")),
    name = "Density"
  ) +
  coord_fixed() +
  theme_classic() +
  labs(
    title = "Mouse Trajectories Heatmap",
    x = "X Position",
    y = "Y Position",
    fill = "Density"
  )

mouseTrap_data_unnested %>%
  ggplot(aes(x = trackingData_x, y = trackingData_y)) +
  facet_wrap(checkpoint~.) +
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE) +
  scale_fill_gradientn(
    colors = c("white", viridis::viridis(5, option = "rocket")),
    name = "Density"
  ) +
  coord_fixed() +
  theme_classic() +
  labs(
    title = "Mouse Trajectories Heatmap",
    x = "X Position",
    y = "Y Position",
    fill = "Density"
  )

mouseTrap_data_unnested %>%
  ggplot(aes(x = trackingData_x, y = trackingData_y)) +
  facet_wrap(condition~.) +
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE) +
  scale_fill_gradientn(
    colors = c("white", viridis::viridis(5, option = "rocket")),
    name = "Density"
  ) +
  coord_fixed() +
  theme_classic() +
  labs(
    title = "Mouse Trajectories Heatmap",
    x = "X Position",
    y = "Y Position",
    fill = "Density"
  )
}
# --------------------------------------------
# Remap trajectories to common (0,0) start point
# --------------------------------------------
valid_starts <- mouseTrap_data_unnested %>%
  group_by(trialId, subject_nr, attemptIndex) %>%
  mutate(
    first_x = first(trackingData_x),
    first_y = first(trackingData_y),
    first_time = first(trackingData_time),
    first_time_prev = nth(trackingData_time, 2)
  ) %>%
  ungroup() %>%
  filter(
    between(first_x, start_button$x, (start_button$x + start_button$width)),
    between(first_y, start_button$y, (start_button$y + start_button$height))
  ) %>%
  group_by(subject_nr) %>%
  summarise(
    median_start_x = median(first_x, na.rm = TRUE),
    median_start_y = median(first_y, na.rm = TRUE),
    median_start_time_gap = median(first_time_prev - first_time, na.rm = TRUE),
    .groups = "drop"
  )

mouse_data_normalized <- mouseTrap_data_unnested %>%
  group_by(trialId, subject_nr, attemptIndex) %>%
  mutate(
    orig_start_x = first(trackingData_x),
    orig_start_y = first(trackingData_y),
    orig_start_time = first(trackingData_time),
    
    start_in_area = between(orig_start_x, start_button$x, start_button$x + start_button$width) &
      between(orig_start_y, start_button$y, 
              start_button$y + start_button$height),
    
    corrected_start_x = if_else(start_in_area,
                                orig_start_x,
                                valid_starts$median_start_x[valid_starts$subject_nr == cur_group()$subject_nr]),
    corrected_start_y = if_else(start_in_area,
                                orig_start_y,
                                valid_starts$median_start_y[valid_starts$subject_nr == cur_group()$subject_nr]),
    
    corrected_start_time = if_else(start_in_area,
                                   orig_start_time,
                                   orig_start_time - valid_starts$median_start_time_gap[valid_starts$subject_nr == cur_group()$subject_nr]),
    
    # time_offset = if_else(start_in_area, 0L, orig_start_time - 0L),
    
    needs_start_correction = !start_in_area
  ) %>%
  group_modify(~ {
    if (.x$needs_start_correction[1]) {
      # Duplicate the first row and update relevant columns
      new_start <- .x[1, ] %>%
        mutate(
          trackingData_x = .x$corrected_start_x[1],
          trackingData_y = .x$corrected_start_y[1],
          trackingData_time = .x$corrected_start_time[1],
          norm_x = 0,
          norm_y = 0
        )
      bind_rows(new_start, .x)
    } else {
      .x
    }
  }) %>% select(-starts_with("orig_"), -starts_with("corrected_"), -corrected_start_time, -needs_start_correction, -start_in_area)

mouse_data_normalized <- mouse_data_normalized %>% 
  group_by(uniqueId) %>%
  mutate(
    norm_x = trackingData_x - first(trackingData_x),
    norm_y = trackingData_y - first(trackingData_y),
    
    # Flip easier responses to left
    norm_x = if_else(response == "easier", -norm_x, norm_x)
  )

mouse_data_normalized %>% filter(subject_nr == "Participant 1") %>%  
  arrange(uniqueId, trackingData_time) %>%
  ggplot(aes(x = norm_x, y = norm_y, group = uniqueId, color = response )) + 
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "white", color = "black", inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "#FC1717", color = "black", inherit.aes = FALSE) + 
  geom_path() +
  ylim(0, height_gameArea) + xlim(-width_gameArea/2, width_gameArea/2) + theme_minimal()

# --------------------------------------------
# Remove trajectories outliers and trajectories with not enough observation
# --------------------------------------------

trial_to_remove <- mouse_data_filtered %>% group_by(uniqueId) %>% summarise(n = n()) %>% arrange(n) %>% filter(n < 11) %>% select(uniqueId) %>% pull

mouse_data_filtered <- mouse_data_normalized %>%
  group_by(uniqueId) %>%
  filter(
    all(norm_y >= -5), # Remove trajectories with any Y < -5
    last(norm_y) >= (start_button$y - 20),
    # Remove if any trackingData_time is an outlier (> 2 SD from mean within trajectory)
    !any(trackingData_time > mean(trackingData_time, na.rm = TRUE) + 
           3 * sd(trackingData_time, na.rm = TRUE)),
    !first(trackingData_time > 1000), # initation Time
  ) %>%
  ungroup() %>% group_by()

mouse_data_filtered <- mouse_data_filtered %>% filter(uniqueId %!in% trial_to_remove)

# write.csv(x = mouse_data_processed, './output/trajectoires_processed.csv')

# ----------------------------------------------------
# TRUNCATE TRAJECTORIES 
# Only consider the LEFT response box
# ----------------------------------------------------

# Define left response box (after mirroring)
resp_xmin <- -(width_clickArea / 2)
resp_xmax <- resp_xmin + 96
resp_ymin <- height_gameArea - 84
resp_ymax <- resp_ymin + 84

mouse_data_truncate <- mouse_data_filtered %>%
  group_by(uniqueId) %>%
  mutate(
    # Check if the sample is inside the left (mirrored) response area
    in_resp_box = norm_x >= resp_xmin &
      norm_x <= resp_xmax &
      norm_y >= resp_ymin &
      norm_y <= resp_ymax,
    
    # First time the trajectory enters the left response box
    first_resp_row = ifelse(any(in_resp_box),
                            which(in_resp_box)[1],
                            NA)
  ) %>%
  # Keep only samples up to that point
  filter(is.na(first_resp_row) | row_number() <= first_resp_row) %>%
  select(-in_resp_box, -first_resp_row) %>%
  ungroup()

mouse_data_truncate %>% filter(subject_nr == "Participant 1") %>%  
  arrange(uniqueId, trackingData_time) %>%
  ggplot(aes(x = norm_x, y = norm_y, group = uniqueId, color = response )) + 
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "white", color = "black", inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), fill = "#FC1717", color = "black", inherit.aes = FALSE) + 
  geom_path() +
  ylim(0, height_gameArea) + xlim(-width_gameArea/2, width_gameArea/2) + theme_minimal()


# ----- Remove trajectories (n = 2) that present issue in the tracking
# bad_ids <- c(
#   "19_Participant 19_9",
#   "19_Participant 38_34"
# )
# 
# mouse_data_truncate <- mouse_data_truncate %>%
#   filter(!uniqueId %in% bad_ids)

# --------------------------------------------
# Step 2: Features Extraction
# --------------------------------------------

# Re-interpoltation (10ms intervals)

mouse_data_processed <- mouse_data_truncate %>%
  group_by(uniqueId) %>%
  mutate(
    time_rel = (trackingData_time - min(trackingData_time))/1000
  ) %>%
  complete(
    time_rel = seq(0, max(time_rel), by = 0.010), # 10ms intervals
    fill = list(movement_phase = "stationary")
  ) %>%
  mutate(
    across(c(norm_x, norm_y),
           ~ zoo::na.approx(., x = time_rel, na.rm = FALSE)),
    trackingData_time = min(trackingData_time) + time_rel * 1000
  ) %>%
  fill(-norm_x, -norm_y, -time_rel, -trackingData_time) %>%
  ungroup()

mouse_data_processed %>% # filter(subject_nr == "Participant 81") %>%
  arrange(uniqueId, time_rel) %>%
  ggplot() +
  facet_wrap(subject_nr~.) +
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y-(height/2), ymax = y+(height/2)), color = "black", alpha = 0.5, inherit.aes = FALSE) +
  geom_path(aes(x = norm_x, y = norm_y, group = uniqueId, color = response)) +
  theme_conan() + scale_color_manual(values = c('#E93609', '#79CB6C', '#F9B734', '#4EC0CA')) + 
  labs(y = expression(Y[normalized]),
       x = expression(X[normalized]),
       color = "Response")

# --------------------------------------------
# Feature Extraction (Spatial)
# --------------------------------------------

mouse_spatial_features <- mouse_data_processed %>%
  group_by(uniqueId, response, trialId, attemptIndex, subject_nr) %>%
  arrange(time_rel, .by_group = TRUE) %>%
  summarise(
    ## Spatial Features
    # 1. Deviation at each timepoint
    dev = list(compute_deviation(norm_x, norm_y)),
    # 2. Maximal Deviation
    max_dev = max(dev[[1]], na.rm = TRUE),
    # 3. Time To Max Deviation (as proportion of total time)
    t_max_dev = time_rel[which.max(dev[[1]])] / max(time_rel, na.rm = TRUE),
    # 4. Area Under the Curve of deviation (normalized)
    auc_dev = pracma::trapz(time_rel, dev[[1]]) / max(time_rel, na.rm = TRUE),
    # 5. Average Deviation
    mean_dev = mean(dev[[1]], na.rm = TRUE),
    # 6. Entropy of the trajectory in X–Y angle space
    # entropy_xy = {
    #   ang <- atan2(diff(norm_y), diff(norm_x))
    #   entropy(discretize(ang, numBins = 30), unit = "log2")
    # },
    # 7. X-flip count (number of times direction in X changes sign)
    x_flip = {
      signs <- sign(diff(norm_x))
      sum(abs(diff(signs)) > 0, na.rm = TRUE)
    },
    
    ## Temporal Features
    total_time = max(time_rel, na.rm = TRUE),
    # # 8. Idle Time: sum of intervals with speed == 0 
    # idle_time = sum((speed_px <= 1) * diff(c(0, time_rel)), na.rm = TRUE),
    # # 9. Time to Max Velocity (as proportion of total time)
    # t_max_vel = {
    #   idx <- which.max(speed_px)
    #   time_rel[idx] / max(time_rel, na.rm = TRUE)
    # },
    # 
    # ## Speed features
    # # 10. Average Velocity
    # mean_speed = mean(speed_px, na.rm = TRUE),
    # 
    # # 11. Maximal Velocity
    # max_speed = max(speed_px, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # remove list‐column
  select(-dev)

# --------------------------------------------
# Feature Extraction (Temporal) from raw trajectories
# --------------------------------------------
mouse_velocity <- mouse_data_truncate %>%
  group_by(uniqueId) %>%
  arrange(trackingData_time, .by_group = TRUE) %>%
  mutate(
    dt = (trackingData_time - lag(trackingData_time)) / 1000,
    dx = trackingData_x - lag(trackingData_x),
    dy = trackingData_y - lag(trackingData_y),
    distance = sqrt(dx^2 + dy^2),
    speed_px_s = distance / dt,
    .groups = "drop"
  ) %>%
  filter(
    is.finite(speed_px_s),
    dt > 0
  )

mouse_velocity %>% 
  ggplot(aes(x = trackingData_time, y = speed_px_s, group = uniqueId)) +
    geom_line(alpha = 0.35, linewidth = 0.4) +
    facet_wrap(. ~ subject_nr, scale = "free_x") +
    labs(x = "Temps (ms)", y = "Vitesse (px/s)") +
    theme_minimal()

mouse_velocity_smooth <- mouse_velocity %>%
  group_by(uniqueId) %>%
  arrange(trackingData_time, .by_group = TRUE) %>%
  mutate(
    speed_smooth = zoo::rollmean(speed_px_s, k = 7, align = "center", fill = NA)
  ) %>%
  ungroup()

mouse_velocity_smooth %>% 
  ggplot(aes(x = trackingData_time, y = speed_smooth, group = uniqueId)) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  facet_wrap(. ~ subject_nr, scale = "free_x") +
  labs(x = "Temps (ms)", y = "Vitesse (px/s)") +
  theme_minimal()

mouse_velocity_smooth <- mouse_velocity_smooth %>%
  group_by(uniqueId) %>%
  summarise(
    max_vel = quantile(speed_smooth, 0.95, na.rm = TRUE)
  ) %>%
  ungroup()

mouse_spatial_features <- mouse_spatial_features %>% left_join(mouse_velocity_smooth, by = "uniqueId")

# --------------------------------------------
# Final Df
# --------------------------------------------

# Bind with trials related data 
game_data_join <- game_data_filtered %>% mutate(uniqueId = paste0(trialId, '_', attemptIndex))

mouse_features_full <- mouse_spatial_features %>% left_join(game_data_join, by = c('uniqueId', 'subject_nr', 'trialId', 'response', 'attemptIndex'))
# rm(game_data_join)

# write.csv(x = mouse_features_full, './output/mouse_features.csv')

# --------------------------------------------
# Time Normalized
# --------------------------------------------

mouse_data_processed_tn <- mouse_data_processed %>% select(-trackingData_x, -trackingData_y, -trackingData_time) %>%
  rename(x = norm_x, y = norm_y, time = time_rel) %>%
  group_by(uniqueId) %>%
  resample_traj(n = 101)

mouse_data_processed_tn <- mouse_data_processed_tn %>% mutate(condition = as.factor(ifelse(condition == 'LongCond', 'Long (6 cp)', 'Short (3 cp)')),
                                                      group_skill = as.factor(group_skill), 
                                                      end_distance_to_goal_f = factor(end_distance_to_goal_f, rev(levels(end_distance_to_goal_f))))


mouse_mean <- mouse_data_processed_tn %>%
  group_by(response, end_distance_to_goal_f, idx) %>%
  summarise(
    norm_x = mean(x, na.rm = TRUE),
    norm_y = mean(y, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(response, end_distance_to_goal_f) %>%
  arrange(idx, .by_group = TRUE) %>%
  mutate(
    norm_x_smooth = zoo::rollmean(norm_x, k = 5, fill = "extend", align = "center"),
    norm_y_smooth = zoo::rollmean(norm_y, k = 5, fill = "extend", align = "center")
  ) %>%
  ungroup()

mouse_mean %>%
  arrange(idx) %>%
  ggplot() +
  facet_grid(.~end_distance_to_goal_f) +
  geom_rect(data = buttons, aes(xmin = x, xmax = x + width, ymin = y - 20, ymax = y + height - 40), color = "#545454", alpha = 0.3, inherit.aes = FALSE, size = 0.5) +
  geom_rect(data = start_button, aes(xmin = x, xmax = x + width, ymin = y-(height/2), ymax = y+(height/2)), color = "#545454", alpha = 0.2, inherit.aes = FALSE, size = 0.5) +
  geom_path(aes(x = norm_x_smooth, y = norm_y_smooth, group = interaction(end_distance_to_goal_f, response), color = response, linetype = response), linewidth = 0.5) +
  geom_path(aes(x = norm_x_smooth, y = norm_y_smooth, group = interaction(end_distance_to_goal_f, response), color = response), linewidth = 1.5, alpha = 0.5) +
  theme_conan() + scale_color_manual(values = c('#F0AA16', '#4A74D9'), labels = c('Gu', "Restart")) + 
  scale_linetype_manual(values = c('dotdash', 'solid')) +
  labs(y = expression(Y[normalized]),
       x = expression(X[normalized]),
       color = "", linetype = "") + guides(linetype = FALSE) + 
  theme(axis.text.x = element_text(size = 8), panel.grid.minor = element_blank(), legend.position = 'top', legend.justification = 'left')

