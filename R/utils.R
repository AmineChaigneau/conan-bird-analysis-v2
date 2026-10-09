#' Load and install required R packages
#' 
#' This function checks whether the specified R packages are installed.  
#' If any are missing, they are automatically installed and then loaded.
#' 
#' @param packages Character vector. Names of the R packages to load.  
#' Defaults to \code{c("tidyverse", "jsonlite", "mousetrap", "lme4")}.
#' @return Invisibly returns a character vector of loaded packages.
#' @importFrom utils install.packages
#' @importFrom base require
#' @importFrom base library
#' @export
load_package <- function(packages = c("tidyverse", "jsonlite", "mousetrap", "lme4", "DHARMa", "emmeans", "glmmTMB", "entropy", "pracma", "ggpattern")) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  invisible(packages)
}
#' Bind all trial JSON files
#' 
#' @param path Character. Path to the directory containing trial JSON files
#' @return data.frame containing combined trial data
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @export
read_trial_data <- function(path = "") {
  # List all trial JSON files
  trial_files <- list.files(
    path = path,
    pattern = "trial_\\d+\\.json$",
    full.names = TRUE
  )
  
  trials_data <- lapply(trial_files, function(file) {
    # Read JSON file
    trial <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
    
    # Create attempts_df
    if (!is.null(trial$attempts) && length(trial$attempts) > 0) {
      attempts_df <- dplyr::bind_rows(trial$attempts)
      attempts_df$attemptIndex <- seq(0, nrow(attempts_df) - 1)
    } else {
      # Create a single-row empty attempts_df for the trial
      attempts_df <- tibble(
        trialNumber = trial$trialNumber,
        time = NA,
        score = NA,
        pipesPassed = NA,
        checkpoint = NA,
        setting = NA,
        choice = NA,
        jumpCount = NA,
        trackingData = NA,
        attemptIndex = 0
      )
    }
    
    # Add trial-level data to each attempt (or the single placeholder row)
    attempts_df <- dplyr::mutate(attempts_df,
                                 level = trial$level,
                                 condition = trial$condition,
                                 difficulty = trial$difficulty,
                                 checkpointTarget = trial$checkpointTarget,
                                 completed = trial$completed,
                                 usedEasierSettings = trial$usedEasierSettings,
                                 rewardEarned = trial$rewardEarned,
                                 duration = trial$duration
    )
    
    return(attempts_df)
  })
  
  # Combine all trials into a single data frame
  combined_trials <- dplyr::bind_rows(trials_data, .id = "trial_id")
  
  # Sort by trial number and attempt index
  combined_trials <- combined_trials[order(combined_trials$trialNumber, combined_trials$attemptIndex), ]
  
  return(combined_trials)
}

#' Bind all grit JSON files
#' 
#' @param path Character. Path to the directory containing grit JSON files
#' @return data.frame containing combined grit data
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
read_grit_data <- function(path = "") {
  # List all grit JSON files
  grit_files <- list.files(
    path = path,
    pattern = "grit\\.json$",
    full.names = TRUE,
    recursive = TRUE  # Search subdirectories
  )
  
  # Read and process each file
  grit_data <- lapply(grit_files, function(file) {
    # Read JSON file
    grit <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
    
    # Create response string
    responses_str <- paste(grit$responses, collapse = ",")
    
    # Create standardized data frame
    tibble::tibble(
      participantId = grit$participantId,
      gritScore = grit$gritScore,
      responses = responses_str,
      timestamp = grit$timestamp,
      file_path = file  # Keep track of source file
    )
  })
  
  # Combine all grit data into a single data frame
  combined_grit <- dplyr::bind_rows(grit_data)
  
  return(combined_grit)
}

#' ggplot theme
#' @importFrom ggplot theme
#' @export
theme_conan <- function() {
  theme_minimal(base_size = 14) +
    theme(
      strip.background = element_rect(fill = '#F5F5F7', color = '#B7B7B7'),
      panel.border = element_rect(fill = NA, color = "#B7B7B7"))
}

#' Invert %in%
#' @export
'%!in%' <- function(x,y)!('%in%'(x,y))


#' Resample tracking data to regular time intervals
#' 
#' This function resamples tracking data to create a regular time series with 10ms intervals.
#' It fills in missing timestamps and forward-fills missing x and y coordinate values.
#' 
#' @param tracking_data A data frame containing tracking data with columns: time, x, y.
#'   If NULL, NA, empty, or not a data frame, an empty tibble is returned.
#' @return A tibble with resampled data containing columns: time, x, y.
#'   Time intervals are regularized to 10ms steps, with missing coordinate values
#'   forward-filled from the last known position.
#' @importFrom dplyr left_join
#' @importFrom tidyr fill
#' @importFrom tibble tibble
#' @export
resample_and_recreate <- function(tracking_data) {
  # Check if the data is NULL, NA, or not a data frame
  if (is.null(tracking_data) || !is.data.frame(tracking_data) || (nrow(tracking_data) == 0 && ncol(tracking_data) == 0)) {
    # Return empty tibble with expected structure for empty or invalid input
    return(tibble(time = numeric(0), x = numeric(0), y = numeric(0)))
  }
  
  # Create a full sequence of timestamps with a step of 10 ms
  min_time <- min(tracking_data$time, na.rm = TRUE)
  max_time <- max(tracking_data$time, na.rm = TRUE)
  full_time <- seq(min_time, max_time, by = 10)
  
  # Join the full timestamps with the original data
  resampled_data <- tibble(time = full_time) %>%
    left_join(tracking_data, by = "time") %>%
    fill(x, y, .direction = "down") # Fill missing x and y values
  
  return(resampled_data)
}

#' Identify and remove outlier trials based on the number of attempts
#' 
#' This function identifies trials with a number of attempts exceeding a specified quantile threshold
#' and returns a list of trial IDs to be removed as outliers.
#' 
#' @param data A data frame containing trial data with a column `num_attempts`.
#' @param quant A numeric value representing the quantile threshold for identifying outliers (default is 0.99).
#' @return A vector of trial IDs identified as outliers.
#' @importFrom dplyr filter select pull
#' @importFrom glue glue
#' @export
trial_to_removed <- function(data, quant = 0.99) {
  threshold <- quantile(data$num_attempts, quant, na.rm = TRUE)
  
  list_to_removed <- data %>%
    filter(num_attempts >= threshold) %>% select(trialId) %>% pull
  
  n_removed <- length(list_to_removed)
  message(glue::glue("Removed {n_removed} outlier rows (top {100 * (1 - quant)}%) based on num_attempts > {round(threshold, 2)}"))
  
  return(list_to_removed)
}

#' Summarise attempts data by trial and subject
#' 
#' This function summarises attempts data by grouping it by subject and trial, calculating the number
#' of attempts and their proportion for each trial.
#' 
#' @param data A data frame containing attempts data with columns: subject_nr, trialNumber, trialId, and others.
#' @param ... Additional grouping variables.
#' @return A data frame with summarised attempts data including the number and proportion of attempts.
#' @importFrom dplyr group_by summarise mutate ungroup
#' @export
summarise_attempts <- function(data, ...) {
  data %>%
    group_by(subject_nr, trialNumber, trialId, ...) %>%
    summarise(
      attempt_on_trial = max(attemptIndex) + 1,
      num_attempts = n(),
      .groups = "drop"
    ) %>% group_by(subject_nr, trialNumber) %>%
    mutate(
      prop_attempts = num_attempts / sum(num_attempts)
    ) %>%
    ungroup()
}

#' Filter out top attempt outliers
#' 
#' This function filters out trials with a number of attempts exceeding a specified quantile threshold,
#' effectively removing outliers from the data.
#' 
#' @param data A data frame containing trial data with a column `num_attempts`.
#' @param quant A numeric value representing the quantile threshold for filtering outliers (default is 0.99).
#' @return A data frame with outliers removed based on the number of attempts.
#' @importFrom dplyr filter
#' @importFrom glue glue
#' @export
filter_top_attempt_outliers <- function(data, quant = 0.99) {
  threshold <- quantile(data$num_attempts, quant, na.rm = TRUE)
  
  filtered_data <- data %>%
    filter(num_attempts <= threshold)
  
  n_removed <- nrow(data) - nrow(filtered_data)
  message(glue::glue("Removed {n_removed} outlier rows (top {100 * (1 - quant)}%) based on num_attempts > {round(threshold, 2)}"))
  
  return(filtered_data)
}

library(pracma)       # for trapz()
library(entropy)     # for entropy()
# library(moveme) 

#' Compute deviation from a straight line
#' 
#' This function computes the perpendicular deviation of points from a straight line connecting the origin
#' to the last point in a series of x and y coordinates.
#' 
#' @param x A numeric vector of x coordinates.
#' @param y A numeric vector of y coordinates.
#' @return A numeric vector of deviations for each point from the line.
#' @importFrom pracma last
#' @export
compute_deviation <- function(x, y) {
  # line from (0,0) to (x_end, y_end)
  x_end <- last(x); y_end <- last(y)
  # for each point, perpendicular distance to that line
  num <- abs((y_end - 0)*x - (x_end - 0)*y)
  den <- sqrt((y_end - 0)^2 + (x_end - 0)^2)
  # signed
  # num <- (y_end * x - x_end * y)
  # den <- sqrt(y_end^2 + x_end^2)
  num / den
}

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

#' Resample Trajectories Within Groups to a Fixed Length
#'
#' Resamples a (x, y, time) trajectory to \code{n} equally spaced time points using linear interpolation.
#' Works with grouped data and retains all other columns (e.g., \code{trial}, \code{condition}, etc.).
#'
#' @param df A data frame containing at least \code{x}, \code{y}, and \code{time} columns.
#'           If grouped (e.g., with \code{group_by(uniqueId)}), the resampling will be applied per group.
#' @param n Number of time points to resample to. Default is 101.
#'
#' @return A tibble with \code{n} rows per group and all original columns preserved, plus \code{idx}.
#'
#' @examples
#' mouse_df %>% group_by(uniqueId) %>% resample_traj()
#'
#' @export
resample_traj <- function(df, n = 101) {
  group_vars <- dplyr::group_vars(df)
  
  resampled <- df %>%
    dplyr::group_split(.keep = TRUE) %>%
    purrr::map_dfr(function(group_df) {
      group_df <- group_df %>% arrange(time)
      
      if (n_distinct(group_df$time) < 2) {
        return(tibble())
      }
      
      # Create interpolation grid
      tt <- seq(min(group_df$time), max(group_df$time), length.out = n)
      
      x_interp <- approx(group_df$time, group_df$x, xout = tt, ties = "ordered")$y
      y_interp <- approx(group_df$time, group_df$y, xout = tt, ties = "ordered")$y
      # v_interp  <- approx(group_df$time, group_df$speed_px, xout = tt, ties = "ordered")$y
      
      # Use first row to replicate all static metadata columns
      meta <- group_df %>% dplyr::slice(1) %>% select(-x, -y, -time) #, -speed_px)
      
      # Build output
      bind_cols(
        meta[rep(1, n), ],
        tibble(
          idx = seq_len(n),
          time = tt,
          x = x_interp,
          y = y_interp,
          # velocity = v_interp
        )
      )
    })
  
  if (length(group_vars) > 0) {
    resampled <- resampled %>% group_by(across(all_of(group_vars)))
  }
  
  return(resampled)
}

