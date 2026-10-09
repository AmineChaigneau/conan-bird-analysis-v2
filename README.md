# Conan Bird Task

An experimental research application studying decision-making and persistence under increasing task difficulty. This project implements a Flappy Bird-style game to analyze how participants make trade-offs between effort and reward under challenging conditions.

![Game Preview](public/welcome.png)

---

## R Repository

1. [Overview](#overview)
2. [Technical Requirements](#technical-requirements)
3. [Installation & Setup](#installation--setup)
4. [Documentation](#documentation)
   - [R Scripts](#r-scripts)
   - [Output Files](#output-files)
5. [Contact](#contact)

---

## Overview

This repository contains all R code and processed data for the “Conan Bird Task” study. Participants play a Flappy Bird–inspired game under varying levels of difficulty, and we record their trajectories, decisions, and effort-related metrics. Downstream analyses focus on:

- Preprocessing raw trajectory and game data
- Extracting spatial and temporal features from player behavior
- Fitting generalized linear mixed models (GLMMs) to model persistence and quitting decisions
- Visualizing feature distributions and model predictions

---

## Technical Requirements

- **R version**: ≥ 4.4.1 (2024-06-14)
- **Required Packages** (minimum versions used in development):

| Package   | Minimum Version |
| --------- | --------------- |
| tidyverse | 2.0.0           |
| dplyr     | 1.1.4           |
| purrr     | 1.0.2           |
| tibble    | 3.2.1           |
| readr     | 2.1.5           |
| tidyr     | 1.3.1           |
| forcats   | 1.0.0           |
| stringr   | 1.5.1           |
| jsonlite  | 1.8.9           |
| lubridate | 1.9.3           |
| lme4      | 1.1-36          |
| lmerTest  | 3.1-3           |
| Matrix    | 1.7-0           |
| ggplot2   | 3.5.1           |
| pracma    | 2.4.4           |
| entropy   | 1.3.2           |

> **Tip**: In the R console, check your versions with:
>
> ```r
> R.version.string
> sessionInfo()
> # or a single package:
> packageVersion("dplyr")
> ```

---

## Setup

1. **Clone this repository**

```bash
  git clone https://github.com/your-org/conan-bird-task.git
  cd conan-bird-task
```

2. **Install required R packages**

```r
  install.packages(c(
  "tidyverse", "jsonlite", "lubridate", "lme4", "lmerTest",
  "Matrix", "pracma", "entropy"
))
```

3. **Load Utily functions**

```r
source("utils.R")
```

---

## Documentation

### R Scripts

- **`utils.R`**  
  This file defines helper functions for processing raw game data.

- **`extract_raw_data.R`**  
  This script sources `utils.R` and applies its functions to every JSON in `data/raw/`. It generates a combined metadata table (one row per attempt). It filters out trials or participants that did not complete the task fully and saves intermediate objects or CSV files for downstream processing.

- **`mt_data_processing.R`**  
  This script takes the combined trajectory tables from `extract_raw_data.R` and performs preprocessing and filtering of “choice” trials. It computes spatial features (such as path length and curvature) and temporal features (such as reaction time) for each trajectory. The result is a cleaned and feature‐enriched trajectories data frame used in later analyses.

- **`game_data_processing.R`**  
  This file loads the raw game‐behavior tables and filters out outlier attempts or participants who skipped all trials. It summarizes per‐trial metrics—such as total time, pipes passed, and jump count—and then standardizes continuous variables (e.g., attempt number, pipes passed, jump count). The script writes two CSV files: an unscaled summary (`game_data_processed.csv`) and its scaled counterpart (`game_data_processed_scaled.csv`).

- **`response_models.R`**  
  This script fits generalized linear mixed models (GLMMs) to predict the binary “give‐up” outcome on each trial. It includes random intercepts for participant and trial, and fixed effects for difficulty and effort‐related features.

- **`feature_outcomes.R`**  
  This script uses feature-enriched trajectories and game summaries to create visualisations that check the output of features (for average velocity, curvature and entropy measures).

- **`distribution.R`**  
  This script plots the distributions of important variables across all trials.

---

### Output Files

- **`trajectoires_processed.csv`**  
  This CSV combines every trial’s raw trajectory points (columns `time`, `x`, `y`) with trial‐level metadata (difficulty, condition, checkpoints, etc.). Coordinates have been flipped so the origin is at the bottom‐left. Each row represents a single timestamped position within a trial, tagged by `uniqueId`.

- **`game_data_processed.csv`**  
  A per‐trial summary of game behavior before scaling. It includes columns such as `uniqueId`, `condition`, `difficulty`, `attempt`, `pipesPassed`, `jumpCount`, `time`, `checkpoint_start`, `checkpoint_end`, and cumulative metrics like total pipes passed or total jumps. This file captures raw behavioral outcomes for each trial.

- **`game_data_processed_scaled.csv`**  
  This file mirrors `game_data_processed.csv` but with continuous variables (e.g., `attempt`, `pipesPassed`, `jumpCount`) z‐scored. It includes `uniqueId`, `condition`, `difficulty`, `attempt_scaled`, `pipesPassed_scaled`, `jumpCount_scaled`, `time`, `checkpoint_start`, `checkpoint_end`, and other standardized cumulative measures. It is ready for GLMM input.

- **`features_data.csv`**  
  This CSV merges `game_data_processed_scaled.csv` with all trajectory‐derived features. Each row represents a trial with its scaled game metrics alongside computed features such as path length, average velocity, curvature, and entropy. It serves as the final dataset for statistical analyses and visualization.

---

## Contact

If you have questions about the code, data formats, please reach out to:

Dr. Amine Chaigneau  
aminechaigneua@gmail.com
