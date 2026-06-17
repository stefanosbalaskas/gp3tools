# 01_create_paper_synthetic_gazepoint_exports.R
# Paper-only synthetic Gazepoint-like export generator for the gp3tools manuscript.
# This script stays outside the package repository.
# It creates a larger, richer, synthetic-realistic Gazepoint export folder
# with all-gaze files, fixation files, and one summary CSV.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(stringr)
})

set.seed(20260617)

base_dir <- "C:/Users/Stefanos-PC/Desktop/gp3tools_paper_showcase"
export_dir <- file.path(base_dir, "synthetic_paper_exports")

dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

# Clean old synthetic files from this paper-only export folder.
old_files <- list.files(export_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(old_files) > 0) {
  unlink(old_files)
}

n_participants <- 48L
n_media <- 4L
hz <- 60L
duration_sec <- 10
n_samples <- hz * duration_sec

media_design <- tibble(
  MEDIA_ID = 0:(n_media - 1),
  MEDIA_NAME = c(
    "Control_Ad",
    "Early_Target_Ad",
    "Late_Target_Ad",
    "Distractor_Heavy_Ad"
  ),
  condition = c(
    "control",
    "early_target",
    "late_target",
    "distractor_heavy"
  )
)

aoi_defs <- tibble(
  AOI = c("target", "price", "brand", "claim", "warning", "distractor", "background"),
  x_center = c(0.25, 0.72, 0.50, 0.38, 0.82, 0.18, 0.50),
  y_center = c(0.35, 0.70, 0.22, 0.58, 0.30, 0.78, 0.50),
  x_sd = c(0.045, 0.040, 0.050, 0.050, 0.045, 0.060, 0.250),
  y_sd = c(0.045, 0.040, 0.050, 0.050, 0.045, 0.060, 0.250)
)

add_filler_cols <- function(df, target_n_cols = 63L) {
  n_extra <- target_n_cols - ncol(df)
  if (n_extra <= 0) {
    return(df)
  }

  for (j in seq_len(n_extra)) {
    df[[paste0("FILLER_", sprintf("%02d", j))]] <- NA
  }

  df
}

draw_aoi <- function(condition, time_sec) {
  # Time-varying AOI probabilities by condition.
  early_boost <- exp(-((time_sec - 2.0)^2) / 3.0)
  late_boost  <- exp(-((time_sec - 7.0)^2) / 3.0)

  probs <- switch(
    condition,
    control = c(
      target = 0.18, price = 0.14, brand = 0.18, claim = 0.14,
      warning = 0.08, distractor = 0.10, background = 0.18
    ),
    early_target = c(
      target = 0.18 + 0.30 * early_boost, price = 0.10, brand = 0.12,
      claim = 0.13, warning = 0.08, distractor = 0.10,
      background = 0.29 - 0.20 * early_boost
    ),
    late_target = c(
      target = 0.15 + 0.35 * late_boost, price = 0.11, brand = 0.12,
      claim = 0.13, warning = 0.08, distractor = 0.12,
      background = 0.29 - 0.22 * late_boost
    ),
    distractor_heavy = c(
      target = 0.10, price = 0.12, brand = 0.14, claim = 0.10,
      warning = 0.08, distractor = 0.34, background = 0.12
    )
  )

  probs <- pmax(probs, 0.01)
  probs <- probs / sum(probs)

  sample(names(probs), size = 1, prob = probs)
}

simulate_pupil <- function(condition, time_sec, subject_intercept) {
  dilation <- switch(
    condition,
    control = 0.02 * sin(time_sec / 1.6),
    early_target = 0.12 * exp(-((time_sec - 2.5)^2) / 4),
    late_target = 0.14 * exp(-((time_sec - 7.2)^2) / 4),
    distractor_heavy = 0.06 * exp(-((time_sec - 4.5)^2) / 6)
  )

  3.45 + subject_intercept + dilation + 0.03 * sin(time_sec / 1.4)
}

simulate_blink_mask <- function(time_sec, participant_quality, condition) {
  n <- length(time_sec)

  # Baseline pupil missingness plus worse data for low-quality participants/conditions.
  base_missing <- switch(
    participant_quality,
    good = 0.015,
    moderate = 0.045,
    poor = 0.110
  )

  condition_extra <- ifelse(condition == "distractor_heavy", 0.015, 0)

  random_missing <- runif(n) < (base_missing + condition_extra)

  # Add blink gaps of about 80-250 ms.
  n_blinks <- switch(
    participant_quality,
    good = sample(2:4, 1),
    moderate = sample(4:7, 1),
    poor = sample(8:12, 1)
  )

  blink_missing <- rep(FALSE, n)

  for (b in seq_len(n_blinks)) {
    onset <- sample(20:(n - 20), 1)
    length_samples <- sample(5:15, 1)
    idx <- onset:min(n, onset + length_samples)
    blink_missing[idx] <- TRUE
  }

  random_missing | blink_missing
}

simulate_one_media <- function(pid, media_row, participant_quality, subject_intercept, drift_x, drift_y) {
  time_sec <- seq(0, duration_sec - 1 / hz, by = 1 / hz)
  CNT <- seq_along(time_sec)

  aoi_vec <- vapply(time_sec, function(t) draw_aoi(media_row$condition, t), character(1))

  aoi_params <- aoi_defs[match(aoi_vec, aoi_defs$AOI), ]

  x <- rnorm(length(time_sec), aoi_params$x_center, aoi_params$x_sd) + drift_x
  y <- rnorm(length(time_sec), aoi_params$y_center, aoi_params$y_sd) + drift_y

  # Deliberately allow a small number of out-of-range samples for diagnostics.
  x <- pmin(pmax(x, -0.15), 1.15)
  y <- pmin(pmax(y, -0.15), 1.15)

  gaze_invalid_prob <- switch(
    participant_quality,
    good = 0.015,
    moderate = 0.050,
    poor = 0.120
  )

  if (media_row$condition == "distractor_heavy") {
    gaze_invalid_prob <- gaze_invalid_prob + 0.025
  }

  BPOGV <- !(runif(length(time_sec)) < gaze_invalid_prob | x < 0 | x > 1 | y < 0 | y > 1)

  pupil_true <- simulate_pupil(media_row$condition, time_sec, subject_intercept)

  pupil_missing <- simulate_blink_mask(time_sec, participant_quality, media_row$condition)

  pupil_left <- pupil_true + rnorm(length(time_sec), 0, 0.025)
  pupil_right <- pupil_true + rnorm(length(time_sec), 0, 0.025)

  pupil_left[pupil_missing] <- NA_real_
  pupil_right[pupil_missing] <- NA_real_

  # A simple fixation-valid indicator and saccade magnitude proxy.
  FPOGV <- BPOGV & runif(length(time_sec)) > 0.25
  SACCADE_MAG <- c(0, sqrt(diff(x)^2 + diff(y)^2))

  tibble(
    CNT = CNT,
    TIME = round(time_sec, 5),
    TIME_TICK = round(time_sec * 1000),
    MEDIA_ID = media_row$MEDIA_ID,
    MEDIA_NAME = media_row$MEDIA_NAME,
    BPOGX = round(x, 5),
    BPOGY = round(y, 5),
    BPOGV = as.integer(BPOGV),
    LPMM = round(pupil_left, 4),
    RPMM = round(pupil_right, 4),
    FPOGV = as.integer(FPOGV),
    SACCADE_MAG = round(SACCADE_MAG, 5),
    AOI = ifelse(BPOGV, aoi_vec, "")
  )
}

make_richer_fixations <- function(gaze_df) {
  media_ids <- sort(unique(gaze_df$MEDIA_ID))
  out <- list()

  for (m in media_ids) {
    g <- gaze_df %>%
      filter(MEDIA_ID == m) %>%
      arrange(TIME)

    if (nrow(g) == 0) next

    media_name <- g$MEDIA_NAME[which(!is.na(g$MEDIA_NAME))[1]]

    trial_end <- max(g$TIME, na.rm = TRUE)
    start_time <- 0
    fix_id <- 1L
    trial_fix <- list()

    while (start_time < trial_end - 0.15) {
      dur <- runif(1, 0.14, 0.42)
      end_time <- min(start_time + dur, trial_end)

      seg <- g %>%
        filter(TIME >= start_time, TIME < end_time, BPOGV == 1)

      if (nrow(seg) >= 4) {
        seg_aoi <- seg$AOI
        seg_aoi <- seg_aoi[!is.na(seg_aoi) & seg_aoi != ""]

        modal_aoi <- if (length(seg_aoi) > 0) {
          names(sort(table(seg_aoi), decreasing = TRUE))[1]
        } else {
          "background"
        }

        fx <- mean(seg$BPOGX, na.rm = TRUE)
        fy <- mean(seg$BPOGY, na.rm = TRUE)

        if (is.finite(fx) && is.finite(fy)) {
          trial_fix[[length(trial_fix) + 1L]] <- tibble(
            CNT = fix_id,
            TIME = round(start_time, 5),
            FPOGS = round(start_time, 5),
            MEDIA_ID = m,
            MEDIA_NAME = media_name,
            FPOGID = fix_id,
            FPOGD = round(end_time - start_time, 5),
            FPOGX = round(fx, 5),
            FPOGY = round(fy, 5),
            BPOGX = round(fx, 5),
            BPOGY = round(fy, 5),
            FPOGV = 1L,
            AOI = modal_aoi
          )

          fix_id <- fix_id + 1L
        }
      }

      # Add a saccade/transition gap between fixations.
      start_time <- end_time + runif(1, 0.025, 0.090)
    }

    out[[as.character(m)]] <- bind_rows(trial_fix)
  }

  bind_rows(out) %>%
    arrange(MEDIA_ID, TIME) %>%
    mutate(
      CNT = row_number(),
      FPOGID = row_number()
    )
}

summary_rows <- list()

for (pid in seq_len(n_participants)) {
  user_id <- sprintf("paper_user_%02d", pid - 1)

  participant_quality <- sample(
    c("good", "moderate", "poor"),
    size = 1,
    prob = c(0.72, 0.20, 0.08)
  )

  subject_intercept <- rnorm(1, 0, 0.18)
  drift_x <- rnorm(1, 0, 0.035)
  drift_y <- rnorm(1, 0, 0.035)

  all_media_gaze <- lapply(seq_len(nrow(media_design)), function(i) {
    simulate_one_media(
      pid = pid,
      media_row = media_design[i, ],
      participant_quality = participant_quality,
      subject_intercept = subject_intercept,
      drift_x = drift_x,
      drift_y = drift_y
    )
  }) %>%
    bind_rows()

  fixation_df <- make_richer_fixations(all_media_gaze)

  all_media_gaze_out <- add_filler_cols(all_media_gaze, 63L)
  fixation_df_out <- add_filler_cols(fixation_df, 63L)

  write_csv(
    all_media_gaze_out,
    file.path(export_dir, paste0(user_id, "_all_gaze.csv")),
    na = ""
  )

  write_csv(
    fixation_df_out,
    file.path(export_dir, paste0(user_id, "_fixations.csv")),
    na = ""
  )

  summary_rows[[pid]] <- all_media_gaze %>%
    group_by(MEDIA_ID, MEDIA_NAME) %>%
    summarise(
      USER_ID = pid - 1,
      USER_FILE = paste0(user_id, "_all_gaze.csv"),
      condition = first(media_design$condition[match(MEDIA_ID, media_design$MEDIA_ID)]),
      participant_quality = participant_quality,
      n_samples = n(),
      gaze_valid_pct = mean(BPOGV == 1, na.rm = TRUE) * 100,
      pupil_valid_pct = mean(!is.na(LPMM) & !is.na(RPMM), na.rm = TRUE) * 100,
      mean_pupil = mean(rowMeans(cbind(LPMM, RPMM), na.rm = TRUE), na.rm = TRUE),
      .groups = "drop"
    )
}

summary_df <- bind_rows(summary_rows)

write_csv(
  summary_df,
  file.path(export_dir, "Data_Summary_export_paper_synthetic.csv"),
  na = ""
)

fix_files <- list.files(export_dir, pattern = "_fixations\\.csv$", full.names = TRUE)
fixation_count_check <- lapply(fix_files, function(f) {
  read_csv(f, show_col_types = FALSE) %>%
    count(MEDIA_ID, name = "n_fixations") %>%
    mutate(file = basename(f))
}) %>%
  bind_rows()

fixation_overview <- fixation_count_check %>%
  summarise(
    files = n_distinct(file),
    media_rows = n(),
    total_fixations = sum(n_fixations),
    mean_fixations_per_media = mean(n_fixations),
    min_fixations_per_media = min(n_fixations),
    max_fixations_per_media = max(n_fixations)
  )

cat("Created paper-only synthetic Gazepoint-like exports in:\n")
cat(export_dir, "\n\n")
cat("Files created:", length(list.files(export_dir)), "\n")
cat("All-gaze files:", length(list.files(export_dir, pattern = "_all_gaze\\.csv$")), "\n")
cat("Fixation files:", length(list.files(export_dir, pattern = "_fixations\\.csv$")), "\n")
cat("Summary files:", length(list.files(export_dir, pattern = "^Data_Summary")), "\n")
cat("Summary rows:", nrow(summary_df), "\n")
cat("Total fixation rows:", fixation_overview$total_fixations, "\n")
cat("Mean fixations per media:", round(fixation_overview$mean_fixations_per_media, 1), "\n")
cat("Min/max fixations per media:", fixation_overview$min_fixations_per_media, "/", fixation_overview$max_fixations_per_media, "\n")
