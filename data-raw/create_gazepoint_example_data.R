# Create lightweight example datasets for gp3tools.
# Run from the package root with:
# source("data-raw/create_gazepoint_example_data.R")

set.seed(20260616)

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required to create example datasets.", call. = FALSE)
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  stop("Package 'tibble' is required to create example datasets.", call. = FALSE)
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required to load gp3tools while creating example datasets.", call. = FALSE)
}

devtools::load_all(".")

subjects <- sprintf("S%02d", 1:6)
conditions <- c("control", "treatment")
trial_index <- 1:2
times <- seq(0, 2000, by = 50)

trial_grid <- expand.grid(
  subject = subjects,
  condition = conditions,
  trial_index = trial_index,
  time = times,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

gazepoint_example_master <- tibble::as_tibble(trial_grid) |>
  dplyr::arrange(.data$subject, .data$condition, .data$trial_index, .data$time) |>
  dplyr::mutate(
    MEDIA_ID = dplyr::if_else(.data$trial_index == 1L, "stim1", "stim2"),
    trial_global = paste(.data$condition, paste0("T", .data$trial_index), sep = "_"),
    USER_FILE = .data$subject,
    time_sec = .data$time / 1000,
    time_scaled = .data$time / max(.data$time),
    subject_index = match(.data$subject, subjects),
    condition_effect = dplyr::if_else(
      .data$condition == "treatment" & .data$time >= 500 & .data$time <= 1500,
      0.12,
      0
    ),
    target_probability = stats::plogis(
      -2.1 +
        3.0 * .data$time_scaled +
        dplyr::if_else(.data$condition == "treatment", 0.85, 0)
    ),
    random_draw = stats::runif(dplyr::n()),
    aoi_current = dplyr::case_when(
      .data$random_draw < .data$target_probability ~ "AOI 2",
      .data$random_draw < .data$target_probability + 0.18 ~ "AOI 1",
      .data$random_draw < .data$target_probability + 0.34 ~ "AOI 0",
      TRUE ~ "Background"
    ),
    valid = !(
      (.data$subject == "S06" & .data$condition == "treatment" &
         .data$trial_index == 2L & .data$time >= 700 & .data$time <= 1100) |
        (.data$subject == "S03" & .data$condition == "control" &
           .data$trial_index == 1L & .data$time %in% c(300, 350, 400))
    ),
    artifact = .data$subject == "S04" &
      .data$condition == "treatment" &
      .data$trial_index == 1L &
      .data$time %in% c(850, 900),
    x_center = dplyr::case_when(
      .data$aoi_current == "AOI 0" ~ 0.25,
      .data$aoi_current == "AOI 1" ~ 0.50,
      .data$aoi_current == "AOI 2" ~ 0.75,
      TRUE ~ 0.50
    ),
    y_center = dplyr::case_when(
      .data$aoi_current == "AOI 0" ~ 0.50,
      .data$aoi_current == "AOI 1" ~ 0.50,
      .data$aoi_current == "AOI 2" ~ 0.50,
      TRUE ~ 0.80
    ),
    x = pmin(pmax(.data$x_center + stats::rnorm(dplyr::n(), 0, 0.035), 0), 1),
    y = pmin(pmax(.data$y_center + stats::rnorm(dplyr::n(), 0, 0.035), 0), 1),
    x = dplyr::if_else(.data$valid, .data$x, NA_real_),
    y = dplyr::if_else(.data$valid, .data$y, NA_real_),
    pupil = 3.10 +
      0.00012 * .data$time +
      .data$condition_effect +
      0.025 * .data$subject_index +
      stats::rnorm(dplyr::n(), 0, 0.05),
    pupil = dplyr::if_else(.data$artifact, .data$pupil + 0.75, .data$pupil),
    pupil = dplyr::if_else(.data$valid, .data$pupil, NA_real_),
    is_fixation = .data$valid & .data$time %% 200 == 0,
    is_saccade = .data$valid & .data$time %% 200 == 100,
    event_label = dplyr::case_when(
      .data$time == 0 ~ "stimulus_onset",
      .data$time == 2000 ~ "response",
      TRUE ~ NA_character_
    ),
    target_x = 0.75,
    target_y = 0.50,
    is_check_target = .data$time %in% c(0, 2000),
    screen_width = 1,
    screen_height = 1,
    TIME = .data$time,
    BPOGX = .data$x,
    BPOGY = .data$y,
    BPOGV = .data$valid,
    LPMM = .data$pupil,
    RPMM = .data$pupil + stats::rnorm(dplyr::n(), 0, 0.015),
    AOI = .data$aoi_current
  ) |>
  dplyr::select(
    dplyr::all_of(c(
      "subject",
      "USER_FILE",
      "MEDIA_ID",
      "trial_global",
      "trial_index",
      "condition",
      "time",
      "TIME",
      "x",
      "y",
      "BPOGX",
      "BPOGY",
      "pupil",
      "LPMM",
      "RPMM",
      "valid",
      "BPOGV",
      "artifact",
      "aoi_current",
      "AOI",
      "is_fixation",
      "is_saccade",
      "event_label",
      "target_x",
      "target_y",
      "is_check_target",
      "screen_width",
      "screen_height"
    ))
  )

gazepoint_example_fixations <- gazepoint_example_master |>
  dplyr::filter(.data$is_fixation) |>
  dplyr::group_by(.data$subject, .data$MEDIA_ID, .data$trial_global) |>
  dplyr::mutate(FPOGID = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    USER_FILE = .data$subject,
    subject = .data$subject,
    MEDIA_ID = .data$MEDIA_ID,
    trial_global = .data$trial_global,
    condition = .data$condition,
    FPOGID = .data$FPOGID,
    FPOGS = .data$time,
    FPOGD = 120,
    FPOGX = .data$x,
    FPOGY = .data$y,
    FPOGV = .data$valid,
    AOI = .data$aoi_current
  )

gazepoint_example_aoi_geometry <- tibble::tibble(
  media_id = rep(c("stim1", "stim2"), each = 3),
  aoi = rep(c("AOI 0", "AOI 1", "AOI 2"), times = 2),
  x_min = rep(c(0.15, 0.40, 0.65), times = 2),
  y_min = rep(c(0.35, 0.35, 0.35), times = 2),
  x_max = rep(c(0.35, 0.60, 0.85), times = 2),
  y_max = rep(c(0.65, 0.65, 0.65), times = 2)
)

gazepoint_example_aoi_windows <- summarise_gazepoint_aoi_windows(
  gazepoint_example_master,
  windows = c(0, 500, 1000, 1500, 2000),
  time_col = "time",
  aoi_col = "aoi_current",
  subject_col = "subject",
  condition_col = "condition",
  group_cols = c("subject", "MEDIA_ID", "trial_global"),
  target_aoi_values = "AOI 2",
  distractor_aoi_values = c("AOI 0", "AOI 1")
)

gazepoint_example_pupil_windows <- summarise_gazepoint_pupil_windows(
  gazepoint_example_master,
  pupil_col = "pupil",
  time_col = "time",
  windows = c(0, 500, 1000, 1500, 2000),
  group_cols = c("subject", "MEDIA_ID", "trial_global", "condition"),
  min_valid_samples = 1
)

save(
  gazepoint_example_master,
  file = "data/gazepoint_example_master.rda",
  compress = "xz"
)

save(
  gazepoint_example_fixations,
  file = "data/gazepoint_example_fixations.rda",
  compress = "xz"
)

save(
  gazepoint_example_aoi_geometry,
  file = "data/gazepoint_example_aoi_geometry.rda",
  compress = "xz"
)

save(
  gazepoint_example_aoi_windows,
  file = "data/gazepoint_example_aoi_windows.rda",
  compress = "xz"
)

save(
  gazepoint_example_pupil_windows,
  file = "data/gazepoint_example_pupil_windows.rda",
  compress = "xz"
)

message("Created example datasets in data/.")
