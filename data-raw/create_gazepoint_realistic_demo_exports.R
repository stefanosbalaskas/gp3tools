set.seed(20260616)
out_dir <- file.path(
"inst",
"extdata",
"gazepoint_realistic_demo_exports"
)
if (dir.exists(out_dir)) {
unlink(out_dir, recursive = TRUE, force = TRUE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
gazepoint_cols <- c(
"MEDIA_ID",
"MEDIA_NAME",
"CNT",
"TIME(2026/02/20 00:53:57.275)",
"TIMETICK(f=10000000)",
"FPOGX",
"FPOGY",
"FPOGS",
"FPOGD",
"FPOGID",
"FPOGV",
"BPOGX",
"BPOGY",
"BPOGV",
"CX",
"CY",
"CS",
"KB",
"KBS",
"USER",
"LPCX",
"LPCY",
"LPD",
"LPS",
"LPV",
"RPCX",
"RPCY",
"RPD",
"RPS",
"RPV",
"BKID",
"BKDUR",
"BKPMIN",
"LPMM",
"LPMMV",
"RPMM",
"RPMMV",
"DIAL",
"DIALV",
"GSR",
"GSR_US",
"GSR_US_TONIC",
"GSR_US_PHASIC",
"GSRV",
"HR",
"HRV",
"HRP",
"IBI",
"TTL0",
"TTL1",
"TTL2",
"TTL3",
"TTL4",
"TTL5",
"TTL6",
"TTLV",
"PIXS",
"PIXV",
"AOI",
"SACCADE_MAG",
"SACCADE_DIR",
"VID_FRAME",
""
)
make_empty_export <- function(n) {
x <- as.data.frame(
replicate(length(gazepoint_cols), rep("", n), simplify = FALSE),
stringsAsFactors = FALSE,
check.names = FALSE
)
names(x) <- gazepoint_cols
x
}
clamp01 <- function(x) {
pmin(pmax(x, 0.001), 0.999)
}
assign_aoi <- function(x, y) {
out <- rep("", length(x))
out[x >= 0.62 & x <= 0.90 & y >= 0.30 & y <= 0.60] <- "AOI 0"
out[x >= 0.35 & x <= 0.60 & y >= 0.62 & y <= 0.88] <- "AOI 1"
out[x >= 0.10 & x <= 0.35 & y >= 0.20 & y <= 0.48] <- "AOI 2"
out
}
simulate_media_samples <- function(user_id, media_id, n_samples = 610, hz = 60) {
time_sec <- seq(0, by = 1 / hz, length.out = n_samples)
media_name <- paste0("DemoMedia", media_id)
subject_shift_x <- rnorm(1, 0, 0.025)
subject_shift_y <- rnorm(1, 0, 0.025)
x <- numeric(n_samples)
y <- numeric(n_samples)
if (media_id == 0) {
# Early target attention to AOI 2, then scattered gaze.
target_phase <- time_sec >= 0.35 & time_sec <= 2.20
x[!target_phase] <- rnorm(sum(!target_phase), 0.50, 0.18)
y[!target_phase] <- rnorm(sum(!target_phase), 0.54, 0.17)
x[target_phase] <- rnorm(sum(target_phase), 0.23, 0.045)
y[target_phase] <- rnorm(sum(target_phase), 0.34, 0.045)
} else {
# Later attention to AOI 0 with occasional AOI 1 visits.
target_phase <- time_sec >= 6.40 & time_sec <= 9.30
alt_phase <- time_sec >= 8.00 & time_sec <= 8.70 & user_id %% 3 == 0
x[!target_phase] <- rnorm(sum(!target_phase), 0.48, 0.18)
y[!target_phase] <- rnorm(sum(!target_phase), 0.52, 0.17)
x[target_phase] <- rnorm(sum(target_phase), 0.75, 0.055)
y[target_phase] <- rnorm(sum(target_phase), 0.45, 0.055)
x[alt_phase] <- rnorm(sum(alt_phase), 0.48, 0.045)
y[alt_phase] <- rnorm(sum(alt_phase), 0.74, 0.045)
}
x <- clamp01(x + subject_shift_x)
y <- clamp01(y + subject_shift_y)
# Gazepoint-like validity pattern.
fpogv_prob <- 0.82
bpogv_prob <- 0.97
pupil_prob <- 0.97
# Intentionally include a few warning-quality cases.
if (user_id == 3 && media_id == 1) {
fpogv_prob <- 0.66
bpogv_prob <- 0.90
pupil_prob <- 0.90
}
if (user_id == 5 && media_id == 0) {
fpogv_prob <- 0.55
bpogv_prob <- 0.88
pupil_prob <- 0.58
}
fpogv <- rbinom(n_samples, 1, fpogv_prob)
bpogv <- rbinom(n_samples, 1, bpogv_prob)
lpv <- rbinom(n_samples, 1, pupil_prob)
rpv <- rbinom(n_samples, 1, pupil_prob)
pupil_base <- 3.55 + rnorm(1, 0, 0.18)
pupil_condition_effect <- ifelse(media_id == 1, 0.12, 0.00)
pupil_wave <- 0.06 * sin(2 * pi * time_sec / max(time_sec))
lpmm <- pupil_base + pupil_condition_effect + pupil_wave + rnorm(n_samples, 0, 0.05)
rpmm <- pupil_base + pupil_condition_effect + pupil_wave + rnorm(n_samples, 0, 0.05)
aoi <- assign_aoi(x, y)
out <- make_empty_export(n_samples)
out[["MEDIA_ID"]] <- media_id
out[["MEDIA_NAME"]] <- media_name
out[["CNT"]] <- seq_len(n_samples) - 1
out[["TIME(2026/02/20 00:53:57.275)"]] <- sprintf("%.5f", time_sec)
out[["TIMETICK(f=10000000)"]] <- 30000000000 + round(time_sec * 10000000) + user_id * 100000000
out[["FPOGX"]] <- sprintf("%.5f", x + rnorm(n_samples, 0, 0.008))
out[["FPOGY"]] <- sprintf("%.5f", y + rnorm(n_samples, 0, 0.008))
out[["FPOGS"]] <- sprintf("%.5f", pmax(time_sec - 0.08, 0))
out[["FPOGD"]] <- sprintf("%.5f", pmin(time_sec, 0.35))
out[["FPOGID"]] <- floor(seq_len(n_samples) / 12) + 1
out[["FPOGV"]] <- fpogv
out[["BPOGX"]] <- sprintf("%.5f", x)
out[["BPOGY"]] <- sprintf("%.5f", y)
out[["BPOGV"]] <- bpogv
out[["CX"]] <- sprintf("%.5f", x + rnorm(n_samples, 0, 0.010))
out[["CY"]] <- sprintf("%.5f", y + rnorm(n_samples, 0, 0.010))
out[["CS"]] <- 0
out[["KB"]] <- 0
out[["KBS"]] <- 0
out[["USER"]] <- user_id
out[["LPCX"]] <- sprintf("%.5f", x - 0.015 + rnorm(n_samples, 0, 0.010))
out[["LPCY"]] <- sprintf("%.5f", y + rnorm(n_samples, 0, 0.010))
out[["LPD"]] <- sprintf("%.5f", lpmm * 0.10)
out[["LPS"]] <- 1
out[["LPV"]] <- lpv
out[["RPCX"]] <- sprintf("%.5f", x + 0.015 + rnorm(n_samples, 0, 0.010))
out[["RPCY"]] <- sprintf("%.5f", y + rnorm(n_samples, 0, 0.010))
out[["RPD"]] <- sprintf("%.5f", rpmm * 0.10)
out[["RPS"]] <- 1
out[["RPV"]] <- rpv
out[["BKID"]] <- 0
out[["BKDUR"]] <- 0
out[["BKPMIN"]] <- 0
out[["LPMM"]] <- sprintf("%.5f", lpmm)
out[["LPMMV"]] <- lpv
out[["RPMM"]] <- sprintf("%.5f", rpmm)
out[["RPMMV"]] <- rpv
out[["DIAL"]] <- 0
out[["DIALV"]] <- ifelse(user_id >= 6, 1, 0)
out[["GSR"]] <- sprintf("%.5f", 0.10 + rnorm(n_samples, 0, 0.01))
out[["GSR_US"]] <- sprintf("%.5f", 0.10 + rnorm(n_samples, 0, 0.01))
out[["GSR_US_TONIC"]] <- sprintf("%.5f", 0.08 + rnorm(n_samples, 0, 0.005))
out[["GSR_US_PHASIC"]] <- sprintf("%.5f", 0.02 + rnorm(n_samples, 0, 0.005))
out[["GSRV"]] <- ifelse(user_id >= 6, 1, 0)
out[["HR"]] <- sprintf("%.5f", 72 + rnorm(n_samples, 0, 2))
out[["HRV"]] <- ifelse(user_id >= 6, 1, 0)
out[["HRP"]] <- 0
out[["IBI"]] <- 0
out[["TTL0"]] <- 0
out[["TTL1"]] <- 0
out[["TTL2"]] <- 0
out[["TTL3"]] <- 0
out[["TTL4"]] <- 0
out[["TTL5"]] <- 0
out[["TTL6"]] <- 0
out[["TTLV"]] <- ifelse(user_id >= 6, 1, 0)
out[["PIXS"]] <- 1
out[["PIXV"]] <- 1
out[["AOI"]] <- aoi
out[["SACCADE_MAG"]] <- sprintf("%.5f", c(0, sqrt(diff(x)^2 + diff(y)^2)))
out[["SACCADE_DIR"]] <- sprintf("%.5f", c(0, atan2(diff(y), diff(x))))
out[["VID_FRAME"]] <- floor(time_sec * 30)
out
}
simulate_fixations <- function(all_gaze, user_id, media_id, hz = 60) {
n_fix <- sample(24:34, 1)
start_idx <- sort(sample(seq(1, nrow(all_gaze) - 20), n_fix))
duration_samples <- sample(8:32, n_fix, replace = TRUE)
out <- make_empty_export(n_fix)
media_name <- unique(all_gaze[["MEDIA_NAME"]])[1]
time_col <- "TIME(2026/02/20 00:53:57.275)"
for (i in seq_len(n_fix)) {
idx <- start_idx[i]
idx_end <- min(idx + duration_samples[i], nrow(all_gaze))
x <- mean(as.numeric(all_gaze[["BPOGX"]][idx:idx_end]), na.rm = TRUE)
y <- mean(as.numeric(all_gaze[["BPOGY"]][idx:idx_end]), na.rm = TRUE)
start_time <- as.numeric(all_gaze[[time_col]][idx])
dur_sec <- (idx_end - idx + 1) / hz
out[["MEDIA_ID"]][i] <- media_id
out[["MEDIA_NAME"]][i] <- media_name
out[["CNT"]][i] <- i - 1
out[[time_col]][i] <- sprintf("%.5f", start_time)
out[["TIMETICK(f=10000000)"]][i] <- 30000000000 + round(start_time * 10000000) + user_id * 100000000
out[["FPOGX"]][i] <- sprintf("%.5f", x)
out[["FPOGY"]][i] <- sprintf("%.5f", y)
out[["FPOGS"]][i] <- sprintf("%.5f", start_time)
out[["FPOGD"]][i] <- sprintf("%.5f", dur_sec)
out[["FPOGID"]][i] <- i
out[["FPOGV"]][i] <- 1
out[["BPOGX"]][i] <- sprintf("%.5f", x)
out[["BPOGY"]][i] <- sprintf("%.5f", y)
out[["BPOGV"]][i] <- 1
out[["USER"]][i] <- user_id
out[["LPV"]][i] <- 1
out[["RPV"]][i] <- 1
out[["LPMM"]][i] <- sprintf("%.5f", mean(as.numeric(all_gaze[["LPMM"]][idx:idx_end]), na.rm = TRUE))
out[["LPMMV"]][i] <- 1
out[["RPMM"]][i] <- sprintf("%.5f", mean(as.numeric(all_gaze[["RPMM"]][idx:idx_end]), na.rm = TRUE))
out[["RPMMV"]][i] <- 1
out[["AOI"]][i] <- assign_aoi(x, y)
out[["SACCADE_MAG"]][i] <- 0
out[["SACCADE_DIR"]][i] <- 0
out[["VID_FRAME"]][i] <- floor(start_time * 30)
}
out
}
n_users <- 12
media_ids <- c(0, 1)
n_samples_per_media <- 610
hz <- 60
for (user_id in seq_len(n_users) - 1) {
all_user <- list()
fix_user <- list()
for (media_id in media_ids) {
all_media <- simulate_media_samples(
user_id = user_id,
media_id = media_id,
n_samples = n_samples_per_media,
hz = hz
)
fix_media <- simulate_fixations(
all_gaze = all_media,
user_id = user_id,
media_id = media_id,
hz = hz
)
all_user[[as.character(media_id)]] <- all_media
fix_user[[as.character(media_id)]] <- fix_media
}
all_user <- do.call(rbind, all_user)
fix_user <- do.call(rbind, fix_user)
rownames(all_user) <- NULL
rownames(fix_user) <- NULL
utils::write.csv(
all_user,
file = file.path(out_dir, paste0("synthetic_user_", sprintf("%02d", user_id), "_all_gaze.csv")),
row.names = FALSE,
na = ""
)
utils::write.csv(
fix_user,
file = file.path(out_dir, paste0("synthetic_user_", sprintf("%02d", user_id), "_fixations.csv")),
row.names = FALSE,
na = ""
)
}
summary_export <- data.frame(
section = c(
"Synthetic Gazepoint demo",
"Number of synthetic users",
"Media stimuli",
"Sampling rate",
"Duration per media",
"Important note"
),
value = c(
"Generated by gp3tools data-raw/create_gazepoint_realistic_demo_exports.R",
n_users,
length(media_ids),
paste0(hz, " Hz"),
paste0(round(n_samples_per_media / hz, 2), " seconds"),
"Synthetic public data; no real participant rows are included"
),
stringsAsFactors = FALSE
)
utils::write.csv(
summary_export,
file = file.path(out_dir, "Data_Summary_export_synthetic_demo.csv"),
row.names = FALSE
)
cat("Synthetic realistic Gazepoint demo exports written to:\n")
cat(normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n\n")
cat("Files created:\n")
print(list.files(out_dir))
