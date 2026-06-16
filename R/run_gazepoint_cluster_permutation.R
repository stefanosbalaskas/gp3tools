#' Run paired cluster-based permutation tests
#'
#' Run a paired cluster-based permutation test on time-course data prepared by
#' `prepare_gazepoint_cluster_data()`. The function tests whether two
#' conditions diverge over time while controlling cluster-level inference using
#' a permutation distribution of maximum cluster statistics.
#'
#' Cluster-based permutation tests are intended for time-course inference.
#' They should not be used to discover a confirmatory time window and then test
#' that same window again in a second confirmatory model.
#'
#' @param data Cluster-ready data produced by
#'   `prepare_gazepoint_cluster_data()`.
#' @param condition_order Optional character vector of length 2 defining the
#'   two conditions and their order. The tested difference is condition 2 minus
#'   condition 1.
#' @param n_permutations Number of sign-flip permutations.
#' @param cluster_threshold Absolute t-statistic threshold for forming
#'   candidate clusters. For `tail = "greater"` or `tail = "less"`, the same
#'   positive threshold is used in the requested direction.
#' @param tail Direction of the test. `"two_sided"` tests positive and negative
#'   clusters. `"greater"` tests condition 2 greater than condition 1.
#'   `"less"` tests condition 2 less than condition 1.
#' @param cluster_stat Cluster statistic. `"sum_abs_t"` sums absolute
#'   t-statistics within a cluster. `"sum_t"` sums signed t-statistics and then
#'   uses the absolute value for cluster-level inference. `"size"` uses the
#'   number of time bins.
#' @param min_time_bins Minimum number of adjacent time bins required for a
#'   cluster to be retained.
#' @param seed Optional random seed for reproducible permutations.
#' @param paired Logical. Currently only paired within-subject sign-flip
#'   permutation is supported.
#'
#' @return A list containing observed time-course statistics, observed
#'   clusters, the permutation distribution, settings, and status fields.
#'
#' @export
#' @importFrom rlang .data
run_gazepoint_cluster_permutation <- function(
    data,
    condition_order = NULL,
    n_permutations = 1000,
    cluster_threshold = 2,
    tail = c("two_sided", "greater", "less"),
    cluster_stat = c("sum_abs_t", "sum_t", "size"),
    min_time_bins = 1,
    seed = NULL,
    paired = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  valid_logical <- function(x, arg) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
    }
  }

  valid_logical(paired, "paired")

  if (!paired) {
    stop(
      "Only paired within-subject cluster permutation is currently supported.",
      call. = FALSE
    )
  }

  tail <- match.arg(tail)
  cluster_stat <- match.arg(cluster_stat)

  if (!is.numeric(n_permutations) ||
      length(n_permutations) != 1L ||
      is.na(n_permutations) ||
      !is.finite(n_permutations) ||
      n_permutations < 1) {
    stop(
      "`n_permutations` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  n_permutations <- as.integer(n_permutations)

  if (!is.numeric(cluster_threshold) ||
      length(cluster_threshold) != 1L ||
      is.na(cluster_threshold) ||
      !is.finite(cluster_threshold) ||
      cluster_threshold <= 0) {
    stop(
      "`cluster_threshold` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_time_bins) ||
      length(min_time_bins) != 1L ||
      is.na(min_time_bins) ||
      !is.finite(min_time_bins) ||
      min_time_bins < 1) {
    stop(
      "`min_time_bins` must be a positive finite numeric scalar.",
      call. = FALSE
    )
  }

  min_time_bins <- as.integer(min_time_bins)

  if (!is.null(seed)) {
    if (!is.numeric(seed) ||
        length(seed) != 1L ||
        is.na(seed) ||
        !is.finite(seed)) {
      stop("`seed` must be NULL or a finite numeric scalar.", call. = FALSE)
    }


    set.seed(as.integer(seed))


  }

  required_cols <- c(
    ".gp3_cluster_subject",
    ".gp3_cluster_condition",
    ".gp3_cluster_time_bin",
    ".gp3_cluster_outcome"
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0L) {
    stop(
      "Missing required cluster-data columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  dat <- tibble::as_tibble(data)

  dat <- dat[
    is.finite(dat$.gp3_cluster_outcome) &
      is.finite(dat$.gp3_cluster_time_bin) &
      !is.na(dat$.gp3_cluster_subject) &
      !is.na(dat$.gp3_cluster_condition),
    ,
    drop = FALSE
  ]

  if (".gp3_cluster_status" %in% names(dat)) {
    dat <- dat[dat[[".gp3_cluster_status"]] == "ok", , drop = FALSE]
  }

  if (nrow(dat) == 0L) {
    stop("No valid cluster-data rows are available.", call. = FALSE)
  }

  dat$.gp3_cluster_subject <- factor(dat$.gp3_cluster_subject)
  dat$.gp3_cluster_condition <- factor(dat$.gp3_cluster_condition)

  available_conditions <- levels(droplevels(dat$.gp3_cluster_condition))

  if (is.null(condition_order)) {
    condition_order <- available_conditions
  } else {
    if (!is.character(condition_order) ||
        length(condition_order) != 2L ||
        any(is.na(condition_order)) ||
        any(!nzchar(condition_order))) {
      stop(
        "`condition_order` must be NULL or a character vector of length 2.",
        call. = FALSE
      )
    }
  }

  condition_order <- unique(condition_order)

  if (length(condition_order) != 2L) {
    stop(
      "Cluster permutation requires exactly two conditions. Found: ",
      paste(available_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  missing_requested_conditions <- setdiff(condition_order, available_conditions)

  if (length(missing_requested_conditions) > 0L) {
    stop(
      "Requested condition(s) not found in data: ",
      paste(missing_requested_conditions, collapse = ", "),
      call. = FALSE
    )
  }

  condition_1 <- condition_order[[1L]]
  condition_2 <- condition_order[[2L]]

  dat <- dat[
    as.character(dat$.gp3_cluster_condition) %in% condition_order,
    ,
    drop = FALSE
  ]

  dat$.gp3_cluster_condition <- factor(
    as.character(dat$.gp3_cluster_condition),
    levels = condition_order
  )

  wide <- dat[
    ,
    c(
      ".gp3_cluster_subject",
      ".gp3_cluster_condition",
      ".gp3_cluster_time_bin",
      ".gp3_cluster_outcome"
    ),
    drop = FALSE
  ]

  wide <- tidyr::pivot_wider(
    wide,
    names_from = ".gp3_cluster_condition",
    values_from = ".gp3_cluster_outcome"
  )

  if (!all(condition_order %in% names(wide))) {
    stop(
      "Both requested conditions must be present after reshaping.",
      call. = FALSE
    )
  }

  wide <- wide[
    is.finite(wide[[condition_1]]) &
      is.finite(wide[[condition_2]]),
    ,
    drop = FALSE
  ]

  if (nrow(wide) == 0L) {
    stop(
      "No paired subject-time bins are available for the requested conditions.",
      call. = FALSE
    )
  }

  wide$.gp3_cluster_difference <- wide[[condition_2]] - wide[[condition_1]]

  complete_subjects <- wide |>
    dplyr::count(
      .data[[".gp3_cluster_subject"]],
      name = ".gp3_n_time_bins"
    )

  n_time_bins_total <- dplyr::n_distinct(wide$.gp3_cluster_time_bin)

  complete_subjects <- complete_subjects |>
    dplyr::filter(.data[[".gp3_n_time_bins"]] == n_time_bins_total) |>
    dplyr::pull(.data[[".gp3_cluster_subject"]])

  wide <- wide[
    wide$.gp3_cluster_subject %in% complete_subjects,
    ,
    drop = FALSE
  ]

  if (nrow(wide) == 0L) {
    stop(
      "No subjects have complete paired data across all retained time bins.",
      call. = FALSE
    )
  }

  subjects <- levels(droplevels(factor(wide$.gp3_cluster_subject)))
  time_bins <- sort(unique(wide$.gp3_cluster_time_bin))

  if (length(subjects) < 2L) {
    stop(
      "At least two paired subjects are required for cluster permutation.",
      call. = FALSE
    )
  }

  safe_paired_t <- function(x) {
    x <- x[is.finite(x)]

    if (length(x) < 2L) {
      return(NA_real_)
    }

    sx <- stats::sd(x)

    if (!is.finite(sx) || sx == 0) {
      return(0)
    }

    mean(x) / (sx / sqrt(length(x)))

  }

  compute_timecourse <- function(diff_data) {
    diff_data |>
      dplyr::group_by(.data[[".gp3_cluster_time_bin"]]) |>
      dplyr::summarise(
        n_subjects = sum(is.finite(.data[[".gp3_cluster_difference"]])),
        mean_difference = mean(
          .data[[".gp3_cluster_difference"]],
          na.rm = TRUE
        ),
        sd_difference = stats::sd(
          .data[[".gp3_cluster_difference"]],
          na.rm = TRUE
        ),
        statistic = safe_paired_t(.data[[".gp3_cluster_difference"]]),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data[[".gp3_cluster_time_bin"]])
  }

  is_cluster_candidate <- function(statistic) {
    if (tail == "two_sided") {
      abs(statistic) >= cluster_threshold
    } else if (tail == "greater") {
      statistic >= cluster_threshold
    } else {
      statistic <= -cluster_threshold
    }
  }

  statistic_direction <- function(statistic) {
    dplyr::case_when(
      statistic > 0 ~ "positive",
      statistic < 0 ~ "negative",
      TRUE ~ "zero"
    )
  }

  cluster_value <- function(statistic) {
    if (cluster_stat == "sum_abs_t") {
      sum(abs(statistic), na.rm = TRUE)
    } else if (cluster_stat == "sum_t") {
      abs(sum(statistic, na.rm = TRUE))
    } else {
      length(statistic)
    }
  }

  find_clusters <- function(timecourse) {
    empty_clusters <- tibble::tibble(
      cluster_id = integer(0),
      cluster_direction = character(0),
      start_time_bin = numeric(0),
      end_time_bin = numeric(0),
      n_time_bins = integer(0),
      cluster_statistic = numeric(0),
      max_abs_statistic = numeric(0),
      mean_difference = numeric(0)
    )

    if (nrow(timecourse) == 0L) {
      return(empty_clusters)
    }

    tc <- timecourse |>
      dplyr::mutate(
        .gp3_candidate = is_cluster_candidate(.data[["statistic"]]),
        .gp3_direction = statistic_direction(.data[["statistic"]])
      )

    if (!any(tc$.gp3_candidate, na.rm = TRUE)) {
      return(empty_clusters)
    }

    cluster_id <- integer(nrow(tc))
    current_cluster <- 0L
    previous_candidate <- FALSE
    previous_direction <- NA_character_

    for (i in seq_len(nrow(tc))) {
      candidate_i <- isTRUE(tc$.gp3_candidate[[i]])
      direction_i <- tc$.gp3_direction[[i]]

      starts_new <- candidate_i &&
        (
          !previous_candidate ||
            is.na(previous_direction) ||
            direction_i != previous_direction
        )

      if (starts_new) {
        current_cluster <- current_cluster + 1L
      }

      if (candidate_i) {
        cluster_id[[i]] <- current_cluster
      }

      previous_candidate <- candidate_i
      previous_direction <- if (candidate_i) direction_i else NA_character_
    }

    tc$.gp3_cluster_id <- cluster_id

    clusters <- tc |>
      dplyr::filter(.data[[".gp3_cluster_id"]] > 0L) |>
      dplyr::group_by(.data[[".gp3_cluster_id"]]) |>
      dplyr::summarise(
        cluster_id = dplyr::first(.data[[".gp3_cluster_id"]]),
        cluster_direction = dplyr::first(.data[[".gp3_direction"]]),
        start_time_bin = min(.data[[".gp3_cluster_time_bin"]], na.rm = TRUE),
        end_time_bin = max(.data[[".gp3_cluster_time_bin"]], na.rm = TRUE),
        n_time_bins = dplyr::n(),
        cluster_statistic = cluster_value(.data[["statistic"]]),
        max_abs_statistic = max(abs(.data[["statistic"]]), na.rm = TRUE),
        mean_difference = mean(.data[["mean_difference"]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(.data[["n_time_bins"]] >= min_time_bins) |>
      dplyr::arrange(.data[["start_time_bin"]])

    if (nrow(clusters) == 0L) {
      return(empty_clusters)
    }

    if (".gp3_cluster_id" %in% names(clusters)) {
      clusters <- clusters[
        ,
        setdiff(names(clusters), ".gp3_cluster_id"),
        drop = FALSE
      ]
    }

    clusters$cluster_id <- seq_len(nrow(clusters))

    clusters <- clusters[
      ,
      c(
        "cluster_id",
        "cluster_direction",
        "start_time_bin",
        "end_time_bin",
        "n_time_bins",
        "cluster_statistic",
        "max_abs_statistic",
        "mean_difference"
      ),
      drop = FALSE
    ]

    tibble::as_tibble(clusters)

  }

  observed_timecourse <- compute_timecourse(wide)
  observed_clusters <- find_clusters(observed_timecourse)

  permute_once <- function() {
    signs <- sample(c(-1, 1), length(subjects), replace = TRUE)
    names(signs) <- subjects

    perm_data <- wide
    perm_data$.gp3_cluster_difference <-
      perm_data$.gp3_cluster_difference *
      signs[as.character(perm_data$.gp3_cluster_subject)]

    perm_timecourse <- compute_timecourse(perm_data)
    perm_clusters <- find_clusters(perm_timecourse)

    if (nrow(perm_clusters) == 0L) {
      return(0)
    }

    max(perm_clusters$cluster_statistic, na.rm = TRUE)

  }

  permutation_max <- vapply(
    seq_len(n_permutations),
    function(i) permute_once(),
    numeric(1)
  )

  permutation_distribution <- tibble::tibble(
    permutation = seq_len(n_permutations),
    max_cluster_statistic = permutation_max
  )

  if (nrow(observed_clusters) > 0L) {
    observed_clusters$p_value <- vapply(
      observed_clusters$cluster_statistic,
      function(x) {
        (sum(permutation_max >= x, na.rm = TRUE) + 1) /
          (length(permutation_max) + 1)
      },
      numeric(1)
    )

    observed_clusters$significant <- observed_clusters$p_value < 0.05

  } else {
    observed_clusters$p_value <- numeric(0)
    observed_clusters$significant <- logical(0)
  }

  observed_timecourse$cluster_id <- NA_integer_

  if (nrow(observed_clusters) > 0L) {
    for (i in seq_len(nrow(observed_clusters))) {
      in_cluster <- observed_timecourse$.gp3_cluster_time_bin >=
        observed_clusters$start_time_bin[[i]] &
        observed_timecourse$.gp3_cluster_time_bin <=
        observed_clusters$end_time_bin[[i]] &
        is_cluster_candidate(observed_timecourse$statistic) &
        statistic_direction(observed_timecourse$statistic) ==
        observed_clusters$cluster_direction[[i]]

      observed_timecourse$cluster_id[in_cluster] <-
        observed_clusters$cluster_id[[i]]
    }

  }

  observed_timecourse$point_candidate <- is_cluster_candidate(
    observed_timecourse$statistic
  )

  observed_timecourse$condition_1 <- condition_1
  observed_timecourse$condition_2 <- condition_2
  observed_timecourse$difference_label <- paste(condition_2, "-", condition_1)

  model_status <- if (nrow(observed_clusters) == 0L) {
    "no_clusters"
  } else if (any(observed_clusters$significant, na.rm = TRUE)) {
    "significant_clusters"
  } else {
    "clusters_not_significant"
  }

  out <- list(
    timecourse = observed_timecourse,
    clusters = observed_clusters,
    permutation_distribution = permutation_distribution,
    data = wide,
    settings = list(
      condition_order = condition_order,
      condition_1 = condition_1,
      condition_2 = condition_2,
      difference = paste(condition_2, "-", condition_1),
      n_permutations = n_permutations,
      cluster_threshold = cluster_threshold,
      tail = tail,
      cluster_stat = cluster_stat,
      min_time_bins = min_time_bins,
      seed = seed,
      paired = paired
    ),
    model_status = model_status,
    n_subjects = length(subjects),
    n_time_bins = length(time_bins),
    warning = paste(
      "Cluster-based permutation tests are for time-course inference;",
      "do not use them to select a confirmatory window and then retest that same window."
    )
  )

  class(out) <- c("gp3_cluster_permutation", class(out))

  out
}
