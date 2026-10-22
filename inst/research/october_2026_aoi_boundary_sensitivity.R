# Synthetic AOI boundary perturbation demonstration, no stable API additions.
gp3tools_october_aoi_points <- function(seed = 20261022L,
                                         participants = 12L, trials = 24L) {
  stopifnot(participants >= 4L, trials >= 4L)
  set.seed(seed)
  id <- rep(seq_len(participants), each = trials)
  near_edge <- stats::rbinom(length(id), 1L, 0.75) == 1L
  xpos <- ifelse(near_edge, stats::rnorm(length(id), 50, 2),
                 stats::runif(length(id), 30, 70))
  data.frame(participant = id, x = xpos, y = stats::runif(length(id), 0, 100))
}

gp3tools_october_aoi_sensitivity <- function(x, boundaries = seq(44, 56, 2),
                                              reference_boundary = 50) {
  stopifnot(is.data.frame(x),
            all(c("participant", "x", "y") %in% names(x)),
            !anyNA(x[, c("participant", "x", "y")]),
            all(is.finite(x$x)), all(is.finite(boundaries)),
            length(unique(x$participant)) >= 2L)
  base <- x$x <= reference_boundary
  rows <- lapply(boundaries, function(boundary) {
    assigned <- x$x <= boundary
    by_person <- split(as.numeric(assigned), x$participant)
    data.frame(boundary = boundary, row_aoi_fraction = mean(assigned),
               participant_equal_fraction = mean(vapply(by_person, mean, numeric(1))),
               changed_fraction = mean(assigned != base),
               n_participants = length(by_person), n_rows = nrow(x))
  })
  do.call(rbind, rows)
}

gp3tools_october_aoi_plot <- function(audit) {
  stopifnot(is.data.frame(audit),
            all(c("boundary", "participant_equal_fraction", "changed_fraction") %in% names(audit)))
  op <- graphics::par(mfrow = c(1L, 2L), mar = c(4, 4, 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot(audit$boundary, audit$participant_equal_fraction,
                 type = "b", pch = 19, col = "#24577B",
                 xlab = "Synthetic vertical AOI edge (pixels)",
                 ylab = "Mean AOI assignment fraction",
                 main = "Edge placement sensitivity", ylim = c(0, 1))
  graphics::abline(v = 50, lty = 2, col = "#999999")
  graphics::plot(audit$boundary, audit$changed_fraction,
                 type = "b", pch = 19, col = "#B05E43",
                 xlab = "Synthetic vertical AOI edge (pixels)",
                 ylab = "Fraction of changed assignments",
                 main = "Labels changing vs reference", ylim = c(0, 1))
  graphics::abline(v = 50, lty = 2, col = "#999999")
  invisible(audit)
}
