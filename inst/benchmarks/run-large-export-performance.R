if (file.exists("DESCRIPTION") && dir.exists("R")) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop(
      "Running from a source checkout requires the devtools package.",
      call. = FALSE
    )
  }
  devtools::load_all(".", quiet = TRUE)
} else {
  suppressPackageStartupMessages(library(gp3tools))
}

output_dir <- Sys.getenv(
  "GP3TOOLS_BENCHMARK_OUTPUT",
  unset = file.path(tempdir(), "gp3tools-large-export-performance")
)

scales <- data.frame(
  total_rows = c(60000L, 240000L, 960000L),
  n_files = c(1L, 4L, 16L)
)

cat("Running gp3tools large-export performance profile\n")
cat("Output directory: ", output_dir, "\n", sep = "")

benchmark <- benchmark_gazepoint_export_performance(
  scales = scales,
  operations = c("generate", "import", "master", "sampling", "quality"),
  trials = 3L,
  limits = gp3tools_performance_limits(),
  stop_on_regression = TRUE,
  output_dir = file.path(output_dir, "generated-exports"),
  keep_exports = FALSE,
  on_error = "stop"
)

files <- write_gazepoint_performance_benchmark(
  benchmark,
  output_dir = output_dir,
  prefix = "gp3tools-large-export"
)

print(benchmark)
print(benchmark$summary)
print(benchmark$regression)
cat("Written files:\n")
cat(paste0("  ", files), sep = "\n")
cat("\n")
