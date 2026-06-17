# 03_render_paper_showcase.R
# Render the paper-only gp3tools synthetic showcase Rmd.

base_dir <- "C:/Users/Stefanos-PC/Desktop/gp3tools_paper_showcase"
rmd_file <- file.path(base_dir, "paper_rmd", "02_gp3tools_paper_showcase.Rmd")
out_dir <- file.path(base_dir, "rendered_html")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(rmd_file)) {
  stop("Rmd file not found: ", rmd_file)
}

rmarkdown::render(
  input = rmd_file,
  output_format = "html_document",
  output_dir = out_dir,
  clean = TRUE
)

cat("Rendered paper-only showcase to:\n")
cat(file.path(out_dir, "02_gp3tools_paper_showcase.html"), "\n")
