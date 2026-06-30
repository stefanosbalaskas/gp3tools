# Read multiple Gazepoint CSV exports from a folder

Reads all Gazepoint all-gaze or fixation CSV exports in a folder that
match a filename pattern and combines them into one tibble.

## Usage

``` r
read_gazepoint_folder(
  folder,
  pattern = "\\.csv$",
  source_col = "USER_FILE",
  recursive = FALSE,
  ...
)
```

## Arguments

- folder:

  Path to the folder containing Gazepoint CSV exports.

- pattern:

  Regular expression used to select files. For example,
  `"_all_gaze\\.csv$"` or `"_fixations\\.csv$"`.

- source_col:

  Name of the column storing the source filename.

- recursive:

  Logical. If `TRUE`, search subfolders recursively.

- ...:

  Additional arguments passed to
  [`read_gazepoint()`](https://stefanosbalaskas.github.io/gp3tools/reference/read_gazepoint.md).

## Value

A tibble containing all matching files combined row-wise.
