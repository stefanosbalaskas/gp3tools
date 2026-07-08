# Read external facial-analysis exports

Reads one or more external facial-analysis CSV files into a single tidy
table. The helper is designed for outputs produced outside Gazepoint,
such as OpenFace-, py-feat-, MediaPipe-, FaceReader-, or generic
frame-level facial behaviour exports. It does not infer facial
expressions from Gazepoint CSV files.

## Usage

``` r
read_gazepoint_face_export(
  path,
  source = c("auto", "openface", "pyfeat", "mediapipe", "facereader", "generic"),
  participant_id = NULL,
  session_id = NULL,
  recursive = TRUE,
  trim_names = TRUE,
  encoding = "UTF-8",
  na = c("", "NA", "NaN"),
  ...
)
```

## Arguments

- path:

  Path to one CSV file, several CSV files, or a directory containing CSV
  files.

- source:

  Facial-analysis source. Use `"auto"` to infer a likely source from
  column names, or one of `"openface"`, `"pyfeat"`, `"mediapipe"`,
  `"facereader"`, or `"generic"`.

- participant_id:

  Optional participant identifier. Either length one or the same length
  as the number of files read.

- session_id:

  Optional session identifier. Either length one or the same length as
  the number of files read.

- recursive:

  If `path` is a directory, should CSV files be searched recursively?

- trim_names:

  Should leading and trailing whitespace be removed from column names?

- encoding:

  File encoding passed to
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html).

- na:

  Character values treated as missing.

- ...:

  Additional arguments passed to
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html).

## Value

A tibble with metadata columns identifying the source file and detected
source. The returned object has class `gp3_face_export`.

## Examples

``` r
tmp <- tempfile(fileext = ".csv")
write.csv(
  data.frame(
    frame = 1:2,
    timestamp = c(0, 0.033),
    confidence = c(0.98, 0.97),
    success = c(1, 1),
    AU12_r = c(0.1, 0.2)
  ),
  tmp,
  row.names = FALSE
)

read_gazepoint_face_export(tmp)
#> # A tibble: 2 × 10
#>   gp3_face_file        gp3_face_path      gp3_face_source gp3_face_participant…¹
#>   <chr>                <chr>              <chr>           <chr>                 
#> 1 file1c4774f21c25.csv /tmp/Rtmp4j1l1s/f… openface        NA                    
#> 2 file1c4774f21c25.csv /tmp/Rtmp4j1l1s/f… openface        NA                    
#> # ℹ abbreviated name: ¹​gp3_face_participant_id
#> # ℹ 6 more variables: gp3_face_session_id <chr>, frame <int>, timestamp <dbl>,
#> #   confidence <dbl>, success <int>, AU12_r <dbl>
```
