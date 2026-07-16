# Create a manual event-review template

Create a sequence-level CSV-ready template for manually annotating
fixation intervals. The returned table contains one or more placeholder
rows per participant/trial sequence together with the observed sequence
boundaries. Reviewers should fill `start_time` and `end_time`, set
`review_status` to `"accepted"`, and add rows when a sequence contains
multiple events.

## Usage

``` r
create_gazepoint_event_review_template(
  data,
  id_col = "USER_ID",
  trial_col = NULL,
  group_cols = NULL,
  time_col = "TIME",
  rows_per_sequence = 1L,
  event_type = "fixation",
  reviewer = NA_character_
)
```

## Arguments

- data:

  Sample-level gaze data.

- id_col:

  Participant identifier column.

- trial_col:

  Optional trial identifier column.

- group_cols:

  Optional additional sequence columns.

- time_col:

  Timestamp column.

- rows_per_sequence:

  Number of placeholder rows created per sequence.

- event_type:

  Event label inserted in the template.

- reviewer:

  Optional reviewer identifier.

## Value

A data frame suitable for export to CSV and manual review.

## Examples

``` r
gaze <- data.frame(
  USER_ID = rep(c("P01", "P02"), each = 10),
  trial = rep("T01", 20),
  TIME = rep(seq(0, 0.09, by = 0.01), 2),
  FPOGX = 0.5,
  FPOGY = 0.5
)

template <- create_gazepoint_event_review_template(
  gaze,
  trial_col = "trial"
)

template
#>   USER_ID trial review_event_id sequence_start sequence_end start_time end_time
#> 1     P01   T01               1              0         0.09         NA       NA
#> 2     P02   T01               1              0         0.09         NA       NA
#>   event_type review_status reviewer notes
#> 1   fixation       pending     <NA>  <NA>
#> 2   fixation       pending     <NA>  <NA>
```
