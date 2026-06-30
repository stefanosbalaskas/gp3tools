# Prepare fixation- or saccade-contingent aligned Gazepoint data

Align Gazepoint observations to a within-trial event such as first
fixation, first target-AOI entry, first fixation to a target AOI, first
saccade to a target AOI, or a custom event marker. The helper returns
the original data with event-aligned time, event metadata,
pre-event/post-event flags, and trial-level summaries that help separate
event-driven looking from looks that were already present before the
event.

## Usage

``` r
prepare_gazepoint_fixation_aligned_data(
  data,
  time_col,
  participant_col = NULL,
  trial_col = NULL,
  aoi_col = NULL,
  target_aoi = NULL,
  fixation_col = NULL,
  saccade_col = NULL,
  event_col = NULL,
  event_value = NULL,
  alignment_event = c("first_target_entry", "first_fixation_to_target",
    "first_saccade_to_aoi", "first_fixation", "custom"),
  baseline_window = NULL,
  analysis_window = NULL,
  keep_unaligned = FALSE,
  name = "gazepoint_fixation_aligned_data"
)
```

## Arguments

- data:

  A data frame containing Gazepoint samples, fixation rows, or
  trial-level time-course rows.

- time_col:

  Time column.

- participant_col:

  Optional participant column.

- trial_col:

  Optional trial column.

- aoi_col:

  Optional AOI column.

- target_aoi:

  Optional character vector identifying the target AOI(s).

- fixation_col:

  Optional fixation indicator column.

- saccade_col:

  Optional saccade indicator column.

- event_col:

  Optional custom event indicator column.

- event_value:

  Optional value(s) in `event_col` defining the custom event. If `NULL`
  and `alignment_event = "custom"`, `event_col` is interpreted as a
  logical-like indicator.

- alignment_event:

  Alignment event. Options are `"first_target_entry"`,
  `"first_fixation_to_target"`, `"first_saccade_to_aoi"`,
  `"first_fixation"`, and `"custom"`.

- baseline_window:

  Optional numeric vector of length two giving the aligned-time baseline
  window, for example `c(-200, 0)`.

- analysis_window:

  Optional numeric vector of length two giving the aligned-time analysis
  window, for example `c(0, 1000)`.

- keep_unaligned:

  Logical. If `FALSE`, groups without an alignment event are removed
  from `aligned_data`. Their status remains in `event_table`.

- name:

  Character label stored in the returned object.

## Value

A list with class `gp3_fixation_aligned_data`.
