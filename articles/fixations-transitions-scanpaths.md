# Fixation, transitions, and scanpaths

This article describes fixation-level workflows, AOI transitions, and
scanpath-style summaries.

## Fixation alignment

``` r

fixation_aligned <- prepare_gazepoint_fixation_aligned_data(
  fixation_data = fixations,
  trial_data = trial_summary,
  by = c('participant_id', 'trial_id')
)
```

## Transition matrices

``` r

transition_matrix <- compute_gazepoint_transition_matrix(
  fixation_aligned,
  aoi_col = 'AOI',
  group_cols = c('participant_id', 'trial_id')
)

time_varying <- compute_gazepoint_time_varying_transition_matrix(
  fixation_aligned,
  aoi_col = 'AOI',
  time_col = 'time_sec',
  bin_width = 0.500
)
```

## Advanced modelling preparation

``` r

semi_markov_data <- prepare_gazepoint_semimarkov_data(fixation_aligned)
hmm_data <- prepare_gazepoint_hmm_data(fixation_aligned)
transition_nb <- fit_gazepoint_transition_count_nb_sensitivity(time_varying)
```

## Interpretation note

Transitions describe observed sequences between AOIs. They should be
interpreted as scanpath or visual-attention dynamics, not as direct
proof of decision strategy.
