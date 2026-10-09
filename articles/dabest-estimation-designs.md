# DABEST-style estimation graphics for Gazepoint studies

## Raw observations, contrast estimation and scientific independence

The [DABEST Python](https://acclab.github.io/DABEST-python/) and
[official R dabestr implementation](https://acclab.github.io/dabestr/)
provide Gardner–Altman and Cumming estimation plots: raw measurements,
paired slopegraphs when warranted, bootstrap effect-size distributions,
and confidence intervals.

**Use the actual R dabestr package** when you want its validated
plotting API or BCa confidence intervals rather than copying Python
internals. A narrower original, participant-level ggplot implementation
is in the development branch of
[gpbiometrics](https://github.com/stefanosbalaskas/gpbiometrics),
`plot_gazepoint_estimation()`. For a strict GP3 analysis, first
aggregate fixation/AOI/pupil measures to the estimand’s
participant/trial unit.

``` r

# Conceptual example of a participant-level comparison, not real study data:
library(dabestr)
dabest_obj <- dabestr::load(
  summary_by_participant, x = Condition, y = MeanDwell,
  idx = c("Control", "Treatment")
)
dabestr::dabest_plot(dabestr::mean_diff(dabest_obj), float_contrast = FALSE)
```

Do **not** treat raw 60-Hz gaze samples as independent participants. For
repeated-participant conditions supply a unique subject ID to dabestr’s
paired loading mode, or use `gpbiometrics::plot_gazepoint_estimation`
with `paired=TRUE` on matched participant-level rows.

For model-adjusted contrasts or random-effects GLMM estimates, raw mean
difference plots are descriptive and should be accompanied by a separate
model-based effect/uncertainty visualization, not substituted for the
hierarchical inferential estimand.

This is an educational integration note and adds **no duplicate
estimator** to gp3tools. No release has been triggered.
