# gp3tools 2.4.2 — CRAN resubmission evidence (candidate)

**Status: not yet qualified or resubmitted. Do not re-upload gp3tools_2.4.1.tar.gz.**

CRAN's incoming pretest for gp3tools 2.4.1 on 22 October 2026 failed under Debian r-devel (1 ERROR, 1 WARNING), though Windows was OK.

- WARNING: direct `eyeprocess::` references to newer survival and gaze-quality exports were unresolved against the installed older CRAN eyeprocess.
- ERROR: `test-gaze-survival-adapter.R` invoked `eyeprocess::simulate_gaze_survival_inputs` after checking only package installation, not namespace capability (1 failed test; 7762 passes and 6 skips).

Pretest record: https://win-builder.r-project.org/incoming_pretest/gp3tools_2.4.1_20261022_131132/

## Resolution in candidate 2.4.2

1. Dynamic, public-export-only lookup is used only *after* a validated optional API capability guard. It prevents static missing-export warnings without silently supplying substitute analysis.
2. Survival tests check the full public API, including the simulator, before running; other core tests continue.
3. A regression test confirms graceful failure against the older optional engine.
4. Minimum optional `eyeprocess` version reflects CRAN 0.11.1; advanced survival/spatial-quality adapters still require all newer exports and error clearly if absent.
5. Ordinary CI uses eyeprocess v0.12.1; an extra job explicitly installs CRAN eyeprocess 0.11.1 and runs R CMD check --as-cran.

**Before resubmitting:** require green exact-head CI including older-engine lane, an independently checked `gp3tools_2.4.2.tar.gz` source archive, and a revised GitHub tag. The historic v2.4.1 tag must remain unchanged. Submit only the validated new archive through CRAN and complete the maintainer confirmation.
