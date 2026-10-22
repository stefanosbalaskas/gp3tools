# gp3tools 2.4.2 — qualified source archive for CRAN resubmission

**Status: GitHub v2.4.2 published 22 October 2026; NOT yet resubmitted or accepted by CRAN.**

Source: https://github.com/stefanosbalaskas/gp3tools/releases/tag/v2.4.2

Exact-source `R CMD build` and `R CMD check --as-cran` workflow: https://github.com/stefanosbalaskas/gp3tools/actions/runs/39836551625 — completed **success**.

Exact-head PR test matrix including `eyeprocess 0.11.1` CRAN regression lane: https://github.com/stefanosbalaskas/gp3tools/actions/runs/39825755159 — all six jobs successful.

Archive: `gp3tools_2.4.2.tar.gz`

Official release source SHA-256: `0c750abb402a50f43cd7646c0b67c85ebc493d6a17ad5ac4425d6bbe66e219e9` (GitHub-published asset metadata).

## Incoming-pretest incident

The submitted `gp3tools_2.4.1.tar.gz` was archived by CRAN after Debian r-devel check (1 ERROR and 1 WARNING; Windows OK).

- Static `eyeprocess::` references required newer optional exports unavailable under CRAN `eyeprocess 0.11.1`.
- One test called an unexported `simulate_gaze_survival_inputs` even though only existence of the optional package was checked.

The `2.4.2` correction replaces those static references with runtime public-export-checked access, guards optional integration tests by the full required API, adds older-CRAN regression tests, and avoids scientific-method changes.

## Maintainer action

Submit exactly the published `gp3tools_2.4.2.tar.gz` through https://cran.r-project.org/submit.html with the corrected-resubmission explanation, then complete CRAN confirmation by email. Do not resubmit the rejected `2.4.1` archive. CRAN's independent incoming checks and approval remain outstanding.
