# CRAN submission review — gp3tools 2.4.1

**Status: NOT SUBMITTED TO CRAN (22 October 2026).**

- Source candidate: GitHub `v2.4.1` — https://github.com/stefanosbalaskas/gp3tools/releases/tag/v2.4.1
- Source archive: `gp3tools_2.4.1.tar.gz` attached to the GitHub Release.
- CRAN version before proposed submission: `2.3.0`.
- Exact-release GitHub workflow: https://github.com/stefanosbalaskas/gp3tools/actions/runs/39761892494 (completed successfully after building and checking the release source archive with `R CMD check --as-cran`).
- October documentation: reproducible synthetic illustrated vignette with explicit interpretation limitations; no empirical instrument validation or automatic calibration claim.

## CRAN-maintainer verification still required

1. Independently examine the exact archive's `00check.log`, any NOTES, installed sources and examples, supported R platforms and CRAN incoming checks. Workflow success alone is not the same as CRAN acceptance.
2. Confirm reverse-dependency effects, links/DOIs, URL validity, and policy compliance. No existing tags or historical CRAN packages should be overwritten.
3. Submit the exact validated `gp3tools_2.4.1.tar.gz` archive through CRAN's maintainer-authenticated channel. Confirm the response email and wait for CRAN processing. This has **not** happened in this campaign.
4. **Publication ordering blocker:** `eyeprocess (>= 0.12.0)` is under `Suggests`, but CRAN currently distributes 0.11.1. Verify downstream CRAN installation with the exact repository dependencies, preferably after eyeprocess 0.12.1 is accepted, before submitting gp3tools 2.4.1.

This file is a release evidence record, not a submission receipt.
