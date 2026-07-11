# Working safely with private Gazepoint exports

Gazepoint exports can contain participant-level behavioural data. This
article summarises practical safeguards for using `gp3tools` in
reproducible workflows without accidentally sharing private raw exports.

## Keep raw exports outside the package repository

Store raw Gazepoint exports in a private project folder, not inside the
package repository. Use output folders for derived summaries, plots, and
reports.

Example local structure:

``` text
study_private_exports/
  User 1_all_gaze.csv
  User 1_fixations.csv
  Data_Summary_export_*.csv

gp3tools_outputs/
  study1_sampling.csv
  study1_quality.csv
  study1_aoi_table.csv
  study1_report.html
```

## Do not commit participant exports

Before committing, always check:

``` bash
git status --short
```

Only source files, documentation, tests, and synthetic example data
should be committed. Raw participant exports should remain outside Git
or be ignored.

## Use synthetic examples in issues

For public bug reports, prefer:

- column names;
- synthetic rows;
- small anonymised examples;
- screenshots with participant identifiers removed.

Do not post raw gaze streams, fixation streams, participant IDs,
timestamps, or study-specific labels unless they have been fully
anonymised.

## Separate raw data from analysis-ready outputs

`gp3tools` workflows can write CSV summaries, diagnostic plots, and HTML
reports. Review these outputs before sharing them. Even derived
summaries can reveal study structure, condition names, or participant
identifiers if these are retained in the input files.

## Recommended habit

For each analysis session:

1.  Keep raw Gazepoint exports in a private folder.
2.  Run import, QC, preprocessing, modelling, and reporting workflows
    locally.
3.  Inspect generated outputs before sharing.
4.  Commit only code, documentation, tests, and synthetic examples.
5.  Re-run `git status --short` before every commit.
