# gp3tools naming conventions

## Canonical language

gp3tools uses British English in new user-facing documentation and
function names. New summary helpers should use the `summarise_*` prefix.

## Backward compatibility

Existing exported `summarize_*` functions remain available. They are not
removed, deprecated, or behaviourally changed by this policy. When an
existing American export lacks a British equivalent, a thin British
alias is provided.

## Documentation

Reference pages and articles should present the British name first and
identify the American name as a compatibility alias when both are
available.

## Scope

This policy covers spelling consistency only. It does not rename
functions whose meaning, statistical contract, or return structure would
change. Broader API renaming requires a separate versioned migration
decision.

## Programmatic audit

``` r

policy <- gp3tools_naming_policy()
audit <- audit_gazepoint_naming_consistency()
audit$pairs
```

A release candidate should contain no `missing_british_alias` rows.
