# Claude Reference - NEPSroutines

## Project Overview

NEPSroutines is an R package for scaling NEPS (National Educational Panel Study) competence data. It implements IRT (Item Response Theory) routines for 1PL and 2PL models with binary and polytomous responses.

## Key Source Files

| File | Purpose |
|------|---------|
| `R/irt_analyses.R` | Core IRT modeling functions |
| `R/data_preparation.R` | Data validation and preparation |
| `R/dif_analysis.R` | Differential item functioning |
| `R/dimensionality_analysis.R` | Dimensionality testing |
| `R/linking.R` | Cross-wave parameter linking |
| `R/utils.R` | Utility and validation functions |
| `R/technical_report_*.r` | Quarto report generation |

## Core Functions

- `irt_analysis()` - Main IRT analysis entry point
- `dif_analysis()` - DIF testing
- `dim_analysis()` - Dimensionality analysis
- `linking()` - Parameter linking across waves
- `mv_item()`, `mv_person()` - Missing value analysis
- `prepare_resp()` - Data preparation
- `create_scores()`, `create_suf()` - Score generation

## Development Conventions

### Documentation
- Uses roxygen2 - edit comments in R files, then run `devtools::document()`
- Never edit `man/*.Rd` files directly

### Testing
- Framework: testthat (edition 3)
- Test fixtures in `tests/testthat/fixtures/`
- Run with `devtools::test()`

### Dependencies
- Core: TAM (IRT), ggplot2, dplyr/tidyr, haven
- R version: >= 4.3.0

## Common Mistakes to Avoid

### R CMD check fixes (2026-02-19)

**MASS package error in examples**: `irt_analysis()` internally uses TAM which uses CDM which requires MASS via `CDM:::CDM_require_namespace("MASS")`. In R CMD check environments, MASS may be unavailable even though it's a recommended package. Fix: wrap examples calling `irt_analysis()`, `dif_analysis()`, or similar slow IRT functions in `\dontrun{}` (not `\donttest{}`, since `--run-donttest` also fails).

**`NEPSroutines:::` calls**: The package uses `NEPSroutines:::fn()` to call its own internal functions. This generates a NOTE in R CMD check. Fix: remove the `NEPSroutines:::` prefix entirely - internal functions are directly accessible within the package namespace.

**`.Rbuildignore` for directories with content**: The pattern `^dirname$` excludes the directory entry but NOT files inside it. Use BOTH `^dirname` AND `^dirname$` patterns (or use `usethis::use_build_ignore("dirname")` which adds `^dirname$` and test that files inside are excluded). Verified by inspecting the tarball with `tar -tzf`.

**Roxygen brace escaping**: In roxygen2 comments, curly braces `{` and `}` must be escaped as `\{` and `\}` to avoid "Lost braces" NOTEs in Rd files. Common cases: `y in {0, 1}` → `y in \{0, 1\}`, `{+-}` → `\{+-\}`.

**RoxygenNote version mismatch**: If installed roxygen2 version doesn't match `RoxygenNote` in DESCRIPTION, `devtools::check()` won't re-document. Update `RoxygenNote` in DESCRIPTION to match installed version before running `devtools::document()`.

**WrightMap/sfsmisc in Imports**: These packages were declared in Imports but only used transitively through TAM. Move to Suggests to avoid "All declared Imports should be used" NOTE.
