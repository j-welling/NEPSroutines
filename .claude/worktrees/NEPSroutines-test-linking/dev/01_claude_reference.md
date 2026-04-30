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

(Add learnings here when Claude makes errors)
