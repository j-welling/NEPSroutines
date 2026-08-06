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
- Markdown is enabled (`Roxygen: list(markdown = TRUE)` in DESCRIPTION):
  - `[fn()]` becomes a link to that function's help; `[pkg::fn()]` links across
    packages (the package must be in Imports/Suggests)
  - Backticks render as `\code{}`; `*`/`-` bullets become `\itemize{}`
  - Legacy Rd macros (`\code{}`, `\itemize{}`, `\describe{}`) still work and are
    passed through unchanged, so both styles can coexist

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

**Roxygen brace escaping**: In roxygen2 comments, curly braces `{` and `}` must be escaped as `\{` and `\}` to avoid "Lost braces" NOTEs in Rd files. Common cases: `y in {0, 1}` → `y in \{0, 1\}`. Inside a markdown code span the braces need no escaping: `` `{+-}` `` is fine.

**Markdown pitfalls in roxygen** (since markdown was enabled): a markdown list must be closed by a blank `#'` line, otherwise the following paragraphs are absorbed into the last bullet and concatenated without spaces. A `<` or `>` starting a line is read as a blockquote marker and silently dropped, and `<word>` is read as raw HTML and wrapped in `\if{html}{\out{}}` (invisible in PDF/text help). Put such text in backticks. After changing roxygen comments, skim `git diff man/` — mangling shows up there, not in `check()`.

**RoxygenNote version mismatch**: If installed roxygen2 version doesn't match `RoxygenNote` in DESCRIPTION, `devtools::check()` won't re-document. Update `RoxygenNote` in DESCRIPTION to match installed version before running `devtools::document()`.

**WrightMap/sfsmisc in Imports**: These packages were declared in Imports but only used transitively through TAM. Move to Suggests to avoid "All declared Imports should be used" NOTE.

### CI test failures (2026-02-20)

**MASS not available in R CMD check tests**: MASS is a "recommended" R package but CDM uses it as an undeclared soft dependency (`CDM:::CDM_require_namespace("MASS")`). In R CMD check clean environments, MASS is absent from the transitive dependency tree, causing tests that call IRT functions to fail with "Package 'MASS' is needed". Fix: add `MASS` to `Suggests` in DESCRIPTION (ensures installation) AND add `skip_if_not_installed("MASS")` before affected test sections as a safety guard. Affected tests: `test-data_preparation.R` (pc_scoring with impute=TRUE), `test-distractor_analysis.R` (dis_analysis with use_wle=TRUE), `test-utils.R` (order_xsi_fixed), `test-irt_analyses.R` (all blocks).

**IRT fixtures store rounded summary tables**: The `irt_*.rds` fixtures in `tests/testthat/fixtures/` were generated when the `digits` argument of `irt_analysis()` defaulted to 2; it is now 3. The `summary` and `steps` tables in those fixtures therefore differ from current output by up to half a unit in the last fixture decimal (max 0.005), even though the estimates themselves are unchanged. This is a formatting difference, not drift: `model.*pl$mod$xsi`, `$se.xsi` and `model_fit` still match the fixtures exactly. Assert regressions against the unrounded model objects rather than against the rounded `summary` table.

**macOS CI: magick::image_graph() crash (SIGABRT)**: On macOS ARM64 GitHub Actions runners, calling `magick::image_graph()` + ggplot2 text rendering (`annotate("text", ...)`) causes a C-level abort (Abort trap: 6 / SIGABRT) due to missing FreeType fonts. This is NOT a catchable R error — it crashes the entire R process. Do NOT try to probe for this with `tryCatch`. Fix: add `skip_on_os("mac")` for any test that calls `Fig()` with a footnote, or `FigWrightMap()` (which always adds a footnote). Basic `magick::image_read/crop/scale` without text rendering works fine on macOS.
