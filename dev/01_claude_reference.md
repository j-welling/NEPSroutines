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

### User documentation (vignettes)

- All user-facing documentation lives in `vignettes/`; `getting_started.Rmd` is
  the entry point and links to the others (users reach it via `open_guide()`)
- When a change affects users (new/renamed/removed arguments, changed defaults
  or output), update the corresponding vignette in the same PR
- Vignettes are maintained by Jana - keep edits minimal and limited to what the
  code change requires; larger restructuring should be left to her

### Data sets

`data/` holds two separate families. They serve different purposes and are not
interchangeable:

| Files | Purpose | Used by |
|-------|---------|---------|
| `ex1`-`ex3` | Test data | `tests/`, roxygen `@examples` |
| `dat1`-`dat4` | Example data for package users | `vignettes/`, `inst/examples/` |

- Do not rename or change `dat1`-`dat4`, `inst/examples/`, or the vignettes that
  document them without asking Jana first - she maintains them
- `ex1`-`ex3` are coupled to the stored fixtures in `tests/testthat/fixtures/`;
  changing them invalidates saved model objects, not just test expectations
- `data-raw/ex.R` generates `dat1`-`dat4` only (despite its name); there is no
  generation script in the repository for `ex1`-`ex3`

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

**WrightMap/sfsmisc in Imports**: These packages were declared in Imports but never called with `::` from this package, which triggers the "All declared Imports should be used" NOTE. They belong in Suggests — but Suggests does *not* guarantee installation, and both are still required at runtime on default code paths: `TAM::IRT.WrightMap()` (used by `wright_map()`) calls `WrightMap::wrightMap()`, and `TAM::tam.mml()` with `snodes > 0` and TAM's default `QMC = TRUE` calls `sfsmisc::QUnif()` (`conduct_dim_analysis()` defaults to `snodes = 5000`). Any move from Imports to Suggests must therefore be paired with a `requireNamespace()` guard at the calling function; otherwise users hit an opaque error raised from inside TAM. General rule: a dependency reached only transitively is still yours to guard if the intermediate package merely Suggests it.

### CI test failures (2026-02-20)

**MASS not available in R CMD check tests**: MASS is a "recommended" R package but CDM uses it as an undeclared soft dependency (`CDM:::CDM_require_namespace("MASS")`). In R CMD check clean environments, MASS is absent from the transitive dependency tree, causing tests that call IRT functions to fail with "Package 'MASS' is needed". Fix: add `MASS` to `Suggests` in DESCRIPTION (ensures installation) AND add `skip_if_not_installed("MASS")` before affected test sections as a safety guard. Affected tests: `test-data_preparation.R` (pc_scoring with impute=TRUE), `test-distractor_analysis.R` (dis_analysis with use_wle=TRUE), `test-utils.R` (order_xsi_fixed), `test-irt_analyses.R` (all blocks).

**IRT fixtures store rounded summary tables**: The `irt_*.rds` fixtures in `tests/testthat/fixtures/` were generated when the `digits` argument of `irt_analysis()` defaulted to 2; it is now 3. The `summary` and `steps` tables in those fixtures therefore differ from current output by up to half a unit in the last fixture decimal (max 0.005), even though the estimates themselves are unchanged. This is a formatting difference, not drift: `model.*pl$mod$xsi`, `$se.xsi` and `model_fit` still match the fixtures exactly. Assert regressions against the unrounded model objects rather than against the rounded `summary` table.

**macOS CI: magick::image_graph() crash (SIGABRT)**: On macOS ARM64 GitHub Actions runners, calling `magick::image_graph()` + ggplot2 text rendering (`annotate("text", ...)`) causes a C-level abort (Abort trap: 6 / SIGABRT) due to missing FreeType fonts. This is NOT a catchable R error — it crashes the entire R process. Do NOT try to probe for this with `tryCatch`. Fix: add `skip_on_os("mac")` for any test that calls `Fig()` with a footnote, or `FigWrightMap()` (which always adds a footnote). Basic `magick::image_read/crop/scale` without text rendering works fine on macOS.

### Asserting on IRT results (2026-08-06)

**Aggregate statistics can be blind to the regression that matters**: `wle_rel` is `1 - mean(error^2) / var(theta)`. Being a variance ratio, it is *exactly* unchanged if theta is permuted across persons, shifted by a constant, or sign-flipped — only a rescaling moves it. A permutation is the failure that matters most here, because `irt_model()` filters persons through `only_valid()` before passing `ID_t` to TAM as `pid`, so a misalignment would attach abilities to the wrong persons while every reported number stayed plausible. Comparing `wle_rel` to a fixture therefore does not cover person parameters. Compare the WLEs themselves, matched on `pid` rather than row position, and assert separately that `pid` equals `resp$ID_t[resp[[valid]]]` — check that against the input data, not the fixture, so it survives a fixture regeneration. See `expect_wle_equal()` in `test-irt_analyses.R`. The general lesson: before trusting a summary statistic as a regression guard, work out which mutations leave it invariant.

**Not every value in `$steps` comes from TAM**: `steps_analysis()` fills the last step of each item from the sum-zero constraint (`-sum` of the others), so those cells have no counterpart in `model.pcm$mod$xsi` and are not covered by asserting on `xsi`. The cells are also pre-formatted strings combining the estimate and its SE, whose text depends on `digits`. Assert the constraint (each item's steps sum to zero) rather than the rendered strings: it holds at any `digits` and is independent of how the SE is formatted. Note the tolerance is only safe because the cell is built with `format(..., nsmall = digits)`, which sets minimum decimals without rounding — if that cell is ever changed to round to `digits` (see #154), residuals grow to ~5e-4 and the tolerance needs loosening to match.
