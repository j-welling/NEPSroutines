# NEPSroutines Package Improvement Plan

## Package Overview
**NEPSroutines** (v1.3.0) - R package for scaling NEPS competence data with IRT analysis, DIF testing, dimensionality analysis, linking, and automated technical report generation.

- **Codebase**: 14 R source files (~10,310 lines), 110 documentation files, 7 test files
- **Quality Grade**: A- (Excellent) - professionally developed, well-documented
- **Core Dependencies**: TAM (IRT), ggplot2, dplyr/tidyr, haven, openxlsx

---

## Proposed Improvements

### 1. Expand Test Coverage for Core Analysis Functions ✅ COMPLETED

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★★★ High |
| **Priority** | ★★★★★ Critical |
| **Effort** | ★★★☆☆ Medium |
| **Risk** | ★☆☆☆☆ Very Low |

**Status**: ✅ **COMPLETED** (2025-01-31) - See `dev/92_test_implementation_plan.md` for details.

**Implementation Summary**:
- Added 215 new tests across 5 test files
- All tests pass (warnings are expected TAM convergence messages)
- PRs: #56 (IRT), #57 (DIF), #58 (DIM), #59 (Linking), #60 (MV)

**Files Created**:
- `tests/testthat/test-irt_analysis.R` - 50 tests
- `tests/testthat/test-dif_analysis.R` - 43 tests
- `tests/testthat/test-dim_analysis.R` - 28 tests
- `tests/testthat/test-linking.R` - 46 tests
- `tests/testthat/test-mv_analysis.R` - 48 tests

---

### 2. Add Input Validation with Informative Error Messages

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★★☆ High |
| **Priority** | ★★★★☆ High |
| **Effort** | ★★☆☆☆ Low |
| **Risk** | ★★☆☆☆ Low |

**Current State**: Validation exists but some error messages lack context (e.g., "Item/s not included in vars" doesn't specify which items).

**Proposed Changes**:
- Enhance `check_items()` to list specific missing/duplicate items
- Add type checking for critical parameters (ensure `resp` is data.frame, `irtmodel` is valid string)
- Add range validation for numeric parameters (e.g., `pweights` must be positive)
- Create a central validation helper for consistent error formatting

**Files to Modify**:
- `R/utils.R` - Enhance existing check_* functions
- `R/irt_analyses.R` - Add parameter validation at entry points
- `R/dif_analysis.R` - Add parameter validation
- `R/linking.R` - Add parameter validation

---

### 3. Implement Parallel Processing for Grouped Analyses

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★★☆ High |
| **Priority** | ★★★☆☆ Medium |
| **Effort** | ★★★☆☆ Medium |
| **Risk** | ★★★☆☆ Medium |

**Current State**: `grouped_irt_analysis()` processes groups sequentially. Large datasets with many groups are slow.

**Proposed Changes**:
- Add optional `parallel` parameter to `grouped_irt_analysis()`
- Use `parallel::mclapply()` on Unix or `parallel::parLapply()` on Windows
- Add `n_cores` parameter with sensible default (detectCores() - 1)
- Ensure proper error handling in parallel context
- Add progress reporting compatible with parallel execution

**Files to Modify**:
- `R/irt_analyses.R` - Add parallel processing to `grouped_irt_analysis()`
- `DESCRIPTION` - No new dependencies needed (parallel is base R)

**Risk Mitigation**: Make parallel processing opt-in with `parallel = FALSE` as default.

---

### 4. Standardize Function Return Patterns

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★☆☆ Medium |
| **Priority** | ★★★☆☆ Medium |
| **Effort** | ★★★☆☆ Medium |
| **Risk** | ★★★★☆ High |

**Current State**: Inconsistent return behavior - some functions return results by default, others require `return = TRUE`, some only print/save.

**Proposed Changes**:
- Standardize on "always return invisibly, print/save optionally" pattern
- Deprecate `return` parameter in favor of always returning
- Add `.Deprecated()` warnings for old behavior
- Update documentation to reflect new patterns

**Files to Modify**:
- `R/irt_analyses.R`
- `R/dif_analysis.R`
- `R/dimensionality_analysis.R`
- `R/distractor_analysis.R`
- `R/mv_item.R`, `R/mv_person.R`
- All corresponding `man/*.Rd` files

**Risk**: Breaking change for existing user scripts. Requires deprecation cycle.

---

### 5. Clean Up Legacy Code and Comments

| Metric | Rating |
|--------|--------|
| **Impact** | ★★☆☆☆ Low |
| **Priority** | ★★☆☆☆ Low |
| **Effort** | ★☆☆☆☆ Very Low |
| **Risk** | ★☆☆☆☆ Very Low |

**Current State**: Some files contain extensive commented-out code (e.g., `create_scores.R` lines 56-79).

**Proposed Changes**:
- Remove commented-out code blocks (git history preserves them)
- Standardize file extensions to `.R` (currently mix of `.R` and `.r`)
- Add consistent file headers with purpose descriptions
- Remove any unused internal helper functions

**Files to Modify**:
- `R/create_scores.R` - Remove commented code
- `R/technical_report_*.r` - Rename to `.R` extension
- Various files - Clean up dead code

---

### 6. Add Performance Optimization for Large Datasets

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★☆☆ Medium |
| **Priority** | ★★☆☆☆ Low |
| **Effort** | ★★★★☆ High |
| **Risk** | ★★★☆☆ Medium |

**Current State**: Uses data.frame throughout; large datasets (100k+ rows) may be slow.

**Proposed Changes**:
- Add optional data.table backend for data preparation functions
- Optimize `convert_mv()` for large datasets
- Add lazy evaluation for result objects
- Profile and optimize hot paths in IRT estimation prep

**Files to Modify**:
- `R/data_preparation.R`
- `R/utils.R`
- `DESCRIPTION` - Add data.table to Suggests

**Risk**: Adds complexity; only beneficial for very large datasets.

---

### 7. Enhance Documentation with Workflow Vignettes

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★★☆ High |
| **Priority** | ★★☆☆☆ Low |
| **Effort** | ★★★☆☆ Medium |
| **Risk** | ★☆☆☆☆ Very Low |

**Current State**: Function documentation is excellent; only one vignette exists (`technical_report.Rmd`).

**Proposed Changes**:
- Add "Getting Started" vignette with complete workflow example
- Add "DIF Analysis" vignette with interpretation guidance
- Add "Longitudinal Linking" vignette for between-wave comparisons
- Add "Troubleshooting" vignette for common issues

**Files to Create**:
- `vignettes/getting_started.Rmd`
- `vignettes/dif_analysis.Rmd`
- `vignettes/linking.Rmd`
- `vignettes/troubleshooting.Rmd`

---

### 8. Add Configuration System for Defaults

| Metric | Rating |
|--------|--------|
| **Impact** | ★★★☆☆ Medium |
| **Priority** | ★★☆☆☆ Low |
| **Effort** | ★★☆☆☆ Low |
| **Risk** | ★★☆☆☆ Low |

**Current State**: Defaults are hardcoded in function signatures; `zzz.R` sets some package options.

**Proposed Changes**:
- Expand options system in `zzz.R` for common defaults
- Add `neps_set_options()` function for user configuration
- Allow setting default paths, verbosity, missing value codes
- Document options in package documentation

**Files to Modify**:
- `R/zzz.R` - Expand options system
- `R/utils.R` - Add `neps_set_options()`, `neps_get_options()`
- `man/neps_options.Rd` (new)

---

## Summary Matrix

| Improvement | Impact | Priority | Effort | Risk | Status |
|-------------|--------|----------|--------|------|--------|
| 1. Test Coverage | ★★★★★ | ★★★★★ | ★★★☆☆ | ★☆☆☆☆ | ✅ Done |
| 2. Input Validation | ★★★★☆ | ★★★★☆ | ★★☆☆☆ | ★★☆☆☆ | Pending |
| 3. Parallel Processing | ★★★★☆ | ★★★☆☆ | ★★★☆☆ | ★★★☆☆ | Pending |
| 4. Return Patterns | ★★★☆☆ | ★★★☆☆ | ★★★☆☆ | ★★★★☆ | Pending |
| 5. Code Cleanup | ★★☆☆☆ | ★★☆☆☆ | ★☆☆☆☆ | ★☆☆☆☆ | Pending |
| 6. Performance | ★★★☆☆ | ★★☆☆☆ | ★★★★☆ | ★★★☆☆ | Pending |
| 7. Vignettes | ★★★★☆ | ★★☆☆☆ | ★★★☆☆ | ★☆☆☆☆ | Pending |
| 8. Configuration | ★★★☆☆ | ★★☆☆☆ | ★★☆☆☆ | ★★☆☆☆ | Pending |

---

## Recommended Implementation Order

**Phase 1 - Quick Wins (Low effort, Low risk)**:
1. Code Cleanup (#5)
2. Input Validation (#2)

**Phase 2 - High Value (High impact, Moderate effort)**:
3. Test Coverage (#1)
4. Configuration System (#8)

**Phase 3 - Enhancement (Medium priority)**:
5. Parallel Processing (#3)
6. Vignettes (#7)

**Phase 4 - Breaking Changes (Requires deprecation cycle)**:
7. Return Patterns (#4)
8. Performance Optimization (#6)

---

## Verification Strategy

For each improvement:
1. Run `devtools::check()` to ensure no new warnings/errors
2. Run `devtools::test()` to verify all tests pass
3. Build and install package locally
4. Test with example datasets (ex1, ex2, ex3)
5. Generate technical report to verify integration

---

## Implementation Plan (Final)

Based on the analysis, I recommend implementing improvements in this order:

### Step 1: Code Cleanup (#5) - Quick Win
- Remove commented-out code in `R/create_scores.R`
- Rename `.r` files to `.R` for consistency

### Step 2: Enhanced Input Validation (#2)
- Improve error messages in `check_items()`, `check_variables()`
- Add specific item names to error messages
- Add type validation for critical parameters
- **Files**: `R/utils.R`, `R/irt_analyses.R`

### Step 3: Test Coverage (#1) - High Value ✅ COMPLETED
- ~~Create test files for core analysis functions~~
- ~~Use fixture-based testing to avoid long compute times~~
- ~~Target: `irt_analysis`, `dif_analysis`, `dim_analysis`, `mv_item/person`~~
- **Files**: 5 new test files in `tests/testthat/` - **215 tests added**
- See `dev/92_test_implementation_plan.md` for details

### Step 4: Configuration System (#8)
- Expand `zzz.R` options system
- Add `neps_set_options()` and `neps_get_options()`
- **Files**: `R/zzz.R`, `R/utils.R`

### Optional Future Steps (if desired):
- Parallel processing (#3)
- Vignettes (#7)
- Return pattern standardization (#4) - requires deprecation cycle
- Performance optimization (#6) - only for very large datasets
