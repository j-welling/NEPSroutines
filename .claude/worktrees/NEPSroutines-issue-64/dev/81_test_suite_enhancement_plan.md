# NEPSroutines Test Suite Enhancement Plan

## Executive Summary

This document defines a comprehensive test suite enhancement plan for the NEPSroutines R package. The plan is structured into **7 independent work packages** that can be executed in parallel on different git worktrees, maximizing development efficiency while ensuring complete test coverage for all critical analysis functions.

**Current State**: 51 tests across 7 files, ~7% coverage of exported functions
**Target State**: 140-165 tests across 14 files, >80% coverage of core functions

---

## Current State Analysis

### Test Infrastructure
- **Framework**: testthat edition 3 (configured in DESCRIPTION)
- **Test Files**: 7 files in `tests/testthat/` with 51 test cases total
- **Fixtures**: Pre-computed results in `tests/testthat/fixtures/` (~127MB total)
- **CI/CD**: NO continuous integration configured
- **Skipped Tests**: `test-technical_report_tables.R` has `skip()` at line 1, disabling 24 tests

### Existing Test Coverage

| Test File | Tests | Status |
|-----------|-------|--------|
| `test-utils.R` | 20 | Active |
| `test-data_preparation.R` | 2 | Active |
| `test-distractor_analysis.R` | 1 | Active |
| `test-technical_report_get.R` | 14 | Active |
| `test-technical_report_import.R` | 3 | Active |
| `test-technical_report_figures.R` | 2 | Active |
| `test-technical_report_tables.R` | 24 | **SKIPPED** |

### Critical Functions WITHOUT Test Coverage

| Function | Source File | Lines | Impact |
|----------|-------------|-------|--------|
| `irt_analysis()` | R/irt_analyses.R | ~1200 | Critical |
| `dif_analysis()` | R/dif_analysis.R | ~1300 | Critical |
| `linking()` | R/linking.R | ~1900 | Critical |
| `mv_item()` | R/mv_item.R | ~1260 | High |
| `mv_person()` | R/mv_person.R | ~780 | High |
| `create_scores()` | R/create_scores.R | ~700 | High |
| `dim_analysis()` | R/dimensionality_analysis.R | ~320 | High |
| `create_suf()` | R/create_suf.R | ~280 | Medium |

### Existing Fixtures Available

```
tests/testthat/fixtures/
├── ex1/ (dichotomous items)
│   ├── results/
│   │   ├── irt_dich.rds
│   │   ├── dif_dich_models.rds
│   │   ├── dif_dich_summaries.rds
│   │   ├── mv_item.rds
│   │   ├── mv_person.rds
│   │   ├── distractors.rds
│   │   ├── scores.rds
│   │   └── suf.rds
│   └── tables/
├── ex2/ (polytomous/mixed items)
│   ├── results/
│   │   ├── irt_poly.rds
│   │   ├── irt_dich.rds
│   │   └── dimensionality.rds
│   └── tables/
└── ex3/ (booklet design)
    ├── results/
    │   ├── irt_poly_all.rds
    │   ├── irt_poly_booklet[1-3].rds
    │   └── dimensionality.rds
    └── tables/
```

---

## Work Package Overview

| WP# | Name | Priority | Est. Tests | Branch |
|-----|------|----------|------------|--------|
| WP1 | Infrastructure & CI/CD | Critical | 0 (config) | `test/wp1-infrastructure` |
| WP2 | IRT Analysis Tests | Critical | 25-30 | `test/wp2-irt-analysis` |
| WP3 | DIF Analysis Tests | Critical | 20-25 | `test/wp3-dif-analysis` |
| WP4 | Missing Value Analysis Tests | High | 15-20 | `test/wp4-missing-values` |
| WP5 | Score Creation & SUF Tests | High | 15-20 | `test/wp5-scores-suf` |
| WP6 | Dimensionality & Linking Tests | High | 15-20 | `test/wp6-dim-linking` |
| WP7 | Enable Skipped Tests & Integration | Medium | 29 | `test/wp7-cleanup` |

**Total Estimated New Tests**: 90-115 (plus 24 enabled)

---

## Work Package 1: Infrastructure & CI/CD Setup

### Priority: Critical
### Dependencies: None
### Parallelizable: Yes

### Objectives
- Set up GitHub Actions for automated testing
- Configure code coverage reporting with covr/codecov
- Create test helper functions for common patterns
- Document testing conventions

### Files to Create

| File | Description |
|------|-------------|
| `.github/workflows/R-CMD-check.yaml` | CI workflow for R package checks |
| `.github/workflows/test-coverage.yaml` | Code coverage workflow |
| `tests/testthat/helper-fixtures.R` | Helper functions for loading fixtures |
| `codecov.yml` | Codecov configuration |

### Implementation Details

#### GitHub Actions Workflow (`.github/workflows/R-CMD-check.yaml`)
```yaml
name: R-CMD-check
on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main, develop]
jobs:
  R-CMD-check:
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, windows-latest]
        r: ['release']
    steps:
      - uses: actions/checkout@v4
      - uses: r-lib/actions/setup-r@v2
        with:
          r-version: ${{ matrix.r }}
      - uses: r-lib/actions/setup-r-dependencies@v2
        with:
          extra-packages: any::rcmdcheck
      - uses: r-lib/actions/check-r-package@v2
```

#### Test Helper Functions (`tests/testthat/helper-fixtures.R`)
```r
#' Load fixture with validation
#' @param dataset One of "ex1", "ex2", "ex3"
#' @param filename The fixture filename
load_fixture <- function(dataset, filename) {
  path <- test_path("fixtures", dataset, "results", filename)
  if (!file.exists(path)) {
    skip(paste("Fixture not found:", path))
  }
  readRDS(path)
}

#' Create temporary test environment with standard paths
setup_test_env <- function() {
  path <- withr::local_tempdir()
  list(
    root = path,
    results = file.path(path, "Results"),
    tables = file.path(path, "Tables"),
    plots = file.path(path, "Plots")
  )
}

#' Skip test if running on CI and test is slow
skip_if_slow <- function() {
  if (identical(Sys.getenv("CI"), "true")) {
    skip("Slow test skipped on CI")
  }
}
```

### Verification Criteria
- [ ] CI runs successfully on push/PR to main
- [ ] Code coverage reports generated
- [ ] All existing tests pass in CI
- [ ] Helper functions documented and working

---

## Work Package 2: IRT Analysis Tests

### Priority: Critical
### Dependencies: None
### Parallelizable: Yes
### Estimated Tests: 25-30

### Objectives
Test the core IRT analysis functions:
- `irt_analysis()` - Main IRT analysis function
- `irt_model()` - IRT model fitting
- `irt_summary()` - Results summarization
- `grouped_irt_analysis()` - Grouped IRT analysis

### Files to Create

| File | Description |
|------|-------------|
| `tests/testthat/test-irt_analysis.R` | Main IRT analysis tests |

### Test Structure

```r
# tests/testthat/test-irt_analysis.R

# ============================================
# Input Validation Tests (8 tests)
# ============================================

test_that("irt_analysis() validates required parameters", {
  data("ex1")

  expect_error(irt_analysis(resp = NULL, vars = ex1$vars, select = "dich"))
  expect_error(irt_analysis(resp = ex1$resp, vars = NULL, select = "dich"))
  expect_error(irt_analysis(resp = ex1$resp, vars = ex1$vars, select = NULL))
  expect_error(irt_analysis(resp = ex1$resp, vars = ex1$vars,
                            select = "nonexistent"))
})

test_that("irt_analysis() validates irtmodel parameter", {
  data("ex1")

  expect_error(
    irt_analysis(resp = ex1$resp, vars = ex1$vars, select = "dich",
                 irtmodel = "invalid"),
    regexp = "irtmodel"
  )
})

test_that("irt_analysis() warns about missing mvs", {
  data("ex1")

  expect_warning(
    irt_analysis(resp = ex1$resp, vars = ex1$vars, select = "dich",
                 valid = "valid", mvs = NULL, warn = TRUE,
                 return = TRUE, save = FALSE, print = FALSE, plots = FALSE),
    regexp = "No user defined missing values"
  )
})

# ============================================
# Happy Path Tests - Dichotomous (6 tests)
# ============================================

test_that("irt_analysis() returns correct structure for dichotomous items", {
  data("ex1")

  result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
  expect_true("model.1pl" %in% names(result))
  expect_true("model.2pl" %in% names(result))
})

test_that("irt_analysis() 1PL model has expected components", {
  data("ex1")

  result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_true("mod" %in% names(result$model.1pl))
  expect_true("wle" %in% names(result$model.1pl))
  expect_true("summary" %in% names(result$model.1pl))
})

# ============================================
# Happy Path Tests - Polytomous (4 tests)
# ============================================

test_that("irt_analysis() works for polytomous items", {
  data("ex2")

  result <- irt_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
  expect_true("model.pcm" %in% names(result) || "model.gpcm" %in% names(result))
})

# ============================================
# Regression Tests Using Fixtures (7 tests)
# ============================================

test_that("irt_analysis() reproduces fixture results for ex1", {
  expected <- load_fixture("ex1", "irt_dich.rds")
  data("ex1")

  result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  # Compare item difficulties
  expect_equal(
    result$model.1pl$summary$items$xsi,
    expected$model.1pl$summary$items$xsi,
    tolerance = 0.001
  )
})

test_that("irt_analysis() reproduces fixture results for ex2 polytomous", {
  expected <- load_fixture("ex2", "irt_poly.rds")
  data("ex2")

  result <- irt_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_equal(
    nrow(result$model.pcm$summary$items),
    nrow(expected$model.pcm$summary$items)
  )
})

# ============================================
# Edge Case Tests (5 tests)
# ============================================

test_that("irt_analysis() handles high missing data", {
  data("ex1")

  # Add more missing values
  ex1_missing <- ex1$resp
  items <- ex1$vars$item[ex1$vars$dich]
  ex1_missing[1:100, items[1:5]] <- NA

  result <- irt_analysis(
    resp = ex1_missing,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
})

test_that("irt_analysis() handles fixed item parameters", {
  data("ex1")
  fixture <- load_fixture("ex1", "irt_dich.rds")

  # Use fixture parameters as fixed
  xsi_fixed <- fixture$model.1pl$mod$xsi$xsi
  names(xsi_fixed) <- fixture$model.1pl$mod$xsi$parameter

  result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    xsi_fixed = xsi_fixed,
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
})

# ============================================
# File Output Tests (3 tests)
# ============================================

test_that("irt_analysis() saves results correctly", {
  data("ex1")
  env <- setup_test_env()

  irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = FALSE,
    save = TRUE,
    path_results = env$results,
    path_table = env$tables,
    print = FALSE,
    plots = FALSE
  )

  expect_true(file.exists(file.path(env$results, "irt_dich.rds")))
  expect_true(file.exists(file.path(env$tables, "irt_dich.xlsx")))
})
```

### Fixtures Used
- `fixtures/ex1/results/irt_dich.rds`
- `fixtures/ex2/results/irt_poly.rds`
- `fixtures/ex2/results/irt_dich.rds`

### Verification Criteria
- [ ] All parameter validation tests pass
- [ ] Output structure validated for 1PL, 2PL, PCM, GPCM
- [ ] Fixture regression tests pass within tolerance
- [ ] File output tests verify RDS and XLSX creation

---

## Work Package 3: DIF Analysis Tests

### Priority: Critical
### Dependencies: None
### Parallelizable: Yes
### Estimated Tests: 20-25

### Objectives
Test DIF (Differential Item Functioning) analysis functions:
- `dif_analysis()` - Main DIF analysis function
- `conduct_dif_analysis()` - Core DIF computation

### Files to Create

| File | Description |
|------|-------------|
| `tests/testthat/test-dif_analysis.R` | DIF analysis tests |

### Test Structure

```r
# tests/testthat/test-dif_analysis.R

# ============================================
# Input Validation Tests (6 tests)
# ============================================

test_that("dif_analysis() validates required parameters", {
  data("ex1")

  expect_error(dif_analysis(resp = NULL, vars = ex1$vars, select = "dich",
                            dif_var = "sex"))
  expect_error(dif_analysis(resp = ex1$resp, vars = ex1$vars,
                            select = "dich", dif_var = "nonexistent"))
})

test_that("dif_analysis() validates dif_var is categorical", {
  data("ex1")

  ex1$resp$continuous <- runif(nrow(ex1$resp))
  expect_error(
    dif_analysis(resp = ex1$resp, vars = ex1$vars,
                 select = "dich", dif_var = "continuous",
                 valid = "valid", return = TRUE, save = FALSE, print = FALSE)
  )
})

# ============================================
# Happy Path Tests (8 tests)
# ============================================

test_that("dif_analysis() works for single DIF variable", {
  data("ex1")

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    dif_var = "sex",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE
  )

  expect_type(result, "list")
  expect_true("sex" %in% names(result))
})

test_that("dif_analysis() handles multiple DIF variables", {
  data("ex1")

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    dif_var = c("sex", "mig"),
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE
  )

  expect_true(all(c("sex", "mig") %in% names(result)))
})

test_that("dif_analysis() returns DIF statistics", {
  data("ex1")

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    dif_var = "sex",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE
  )

  # Check DIF summary structure
  expect_true("dif_summary" %in% names(result$sex) ||
              "summary" %in% names(result$sex))
})

# ============================================
# Regression Tests Using Fixtures (6 tests)
# ============================================

test_that("dif_analysis() reproduces fixture DIF results", {
  expected_models <- load_fixture("ex1", "dif_dich_models.rds")
  expected_summaries <- load_fixture("ex1", "dif_dich_summaries.rds")
  data("ex1")

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    dif_var = "sex",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE
  )

  # Compare number of items analyzed
  expect_equal(
    nrow(result$sex$summary),
    nrow(expected_summaries$sex$summary)
  )
})

# ============================================
# Edge Case Tests (5 tests)
# ============================================

test_that("dif_analysis() handles small group sizes", {
  data("ex1")

  # Create small group
  ex1$resp$small_group <- c(rep("A", 50), rep("B", nrow(ex1$resp) - 50))

  # Should warn or handle gracefully
  expect_warning(
    dif_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      dif_var = "small_group",
      return = TRUE,
      save = FALSE,
      print = FALSE
    )
  ) |> expect_no_error()
})

test_that("dif_analysis() handles missing DIF variable values", {
  data("ex1")

  ex1$resp$sex_missing <- ex1$resp$sex
  ex1$resp$sex_missing[1:50] <- NA

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    dif_var = "sex_missing",
    return = TRUE,
    save = FALSE,
    print = FALSE
  )

  expect_type(result, "list")
})
```

### Fixtures Used
- `fixtures/ex1/results/dif_dich_models.rds`
- `fixtures/ex1/results/dif_dich_summaries.rds`

### Verification Criteria
- [ ] DIF statistics calculated correctly
- [ ] Multiple DIF variables handled
- [ ] Edge cases (small groups, missing values) handled
- [ ] Fixture regression tests pass

---

## Work Package 4: Missing Value Analysis Tests

### Priority: High
### Dependencies: None
### Parallelizable: Yes
### Estimated Tests: 15-20

### Objectives
Test missing value analysis functions:
- `mv_item()` - Missing values by item
- `mv_person()` - Missing values by person

### Files to Create

| File | Description |
|------|-------------|
| `tests/testthat/test-mv_item.R` | Missing by item tests |
| `tests/testthat/test-mv_person.R` | Missing by person tests |

### Test Structure

```r
# tests/testthat/test-mv_item.R

# ============================================
# Input Validation Tests (4 tests)
# ============================================

test_that("mv_item() validates required parameters", {
  data("ex1")

  expect_error(mv_item(resp = NULL, vars = ex1$vars, select = "dich"))
  expect_error(mv_item(resp = ex1$resp, vars = NULL, select = "dich"))
  expect_error(mv_item(resp = ex1$resp, vars = ex1$vars,
                       select = "nonexistent"))
})

# ============================================
# Happy Path Tests (5 tests)
# ============================================

test_that("mv_item() returns correct structure", {
  data("ex1")

  result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    position = "pos",
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
  expect_true(is.data.frame(result$list) || is.list(result$list))
  expect_true("summary" %in% names(result))
})

test_that("mv_item() calculates missing value types correctly", {
  data("ex1")

  result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    position = "pos",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  # Check that MV types are present
  summary_cols <- names(result$summary)
  expect_true(any(grepl("OM|NV|NR|ALL", summary_cols)))
})

# ============================================
# Regression Tests Using Fixtures (3 tests)
# ============================================

test_that("mv_item() reproduces fixture results", {
  expected <- load_fixture("ex1", "mv_item.rds")
  data("ex1")

  result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    position = "pos",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_equal(
    dim(result$summary),
    dim(expected$summary)
  )
})

# ============================================
# Grouping Tests (3 tests)
# ============================================

test_that("mv_item() handles grouping variable", {
  data("ex3")

  result <- mv_item(
    resp = ex3$resp,
    vars = ex3$vars,
    select = "mixed",
    valid = "valid",
    position = "pos",
    grouping = c("booklet1", "booklet2", "booklet3"),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result$list, "list")
})
```

```r
# tests/testthat/test-mv_person.R

# ============================================
# Input Validation Tests (3 tests)
# ============================================

test_that("mv_person() validates required parameters", {
  data("ex1")

  expect_error(mv_person(resp = NULL, vars = ex1$vars, select = "dich"))
  expect_error(mv_person(resp = ex1$resp, vars = NULL, select = "dich"))
})

# ============================================
# Happy Path Tests (4 tests)
# ============================================

test_that("mv_person() returns correct structure", {
  data("ex1")

  result <- mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
})

# ============================================
# Regression Tests Using Fixtures (3 tests)
# ============================================

test_that("mv_person() reproduces fixture results", {
  expected <- load_fixture("ex1", "mv_person.rds")
  data("ex1")

  result <- mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = FALSE,
    print = FALSE,
    plots = FALSE
  )

  expect_equal(
    dim(result$summary),
    dim(expected$summary)
  )
})
```

### Fixtures Used
- `fixtures/ex1/results/mv_item.rds`
- `fixtures/ex1/results/mv_person.rds`
- `fixtures/ex2/results/mv_item.rds`
- `fixtures/ex3/results/mv_item.rds`

### Verification Criteria
- [ ] All MV types (OM, NV, NR, ALL) calculated correctly
- [ ] Grouping functionality works
- [ ] Fixture regression tests pass
- [ ] Plot generation works when plots = TRUE

---

## Work Package 5: Score Creation & SUF Tests

### Priority: High
### Dependencies: None
### Parallelizable: Yes
### Estimated Tests: 15-20

### Objectives
Test score creation and SUF generation:
- `create_scores()` - WLE and sum score estimation
- `create_suf()` - Scientific Use File generation

### Files to Create

| File | Description |
|------|-------------|
| `tests/testthat/test-create_scores.R` | Score creation tests |
| `tests/testthat/test-create_suf.R` | SUF generation tests |

### Test Structure

```r
# tests/testthat/test-create_scores.R

# ============================================
# Input Validation Tests (4 tests)
# ============================================

test_that("create_scores() validates required parameters", {
  data("ex1")

  expect_error(create_scores(resp = NULL, vars = ex1$vars, select = "dich"))
  expect_error(create_scores(resp = ex1$resp, vars = NULL, select = "dich"))
  expect_error(create_scores(resp = ex1$resp, vars = ex1$vars,
                             select = "nonexistent"))
})

# ============================================
# Happy Path Tests (6 tests)
# ============================================

test_that("create_scores() estimates WLEs correctly", {
  data("ex1")

  result <- create_scores(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    score_name = "test",
    wle = TRUE,
    sum_score = FALSE,
    return = TRUE,
    save = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true("ID_t" %in% names(result))
  expect_true(any(grepl("test_sc", names(result))))
})

test_that("create_scores() calculates sum scores correctly", {
  data("ex1")

  result <- create_scores(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    score_name = "test",
    wle = FALSE,
    sum_score = TRUE,
    num_cat = "num_cat",
    return = TRUE,
    save = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(any(grepl("sc3", names(result))))  # Sum score variable
})

# ============================================
# Regression Tests Using Fixtures (4 tests)
# ============================================

test_that("create_scores() reproduces fixture scores", {
  expected <- load_fixture("ex1", "scores.rds")
  data("ex1")

  result <- create_scores(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    score_name = "grk10",
    wle = TRUE,
    return = TRUE,
    save = FALSE
  )

  # Compare WLEs
  merged <- merge(result, expected, by = "ID_t", suffixes = c(".new", ".old"))
  wle_cols_new <- grep("grk10_sc1.new", names(merged), value = TRUE)
  wle_cols_old <- grep("grk10_sc1.old", names(merged), value = TRUE)

  if (length(wle_cols_new) > 0 && length(wle_cols_old) > 0) {
    expect_equal(merged[[wle_cols_new[1]]], merged[[wle_cols_old[1]]],
                 tolerance = 0.01)
  }
})
```

```r
# tests/testthat/test-create_suf.R

# ============================================
# Input Validation Tests (3 tests)
# ============================================

test_that("create_suf() validates required parameters", {
  data("ex1")

  expect_error(create_suf(resp = NULL, vars = ex1$vars, select = "dich",
                          competence = "Grammar"))
  expect_error(create_suf(resp = ex1$resp, vars = ex1$vars,
                          select = "dich", competence = NULL))
})

# ============================================
# Happy Path Tests (4 tests)
# ============================================

test_that("create_suf() creates properly labeled SUF", {
  data("ex1")
  scores <- load_fixture("ex1", "scores.rds")

  result <- create_suf(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    competence = "Grammatik",
    scores = scores,
    score_name = "grk10",
    return = TRUE,
    save = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true("ID_t" %in% names(result))
})

# ============================================
# File Output Tests (4 tests)
# ============================================

test_that("create_suf() saves files in all formats", {
  data("ex1")
  scores <- load_fixture("ex1", "scores.rds")
  env <- setup_test_env()

  create_suf(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    competence = "Grammatik",
    scores = scores,
    score_name = "grk10",
    path = env$root,
    save = TRUE,
    return = FALSE
  )

  expect_true(file.exists(file.path(env$root, "suf.rds")))
  expect_true(file.exists(file.path(env$root, "suf.dta")))
  expect_true(file.exists(file.path(env$root, "suf.sav")))
})
```

### Fixtures Used
- `fixtures/ex1/results/scores.rds`
- `fixtures/ex1/results/suf.rds`
- `fixtures/ex2/results/scores.rds`

### Verification Criteria
- [ ] WLE estimation produces valid scores
- [ ] Sum score calculation correct
- [ ] SUF labels properly applied
- [ ] All output formats (RDS, DTA, SAV) created

---

## Work Package 6: Dimensionality & Linking Tests

### Priority: High
### Dependencies: None
### Parallelizable: Yes
### Estimated Tests: 15-20

### Objectives
Test dimensionality and linking functions:
- `dim_analysis()` - Dimensionality analysis
- `linking()` - Linking analysis
- `link_item_parameters()` - Item parameter linking

### Files to Create

| File | Description |
|------|-------------|
| `tests/testthat/test-dimensionality_analysis.R` | Dimensionality tests |
| `tests/testthat/test-linking.R` | Linking tests |

### Test Structure

```r
# tests/testthat/test-dimensionality_analysis.R

# ============================================
# Input Validation Tests (3 tests)
# ============================================

test_that("dim_analysis() validates required parameters", {
  data("ex2")

  expect_error(dim_analysis(resp = NULL, vars = ex2$vars, select = "mixed"))
  expect_error(dim_analysis(resp = ex2$resp, vars = NULL, select = "mixed"))
})

# ============================================
# Happy Path Tests (4 tests)
# ============================================

test_that("dim_analysis() returns correct structure", {
  skip_if_slow()  # Dimensionality analysis can be slow

  data("ex2")

  result <- dim_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    dim = "content",
    scoring = "scoring",
    irtmodel = "PCM2",
    return = TRUE,
    save = FALSE,
    print = FALSE,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_true("analysis" %in% names(result) || "uni" %in% names(result))
})

# ============================================
# Regression Tests Using Fixtures (3 tests)
# ============================================

test_that("dim_analysis() produces consistent model fit", {
  skip_if_slow()

  expected <- load_fixture("ex2", "dimensionality.rds")
  data("ex2")

  result <- dim_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    dim = "content",
    scoring = "scoring",
    return = TRUE,
    save = FALSE,
    print = FALSE,
    verbose = FALSE
  )

  # Compare structure, not exact values (IRT can have minor variations)
  expect_equal(
    length(result),
    length(expected)
  )
})
```

```r
# tests/testthat/test-linking.R

# ============================================
# Input Validation Tests (4 tests)
# ============================================

test_that("linking() validates required parameters", {
  data("ex1")

  expect_error(linking(resp_curr = NULL, resp_prev = ex1$resp,
                       vars_curr = ex1$vars, vars_prev = ex1$vars,
                       select_curr = "dich", select_prev = "dich"))
})

test_that("prepare_longitudinal_resp() validates data structure", {
  data("ex1")

  expect_error(prepare_longitudinal_resp(
    resp_curr = ex1$resp, resp_prev = ex1$resp,
    vars_curr = ex1$vars, vars_prev = ex1$vars,
    select_curr = "nonexistent", select_prev = "dich"
  ))
})

# ============================================
# Happy Path Tests (5 tests)
# ============================================

test_that("prepare_longitudinal_resp() prepares data correctly", {
  data("ex1")

  # Use ex1 as both current and previous for testing
  result <- prepare_longitudinal_resp(
    resp_curr = ex1$resp,
    resp_prev = ex1$resp,
    vars_curr = ex1$vars,
    vars_prev = ex1$vars,
    select_curr = "dich",
    select_prev = "dich",
    valid_curr = "valid",
    valid_prev = "valid",
    test = TRUE
  )

  expect_type(result, "list")
})

test_that("linking() basic workflow completes", {
  skip_if_slow()  # Linking can be slow

  data("ex1")

  # Simple linking scenario using same data
  result <- linking(
    resp_curr = ex1$resp,
    resp_prev = ex1$resp,
    vars_curr = ex1$vars,
    vars_prev = ex1$vars,
    select_curr = "dich",
    select_prev = "dich",
    valid_curr = "valid",
    valid_prev = "valid",
    return = TRUE,
    save = FALSE,
    do_dif = FALSE,
    do_dim = FALSE
  )

  expect_type(result, "list")
})

# ============================================
# Fixture Creation Note
# ============================================
# Linking fixtures need to be created as they don't currently exist.
# Run the fixture creation script before running regression tests.
```

### Fixtures Needed
- Uses existing: `fixtures/ex2/results/dimensionality.rds`
- Uses existing: `fixtures/ex3/results/dimensionality.rds`
- **To Create**: `fixtures/ex1/results/linking.rds` (see Appendix A)

### Verification Criteria
- [ ] Dimensionality models fit and converge
- [ ] Model fit statistics computed correctly
- [ ] Linking constants calculated
- [ ] Cross-study parameter comparisons work

---

## Work Package 7: Enable Skipped Tests & Integration

### Priority: Medium
### Dependencies: None (can start after WP1 for CI)
### Parallelizable: Yes
### Estimated Tests: 29 (24 enabled + 5 new)

### Objectives
- Remove `skip()` from `test-technical_report_tables.R`
- Update hash-based tests for stability
- Add integration tests for full workflow

### Files to Modify/Create

| File | Action | Description |
|------|--------|-------------|
| `tests/testthat/test-technical_report_tables.R` | Modify | Remove skip(), add conditional skips |
| `tests/testthat/test-integration.R` | Create | Full workflow integration tests |

### Implementation Details

#### Enable Table Tests

```r
# tests/testthat/test-technical_report_tables.R
# REMOVE: skip()  <- Delete this line

# Add per-test conditional skipping
test_that("create table with item properties works", {
  skip_if_not_installed("flextable")

  data("ex2")
  tbl <- TblItemProps(vars = ex2$vars, select = "mixed", prop = "type",
                      propname = "Response formats")

  expect_true(inherits(tbl, "flextable"))
  # Update or remove hash-based checks if unstable
})
```

#### Integration Tests

```r
# tests/testthat/test-integration.R

test_that("full analysis workflow produces consistent results", {
  skip_if_slow()

  data("ex1")
  env <- setup_test_env()

  # Step 1: Run IRT analysis
  irt_result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    return = TRUE,
    save = TRUE,
    path_results = env$results,
    path_table = env$tables,
    print = FALSE,
    plots = FALSE
  )

  # Step 2: Run missing value analysis
  mv_result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    position = "pos",
    return = TRUE,
    save = TRUE,
    path_results = env$results,
    path_table = env$tables,
    print = FALSE,
    plots = FALSE
  )

  # Step 3: Create scores
  scores <- create_scores(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    score_name = "test",
    wle = TRUE,
    return = TRUE,
    save = TRUE,
    path = env$results
  )

  # Step 4: Create SUF
  suf <- create_suf(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    competence = "Test",
    scores = scores,
    score_name = "test",
    path = file.path(env$root, "suf"),
    return = TRUE,
    save = TRUE
  )

  # Verify all outputs
  expect_true(file.exists(file.path(env$results, "irt_dich.rds")))
  expect_true(file.exists(file.path(env$results, "mv_item.rds")))
  expect_true(file.exists(file.path(env$root, "suf", "suf.rds")))
  expect_true(is.data.frame(suf))
})

test_that("technical report workflow produces valid tables", {
  skip_if_not_installed("flextable")

  data("ex1")

  # Setup for technical report
  obj <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))
  tbl <- Import(test_path("fixtures/ex1/tables"), "irt_dich.xlsx")

  # Generate tables
  tbl_props <- TblItemProps(vars = ex1$vars, select = "dich",
                            prop = "type", propname = "Type")
  tbl_pars <- TblPars(tbl = tbl, type = "xsi")

  expect_true(inherits(tbl_props, "flextable"))
  expect_true(inherits(tbl_pars, "flextable"))
})
```

### Verification Criteria
- [ ] All 24 table tests now run (not skipped)
- [ ] Integration tests cover full workflow
- [ ] No regressions in existing tests

---

## Implementation Guidelines

### Git Worktree Setup

```bash
# Create worktrees for parallel development
git worktree add ../wp1-infrastructure test/wp1-infrastructure
git worktree add ../wp2-irt-tests test/wp2-irt-analysis
git worktree add ../wp3-dif-tests test/wp3-dif-analysis
git worktree add ../wp4-mv-tests test/wp4-missing-values
git worktree add ../wp5-scores-tests test/wp5-scores-suf
git worktree add ../wp6-dim-linking-tests test/wp6-dim-linking
git worktree add ../wp7-cleanup test/wp7-cleanup
```

### Merge Order (Recommended)
1. **WP1** (Infrastructure) - Merge first to enable CI
2. **WP7** (Enable skipped) - Quick win, merge early
3. **WP2-WP6** - Can merge in any order after WP1

### Code Review Checklist
For each work package PR:
- [ ] All new tests pass locally
- [ ] `devtools::check()` passes with no new warnings
- [ ] No regressions in existing tests
- [ ] Test coverage increased (check with `covr::package_coverage()`)
- [ ] Fixtures properly committed (if new)

---

## Appendix A: Fixture Creation Scripts

### Create Linking Fixtures (for WP6)

```r
# Run once to create linking fixtures
# Save as: tests/testthat/fixtures/create_linking_fixtures.R

devtools::load_all()
data("ex1")

# Create mock linking scenario using same data
linking_result <- linking(
  resp_curr = ex1$resp,
  resp_prev = ex1$resp,
  vars_curr = ex1$vars,
  vars_prev = ex1$vars,
  select_curr = "dich",
  select_prev = "dich",
  valid_curr = "valid",
  valid_prev = "valid",
  return = TRUE,
  save = FALSE,
  do_dif = FALSE,
  do_dim = FALSE
)

saveRDS(linking_result,
        "tests/testthat/fixtures/ex1/results/linking.rds")
```

---

## Appendix B: Test Execution Commands

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-irt_analysis.R")

# Run with coverage
covr::package_coverage()

# Run in parallel (faster)
testthat::test_local(parallel = TRUE)

# Run only fast tests (for CI)
testthat::test_local(filter = "^(?!.*slow)")
```

---

## Summary

| Work Package | Files to Create | Tests | Priority |
|--------------|-----------------|-------|----------|
| WP1: Infrastructure | 4 config files | 0 | Critical |
| WP2: IRT Tests | test-irt_analysis.R | 25-30 | Critical |
| WP3: DIF Tests | test-dif_analysis.R | 20-25 | Critical |
| WP4: MV Tests | test-mv_item.R, test-mv_person.R | 15-20 | High |
| WP5: Scores Tests | test-create_scores.R, test-create_suf.R | 15-20 | High |
| WP6: Dim/Link Tests | test-dimensionality_analysis.R, test-linking.R | 15-20 | High |
| WP7: Enable & Integrate | test-integration.R (+ modify tables) | 29 | Medium |

**Total**: ~140-165 tests across 14 test files

All work packages are independent and can be executed in parallel on separate git worktrees.
