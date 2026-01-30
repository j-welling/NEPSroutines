---
name: testthat
description: Use when writing, evaluating, reviewing, or debugging R package tests using testthat. Triggered for test files (test-*.R), test coverage discussions, or questions about testing R packages.
---

# testthat Skill

## Overview

testthat is the most widely-used unit testing framework for R packages. This skill provides comprehensive knowledge for writing effective tests.

## Test File Organization

### Directory Structure
```
tests/
├── testthat.R          # Standard runner (do not modify)
└── testthat/
    ├── test-*.R        # Test files (must start with test-)
    ├── helper-*.R      # Helper functions (loaded by load_all())
    ├── setup.R         # Global setup (NOT loaded by load_all())
    ├── fixtures/       # Test data files
    └── _snaps/         # Snapshot files (auto-generated)
```

### File Naming Convention
- Mirror `R/` directory structure: `R/foofy.R` → `tests/testthat/test-foofy.R`
- Test files MUST start with `test-` or `test_`
- Helper files start with `helper`
- Setup files start with `setup`

## Core Functions

### test_that()
```r
test_that("descriptive message of what is being tested", {
  # one or more expectations
  expect_equal(actual, expected)
})
```

Key principles:
- Description should clearly state the test's purpose
- Each test should cover a single unit of functionality
- Tests should be self-contained and independent
- Never use `library()` or `source()` in test files

## Expectation Functions Reference

### Equality Testing
| Function | Purpose |
|----------|---------|
| `expect_equal(x, y)` | Near equality (tolerates small floating point differences) |
| `expect_identical(x, y)` | Exact equality (no tolerance, type-sensitive) |
| `expect_all_equal(x, y)` | All elements equal |

### Comparison Testing
| Function | Purpose |
|----------|---------|
| `expect_lt(x, y)` | x < y |
| `expect_lte(x, y)` | x <= y |
| `expect_gt(x, y)` | x > y |
| `expect_gte(x, y)` | x >= y |

### Boolean Testing
| Function | Purpose |
|----------|---------|
| `expect_true(x)` | x is TRUE |
| `expect_false(x)` | x is FALSE |
| `expect_all_true(x)` | All elements TRUE |
| `expect_all_false(x)` | All elements FALSE |
| `expect_null(x)` | x is NULL |

### Type and Class Testing
| Function | Purpose |
|----------|---------|
| `expect_type(x, type)` | Check typeof() |
| `expect_s3_class(x, class)` | Check S3 class |
| `expect_s4_class(x, class)` | Check S4 class |
| `expect_r6_class(x, class)` | Check R6 class |
| `expect_vector(x)` | x is a vector |

### Collection Testing
| Function | Purpose |
|----------|---------|
| `expect_length(x, n)` | length(x) == n |
| `expect_shape(x, shape)` | Check dimensions |
| `expect_named(x, names)` | Check names |
| `expect_setequal(x, y)` | Same elements, any order |
| `expect_mapequal(x, y)` | Same name-value pairs |
| `expect_contains(x, y)` | x contains all elements of y |
| `expect_in(x, y)` | All x elements are in y |
| `expect_disjoint(x, y)` | No shared elements |

### Pattern Matching
| Function | Purpose |
|----------|---------|
| `expect_match(x, regexp)` | x matches pattern |
| `expect_no_match(x, regexp)` | x doesn't match pattern |

### Error/Warning/Message Testing
| Function | Purpose |
|----------|---------|
| `expect_error(code, regexp, class)` | Code throws error |
| `expect_warning(code, regexp, class)` | Code throws warning |
| `expect_message(code, regexp, class)` | Code produces message |
| `expect_condition(code, class)` | Code signals condition |
| `expect_no_error(code)` | Code runs without error |
| `expect_no_warning(code)` | Code runs without warning |
| `expect_no_message(code)` | Code runs without message |
| `expect_no_condition(code)` | Code runs without any condition |

**Best Practice**: Use `class` argument instead of `regexp` for robustness:
```r
# Preferred - tests condition class
expect_error(code(), class = "my_error_class")

# Less robust - tests message text which may change
expect_error(code(), regexp = "error message")
```

### Output Testing
| Function | Purpose |
|----------|---------|
| `expect_output(code, regexp)` | Code produces output matching pattern |
| `expect_silent(code)` | Code produces no output/messages/warnings |
| `expect_visible(code)` | Result is visible |
| `expect_invisible(code)` | Result is invisible |

### Snapshot Testing
| Function | Purpose |
|----------|---------|
| `expect_snapshot(code)` | Capture all output to snapshot file |
| `expect_snapshot_value(x, style)` | Capture return value |
| `expect_snapshot_file(path)` | Capture file contents |
| `expect_snapshot(code, error = TRUE)` | Capture error message |

Snapshot management:
- `snapshot_accept()` - Accept new snapshots
- `snapshot_reject()` - Reject changes
- `snapshot_review()` - Interactive review (Shiny app)

## Skip Functions

```r
skip()                        # Skip unconditionally
skip_if(condition)            # Skip if condition is TRUE
skip_if_not(condition)        # Skip if condition is FALSE
skip_if_not_installed("pkg")  # Skip if package not installed
skip_on_cran()                # Skip on CRAN
skip_on_ci()                  # Skip on CI systems
skip_on_os("windows")         # Skip on specific OS
skip_if_offline()             # Skip if no internet
skip_unless_r(version)        # Skip unless R version meets requirement
```

## Mocking Functions

```r
# Temporarily replace function binding
local_mocked_bindings(
  function_name = function(...) mock_return_value,
  .package = "package_name"
)

# Block-scoped mocking
with_mocked_bindings(
  code = { ... },
  function_name = mock_function
)

# S3 method mocking
local_mocked_s3_method(
  method = "print",
  class = "myclass",
  mock_function
)
```

**Mocking Limitations**:
1. Cannot add new bindings to locked namespace - must first create binding (e.g., `mean <- NULL`)
2. Explicit `pkg::fun()` calls bypass namespace mocking

## Test Fixtures and Setup

### Using test_path()
```r
test_that("reads fixture data", {
  data <- readRDS(test_path("fixtures", "test_data.rds"))
  expect_equal(nrow(data), 100)
})
```

### Temporary Files with withr
```r
test_that("writes output correctly", {
  # Single temp file (auto-deleted)
  path <- withr::local_tempfile()
  write.csv(data, path)
  expect_true(file.exists(path))
})

test_that("creates directory structure", {
  # Temp directory (auto-deleted)
  dir <- withr::local_tempdir()
  save_results(data, file.path(dir, "output.rds"))
  expect_true(file.exists(file.path(dir, "output.rds")))
})
```

### Temporary State Changes
```r
test_that("respects width option", {
  withr::local_options(width = 40)
  # Test code; option automatically reverts
})

test_that("uses env variable", {
  withr::local_envvar(MY_VAR = "test_value")
  # Test code; env var automatically reverts
})
```

## Running Tests

| Command | Scope |
|---------|-------|
| `devtools::test()` | All tests (Ctrl/Cmd + Shift + T) |
| `devtools::test_active_file()` | Current file (Ctrl/Cmd + T) |
| `testthat::test_file("path")` | Specific file |
| `devtools::check()` | Full R CMD check |

## Best Practices

### 1. Self-Contained Tests
Each test should include all necessary setup:
```r
# Good - self-contained
test_that("foofy() handles empty input", {
  dat <- data.frame(x = character(), y = numeric())
  expect_equal(foofy(dat), expected_result)
})

# Bad - depends on external state
dat <- data.frame(x = character(), y = numeric())
test_that("foofy() handles empty input", {
  expect_equal(foofy(dat), expected_result)
})
```

### 2. Test Each Behavior Once
Design tests so each behavior is verified in exactly one test.

### 3. Accept Repetition
Prioritize clarity over DRY principles in tests. Repeated setup code is acceptable.

### 4. Descriptive Test Names
```r
# Good
test_that("multiplication of negative numbers returns positive", { ... })

# Bad
test_that("mult works", { ... })
```

### 5. Test Edge Cases
Focus on:
- Empty inputs
- NULL/NA values
- Boundary conditions
- Invalid inputs (expect errors)

### 6. Bug-Driven Testing
When you find a bug, write a failing test first, then fix it.

### 7. Never Modify Global State
Use withr for temporary changes. Never use:
- `library()` in test files
- `source()` in test files
- Direct assignment to global environment

## Project-Specific Patterns

This project (NEPSroutines) uses these testing patterns:

### Fixtures Location
Test fixtures are stored in `tests/testthat/fixtures/` with subdirectories for different test scenarios (ex1, ex2).

### Common Expectations Used
- `expect_no_error()` for function execution success
- `expect_error()` with `regexp` for error message validation
- `expect_warning()` / `expect_no_warning()` for warning testing
- `expect_equal()` for value comparisons
- `expect_true()` / `expect_false()` for boolean checks
- `expect_contains()` for checking vector contents

### Example Pattern from This Project
```r
test_that("function_name() works", {
  # Setup test data
  data <- data.frame(...)

  # Test successful execution
  expect_no_error(function_name(data, param = value))

  # Test expected warnings
  expect_warning(function_name(data, warn = TRUE),
                 regexp = "^Expected warning pattern.+")

  # Test error conditions
  expect_error(function_name(data, invalid_param = "bad"),
               regexp = "^Expected error pattern.+")

  # Test return values
  result <- function_name(data)
  expect_equal(dim(result), c(expected_rows, expected_cols))
})
```

## Resources

- [testthat documentation](https://testthat.r-lib.org/)
- [R Packages book - Testing](https://r-pkgs.org/testing-basics.html)
- [Testing Design chapter](https://r-pkgs.org/testing-design.html)
- [Snapshot testing vignette](https://testthat.r-lib.org/articles/snapshotting.html)
- [Mocking vignette](https://testthat.r-lib.org/articles/mocking.html)
