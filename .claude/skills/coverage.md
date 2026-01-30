# /coverage - Test Coverage Report

Generate and analyze test coverage report.

## Usage

- `/coverage` - Generate overall coverage report
- `/coverage <file>` - Show coverage for specific source file
- `/coverage --report` - Generate HTML report

## Instructions

When the user invokes `/coverage`:

1. **Parse arguments:**
   - No arguments: Overall package coverage
   - File name: Coverage for specific R/ file
   - `--report` flag: Generate interactive HTML report

2. **Execute the appropriate command:**

   **Overall coverage:**
   ```bash
   Rscript -e "covr::package_coverage()"
   ```

   **Specific file coverage:**
   ```bash
   Rscript -e "covr::file_coverage('R/<file>.R', 'tests/testthat/')"
   ```

   **HTML report:**
   ```bash
   Rscript -e "covr::report()"
   ```

3. **Report results:**
   - Overall percentage
   - Per-file breakdown
   - Highlight files with low coverage

## Examples

```
/coverage                # Overall package coverage
/coverage utils          # Coverage for R/utils.R
/coverage irt_analyses   # Coverage for R/irt_analyses.R
/coverage --report       # Open HTML report in browser
```

## Current Coverage Status

**Target: 80%+** | **Current: ~7%**

| File | Coverage | Status |
|------|----------|--------|
| utils.R | ~70% | Good |
| data_preparation.R | ~10% | Needs work |
| irt_analyses.R | 0% | Critical gap |
| dif_analysis.R | 0% | Critical gap |
| linking.R | 0% | Critical gap |
| create_scores.R | 0% | Critical gap |
| mv_item.R | 0% | Critical gap |
| mv_person.R | 0% | Critical gap |

## Dependencies

Requires the `covr` package:
```r
install.packages("covr")
```

## Notes

- Coverage is calculated by running tests and tracking which lines execute
- Low coverage files are priorities for test development
- See `dev/81_test_suite_enhancement_plan.md` for test priorities
