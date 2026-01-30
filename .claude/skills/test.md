# /test - Run Test Suite

Run the package test suite with various options.

## Usage

- `/test` - Run all tests
- `/test <pattern>` - Run tests matching pattern (e.g., `/test utils`)
- `/test --coverage` - Run tests with coverage report

## Instructions

When the user invokes `/test`:

1. **Parse arguments:**
   - No arguments: Run all tests
   - Pattern argument: Filter tests by pattern
   - `--coverage` flag: Run with coverage

2. **Execute the appropriate command:**

   **All tests:**
   ```bash
   Rscript -e "devtools::test()"
   ```

   **Filtered tests:**
   ```bash
   Rscript -e "devtools::test(filter = '<pattern>')"
   ```

   **With coverage:**
   ```bash
   Rscript -e "covr::package_coverage()"
   ```

3. **Report results:**
   - Summarize passed/failed/skipped tests
   - If failures occurred, highlight the failing test files
   - For coverage, report the overall percentage

## Examples

```
/test              # Run test suite (27 active tests, 24 skipped)
/test utils        # Run only test-utils.R tests
/test irt          # Run tests matching "irt"
/test --coverage   # Run with coverage report
```

## Notes

- Test fixtures are in `tests/testthat/fixtures/` (ex1, ex2, ex3)
- Currently 24 tests are skipped in `test-technical_report_tables.R`
- Target coverage: 80%+ (currently ~7%)
