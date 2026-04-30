# /check - R CMD Check

Run R CMD check to validate package integrity.

## Usage

- `/check` - Run check (skips tests since `/test` should be run separately)
- `/check --full` - Full check including tests

## Instructions

When the user invokes `/check`:

1. **Parse arguments:**
   - No arguments: Skip tests (recommended workflow)
   - `--full` flag: Include tests

2. **Execute the appropriate command:**

   **Standard check (skip tests):**
   ```bash
   Rscript -e "devtools::check(args = '--no-tests')"
   ```

   **Full check (include tests):**
   ```bash
   Rscript -e "devtools::check()"
   ```

3. **Report results:**
   - Summarize ERRORs, WARNINGs, and NOTEs
   - Highlight any issues that need attention
   - For ERRORs: Show the specific failure details

## Examples

```
/check        # Standard check, skip tests
/check --full # Full check including tests
```

## Common Issues

### Expected NOTEs
- "checking CRAN incoming feasibility" - OK for internal packages
- "Non-standard file/directory found" - Check if intentional

### Common WARNINGs to fix
- Undocumented arguments in roxygen
- Missing imports in NAMESPACE
- Examples that don't run

### ERRORs require immediate attention
- Package installation failure
- Missing dependencies
- R version incompatibility

## Notes

- Per CLAUDE.md workflow: Run `/test` first, then `/check`
- The `--no-tests` flag avoids running tests twice
- Check takes 1-3 minutes depending on system
