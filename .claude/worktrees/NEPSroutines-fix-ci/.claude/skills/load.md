# /load - Load Package for Testing

Load the package into the R session for interactive testing.

## Usage

- `/load` - Load all exported and internal functions

## Instructions

When the user invokes `/load`:

1. **Execute the command:**
   ```bash
   Rscript -e "devtools::load_all()"
   ```

2. **Report results:**
   - Confirm package loaded successfully
   - Note any loading warnings or errors
   - Mention the package is ready for interactive use

## What This Does

1. Sources all R files in R/ directory
2. Loads all dependencies from Imports
3. Makes both exported AND internal functions available
4. Simulates having the package installed

## Examples

```
/load   # Load package for testing
```

## Use Cases

### Testing a Function Interactively
```r
# After /load, you can test functions:
devtools::load_all()
result <- irt_analysis(resp = test_data, items = item_cols)
```

### Debugging Internal Functions
```r
# Internal functions (marked @noRd) are also available:
devtools::load_all()
internal_result <- some_internal_function(x)
```

### Quick Iteration
```r
# Edit code, then reload:
devtools::load_all()  # Picks up changes immediately
```

## Notes

- Faster than `devtools::install()` for development
- Changes to R/ files are picked up on next `/load`
- Does NOT run tests or documentation - use `/verify` for that
- Use for quick interactive testing before formal test runs
