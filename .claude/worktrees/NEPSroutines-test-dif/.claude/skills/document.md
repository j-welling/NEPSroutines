# /document - Regenerate Documentation

Regenerate roxygen2 documentation from source files.

## Usage

- `/document` - Update man/ pages from roxygen comments

## Instructions

When the user invokes `/document`:

1. **Execute the command:**
   ```bash
   Rscript -e "devtools::document()"
   ```

2. **Report results:**
   - List any new or updated man pages
   - Note any roxygen warnings (undocumented parameters, etc.)
   - Confirm NAMESPACE was updated if exports changed

## What This Does

1. Parses roxygen2 comments (`#'`) in R/ source files
2. Generates/updates `.Rd` files in man/ directory
3. Updates NAMESPACE with exports and imports
4. Updates DESCRIPTION collation order

## Examples

```
/document   # Regenerate all documentation
```

## Common Warnings to Address

### Missing Parameter Documentation
```
@param item_name Parameter description here
```

### Missing Return Value
```
@return Description of what the function returns
```

### Missing Examples
```
@examples
my_function(data = example_data)
```

## Notes

- man/ directory is auto-generated - never edit directly
- ~110 man pages in this package
- Run before `/check` to ensure docs are current
- Part of standard workflow: edit R/ → `/document` → `/test` → `/check`
