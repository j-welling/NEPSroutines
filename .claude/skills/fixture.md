# /fixture - Test Fixture Helper

Help create, list, or validate test fixtures for IRT analysis results.

## Usage

- `/fixture list` - List available fixtures and their contents
- `/fixture create <name>` - Guide for creating a new fixture
- `/fixture validate` - Validate fixture integrity

## Instructions

When the user invokes `/fixture`:

### `/fixture list`

List the fixture directories and their contents:
```bash
ls -la tests/testthat/fixtures/
ls -la tests/testthat/fixtures/ex1/
ls -la tests/testthat/fixtures/ex2/
ls -la tests/testthat/fixtures/ex3/
```

Report:
- Total size (~127MB)
- Available examples (ex1, ex2, ex3)
- Key files in each fixture

### `/fixture create <name>`

Provide guidance for creating a new fixture:

1. **Create fixture directory:**
   ```r
   dir.create("tests/testthat/fixtures/<name>", recursive = TRUE)
   ```

2. **Generate fixture data:**
   ```r
   # Load package
   devtools::load_all()

   # Run analysis and save results
   result <- irt_analysis(resp = data, items = items)
   saveRDS(result, "tests/testthat/fixtures/<name>/irt_result.rds")
   ```

3. **Document the fixture:**
   - Add description to fixture README
   - Note data characteristics

### `/fixture validate`

Validate that fixtures can be loaded:
```r
# Check each fixture loads correctly
ex1 <- readRDS("tests/testthat/fixtures/ex1/irt_result.rds")
ex2 <- readRDS("tests/testthat/fixtures/ex2/irt_result.rds")
ex3 <- readRDS("tests/testthat/fixtures/ex3/irt_result.rds")
```

## Available Fixtures

### ex1 - Dichotomous Items
- ~50 dichotomous items
- Basic IRT analysis results
- DIF analysis results
- Score files

### ex2 - Polytomous/Mixed Items
- Polytomous response data
- PCM/GPCM model results
- Mixed item types

### ex3 - Booklet Design
- Multi-form assessment
- Multiple booklet versions
- Linking scenarios

## Fixture Contents

Each fixture typically contains:
- `irt_result.rds` - IRT model output
- `dif_result.rds` - DIF analysis output
- `scores.rds` - Score estimates
- `suf.rds` - Scientific use file data

## Notes

- Fixtures are pre-computed to avoid slow test runs
- Total size: ~127MB (tracked in Git as regular files, not via Git LFS)
- Update fixtures when algorithm changes
- Used for regression testing
