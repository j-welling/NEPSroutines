# /verify - Full Verification Workflow

Run the complete verification workflow: document, test, and check.

## Usage

- `/verify` - Run document → test → check sequence
- `/verify --quick` - Skip documentation step

## Instructions

When the user invokes `/verify`:

1. **Parse arguments:**
   - No arguments: Full workflow (document → test → check)
   - `--quick` flag: Skip documentation (test → check)

2. **Execute the workflow:**

   **Full verification:**
   ```bash
   Rscript -e "devtools::document()"
   Rscript -e "devtools::test()"
   Rscript -e "devtools::check(args = '--no-tests')"
   ```

   **Quick verification:**
   ```bash
   Rscript -e "devtools::test()"
   Rscript -e "devtools::check(args = '--no-tests')"
   ```

3. **Report results at each step:**
   - Document: Any roxygen warnings
   - Test: Pass/fail summary
   - Check: ERRORs, WARNINGs, NOTEs

4. **Stop on failure:**
   - If tests fail, stop and report
   - If check has ERRORs, highlight them

## Examples

```
/verify         # Full workflow
/verify --quick # Skip documentation
```

## Workflow Alignment

This skill implements steps 7-8 of CLAUDE.md workflow:

1. ~~Spec~~ → ~~Plan~~ → ~~Draft~~ → ~~Simplify~~
2. ~~Update Tests~~ → ~~Update Docs~~
3. **Test** → `/verify` step 1
4. **Verify** → `/verify` step 2
5. Quality Check → Commit & PR

## Expected Duration

- Document: ~5-10 seconds
- Test: ~30-60 seconds (27 running tests; 51 total, 24 skipped)
- Check: ~1-3 minutes

Total: ~2-4 minutes for full verification

## Notes

- Run this before committing changes
- Tests are skipped in check (already run)
- Equivalent to the verification step in CLAUDE.md
