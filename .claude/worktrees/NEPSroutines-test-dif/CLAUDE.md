# CLAUDE.md

## Workflow

Always work on a git worktree, not the main repository.

1. **Spec** - Understand the task and read relevant code first
2. **Plan** - Use plan mode to design the approach
3. **Draft** - Implement the changes
4. **Simplify** - Review and simplify the solution
5. **Update Tests** - Add or update tests for changed functionality
6. **Update Docs** - Update roxygen comments and `dev/` documentation
7. **Test** - Run `devtools::test()`
8. **Verify** - Run `devtools::check(args = "--no-tests")` (skip tests since already run)
9. **Quality Check** - Use a subagent to verify code quality and safety
10. **Commit & PR** - Commit changes and create a pull request

## Quick Commands

```bash
Rscript -e "devtools::test()"                       # Run tests
Rscript -e "devtools::check(args = '--no-tests')"   # R CMD check (skip tests)
Rscript -e "devtools::document()"                   # Regenerate docs
Rscript -e "devtools::load_all()"                   # Load for testing
```

## Project Structure

```
R/                  # Source code
man/                # Documentation (auto-generated, don't edit)
tests/testthat/     # Tests (testthat edition 3)
dev/                # Development docs
```

## Reference

See `dev/01_claude_reference.md` for:
- Key source files and their purposes
- Core functions
- Development conventions
- Dependencies

See `dev/91_improvement_plan.md` for the roadmap.
