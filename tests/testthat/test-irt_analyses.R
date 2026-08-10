
test_that("steps_analysis() formats SEs with `digits` decimals", {

  mod <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))$model.pcm

  steps <- steps_analysis(mod, digits = 2, save = FALSE)

  # Cells with an estimated SE read "<xsi> (<se>)". A misplaced parenthesis
  # used to push `nsmall = digits` into paste0()'s `...`, appending the *value*
  # of `digits` to every SE, e.g. "1.67 (0.072)" for an SE of 0.07.
  # unlist() first: grepl() on a data.frame matches against deparsed *columns*,
  # not cells, so filtering `steps` directly would silently select everything.
  cells <- unlist(steps, use.names = FALSE)
  cells <- cells[!is.na(cells) & grepl("(", cells, fixed = TRUE)]
  expect_length(cells, 6)

  se <- sub("^.+\\((.+)\\)$", "\\1", cells)
  expect_true(all(grepl("^[0-9]+\\.[0-9]{2}$", se)))

  expect_equal(steps["mag120003_c", "step1"], "1.67 (0.07)")
  expect_equal(steps["mag120016_c", "step2"], "-0.96 (0.09)")

  # digits propagates to both the estimate and the SE.
  steps3 <- steps_analysis(mod, digits = 3, save = FALSE)
  expect_equal(steps3["mag120003_c", "step1"], "1.671 (0.071)")

})


test_that("steps_analysis() keeps trailing zeros in SEs", {

  # An SE that rounds to 0.10 formats as "0.1" without nsmall, so the appended
  # `digits` landed in the hundredths place and produced a plausible but wrong
  # SE ("0.12") that survived the re-rounding in TblSteps().
  mod <- readRDS(
    test_path("fixtures/ex3/results/irt_poly_booklet1.rds")
  )$model.pcm

  steps <- steps_analysis(mod, digits = 2, save = FALSE)
  expect_equal(steps["reg70002_c", "step1"], "0.29 (0.10)")

})


test_that("steps_analysis() strips step suffixes of two or more digits", {

  # The suffix pattern is anchored with '+'. A single-digit pattern leaves
  # 'itemA_step10' unstripped, so the item gains a second, bogus row.
  xsi <- data.frame(xsi = seq(-1, 1, length.out = 10), se.xsi = rep(0.05, 10))
  rownames(xsi) <- paste0("itemA_step", 1:10)

  steps <- steps_analysis(list(mod = list(xsi = xsi)), digits = 2, save = FALSE)

  expect_equal(rownames(steps), "itemA")
  expect_equal(ncol(steps), 11)

})


test_that("steps_analysis() writes the constrained last step without an SE", {

  # This cell is filled by a separate loop from the one that formats the
  # estimated steps, so it needs its own guard. See #154 for its rounding.
  mod <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))$model.pcm

  steps <- steps_analysis(mod, digits = 2, save = FALSE)

  expect_equal(steps["mag120003_c", "step3"], "-0.11")
  expect_equal(steps["mag120007_c", "step2"], "-0.36")

})


test_that("steps_analysis() does not warn on non-step xsi rownames", {

  # xsi holds one row per item plus one per step; selecting steps by coercing
  # every rowname with as.numeric() emitted "NAs introduced by coercion".
  mod <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))$model.pcm

  expect_no_warning(steps_analysis(mod, digits = 2, save = FALSE))

})
