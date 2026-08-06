
# All tests in this file call irt_model()/irt_analysis(), which reach MASS via
# TAM -> CDM. MASS is a recommended package that is missing from clean
# R CMD check environments, so every block is guarded (see
# dev/01_claude_reference.md).

# Maximum absolute deviation per column between two summary tables
max_deviation <- function(new, old, cols) {
  vapply(cols, function(col) {
    max(abs(new[[col]] - old[[col]]), na.rm = TRUE)
  }, numeric(1))
}

# Compare WLEs against a fixture, matched on person ID rather than row position.
#
# wle_rel alone is not enough: it is 1 - mean(error^2) / var(theta), a variance
# ratio, so it is unchanged if theta is permuted across persons, shifted by a
# constant, or sign-flipped. A permutation is exactly the regression that
# matters here, because irt_model() filters persons via only_valid() before
# handing ID_t to TAM as pid, and a misalignment would attach abilities to the
# wrong persons while leaving every reported number intact.
#
# Matching on pid rather than row order also means a change in the order
# only_valid() returns rows is not by itself a failure, as long as each person
# keeps their own estimate.
expect_wle_equal <- function(new, old, tolerance = 1e-4) {

  cols <- c("pid", "theta", "error")
  new <- as.data.frame(new)[, cols]
  old <- as.data.frame(old)[, cols]

  expect_setequal(new$pid, old$pid)

  merged <- merge(new, old, by = "pid", suffixes = c("", "_fix"))

  # Guards against a duplicated pid silently expanding the join
  expect_equal(nrow(merged), nrow(old))

  expect_equal(merged$theta, merged$theta_fix, tolerance = tolerance)
  expect_equal(merged$error, merged$error_fix, tolerance = tolerance)
}


test_that("irt_model() runs without error for 1PL", {

  skip_if_not_installed("MASS")

  data(ex1)

  result <- try({
    irt_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      irtmodel = "1PL",
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("mod" %in% names(result))
  expect_true("fit" %in% names(result))
  expect_true("pars" %in% names(result))
  expect_true("wle" %in% names(result))
  expect_true("wle_rel" %in% names(result))
  expect_equal(result$irtmodel, "1PL")

})


test_that("irt_model() runs without error for 2PL", {

  skip_if_not_installed("MASS")

  data(ex1)

  result <- try({
    irt_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      irtmodel = "2PL",
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_equal(result$irtmodel, "2PL")

})


test_that("irt_model() runs without error for PCM2", {

  skip_if_not_installed("MASS")

  data(ex2)

  result <- try({
    irt_model(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      valid = "valid",
      scoring = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      irtmodel = "PCM2",
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_equal(result$irtmodel, "PCM2")

})


test_that("irt_model() runs without error for GPCM", {

  skip_if_not_installed("MASS")

  data(ex2)

  result <- try({
    irt_model(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      valid = "valid",
      scoring = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      irtmodel = "GPCM",
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_equal(result$irtmodel, "GPCM")

})


test_that("irt_analysis() dichotomous produces expected structure", {

  skip_if_not_installed("MASS")

  data(ex1)

  result <- try({
    irt_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("model.1pl" %in% names(result))
  expect_true("model.2pl" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("model_fit" %in% names(result))

})


test_that("irt_analysis() polytomous produces expected structure", {

  skip_if_not_installed("MASS")

  data(ex2)

  result <- try({
    irt_analysis(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      valid = "valid",
      scoring = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("model.pcm" %in% names(result))
  expect_true("model.gpcm" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("model_fit" %in% names(result))
  expect_true("steps" %in% names(result))

})


test_that("irt_analysis() dichotomous matches fixture", {

  skip_if_not_installed("MASS")

  data(ex1)
  path <- withr::local_tempdir()

  result <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  # Both output files are written
  expect_true(file.exists(file.path(path, "irt_dich.rds")))
  expect_true(file.exists(file.path(path, "irt_dich.xlsx")))

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))

  # Item parameters. The unrounded TAM estimates are the numerical ground
  # truth: they must not drift, and they are unaffected by any change to the
  # `digits` used for the printed tables.
  expect_equal(result$model.1pl$mod$xsi$xsi,
               fixture$model.1pl$mod$xsi$xsi, tolerance = 1e-4)
  expect_equal(result$model.1pl$mod$xsi$se.xsi,
               fixture$model.1pl$mod$xsi$se.xsi, tolerance = 1e-4)
  expect_equal(result$model.2pl$mod$xsi$xsi,
               fixture$model.2pl$mod$xsi$xsi, tolerance = 1e-4)
  expect_equal(result$model.2pl$mod$B, fixture$model.2pl$mod$B,
               tolerance = 1e-4)

  # Person parameters
  expect_equal(result$model.1pl$wle_rel, fixture$model.1pl$wle_rel,
               tolerance = 1e-4)
  expect_equal(result$model.2pl$wle_rel, fixture$model.2pl$wle_rel,
               tolerance = 1e-4)

  # The estimates behind that reliability, per person (see expect_wle_equal)
  expect_wle_equal(result$model.1pl$wle, fixture$model.1pl$wle)
  expect_wle_equal(result$model.2pl$wle, fixture$model.2pl$wle)

  # pid must carry the ID_t of the valid cases, which is what makes the WLEs
  # attributable to persons at all. Checked against the input data rather than
  # the fixture, so it still holds if the fixture is ever regenerated.
  valid_ids <- ex1$resp$ID_t[ex1$resp$valid]
  expect_equal(result$model.1pl$wle$pid, valid_ids)
  expect_equal(result$model.2pl$wle$pid, valid_ids)

  # Model fit table is rounded to whole numbers / 3 decimals and is therefore
  # unaffected by the digits change: it must match exactly.
  expect_equal(result$model_fit, fixture$model_fit)

  # Summary table structure
  expect_equal(names(result$summary), names(fixture$summary))
  expect_equal(nrow(result$summary), nrow(fixture$summary))
  expect_equal(result$summary$Item, fixture$summary$Item)

  # Summary table values. The fixture was written when `digits` defaulted to 2
  # and is now produced with 3 decimals, so allow half a unit in the fixture's
  # last decimal.
  deviation <- max_deviation(
    result$summary, fixture$summary,
    cols = c("N_administered", "N_valid", "correct", "xsi", "SE", "WMNSQ",
             "t", "rit", "aQ3", "Discr.")
  )
  expect_lt(max(deviation), 0.01)

})


test_that("irt_analysis() polytomous matches fixture", {

  skip_if_not_installed("MASS")

  data(ex2)
  path <- withr::local_tempdir()

  result <- irt_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  expect_true(file.exists(file.path(path, "irt_poly.rds")))
  expect_true(file.exists(file.path(path, "irt_poly.xlsx")))

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))

  # Item parameters (see comment in the dichotomous test above)
  expect_equal(result$model.pcm$mod$xsi$xsi,
               fixture$model.pcm$mod$xsi$xsi, tolerance = 1e-4)
  expect_equal(result$model.pcm$mod$xsi$se.xsi,
               fixture$model.pcm$mod$xsi$se.xsi, tolerance = 1e-4)
  expect_equal(result$model.gpcm$mod$xsi$xsi,
               fixture$model.gpcm$mod$xsi$xsi, tolerance = 1e-4)

  # Slopes. For GPCM these are freely estimated and are the whole reason to fit
  # it alongside PCM, so they need the same guard the 2PL slopes get above;
  # mod$xsi holds the step parameters and says nothing about discrimination.
  expect_equal(result$model.gpcm$mod$B, fixture$model.gpcm$mod$B,
               tolerance = 1e-4)

  # PCM slopes are fixed at the scoring weights rather than estimated, so this
  # pins the scoring matrix that was handed to TAM.
  expect_equal(result$model.pcm$mod$B, fixture$model.pcm$mod$B,
               tolerance = 1e-4)

  # Person parameters
  expect_equal(result$model.pcm$wle_rel, fixture$model.pcm$wle_rel,
               tolerance = 1e-4)
  expect_equal(result$model.gpcm$wle_rel, fixture$model.gpcm$wle_rel,
               tolerance = 1e-4)

  # The estimates behind that reliability, per person (see expect_wle_equal)
  expect_wle_equal(result$model.pcm$wle, fixture$model.pcm$wle)
  expect_wle_equal(result$model.gpcm$wle, fixture$model.gpcm$wle)

  valid_ids <- ex2$resp$ID_t[ex2$resp$valid]
  expect_equal(result$model.pcm$wle$pid, valid_ids)
  expect_equal(result$model.gpcm$wle$pid, valid_ids)

  expect_equal(result$model_fit, fixture$model_fit)

  # Summary table structure
  expect_equal(names(result$summary), names(fixture$summary))
  expect_equal(nrow(result$summary), nrow(fixture$summary))
  expect_equal(result$summary$Item, fixture$summary$Item)

  # Summary table values (see comment in the dichotomous test above)
  deviation <- max_deviation(
    result$summary, fixture$summary,
    cols = c("N_administered", "N_valid", "correct", "xsi", "SE", "WMNSQ",
             "t", "rit", "aQ3", "Discr.")
  )
  expect_lt(max(deviation), 0.01)

  # Step parameters are pre-formatted strings whose precision follows `digits`,
  # so their layout rather than their text is compared here.
  expect_equal(dim(result$steps), dim(fixture$steps))
  expect_equal(names(result$steps), names(fixture$steps))
  expect_equal(rownames(result$steps), rownames(fixture$steps))

  # Not every step value comes from model.pcm$mod$xsi: the last step of each
  # item is derived in steps_analysis() from the sum-zero constraint, so it is
  # package logic with no TAM counterpart to compare against. Assert the
  # constraint itself, which holds at any `digits` and does not depend on the
  # standard errors also printed in these cells.
  step_values <- vapply(result$steps, function(col) {
    as.numeric(sub(" .*$", "", col))
  }, numeric(nrow(result$steps)))

  expect_equal(rowSums(step_values, na.rm = TRUE),
               rep(0, nrow(step_values)),
               tolerance = 1e-8,
               ignore_attr = TRUE)

  # Every item must actually have a derived cell, otherwise the row sums above
  # are trivially satisfied by an all-NA row.
  expect_true(all(rowSums(!is.na(step_values)) >= 2))

})


test_that("irt_summary() produces valid output", {

  skip_if_not_installed("MASS")

  data(ex1)

  model_1pl <- irt_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "1PL",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  model_2pl <- irt_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "2PL",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  summary <- irt_summary(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    results = model_1pl,
    disc = model_2pl,
    save = FALSE,
    warn = FALSE
  )

  expect_true(is.data.frame(summary))
  expect_true("Item" %in% names(summary))
  expect_true("N_administered" %in% names(summary))
  expect_true("N_valid" %in% names(summary))
  expect_true("xsi" %in% names(summary))
  expect_true("WMNSQ" %in% names(summary))
  expect_true("rit" %in% names(summary))
  expect_true("Discr." %in% names(summary))

})


test_that("irt_model_fit() produces valid output", {

  skip_if_not_installed("MASS")

  data(ex1)

  model_1pl <- irt_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "1PL",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  model_2pl <- irt_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "2PL",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  mfit <- irt_model_fit(
    model_1p = model_1pl,
    model_2p = model_2pl,
    save = FALSE
  )

  expect_true(is.data.frame(mfit))
  expect_equal(nrow(mfit), 2)
  expect_true("N" %in% names(mfit))
  expect_true("AIC" %in% names(mfit))
  expect_true("BIC" %in% names(mfit))
  expect_true("EAPrel" %in% names(mfit))
  expect_true("WLErel" %in% names(mfit))

  # Row labels come from the irt_type branch in irt_model_fit() and are what
  # identifies the models in the technical report
  expect_equal(rownames(mfit), c("1PL model", "2PL model"))

})


test_that("irt_summary() produces valid output for polytomous models", {

  skip_if_not_installed("MASS")

  data(ex2)

  model_pcm <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  model_gpcm <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "GPCM",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  summary <- irt_summary(
    resp = ex2$resp,
    vars = ex2$vars,
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    results = model_pcm,
    disc = model_gpcm,
    save = FALSE,
    warn = FALSE
  )

  expect_true(is.data.frame(summary))
  expect_true("Item" %in% names(summary))
  expect_true("N_administered" %in% names(summary))
  expect_true("N_valid" %in% names(summary))
  expect_true("xsi" %in% names(summary))
  expect_true("WMNSQ" %in% names(summary))
  expect_true("rit" %in% names(summary))
  expect_true("Discr." %in% names(summary))

  # One row per scaled item, polytomous ones included
  expect_equal(nrow(summary), sum(ex2$vars$mixed))

})


test_that("irt_model_fit() produces valid output for polytomous models", {

  skip_if_not_installed("MASS")

  data(ex2)

  model_pcm <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  model_gpcm <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "GPCM",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  mfit <- irt_model_fit(
    model_1p = model_pcm,
    model_2p = model_gpcm,
    save = FALSE
  )

  expect_true(is.data.frame(mfit))
  expect_equal(nrow(mfit), 2)
  expect_true("EAPrel" %in% names(mfit))
  expect_true("WLErel" %in% names(mfit))

  # The 'poly' branch of irt_model_fit() is reachable only from a PCM2 model
  # and was previously never executed by any test
  expect_equal(rownames(mfit), c("PCM model", "GPCM model"))

})


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
