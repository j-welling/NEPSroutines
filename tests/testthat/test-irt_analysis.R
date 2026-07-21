
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

  # Person parameters
  expect_equal(result$model.pcm$wle_rel, fixture$model.pcm$wle_rel,
               tolerance = 1e-4)
  expect_equal(result$model.gpcm$wle_rel, fixture$model.gpcm$wle_rel,
               tolerance = 1e-4)

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

  # Step parameters are pre-formatted strings ("1.671 (0.0713)") whose
  # precision follows `digits`, so only their layout is compared here; the
  # underlying values are checked via model.pcm$mod$xsi above.
  expect_equal(dim(result$steps), dim(fixture$steps))
  expect_equal(names(result$steps), names(fixture$steps))
  expect_equal(rownames(result$steps), rownames(fixture$steps))

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

})
