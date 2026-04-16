
test_that("irt_model() runs without error for 1PL", {

  data(ex1)
  path <- withr::local_tempdir()

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


test_that("irt_model() rejects invalid irtmodel", {

  data(ex1)

  expect_error(
    irt_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      irtmodel = "INVALID",
      save = FALSE
    ),
    regexp = "Invalid irtmodel"
  )

})


test_that("irt_analysis() dichotomous produces expected structure", {

  data(ex1)
  path <- withr::local_tempdir()

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

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))

  # Compare summary table structure

  expect_equal(names(result$summary), names(fixture$summary))
  expect_equal(nrow(result$summary), nrow(fixture$summary))

  # Compare item names
  expect_equal(result$summary$Item, fixture$summary$Item)

  # Compare model fit table structure
  expect_equal(rownames(result$model_fit), rownames(fixture$model_fit))

})


test_that("irt_analysis() polytomous matches fixture", {

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

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))

  # Compare summary table structure
  expect_equal(names(result$summary), names(fixture$summary))
  expect_equal(nrow(result$summary), nrow(fixture$summary))

  # Compare steps table structure
  expect_equal(ncol(result$steps), ncol(fixture$steps))

})


test_that("irt_summary() produces valid output", {

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


test_that("irt_analysis() saves files correctly", {

  data(ex1)
  path <- withr::local_tempdir()

  irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  expect_true(file.exists(paste0(path, "/irt_dich.rds")))
  expect_true(file.exists(paste0(path, "/irt_dich.xlsx")))

})
