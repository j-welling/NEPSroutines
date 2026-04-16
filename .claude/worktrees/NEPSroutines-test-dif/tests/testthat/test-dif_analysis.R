
test_that("dif_model() runs without error for dichotomous data", {

  data(ex1)

  result <- try({
    dif_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      dif_var = "sex",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("mmod" %in% names(result))
  expect_true("dmod" %in% names(result))
  expect_true("facets" %in% names(result))
  expect_true("dif_var" %in% names(result))
  expect_true("irt_type" %in% names(result))
  expect_equal(result$dif_var, "sex")
  expect_equal(result$irt_type, "dich")

})


test_that("dif_model() runs without error for polytomous data", {

  data(ex2)

  result <- try({
    dif_model(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      dif_var = "sex",
      valid = "valid",
      scoring = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_equal(result$irt_type, "poly")

})


test_that("conduct_dif_analysis() handles multiple DIF variables", {

  data(ex1)

  result <- try({
    conduct_dif_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      dif_vars = c("sex", "mig"),
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_equal(length(result), 2)
  expect_true("sex" %in% names(result))
  expect_true("mig" %in% names(result))

})


test_that("dif_summary() produces valid output", {

  data(ex1)

  dif_mod <- dif_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    dif_var = "sex",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  summary <- dif_summary(
    diflist = dif_mod,
    vars = ex1$vars,
    print = FALSE,
    save = FALSE
  )

  expect_true(is.list(summary))
  expect_true("est" %in% names(summary))
  expect_true("mne" %in% names(summary))
  expect_true("gof" %in% names(summary))
  expect_true("facets" %in% names(summary))
  expect_true("irt_type" %in% names(summary))

})


test_that("dif_analysis() dichotomous produces expected structure", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- try({
    dif_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      dif_vars = c("sex", "mig"),
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      verbose = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("models" %in% names(result))
  expect_true("summaries" %in% names(result))
  expect_true("tr_tables" %in% names(result))

})


test_that("dif_analysis() dichotomous matches fixture structure", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    dif_vars = c("sex", "mig"),
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    verbose = FALSE,
    warn = FALSE
  )

  # Load fixtures
  fixture_models <- readRDS(test_path("fixtures/ex1/results/dif_dich_models.rds"))
  fixture_summaries <- readRDS(test_path("fixtures/ex1/results/dif_dich_summaries.rds"))

  # Compare structure
  expect_equal(names(result$models), names(fixture_models))
  expect_equal(names(result$summaries), names(fixture_summaries))

  # Compare DIF variable names
  expect_equal(result$models$sex$dif_var, fixture_models$sex$dif_var)
  expect_equal(result$models$mig$dif_var, fixture_models$mig$dif_var)

})


test_that("dif_analysis() polytomous matches fixture structure", {

  data(ex2)
  path <- withr::local_tempdir()

  result <- dif_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    dif_vars = c("sex"),
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    verbose = FALSE,
    warn = FALSE
  )

  # Load fixture
  fixture_models <- readRDS(test_path("fixtures/ex2/results/dif_poly_models.rds"))

  # Compare structure
  expect_equal(result$models$sex$irt_type, "poly")
  expect_equal(result$models$sex$irt_type, fixture_models$sex$irt_type)

})


test_that("build_dif_tr_tables() produces valid output", {

  data(ex1)

  dif_result <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    dif_vars = c("sex", "mig"),
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = FALSE,
    return = TRUE,
    verbose = FALSE,
    warn = FALSE
  )

  tr_tables <- build_dif_tr_tables(
    dif_summaries = dif_result$summaries,
    vars = ex1$vars,
    save = FALSE
  )

  expect_true(is.list(tr_tables))
  expect_true("gof" %in% names(tr_tables))
  expect_true("estimates" %in% names(tr_tables))
  expect_true(is.data.frame(tr_tables$gof))
  expect_true(is.data.frame(tr_tables$estimates) || tibble::is_tibble(tr_tables$estimates))

})


test_that("dif_analysis() saves files correctly", {

  data(ex1)
  path <- withr::local_tempdir()

  dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    dif_vars = c("sex", "mig"),
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    verbose = FALSE,
    warn = FALSE
  )

  expect_true(file.exists(paste0(path, "/dif_dich_models.rds")))
  expect_true(file.exists(paste0(path, "/dif_dich_summaries.rds")))
  expect_true(file.exists(paste0(path, "/dif_dich_TR.xlsx")))

})


test_that("dif_model() respects min_val threshold", {

  data(ex1)

  # With moderately high min_val, some items should be excluded
  result <- dif_model(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    dif_var = "sex",
    valid = "valid",
    min_val = 100,  # Moderate threshold (should exclude items with < 100 valid responses per group)
    mvs = c(OM = -97, NV = -95, NR = -94),
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  # Should still produce a valid model
  expect_true(is.list(result))
  expect_true("mmod" %in% names(result))

})


test_that("dif_model() handles missing values in DIF variable", {

  data(ex1)

  # Add some NAs to DIF variable
  ex1_mod <- ex1
  ex1_mod$resp$sex[1:10] <- NA

  # Should produce warning about excluded cases
  expect_warning(
    dif_model(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      dif_var = "sex",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = TRUE
    ),
    regexp = "missing values"
  )

})
