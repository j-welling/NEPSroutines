
# dif_analysis() estimates a main effects model and a DIF model for every DIF
# variable. Each analysis is therefore run only once per example dataset and all
# assertions share that result, following test-distractor_analysis.R.


test_that("dif_analysis() works for dichotomous data", {

  # Temporary output directory
  path <- withr::local_tempdir()

  data(ex1)
  # Estimated without error; expect_no_error() surfaces the message on failure.
  expect_no_error(
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
  )

  # Contains the output of conduct_dif_analysis(), summarize_dif_analysis()
  # and build_dif_tr_tables()
  expect_true(all(c("models", "summaries", "tr_tables") %in% names(result)))

  # One model and one summary per DIF variable
  expect_equal(names(result$models), c("sex", "mig"))
  expect_equal(names(result$summaries), c("sex", "mig"))
  expect_equal(result$models$sex$dif_var, "sex")
  expect_equal(result$models$mig$dif_var, "mig")
  expect_equal(result$models$sex$irt_type, "dich")
  expect_true(all(c("mmod", "dmod", "facets") %in% names(result$models$sex)))
  expect_true(
    all(c("est", "mne", "gof", "facets", "irt_type") %in%
          names(result$summaries$sex))
  )

  # Technical report tables (tibbles inherit from data.frame)
  expect_s3_class(result$tr_tables$gof, "data.frame")
  expect_s3_class(result$tr_tables$estimates, "data.frame")

  # All files created
  expect_true(file.exists(paste0(path, "/dif_dich_models.rds")))
  expect_true(file.exists(paste0(path, "/dif_dich_summaries.rds")))
  expect_true(file.exists(paste0(path, "/dif_dich_TR.xlsx")))
  expect_true(file.exists(paste0(path, "/dif_dich_sex.xlsx")))
  expect_true(file.exists(paste0(path, "/dif_dich_mig.xlsx")))

  # The saved results are the same objects that are returned
  expect_identical(
    readRDS(paste0(path, "/dif_dich_summaries.rds")), result$summaries
  )

  # Same structure as the precomputed results. Only the structure is compared:
  # the ex1 DIF fixtures were saved with different group sizes than the current
  # ex1 data produces, so their values cannot currently be reproduced.
  models_fix <- readRDS(test_path("fixtures/ex1/results/dif_dich_models.rds"))
  summaries_fix <- readRDS(
    test_path("fixtures/ex1/results/dif_dich_summaries.rds")
  )
  expect_equal(names(result$models), names(models_fix))
  expect_equal(names(result$summaries), names(summaries_fix))
  expect_equal(names(result$summaries$sex), names(summaries_fix$sex))
  # The goodness-of-fit column schema and the group-contrast keys do not depend
  # on group sizes, so they can be checked against the fixtures despite the
  # group-size drift. This catches a renamed or reordered column that the
  # top-level name comparison above would miss.
  expect_equal(names(result$summaries$sex$gof), names(summaries_fix$sex$gof))
  expect_equal(names(result$summaries$sex$est), names(summaries_fix$sex$est))
  expect_equal(names(result$summaries$sex$mne), names(summaries_fix$sex$mne))

  # Written tables can be read back in and contain the expected sheets
  expect_equal(names(Import(path, "dif_dich_TR.xlsx")), c("gof", "estimates"))
  expect_true(
    all(c("gof", "facets") %in% names(Import(path, "dif_dich_sex.xlsx")))
  )

})


test_that("dif_analysis() works for polytomous data", {

  # Temporary output directory
  path <- withr::local_tempdir()

  data(ex2)
  # Estimated without error; expect_no_error() surfaces the message on failure.
  expect_no_error(
    result <- dif_analysis(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      dif_vars = "sex",
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
  )

  # Polytomous items are modelled with a PCM
  models_fix <- readRDS(test_path("fixtures/ex2/results/dif_poly_models.rds"))
  expect_equal(result$models$sex$irt_type, "poly")
  expect_equal(result$models$sex$irt_type, models_fix$sex$irt_type)

  # All files created
  expect_true(file.exists(paste0(path, "/dif_poly_models.rds")))
  expect_true(file.exists(paste0(path, "/dif_poly_summaries.rds")))

})


test_that("dif_model() and dif_summary() work for a single DIF variable", {

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

  expect_true(
    all(c("mmod", "dmod", "facets", "dif_var", "irt_type") %in% names(dif_mod))
  )
  expect_equal(dif_mod$dif_var, "sex")
  expect_equal(dif_mod$irt_type, "dich")

  smry <- dif_summary(
    diflist = dif_mod,
    vars = ex1$vars,
    print = FALSE,
    save = FALSE
  )

  expect_true(
    all(c("est", "mne", "gof", "facets", "irt_type") %in% names(smry))
  )
  expect_equal(smry$irt_type, "dich")

})


test_that("dif_model() excludes items below the min_val threshold", {

  data(ex1)

  # warn = TRUE, otherwise the exclusion of items is not observable.
  # capture_warnings() keeps the test robust against additional warnings.
  warns <- testthat::capture_warnings(
    result <- dif_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      dif_var = "sex",
      valid = "valid",
      # The lowest number of valid responses per group and item in ex1 is 168
      min_val = 200,
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = TRUE
    )
  )

  # Items without the minimum number of valid responses were dropped
  expect_true(any(grepl("minimum number of valid responses", warns)))

  # Still produces a valid model
  expect_true(all(c("mmod", "dmod") %in% names(result)))

})


test_that("dif_model() excludes missing values in the DIF variable", {

  data(ex1)

  # Add some NAs to the DIF variable
  ex1_mod <- ex1
  ex1_mod$resp$sex[1:10] <- NA

  warns <- testthat::capture_warnings(
    result <- dif_model(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      dif_var = "sex",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = TRUE
    )
  )

  expect_true(any(grepl(
    "missing values were found in the DIF variable", warns
  )))
  expect_true(all(c("mmod", "dmod") %in% names(result)))

})


test_that("dif_model() includes DIF-variable missings as an extra group", {

  data(ex1)

  # ex1$resp$mig carries 208 NAs, above the default include_mv threshold (200),
  # so the missing cases are kept as an additional group rather than dropped.
  # This is the complement of the exclusion branch tested above.
  warns <- testthat::capture_warnings(
    result <- dif_model(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      dif_var = "mig",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      verbose = FALSE,
      save = FALSE,
      warn = TRUE
    )
  )

  expect_true(any(grepl(
    "included in the analysis as an extra group", warns
  )))
  expect_true(all(c("mmod", "dmod") %in% names(result)))

})


test_that("dif_analysis() rejects mismatched 'select' and 'dif_vars'", {

  data(ex1)

  expect_error(
    dif_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = c("dich", "dich"),
      dif_vars = c("sex", "mig", "school"),
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      verbose = FALSE,
      warn = FALSE
    ),
    regexp = "'select' and 'dif_vars'"
  )

})
