# dim_analysis() estimates a unidimensional reference model plus one model per
# dimension variable, and every TAM estimation on ex3 takes several minutes.
# Each configuration is therefore run only once and all assertions share that
# result, following test-distractor_analysis.R.


test_that("dim_analysis() works for a multidimensional model", {

  # Temporary output directory
  path <- withr::local_tempdir()

  data(ex3)

  # The texttype model does not converge on ex3 (the precomputed fixture hit the
  # iteration limit at maxiter = 10000 as well), so the convergence warning is
  # expected. capture_warnings() keeps the test robust against additional
  # warnings. An estimation error would abort the block on its own, so no
  # try() wrapper is needed to report one.
  warns <- testthat::capture_warnings(
    result <- dim_analysis(
      resp = ex3$resp,
      vars = ex3$vars,
      select = "mixed",
      scoring = "scoring",
      dim = "texttype",
      valid = "valid",
      irtmodel = "PCM2",
      mvs = c(OM = -97, NV = -95, NR = -94, MBD = -56, LT = -54),
      maxiter = 500,
      snodes = 1000,
      print = FALSE,
      save = TRUE,
      return = TRUE,
      path_results = path,
      path_table = path,
      overwrite = TRUE,
      verbose = FALSE
    )
  )

  expect_true(any(grepl("did not converge", warns)))

  # Contains the output of conduct_dim_analysis() and dim_summary()
  expect_equal(names(result), c("analysis", "summary"))
  expect_equal(names(result$analysis), c("uni", "texttype"))
  expect_equal(
    names(result$summary),
    c("Cor-Var uni", "Cor-Var texttype", "Goodness of fit")
  )

  # dim_summary() reproduces the summary that dim_analysis() embeds
  expect_identical(dim_summary(result$analysis, save = FALSE), result$summary)

  # Goodness of fit table structure
  gof <- result$summary$`Goodness of fit`
  expect_equal(names(gof), c("Stat", "uni", "texttype"))
  expect_equal(gof$Stat, c("Npars", "loglik", "AIC", "BIC"))

  # Goodness of fit values are internally consistent. AIC = 2 * Npars -
  # 2 * loglik, up to the rounding to integers applied by dim_summary(), which
  # can shift the identity by at most 1.5.
  for (model in c("uni", "texttype")) {
    ic <- stats::setNames(gof[[model]], gof$Stat)
    expect_true(all(is.finite(ic)))
    expect_lt(abs(ic[["AIC"]] - (2 * ic[["Npars"]] - 2 * ic[["loglik"]])), 2)
    # BIC penalises more heavily than AIC for any sample larger than 7 cases
    expect_gt(ic[["BIC"]], ic[["AIC"]])
  }

  # The multidimensional model estimates more parameters than the reference
  expect_gt(gof$texttype[gof$Stat == "Npars"], gof$uni[gof$Stat == "Npars"])

  # One row and column per texttype, named after the texttype levels
  corvar <- result$summary$`Cor-Var texttype`
  texttypes <- as.character(sort(unique(ex3$vars$texttype[ex3$vars$mixed])))
  expect_equal(dim(corvar), c(length(texttypes), length(texttypes)))
  expect_equal(rownames(corvar), texttypes)
  expect_equal(colnames(corvar), texttypes)

  # dim_summary() puts variances on the diagonal and correlations off it
  expect_true(all(diag(corvar) > 0))
  expect_true(all(corvar[lower.tri(corvar)] >= -1))
  expect_true(all(corvar[lower.tri(corvar)] <= 1))

  # All files created
  expect_true(file.exists(file.path(path, "dimensionality.rds")))
  expect_true(file.exists(file.path(path, "dimensionality.xlsx")))

  # The saved results are the same object that is returned
  expect_identical(readRDS(file.path(path, "dimensionality.rds")), result)

  # Written table can be read back in and contains the expected sheets
  expect_equal(names(Import(path, "dimensionality.xlsx")),
               names(result$summary))

  # Same structure as the precomputed results. The fixture was estimated with
  # maxiter = 10000 and snodes = 5, so its loglik, AIC and BIC cannot be
  # reproduced with the settings used here. Npars is determined by the model
  # specification rather than by the estimation path and is compared by value.
  #
  # Repeated runs on one machine give identical summaries, so a value-based
  # comparison would be possible against a regenerated fixture. Whether that
  # holds across platforms and BLAS builds has not been tested.
  fixture <- readRDS(test_path("fixtures/ex3/results/dimensionality.rds"))
  gof_fix <- fixture$summary$`Goodness of fit`
  expect_equal(names(result$analysis), names(fixture$analysis))
  expect_equal(names(result$summary), names(fixture$summary))
  expect_equal(names(gof), names(gof_fix))
  expect_equal(gof[gof$Stat == "Npars", ], gof_fix[gof_fix$Stat == "Npars", ])

})


test_that("conduct_dim_analysis() produces a unidimensional reference model", {

  data(ex3)

  # dim = NULL estimates the reference model only
  dim_results <- conduct_dim_analysis(
    resp = ex3$resp,
    vars = ex3$vars,
    select = "mixed",
    scoring = "scoring",
    dim = NULL,
    valid = "valid",
    irtmodel = "PCM2",
    mvs = c(OM = -97, NV = -95, NR = -94, MBD = -56, LT = -54),
    maxiter = 500,
    snodes = 1000,
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  expect_equal(names(dim_results), "uni")

  smry <- dim_summary(dim_results, save = FALSE)

  # Without a dimension variable there is no second model to compare against
  expect_equal(names(smry), c("Cor-Var uni", "Goodness of fit"))
  expect_equal(names(smry$`Goodness of fit`), c("Stat", "uni"))
  expect_equal(smry$`Goodness of fit`$Stat, c("Npars", "loglik", "AIC", "BIC"))
  expect_true(all(is.finite(smry$`Goodness of fit`$uni)))

  # A single dimension leaves a 1 x 1 matrix holding that dimension's variance
  expect_equal(dim(smry$`Cor-Var uni`), c(1L, 1L))
  expect_gt(smry$`Cor-Var uni`[1, 1], 0)

})


test_that("dim_summary() names the group and print_dim_summary() shows it", {

  # Temporary output directory
  path <- withr::local_tempdir()

  # Neither function estimates anything, so both can be driven from the
  # precomputed models and this block costs no TAM run.
  fixture <- readRDS(test_path("fixtures/ex3/results/dimensionality.rds"))

  smry <- dim_summary(
    fixture$analysis,
    save = TRUE,
    name_group = "easy",
    path = path,
    overwrite = TRUE
  )

  # create_name() appends the group to the file name
  expect_true(file.exists(file.path(path, "dimensionality_easy.xlsx")))
  expect_false(file.exists(file.path(path, "dimensionality.xlsx")))
  expect_equal(names(Import(path, "dimensionality_easy.xlsx")), names(smry))

  # print_dim_summary() announces the results and prints every sheet, so both
  # the message and the printed output have to be captured. It emits a trailing
  # blank message as well, which capture_messages() absorbs along with the
  # header instead of letting it surface as suite noise.
  msgs <- testthat::capture_messages(
    expect_output(print_dim_summary(smry), "Goodness of fit")
  )
  expect_true(any(grepl("RESULTS", msgs)))

})
