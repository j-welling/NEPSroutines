
test_that("conduct_dim_analysis() runs without error", {

  data(ex3)

  result <- try({
    conduct_dim_analysis(
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
      verbose = FALSE,
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("uni" %in% names(result))
  expect_true("texttype" %in% names(result))

})


test_that("conduct_dim_analysis() produces unidimensional reference model", {

  data(ex3)

  result <- conduct_dim_analysis(
    resp = ex3$resp,
    vars = ex3$vars,
    select = "mixed",
    scoring = "scoring",
    dim = NULL,  # No multidimensional model
    valid = "valid",
    irtmodel = "PCM2",
    mvs = c(OM = -97, NV = -95, NR = -94, MBD = -56, LT = -54),
    maxiter = 500,
    snodes = 1000,
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  expect_true("uni" %in% names(result))
  expect_equal(length(result), 1)

})


test_that("dim_summary() produces valid output", {

  data(ex3)

  dim_results <- conduct_dim_analysis(
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
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  summary <- dim_summary(dim_results, save = FALSE)

  expect_true(is.list(summary))
  expect_true("Cor-Var uni" %in% names(summary))
  expect_true("Cor-Var texttype" %in% names(summary))
  expect_true("Goodness of fit" %in% names(summary))

  # Check goodness of fit table structure
  gof <- summary$`Goodness of fit`
  expect_true("Stat" %in% names(gof))
  expect_true("uni" %in% names(gof))
  expect_true("texttype" %in% names(gof))
  expect_true("Npars" %in% gof$Stat)
  expect_true("loglik" %in% gof$Stat)
  expect_true("AIC" %in% gof$Stat)
  expect_true("BIC" %in% gof$Stat)

})


test_that("dim_analysis() produces expected structure", {

  data(ex3)
  path <- withr::local_tempdir()

  result <- try({
    dim_analysis(
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
      save = FALSE,
      return = TRUE,
      verbose = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("analysis" %in% names(result))
  expect_true("summary" %in% names(result))

})


test_that("dim_analysis() matches fixture structure", {

  data(ex3)
  path <- withr::local_tempdir()

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

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex3/results/dimensionality.rds"))

  # Compare structure
  expect_equal(names(result$analysis), names(fixture$analysis))
  expect_equal(names(result$summary), names(fixture$summary))

  # Compare goodness of fit structure
  expect_equal(names(result$summary$`Goodness of fit`),
               names(fixture$summary$`Goodness of fit`))

})


test_that("dim_analysis() saves files correctly", {

  data(ex3)
  path <- withr::local_tempdir()

  dim_analysis(
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
    return = FALSE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(file.exists(paste0(path, "/dimensionality.rds")))
  expect_true(file.exists(paste0(path, "/dimensionality.xlsx")))

})


test_that("dim_analysis() works with unidimensional model only", {

  data(ex3)

  # Test unidimensional model only (dim = NULL)
  result <- try({
    dim_analysis(
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
      print = FALSE,
      save = FALSE,
      return = TRUE,
      verbose = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("uni" %in% names(result$analysis))
  expect_equal(length(result$analysis), 1)

})


test_that("dim_summary() variance-covariance matrix is symmetric", {

  data(ex3)

  dim_results <- conduct_dim_analysis(
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
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  summary <- dim_summary(dim_results, save = FALSE)

  # Multidimensional model should have symmetric correlation matrix
  if (ncol(summary$`Cor-Var texttype`) > 1) {
    corvar <- summary$`Cor-Var texttype`
    # Check symmetry (off-diagonal elements)
    expect_equal(corvar[1, 2], corvar[2, 1])
  }

})
