

test_that("dichotomous_scoring() works", {

  data(ex1)
  resp <- ex1$resp[ex1$resp$valid, c("ID_t", ex1$vars$item[ex1$vars$raw])]
  vars <- ex1$vars[ex1$vars$raw, ]

  resp_new <- tryCatch({
    dichotomous_scoring(resp = resp, vars = vars,
                        old_names = vars$item, correct = "correct")
  }, error = \(e) e, warning = \(w) w)

  expect_contains(colnames(resp_new), paste0(vars$item, "_c"))
  expect_equal(sum(resp_new[, paste0(vars$item, "_c")] -
               ex1$resp[ex1$resp$valid, paste0(vars$item, "_c")]), 0L)

})



test_that("pc_scoring() works", {

  # Simulated data
  data(ex1)
  resp <- ex1$resp#[ex1$resp$valid, c("ID_t", ex1$vars$item[!ex1$vars$raw])]
  resp$grk10014_c[resp$grk10014_c < 0] <- 0
  resp$grk10015_c[resp$grk10015_c < 0] <- 0
  vars <- ex1$vars[!ex1$vars$raw, ]

  # Define polytomous items
  poly_items <- list(
    "grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"), # with missing
    "grk1001s_c" = c("grk10014_c", "grk10015_c")               # without missing
  )

  # Without imputation
  expect_no_error(
    pc_scoring(resp = resp, poly_items = poly_items,
               impute = FALSE, warn = FALSE)
  )
  expect_warning(
    pc_scoring(resp = resp, poly_items = poly_items,
               impute = FALSE, warn = TRUE)
  )
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items,
               impute = FALSE, warn = FALSE)$grk1000s_c[26],
    -95)
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items,
               impute = FALSE, warn = FALSE)$grk1000s_c[43],
    -97)
  expect_true(
    all(pc_scoring(resp = resp, poly_items = poly_items,
                   impute = FALSE, warn = FALSE)$mag9100s_c[1:2] == 3)
  )
  expect_true(
    all(pc_scoring(resp = resp, poly_items = poly_items,
                   impute = FALSE, warn = FALSE)$mag9100s_c[c(6, 22, 34)] == 1)
  )

  # With imputation (requires MASS via TAM)
  skip_if_not_installed("MASS")
  expect_no_error(
    pc_scoring(resp = resp, poly_items = poly_items,
               vars = vars, select = "dich", mvs = -99:-1,
               impute = TRUE, warn = TRUE, verbose = FALSE)
  )
  expect_no_error(
    pc_scoring(resp = resp, poly_items = poly_items,
               vars = vars, select = "dich",
               verbose = FALSE, impute = TRUE, warn = FALSE)
  )
  expect_warning(
    pc_scoring(resp = resp, poly_items = poly_items,
               vars = vars, select = "dich",
               verbose = FALSE, impute = TRUE, warn = TRUE)
  )
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items,
               vars = vars, select = "dich",
               verbose = FALSE, impute = TRUE, warn = FALSE)$grk1000s_c[26],
    0
  )
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items,
               vars = vars, select = "dich",
               verbose = FALSE, impute = TRUE, warn = FALSE)$grk1000s_c[367],
    -97
  )


})


# --- pc_scoring() validation tests ---

test_that("pc_scoring() validates resp", {
  expect_error(
    pc_scoring(resp = "not a df", poly_items = list(a = "x"),
               impute = FALSE, warn = FALSE),
    "must be a data.frame"
  )
  expect_error(
    pc_scoring(resp = matrix(1:4, 2, 2), poly_items = list(a = "x"),
               impute = FALSE, warn = FALSE),
    "must be a data.frame"
  )
})

test_that("pc_scoring() validates poly_items", {
  data(ex1)
  resp <- ex1

  # Not a list
  expect_error(
    pc_scoring(resp = resp, poly_items = "x", impute = FALSE, warn = FALSE),
    "must be a list"
  )
  # Empty list
  expect_error(
    pc_scoring(resp = resp, poly_items = list(), impute = FALSE, warn = FALSE),
    "at least one element"
  )
  # Unnamed list
  expect_error(
    pc_scoring(resp = resp, poly_items = list(c("grk10001_c")),
               impute = FALSE, warn = FALSE),
    "must be named"
  )
  # Non-character element
  expect_error(
    pc_scoring(resp = resp, poly_items = list(a = 1:3),
               impute = FALSE, warn = FALSE),
    "character vector"
  )
})

test_that("pc_scoring() validates logical parameters", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  expect_error(
    pc_scoring(resp = resp, poly_items = pi, impute = "yes", warn = FALSE),
    "'impute' must be TRUE or FALSE"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, impute = FALSE, warn = NA),
    "'warn' must be TRUE or FALSE"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, impute = FALSE, warn = FALSE,
               save = 1),
    "'save' must be TRUE or FALSE"
  )
})

test_that("pc_scoring() validates threshold", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  expect_error(
    pc_scoring(resp = resp, poly_items = pi, threshold = "high",
               impute = FALSE, warn = FALSE),
    "threshold"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, threshold = 1.5,
               impute = FALSE, warn = FALSE),
    "threshold"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, threshold = -0.1,
               impute = FALSE, warn = FALSE),
    "threshold"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, threshold = NA_real_,
               impute = FALSE, warn = FALSE),
    "threshold"
  )
})

test_that("pc_scoring() validates mvs", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  expect_error(
    pc_scoring(resp = resp, poly_items = pi, mvs = "missing",
               impute = FALSE, warn = FALSE),
    "mvs.*numeric vector"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, mvs = character(0),
               impute = FALSE, warn = FALSE),
    "mvs.*numeric vector"
  )
})

test_that("pc_scoring() validates missing_by_design", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  expect_error(
    pc_scoring(resp = resp, poly_items = pi, missing_by_design = "x",
               impute = FALSE, warn = FALSE),
    "missing_by_design.*single numeric"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, missing_by_design = NA_real_,
               impute = FALSE, warn = FALSE),
    "missing_by_design.*single numeric"
  )
})

test_that("pc_scoring() validates string parameters", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  expect_error(
    pc_scoring(resp = resp, poly_items = pi, path_results = 123,
               impute = FALSE, warn = FALSE),
    "path_results.*character string"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, path_table = NULL,
               impute = FALSE, warn = FALSE),
    "path_table.*character string"
  )
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, select = 42,
               impute = FALSE, warn = FALSE),
    "select.*character string"
  )
})

test_that("pc_scoring() validates imputation prerequisites early", {
  data(ex1)
  resp <- ex1
  pi <- list("grk1000s_c" = c("grk10001_c", "grk10002_c", "grk10003_c"))

  # impute=TRUE but no vars
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, impute = TRUE,
               vars = NULL, select = "dich", warn = FALSE, verbose = FALSE),
    "impute.*TRUE.*vars"
  )
  # impute=TRUE but no select
  expect_error(
    pc_scoring(resp = resp, poly_items = pi, impute = TRUE,
               vars = ex1, select = NULL, warn = FALSE, verbose = FALSE),
    "impute.*TRUE.*select"
  )
})


