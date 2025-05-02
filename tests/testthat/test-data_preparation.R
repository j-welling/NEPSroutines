

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

  # With imputation
  x=  pc_scoring(resp = resp, poly_items = poly_items,
                 vars = vars, select = "dich", mvs = -99:-1,
                 impute = TRUE, warn = TRUE, verbose = FALSE)
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


