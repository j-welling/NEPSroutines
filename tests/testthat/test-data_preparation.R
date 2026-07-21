
test_that("dichotomous_scoring() works", {

  data(ex1)
  resp <- ex1$resp[ex1$resp$valid, c("ID_t", ex1$vars$item[ex1$vars$raw])]
  vars <- ex1$vars[ex1$vars$raw, ]

  scored <- dichotomous_scoring(
    resp = resp,
    vars = vars,
    old_names = vars$item,
    correct = "correct"
  )

  scored_names <- paste0(vars$item, "_c")
  expect_contains(colnames(scored), scored_names)
  expect_equal(
    scored[, scored_names],
    ex1$resp[ex1$resp$valid, scored_names],
    ignore_attr = TRUE
  )

  custom_names <- paste0(vars$item[1:2], "_custom")
  custom <- dichotomous_scoring(
    resp = resp,
    vars = vars,
    old_names = vars$item[1:2],
    new_names = custom_names,
    correct = "correct"
  )

  expect_contains(colnames(custom), custom_names)
  expect_equal(
    custom[, custom_names],
    ex1$resp[ex1$resp$valid, paste0(vars$item[1:2], "_c")],
    ignore_attr = TRUE
  )

})


test_that("dichotomous_scoring() preserves missing codes and handles non-numeric raw responses", {

  resp <- data.frame(
    ID_t = 1:4,
    item_numeric = c(1, 2, -97, 3),
    item_factor = factor(c("A", "B", "-95", "A")),
    item_character = c("x", "z", "-94", "y")
  )
  vars <- data.frame(
    item = c("item_numeric", "item_factor", "item_character"),
    correct = c("1;2", "A", "x;y")
  )

  scored <- dichotomous_scoring(
    resp = resp,
    vars = vars,
    old_names = vars$item,
    correct = "correct"
  )

  expect_equal(scored$item_numeric_c, c(1, 1, -97, 0))
  expect_equal(scored$item_factor_c, c(1, 0, -95, 1))
  expect_equal(scored$item_character_c, c(1, 0, -94, 1))

})


test_that("pc_scoring() works without imputation", {

  data(ex1)

  resp <- ex1$resp
  resp$grk10014_c[resp$grk10014_c < 0] <- 0
  resp$grk10015_c[resp$grk10015_c < 0] <- 0
  poly_items <- list(
    grk1000s_c = c("grk10001_c", "grk10002_c", "grk10003_c"),
    grk1001s_c = c("grk10014_c", "grk10015_c")
  )
  results_dir <- withr::local_tempdir()
  tables_dir <- withr::local_tempdir()

  scored <- pc_scoring(
    resp = resp,
    poly_items = poly_items,
    impute = FALSE,
    warn = FALSE,
    save = FALSE,
    path_results = results_dir,
    path_table = tables_dir
  )

  expect_contains(colnames(scored), names(poly_items))
  expect_equal(scored$grk1000s_c[26], -95)
  expect_equal(scored$grk1000s_c[43], -97)
  expect_true(all(scored$mag9100s_c[1:2] == 3))
  expect_true(all(scored$mag9100s_c[c(6, 22, 34)] == 1))
  expect_false(file.exists(file.path(results_dir, "pc_subitems_mv_indicators.rds")))
  expect_false(file.exists(file.path(results_dir, "pc_subitems_imputations.rds")))
  expect_false(file.exists(file.path(tables_dir, "summary_pc_subitems_mv_indicators.xlsx")))

})


test_that("pc_scoring() works with imputation", {

  skip_if_not_installed("MASS")
  skip_if_not_installed("openxlsx")

  data(ex1)

  resp <- ex1$resp
  resp$grk10014_c[resp$grk10014_c < 0] <- 0
  resp$grk10015_c[resp$grk10015_c < 0] <- 0
  vars <- ex1$vars[!ex1$vars$raw, ]
  poly_items <- list(
    grk1000s_c = c("grk10001_c", "grk10002_c", "grk10003_c"),
    grk1001s_c = c("grk10014_c", "grk10015_c")
  )
  results_dir <- withr::local_tempdir()
  tables_dir <- withr::local_tempdir()

  output <- capture.output({
    scored <- suppressMessages(pc_scoring(
      resp = resp,
      poly_items = poly_items,
      vars = vars,
      select = "dich",
      mvs = -99:-1,
      impute = TRUE,
      warn = FALSE,
      verbose = FALSE,
      save = TRUE,
      path_results = results_dir,
      path_table = tables_dir,
      overwrite = TRUE
    ))
  })

  expect_type(output, "character")
  expect_equal(scored$grk1000s_c[26], 0)
  expect_equal(scored$grk1000s_c[367], -97)

  indicator_file <- file.path(results_dir, "pc_subitems_mv_indicators.rds")
  imputation_file <- file.path(results_dir, "pc_subitems_imputations.rds")
  summary_file <- file.path(tables_dir, "summary_pc_subitems_mv_indicators.xlsx")

  expect_true(file.exists(indicator_file))
  expect_true(file.exists(imputation_file))
  expect_true(file.exists(summary_file))

  indicators <- readRDS(indicator_file)
  imputations <- readRDS(imputation_file)

  expect_named(
    indicators,
    c(
      "indicators", "tab_sumMV", "tab_impMV", "summary_items_impMV",
      "desc_items_impMV", "summary_cases_impMV"
    )
  )
  expect_named(
    imputations,
    c("fit", "pred_resp", "error_rates", "mean_error_rates", "resp_imp")
  )
  expect_equal(indicators$summary_items_impMV$Item, names(poly_items))
  expect_equal(imputations$resp_imp$grk10001_c[26], 0)

})

