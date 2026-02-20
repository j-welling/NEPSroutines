library(testthat)
source("tests/testthat/helpers.R")

################################################################################
#################### IRT ANLYSIS FOR DICHOTOMOUS ITEMS #########################
################################################################################

tables_to_test <- list(
  summary = "Item",
  model_fit = NULL
)

test_that("irt_analysis() works for dichotomous items", {

  # Load data and previous results
  data(b129)
  result_dich_old <- readRDS("tests/testthat/fixtures/results_prev/irt_dich_all.rds")

  # Run function and load new results
  irt_analysis(
    resp = resp, vars = vars, select = "dich", valid = "valid", mvs = mvs,
    save = TRUE, overwrite = TRUE, print = FALSE,
    path_results = "tests/testthat/fixtures/results_curr/",
    path_table = "tests/testthat/fixtures/tables_curr/",
    name_group = "all"
  )
  result_dich_new <- readRDS("tests/testthat/fixtures/results_curr/irt_dich_all.rds")

  # Compare two version, separately for each result table
  for (tbl in names(tables_to_test)) {
    res <- compare_table_objects(
      tab_new = result_dich_new[[tbl]],
      tab_old = result_dich_old[[tbl]],
      analysis_type = "irt_dich",
      table_name = tbl,
      key_column = tables_to_test[[tbl]]
    )

    # Report when detailed comparison has been performed
    expect_true(
      res$status,
      info = paste0(
        "\n❌ Tables differ in ", tbl, ":\n",
        paste(res$summary, collapse = "\n"),
        if (!is.null(res$differences)) {
          paste0(
            "\n\nDetailed differences:\n",
            paste(capture.output(print(res$differences)), collapse = "\n")
          )
        } else {
          ""
        }
      )
    )
  }
})


################################################################################
#################### IRT ANLYSIS FOR POLYTOMOUS ITEMS ##########################
################################################################################

tables_to_test <- list(
  summary = "Item",
  model_fit = NULL,
  steps = NULL
)

test_that("irt_analysis() works for polytomous items", {

  # Load data and previous results
  data(b129)
  result_poly_old <- readRDS("tests/testthat/fixtures/results_prev/irt_poly_all.rds")

  # Run function and load new results
  irt_analysis(resp = resp, vars = vars, select = "poly", valid = "valid", mvs = mvs,
               scoring = "scoring",
               save = TRUE, overwrite = TRUE, print = FALSE,
               path_results = "tests/testthat/fixtures/results_curr/",
               name_group = "all")
  result_poly_new <- readRDS("tests/testthat/fixtures/results_curr/irt_poly_all.rds")

  # Compare two version, separately for each result table
  for (tbl in names(tables_to_test)) {
    res <- compare_table_objects(
      tab_new = result_poly_new[[tbl]],
      tab_old = result_poly_old[[tbl]],
      analysis_type = "irt_poly",
      table_name = tbl,
      key_column = tables_to_test[[tbl]]
    )

    # Report when detailed comparison has been performed
    expect_true(
      res$status,
      info = paste0(
        "\n❌ Tables differ in ", tbl, ":\n",
        paste(res$summary, collapse = "\n"),
        if (!is.null(res$differences)) {
          paste0(
            "\n\nDetailed differences:\n",
            paste(capture.output(print(res$differences)), collapse = "\n")
          )
        } else {
          ""
        }
      )
    )
  }
})


