
test_that("Import() imports a single sheet", {

  # no filename and no regexp
  expect_error(Import(path = test_path("fixtures/ex1/tables")))

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "mv_item.xlsx",
           sheet = "list")
  }, silent = TRUE)
  expect_false(inherits(tbl, "try-error"))
  expect_true(is.data.frame(tbl))
  expect_equal(dim(tbl), c(15, 9))

})


test_that("Import() reports missing folders and files clearly", {

  missing_path <- file.path(tempdir(), "nepsroutines-does-not-exist")
  expect_error(
    Import(path = missing_path, filename = "mv_item.xlsx"),
    "The folder .* does not exist"
  )

  missing_file_path <- normalizePath(
    file.path(test_path("fixtures/ex1/tables"), "missing.xlsx"),
    winslash = "/",
    mustWork = FALSE
  )
  missing_file_path_regexp <- gsub(
    "([][{}()+*^$|\\\\.?])",
    "\\\\\\1",
    missing_file_path
  )
  expect_error(
    Import(path = test_path("fixtures/ex1/tables"), filename = "missing.xlsx"),
    paste0("Cannot find Excel file.*", missing_file_path_regexp)
  )

})


test_that("Import() imports all sheets", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "mv_item.xlsx")
  }, silent = TRUE)
  expect_false(inherits(tbl, "try_error"))
  expect_length(tbl, 2)
  expect_contains(names(tbl), c("list", "summary"))
  expect_equal(dim(tbl$list), c(15, 9))

})


test_that("Import() imports using regular expression", {

  tbl <- try(
    Import(path = test_path("fixtures/ex1/tables"),
           regexp = "^mv_",
           rename = c("mv_item" = "mvi", "mv_person" = "mvp"))
  , silent = TRUE)
  expect_false(inherits(tbl, "try_error"))
  expect_length(tbl, 2)
  expect_contains(names(tbl), c("mvi", "mvp"))

})

