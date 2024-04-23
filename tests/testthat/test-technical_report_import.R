
test_that("import single sheet works", {

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


test_that("import all sheets works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "mv_item.xlsx")
  }, silent = TRUE)
  expect_false(inherits(tbl, "try_error"))
  expect_length(tbl, 2)
  expect_contains(names(tbl), c("list", "summary"))
  expect_equal(dim(tbl$list), c(15, 9))

})


test_that("import using regular expression works", {

  tbl <- try(
    Import(path = test_path("fixtures/ex1/tables"),
           regexp = "^mv_",
           rename = c("mv_item" = "mvi", "mv_person" = "mvp"))
  , silent = TRUE)
  expect_false(inherits(tbl, "try_error"))
  expect_length(tbl, 2)
  expect_contains(names(tbl), c("mvi", "mvp"))

})

