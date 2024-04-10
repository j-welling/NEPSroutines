
test_that("create table with item properties works", {

  skip_if_not_installed("flextable")
  data("ex2")

  tbl <- try({
    TblItemProps(vars = ex2$vars, select = "mixed", prop = "type",
                 propname = "Response formats")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "d2d32dd355e6e8b6c57d008b24e159c5")

})


test_that("create table with item facets works", {

  skip_if_not_installed("flextable")
  data("ex2")

  tbl <- try({
    TblItemFacets(vars = ex2$vars, select = "mixed",
                  facets = c("Content area" = "content"),
                  position = "pos")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "c12fc5b0b40bf323562e49f1e9e8faa7")

})


test_that("create table for missing values by item works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex1/tables"), "mv_item.xlsx")
    TblMvi(tab, footnote = "Nothing of note happend.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "61e85e57564ff9e836a01a23be197f8b")

})


test_that("create table for item parameters works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex2/tables"), "irt_poly.xlsx")
    TblPars(tab, footnote = "Nothing of note happend.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "8c5cbfb62a25fe833b0227eb27d897ad")

})


test_that("create table for step parameters works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex2/tables"), "irt_poly.xlsx")
    TblSteps(tab, footnote = "Nothing of note happend.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "2813e6300c376a597ca9e7bf81496d69")

})


test_that("create table for facet correlations works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex2/tables"), "dimensionality.xlsx")
    TblDim(tab, model = "content", footnote = "Nothing of note happend.",
           rownames = c("Change", "Data", "Units", "Space"))
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "d1c9d30f1e4d628a7c5e2aef0c08cb4b")

})


test_that("create table for dif works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex1/tables"), "dif_dich_TR.xlsx")
    TblDif(tab, footnote = "Nothing to note.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "8e800d5b05f4bd7f839c5b4b559afa87")

})


test_that("create table for dif model fit works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex1/tables"), "dif_dich_TR.xlsx")
    TblDifFit(tab, footnote = "Nothing to note.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "e33303258c01b59166e5ed3bd83af00a")

})



test_that("create table with analysis code works", {

  skip_if_not_installed("flextable")
  data(ex3)
  tbl <- try({
    TblCode(vars = ex3$vars, select ="mixed", tbl = FALSE)
  })
  expect_false(inherits(tbl, "try-error"))
  expect_equal(rlang::hash(tbl), "ab05c01324651334464a80f141f61396")

})

