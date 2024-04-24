
test_that("create table with item properties works", {

  skip_if_not_installed("flextable")
  data("ex2")
  data("ex3")

  tbl <- try({
    TblItemProps(vars = ex2$vars, select = "mixed", prop = "type",
                 propname = "Response formats")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "d2d32dd355e6e8b6c57d008b24e159c5")

  tbl <- try({
    ex3$vars$mixed1 <- ex3$vars$mixed & ex3$vars$booklet1
    ex3$vars$mixed2 <- ex3$vars$mixed & ex3$vars$booklet2
    ex3$vars$mixed3 <- ex3$vars$mixed & ex3$vars$booklet3
    TblItemProps(vars = ex3$vars,
                 select = c("Booklet 1" = "mixed1", "Booklet 2" = "mixed2",
                            "Booklet 3" = "mixed3"),
                 prop = "type",
                 propname = "Response formats")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "9967500c8157d9966882dc8f738be307")

})


test_that("create table with item facets works", {

  skip_if_not_installed("flextable")
  data("ex2")

  tbl <- try({
    TblItemFacets(vars = ex2$vars, select = "mixed",
                  facets = c("Content area" = "content"))
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "48aeb0e742ee85d4c77ea1408ae507cf")

  tbl <- try({
    TblItemFacets(vars = ex2$vars, select = "mixed",
                  facets = c("Content area" = "content",
                             "Response formats" = "type"),
                  position = "pos",
                  footnote = "MC = simple multiple-choice, CMC = complex multiple-choice")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "a5b9d782fcfe198aac7439511572edd6")

})


test_that("create table for missing values by item works", {

  skip_if_not_installed("flextable")

  tab <- try({ Import(test_path("fixtures/ex1/tables"), "mv_item.xlsx") })
  tbl <- try({ TblMvi(tab, footnote = "Nothing of note happend.") })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "4761e5a32f8f19ea22417ccf0b27d706")

  tbl <- try({ TblMvi(tab, excl = "", sort = "N_valid") })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "fe0837b96ed2e2400c8ddc613507e174")

})


test_that("create table for item parameters works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex2/tables"), "irt_poly.xlsx")
    TblPars(tab, footnote = "Nothing of note happend.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "e7e7775f96f34e5d87660a9d7bd1e4be")

})


test_that("create table for step parameters works", {

  skip_if_not_installed("flextable")
  tbl <- try({
    tab <- Import(test_path("fixtures/ex2/tables"), "irt_poly.xlsx")
    TblSteps(tab, footnote = "Nothing of note happend.")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "fc421624eb958b6b3ca35b8b3c9290a6")

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
    TblDif(tab, footnote = "Nothing to note.",
           colnames2 = c("mig.1-3" = "without vs. missing",
                         "mig.2-3" = "with vs. missing"))
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "2700025522a8132ad26759bf2b4979db")

  tbl <- try({
    tab <- Import(test_path("fixtures/ex1/tables"), "dif_dich_TR.xlsx")
    TblDif(tab, excl = c("mig.1-3", "mig.2-3"), size = 12)
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "6c30e6c607a8ba71af8abbceab64a642")

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

  tbl <- try({
    tab <- Import(test_path("fixtures/ex1/tables"), "dif_dich_TR.xlsx")
    TblDifFit(tab, excl = "sex", label = c("mig" = "Migrant background"))
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))
  expect_equal(rlang::hash(tbl), "2a6f0c7c34fb090331110074937bf926")

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

