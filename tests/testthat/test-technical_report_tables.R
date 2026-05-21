
test_that("Tbl() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  obj <- data.frame(
    value = c(1.234, 5.678),
    label = c("first", "second")
  )

  tbl <- Tbl(
    obj,
    footnote = "Generated for a test.",
    lbl = c("Value", "Label"),
    width = 0.5,
    digits = c(1, 2),
    align = c("right", "left"),
    align_head = "center"
  )

  expect_s3_class(tbl, "flextable")
  expect_equal(tbl$body$dataset, obj)
  expect_equal(tbl$header$dataset, data.frame(value = "value", label = "label"))
  expect_equal(nrow(tbl$footer$dataset), 1)
  expect_equal(tbl$properties$align, "left")
  expect_equal(unname(tbl$body$styles$pars$text.align$data[1, ]), c("right", "left"))

})


test_that("TblItemProps() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  data("ex2")

  tbl <- TblItemProps(
    vars = ex2$vars,
    select = c("Number of items" = "mixed"),
    prop = "type",
    propname = "Response formats",
    width = 0.5
  )

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c("Response formats", "Number of items"))
  expect_equal(tbl$body$dataset[["Response formats"]], c(
    "Simple multiple-choice items",
    "Complex multiple-choice items",
    "Total number of items"
  ))
  expect_equal(tbl$body$dataset[["Number of items"]], c("13", "4", "17"))

})


test_that("TblItemFacets() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  data("ex2")

  tbl <- TblItemFacets(
    vars = ex2$vars,
    select = "mixed",
    facets = c("Content area" = "content", "Response format" = "type"),
    position = "pos",
    footnote = "MC = simple multiple-choice",
    width = 0.5
  )

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "Pos.", "Item", "Content area", "Response format"
  ))
  expect_equal(tbl$body$dataset[["Pos."]][1:3], c(1, 2, 3))
  expect_equal(tbl$body$dataset[["Item"]][1:3], c(
    "mag120001_c", "mag120002_c", "mag120003_c"
  ))
  expect_equal(tbl$body$dataset[["Content area"]][1:3], c(
    "Change and relationship",
    "Change and relationship",
    "Data and chance"
  ))
  expect_equal(tbl$body$dataset[["Response format"]][3],
               "Complex multiple-choice items")
  expect_equal(nrow(tbl$footer$dataset), 1)

})


test_that("TblMvi() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "mv_item.xlsx")
  tbl <- TblMvi(tab, excl = "", sort = "N_valid")

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "Nr.", "Item", "Pos.", "Total", "N", "OM", "NV", "NR", "ALL"
  ))
  expect_equal(tbl$body$dataset[["Nr."]][1:3], c(1, 2, 3))
  expect_equal(tbl$body$dataset[["Item"]][1:3], c(
    "grk10015_c", "grk10014_c", "grk10013_c"
  ))
  expect_equal(tbl$body$dataset[["N"]][1:3], c(345, 552, 679))
  expect_equal(tbl$body$dataset[["ALL"]][1:3], c(65.5, 44.8, 32.1))
  expect_equal(TblMVI(tab)$body$dataset, TblMvi(tab)$body$dataset)

})


test_that("TblPars() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex2", "tables"), "irt_poly.xlsx")
  tbl <- TblPars(tab, footnote = "Nothing of note happened.")

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "Nr.", "Item", "N", "Percentage\n correct", "Difficulty", "SE",
    "WMNSQ", "t", "rit", "aQ3", "Discr."
  ))
  expect_equal(tbl$body$dataset[["Nr."]][1:3], c("1", "2", "3"))
  expect_equal(tbl$body$dataset[["Item"]][1:3], c(
    "mag120001_c", "mag120002_c", "mag120003_c"
  ))
  expect_equal(tbl$body$dataset[["N"]][1:3], c(1440, 1431, 1430))
  expect_equal(tbl$body$dataset[["Difficulty"]][1:3], c(-1.96, -1.71, -1.56))
  expect_true(is.na(tbl$body$dataset[["Percentage\n correct"]][3]))

})


test_that("TblSteps() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex2", "tables"), "irt_poly.xlsx")
  tbl <- TblSteps(tab, footnote = "Nothing of note happened.")

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c("Item", "Step 1", "Step 2", "Step 3"))
  expect_equal(tbl$body$dataset[["Item"]][1:4], c(
    "mag120003_c", "mag120007_c", "mag120014_c", "mag120016_c"
  ))
  expect_equal(tbl$body$dataset[["Step 1"]][1:4], c(
    "1.67 (0.07)", "0.36 (0.07)", "0.84 (0.08)", "1.26 (0.08)"
  ))
  expect_equal(tbl$body$dataset[["Step 2"]][1:4], c(
    "-1.56 (0.07)", "-0.36", "-0.84", "-0.96 (0.09)"
  ))
  expect_equal(tbl$body$dataset[["Step 3"]][1:4], c("-0.11", NA, NA, "-0.30"))

})


test_that("TblDim() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex2", "tables"), "dimensionality.xlsx")
  tbl <- TblDim(
    tab,
    model = "content",
    rownames = c("Change", "Data", "Units", "Space"),
    footnote = "Nothing of note happened."
  )

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "Dimension", "Dim 1", "Dim 2", "Dim 3", "Dim 4"
  ))
  expect_equal(tbl$body$dataset[["Dimension"]], c(
    "Dim 1: Change", "Dim 2: Data", "Dim 3: Units", "Dim 4: Space"
  ))
  expect_equal(tbl$body$dataset[["Dim 1"]], c("2.04", ".67", ".74", ".75"))
  expect_equal(tbl$body$dataset[["Dim 4"]], c("", "", "", "2.39"))

})


test_that("TblDif() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")
  tbl <- TblDif(
    tab,
    footnote = "Nothing to note.",
    colnames2 = c(
      "mig.1-3" = "without vs. missing",
      "mig.2-3" = "with vs. missing"
    )
  )

  expect_s3_class(tbl, "flextable")
  expect_equal(nrow(tbl$header$dataset), 2)
  expect_equal(
    tbl$header$dataset[1, ],
    setNames(
      data.frame("item", "sex.0-1", "mig.1-2", "mig.1-3", "mig.2-3"),
      c("item", "sex.0-1", "mig.1-2", "mig.1-3", "mig.2-3")
    )
  )
  expect_equal(unname(unlist(tbl$header$dataset[2, ])), c(
    "",
    "men vs. women",
    "without vs. with",
    "without vs. missing",
    "with vs. missing"
  ))
  expect_equal(tbl$body$dataset[["item"]][16:17], c(
    "Main effect\n (DIF model)",
    "Main effect\n (Main effect model)"
  ))
  expect_equal(tbl$body$dataset[["sex.0-1"]][1], "-0.01 (-0.01)")
  expect_equal(TblDIF(tab)$body$dataset, TblDif(tab)$body$dataset)

})


test_that("TblDifFit() works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")
  tbl <- TblDifFit(tab, excl = "sex", label = c("mig" = "Migrant background"))

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "DIF variable", "Model", "N", "Deviance", "Number of parameters",
    "AIC", "BIC"
  ))
  expect_equal(tbl$body$dataset[["DIF variable"]], c(
    "Migrant background", "Migrant background"
  ))
  expect_equal(tbl$body$dataset[["Model"]], c("Main effect", "DIF"))
  expect_equal(tbl$body$dataset[["AIC"]], c(13633, 13662))
  expect_equal(tbl$body$dataset[["BIC"]], c(13721, 13888))
  expect_equal(TblDIFFit(tab)$body$dataset, TblDifFit(tab)$body$dataset)

})


test_that("TblCode() works", {

  data("ex3")

  code <- TblCode(vars = ex3$vars, select = "mixed", tbl = FALSE)

  expect_type(code, "character")
  expect_equal(code[1:4], c(
    "# load packages",
    "library(rio)      # to import SPSS files",
    "library(doBy)  # to recode variables",
    "library(TAM)   # for IRT analyses"
  ))
  expect_true(any(grepl('^  "reg70001_c"', code)))
  expect_true(any(grepl("^# polytomous items$", code)))
  expect_true(any(grepl("TAM::tam.mml\\(resp = dat\\[, items\\], Q = Q", code)))
  expect_equal(tail(code, 1), "TAM::tam.wle(mod)")

})


