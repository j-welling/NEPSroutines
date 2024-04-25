

test_that("get item property works", {

  data("ex1")

  expect_equal(
    GetProp(ex1$vars, select = "dich", prop = "type", val = "MC", item = FALSE),
    15
  )
  expect_equal(
    GetProp(ex1$vars, select = "dich", prop = "type", val = "CMC", item = FALSE),
    0
  )
  expect_contains(
    GetProp(ex1$vars, select = "dich", prop = "pos", val = c(3:5), item = TRUE),
    c("grk10003_c", "grk10004_c", "grk10005_c")
  )

})


test_that("get missing values by item works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "mv_item.xlsx",
           sheet = "summary")
  })

  expect_equal(GetMvi(tbl, type = "OM", stat = "Mean"), "3")
  expect_equal(GetMvi(tbl, type = "ALL", stat = "Median", digits = 1), "7.4")

})


test_that("get missing values by person works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "mv_person.xlsx")
  })

  expect_equal(GetMvp(tbl, type = "OM", value = "=2"), "4")
  expect_equal(GetMvp(tbl, type = "NR", value = "<2", digits = 1), "57.5")

})


test_that("summarize item parameters works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "irt_dich.xlsx")
  })

  expect_equal(GetPars(tbl, type = "xsi", stat = mean), "-0.06")
  expect_equal(GetPars(tbl, type = "WMNSQ", stat = ">1"), "7")
  expect_equal(GetPars(tbl, type = "N_valid", stat = ">500&<700"), "2")
  expect_error(GetPars(tbl, type = "WMNSQ", stat = "+1"),
               "Unknown stat function.")
  expect_equal(
    GetPars(tbl, type = "t", stat = min, excl = "<.5", digits = 1),
    "0.5"
  )
  expect_error(
    GetPars(tbl, type = "t", stat = min, excl = "+.5", digits = 1),
    "Unknown stat function."
  )
  expect_equal(
    GetPars(tbl, type = "t", stat = min, excl = "<.5&>-1", digits = 1),
    "0.5"
  )
  expect_equal(
    GetPars(tbl, type = "N_valid", stat = "<600", item = TRUE),
    "grk10014_c, grk10015_c"
  )
  expect_equal(
    GetPars(tbl, type = "N_valid", stat = max, item = TRUE),
    "grk10003_c"
  )

})


test_that("summarize category thresholds works", {

  obj <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))

  expect_equal(GetCat(obj, stat = median), "-0.14")
  expect_equal(GetCat(obj, stat = min, item = TRUE), "grk10001_c")

})


test_that("get population variance works", {

  obj <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))

  expect_equal(GetVar(obj), "1.46")

})


test_that("get reliablity works", {

  obj <- readRDS(test_path("fixtures/ex1/results/irt_dich.rds"))

  expect_equal(GetRel(obj), "0.74")
  expect_equal(GetRel(obj, WLE = TRUE, digits = 1), "0.7")

})


test_that("summarize distractors works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "distractors_summary.xlsx")
  })

  expect_equal(GetDist(tbl, stat = mean, digits = 3), "-0.161")
  expect_equal(GetDist(tbl, stat = median, correct = TRUE), "0.38")
  expect_equal(GetDist(tbl,stat = max, item = TRUE), "grk10015_c")

})


test_that("get model fit works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "irt_dich.xlsx")
  })

  expect_equal(GetFit(tbl, type = "AIC"), "13,946")
  expect_equal(GetFit(tbl, type = "Npars", GPCM = TRUE), "30")

})


test_that("get model fit for dimensionality analyses works", {

  skip("File to large for automated tests.")

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "dimensionality.xlsx")
  })

  expect_equal(GetDimFit(tbl, model = "uni", type = "AIC"), "39,117")
  expect_equal(GetDimFit(tbl, model = "dim", type = "Npars"), "35")

})


test_that("summarize latent correlations works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex2/tables"),
           filename = "dimensionality.xlsx")
  })

  expect_equal(GetDim(tbl, model = "content", stat = median), ".72")
  expect_equal(GetDim(tbl, model = "uni", var = TRUE, digits = 1), "1.5")

})


test_that("summarize DIF works", {

  tbl1 <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "dif_dich_sex.xlsx")
  })
  tbl2 <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "dif_dich_mig.xlsx")
  })

  expect_equal(GetDif(tbl1, n = 1), "509")
  expect_equal(GetDif(tbl2, main = "std", group = "1-3"), "0.46")
  expect_equal(GetDif(tbl2, main = "ustd"), "0.76")
  expect_error(GetDif(tbl2, main = "unknown"),
               "Allowed values for argument main")
  expect_equal(GetDif(tbl2, main = "ustd", model = "main"), "0.73")
  expect_equal(GetDif(tbl1, dif = median), "0.07")
  expect_equal(GetDif(tbl2, dif = max), "0.60")
  expect_equal(GetDif(tbl2, dif = max, item = TRUE), "grk10005_c")
  expect_equal(GetDif(tbl2, dif = max, group = "1-2"), "0.46")
  expect_equal(GetDif(tbl1, dif = "<-0.1", signed = TRUE), "3")
  expect_equal(GetDif(tbl2, dif = "<-0.1", signed = TRUE, group = "1-2"), "2")
  expect_equal(GetDif(tbl1, dif = ">0.10&<0.15"), "3")
  expect_equal(GetDif(tbl1, dif = ">0.25", item  = TRUE), "grk10012_c")
  expect_error(GetDif(tbl1, dif = "+0.25"),
               "Unknown stat function.")
})


test_that("get model fit for DIF analyses works", {

  tbl <- try({
    Import(path = test_path("fixtures/ex1/tables"),
           filename = "dif_dich_sex.xlsx")
  })

  expect_equal(GetDifFit(tbl, difvar = "sex", type = "BIC"), "13,842")
  expect_equal(GetDifFit(tbl, difvar = "sex", type = "AIC", model = "main"), "13,667")

})


