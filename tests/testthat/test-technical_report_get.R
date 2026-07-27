
test_that("GetProp() and GetPropLabels() work", {

  data("ex2")

  expect_equal(GetPropLabels("MC"), c(MC = "Simple multiple-choice items"))
  expect_equal(GetPropLabels("unknown"), "")
  expect_named(GetPropLabels(), c(
    "MC", "CMC", "SR", "MA", "TET", "HL",
    "finding", "conclusion", "reflecting", "information", "instruction",
    "advertising", "commenting", "literary",
    "change", "data", "units", "space",
    "access", "create", "evaluate", "manage", "email", "internet",
    "spreadsheet", "word"
  ))

  expect_equal(
    GetProp(ex2$vars, select = "mixed", prop = "type", val = c("CMC", "MA")),
    4
  )
  expect_equal(
    GetProp(
      ex2$vars,
      select = "mixed",
      prop = "type",
      val = c("CMC", "MA"),
      item = TRUE
    ),
    c("mag120003_c", "mag120007_c", "mag120014_c", "mag120016_c")
  )

})


test_that("GetMVI works", {

  # Missing values analysis
  data("ex1")
  outdir <- withr::local_tempdir()
  mvi <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    position = "pos",
    digits = 2,
    plots = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE
  )
  mvi <- Import(outdir, "mv_item.xlsx")

  expect_equal(GetMVI(mvi, type = "NR", stat = "Max"), "64")
  expect_equal(GetMvi(mvi$summary, type = "NV", stat = "SD", digits = 2), "0.27")
  expect_equal(GetMVI(mvi, "OM", "Mean"), GetMvi(mvi, "OM", "Mean"))

})

test_that("GetMVP works", {

  # Missing values analysis
  data("ex1")
  outdir <- withr::local_tempdir()
  mvp <- mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    digits = 2,
    plots = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE
  )
  mvp <- Import(outdir, "mv_person.xlsx")

  expect_equal(GetMVP(mvp, type = "ALL", value = ">5", digits = 1), "9.2")
  expect_equal(GetMvp(mvp, type = "NV", value = "0"), "89")
  expect_equal(GetMVP(mvp, "NR", "<2"), GetMvp(mvp, "NR", "<2"))

  # two-character operators are parsed instead of silently returning NA
  expect_false(GetMvp(mvp, "OM", ">=3") == "NA")
  expect_equal(GetMvp(mvp, "OM", ">=3", digits = 2),
               GetMvp(mvp, "OM", ">2", digits = 2))
  expect_equal(GetMvp(mvp, "OM", "<=2", digits = 2),
               GetMvp(mvp, "OM", "<3", digits = 2))
  expect_equal(GetMvp(mvp, "OM", "==0", digits = 2),
               GetMvp(mvp, "OM", "0", digits = 2))
  expect_equal(GetMvp(mvp, "OM", "!=0", digits = 2),
               GetMvp(mvp, "OM", ">0", digits = 2))

  # conditions can be combined
  expect_equal(
    as.numeric(GetMvp(mvp, "OM", ">=1 & <=2", digits = 2)),
    as.numeric(GetMvp(mvp, "OM", "1", digits = 2)) +
      as.numeric(GetMvp(mvp, "OM", "2", digits = 2)),
    tolerance = 1e-6
  )

  # invalid conditions raise an error rather than returning NA
  expect_error(GetMvp(mvp, "OM", "=>3"), "did you mean")
  expect_error(GetMvp(mvp, "OM", ">abc"), "Invalid number")

})


test_that("GetPars() works", {

  # IRT analysis
  data("ex1")
  outdir <- withr::local_tempdir()
  irtmod <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  )
  pars <- Import(outdir, "irt_dich.xlsx")

  expect_equal(GetPars(pars, type = "xsi", stat = min, item = TRUE), "grk10001_c")
  expect_equal(GetPars(pars, type = "N_valid", stat = "<600|>900"), "11")
  expect_equal(GetPars(pars, type = "WMNSQ", stat = max, excl = "<1|>1.1"), "1.03")
  expect_equal(
    GetPars(pars, type = "t", stat = min, excl = "<.5&>-1", digits = 1),
    "0.5"
  )

  # robustness: whitespace around &/| is tolerated (same result as no spaces)
  expect_equal(GetPars(pars, type = "WMNSQ", stat = max, excl = "<1 | >1.1"), "1.03")
  expect_equal(GetPars(pars, type = "N_valid", stat = "<600 | >900"), "11")

  # robustness: two-character operators >= and <= are supported
  expect_equal(
    GetPars(pars, type = "N_valid", stat = ">=600"),
    GetPars(pars, type = "N_valid", stat = ">599")
  )
  expect_equal(
    GetPars(pars, type = "N_valid", stat = "<=900"),
    GetPars(pars, type = "N_valid", stat = "<901")
  )

  # robustness: a vector of filters is rejected (combine with & or | instead)
  expect_error(
    GetPars(pars, type = "WMNSQ", stat = max, excl = c(">1.15", "<1.20")),
    "single condition"
  )

  # robustness: the reversed operator => is rejected with a hint (R uses >=)
  expect_error(
    GetPars(pars, type = "WMNSQ", stat = "=>1.15"),
    "did you mean '>='",
    fixed = TRUE
  )

  expect_error(
    GetPars(pars, type = "WMNSQ", stat = "+1"),
    "Unknown stat function.",
    fixed = TRUE
  )
  expect_error(
    GetPars(pars, type = "WMNSQ", stat = max, excl = "+1"),
    "Unknown stat function.",
    fixed = TRUE
  )
})

test_that("GetCat() works", {

  # IRT analysis for dichotomous items
  data("ex1")
  outdir <- withr::local_tempdir()
  irtmod1 <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  )
  dich <- readRDS(file.path(outdir, "irt_dich.rds"))

  # IRT analysis for polytomous items
  data("ex2")
  outdir <- withr::local_tempdir()
  irtmod2 <- suppressWarnings(irt_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    valid = "valid",
    select = "mixed",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  ))
  poly <- readRDS(file.path(outdir, "irt_poly.rds"))

  expect_equal(GetCat(dich, stat = median), "-0.14")
  expect_equal(GetCat(dich, stat = min, item = TRUE), "grk10001_c")
  expect_equal(GetCat(poly, stat = max), "1.61")
  expect_equal(GetCat(poly, stat = max, item = TRUE), "mag120017_c")

})


test_that("GetVar() and GetRel() work", {

  # IRT analysis for dichotomous items
  data("ex1")
  outdir <- withr::local_tempdir()
  irtmod1 <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  )
  dich <- readRDS(file.path(outdir, "irt_dich.rds"))

  # IRT analysis for polytomous items
  data("ex2")
  outdir <- withr::local_tempdir()
  irtmod2 <- suppressWarnings(irt_analysis(
    resp = ex2$resp,
    vars = ex2$vars,
    valid = "valid",
    select = "mixed",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  ))
  poly <- readRDS(file.path(outdir, "irt_poly.rds"))

  expect_equal(GetVar(dich), "1.46")
  expect_equal(GetVar(poly, digits = 1), "1.2")

  expect_equal(GetRel(dich), "0.74")
  expect_equal(GetRel(dich, WLE = TRUE, digits = 1), "0.7")
  expect_equal(GetRel(poly), "0.77")
  expect_equal(GetRel(poly, WLE = TRUE), "0.70")

})


test_that("GetDist() works", {

  # Distractor analysis
  data("ex1")
  outdir <- withr::local_tempdir()
  dis_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select_raw = "raw",
    select_score = "dich",
    correct = "correct",
    use_wle = TRUE,
    digits = 2,
    save = TRUE,
    overwrite = TRUE,
    path_table = outdir,
    path_results = outdir,
    print = FALSE,
    warn  = FALSE
  )
  dist <- Import(
    test_path("fixtures", "ex1", "tables"),
    "distractors_summary.xlsx"
  )

  expect_equal(GetDist(dist, stat = mean, digits = 3), "-.161")
  expect_equal(GetDist(dist, stat = min, correct = TRUE), ".31")
  expect_equal(GetDist(dist, stat = min, item = TRUE), "grk10003_c")

})


test_that("GetFit() works", {

  # IRT analysis
  data("ex1")
  outdir <- withr::local_tempdir()
  irtmod <- irt_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    plots = FALSE,
    verbose = FALSE,
    path_results = outdir,
    path_table = outdir,
    path_plots = outdir,
    overwrite = TRUE,
    print = FALSE,
    warn = FALSE
  )
  pars <- Import(outdir, "irt_dich.xlsx")

  expect_equal(GetFit(pars, type = "AIC"), "13,946")
  expect_equal(GetFit(pars, type = "BIC"), "14,024")
  expect_equal(GetFit(pars, type = "Npars"), "16")
  expect_equal(GetFit(pars, type = "Deviance", GPCM = TRUE), "13,902")

})


test_that("GetDim() and GetDimFit() work", {

  dim <- Import(test_path("fixtures", "ex2", "tables"), "dimensionality.xlsx")

  expect_equal(GetDimFit(dim, model = "content", type = "BIC"), "26,151")
  expect_equal(GetDim(dim, model = "content", stat = median), ".74")
  expect_equal(GetDim(dim, model = "content", stat = max, digits = 1), ".8")
  expect_equal(GetDim(dim, model = "content", stat = min, var = TRUE), "2.00")

})


test_that("GetDIF() works", {

  data("ex1")
  outdir <- withr::local_tempdir()
  diffit <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    dif_vars = c("sex", "mig"),
    overwrite = TRUE,
    save = TRUE,
    print = FALSE,
    return = FALSE,
    path_results = outdir,
    path_table = outdir,
    verbose = FALSE,
    warn = FALSE
  )
  sex <- Import(outdir, "dif_dich_sex.xlsx")
  mig <- Import(outdir, "dif_dich_mig.xlsx")

  expect_equal(GetDIF(sex, n = 0), "493")
  expect_equal(GetDif(sex, main = "std", model = "main"), "-0.32")
  expect_equal(GetDif(mig, main = "ustd", group = "1-3"), "0.42")
  expect_equal(GetDif(mig, main = "ustd", group = "1-3", model = "main"), "0.40")

  expect_equal(GetDif(mig, dif = ">.4|<.05"), "10")
  # robustness: whitespace around &/| tolerated; >= supported (no xsi is exactly .4)
  expect_equal(GetDif(mig, dif = ">.4 | <.05"), "10")
  expect_equal(GetDif(mig, dif = ">=.4"), GetDif(mig, dif = ">.4"))
  expect_equal(GetDif(mig, dif = median), "0.14")
  expect_equal(GetDif(mig, dif = "<0.1", signed = FALSE, group = "1-2"), "7")
  expect_equal(
    GetDif(mig, dif = ">.4", group = c("1-2", "2-3"), item = TRUE),
    "grk10015_c, grk10013_c, grk10014_c"
  )
  expect_equal(GetDif(sex, dif = "<-0.1", signed = TRUE), "1")

  expect_error(
    GetDif(sex, main = "unknown"),
    "Allowed values for argument main",
    fixed = TRUE
  )
  expect_error(
    GetDif(sex, dif = "+0.25"),
    "Unknown stat function.",
    fixed = TRUE
  )

  expect_equal(GetDIF(sex, n = 1), GetDif(sex, n = 1))

})


test_that("GetDIFFit works", {

  data("ex1")
  outdir <- withr::local_tempdir()
  diffit <- dif_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    valid = "valid",
    select = "dich",
    digits = 2,
    dif_vars = c("sex", "mig"),
    overwrite = TRUE,
    save = TRUE,
    print = FALSE,
    return = FALSE,
    path_results = outdir,
    path_table = outdir,
    verbose = FALSE,
    warn = FALSE
  )
  tr <- Import(outdir, "dif_dich_TR.xlsx")

  expect_equal(GetDIFFit(tr, difvar = "mig", type = "AIC"), "13,914")
  expect_equal(GetDifFit(tr, difvar = "mig", type = "BIC", model = "main"), "13,967")
  expect_equal(GetDIFFit(tr, difvar = "mig", type = "Number.of.parameters"), "46")
  expect_equal(GetDIFFit(tr, difvar = "mig", type = "Deviance"), "13,822")
  expect_equal(
    GetDIFFit(tr, difvar = "sex", type = "BIC"),
    GetDifFit(tr, difvar = "sex", type = "BIC")
  )

})


