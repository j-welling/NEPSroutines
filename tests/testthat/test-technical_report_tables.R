
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

test_that("TblItemProps() with na.rm works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  vars <- data.frame(
    txttyp = factor(c("SR", "HL", "CMC")),
    easy = c(TRUE, FALSE, FALSE),
    medium = c(FALSE, FALSE, FALSE),
    difficult = c(FALSE, FALSE, TRUE)
  )

  # na.rm = TRUE
  tbl1 <- TblItemProps(
    vars = vars,
    select = c("Easy test" = "easy",
               "Medium test" = "medium",
               "Difficult test" = "difficult"),
    prop = "txttyp",
    propname = "Text types",
    na.rm = TRUE
  )
  expect_s3_class(tbl1, "flextable")
  expect_equal(names(tbl1$body$dataset), c("Text types", "Easy test",
                                           "Difficult test"))
  expect_equal(tbl1$body$dataset[["Text types"]], c(
    "Complex multiple-choice items",
    "Short constructed responses",
    "Total number of items"
  ))
  expect_equal(tbl1$body$dataset[["Easy test"]], c("0", "1", "1"))
  expect_equal(tbl1$body$dataset[["Difficult test"]], c("1", "0", "1"))

  # na.rm = FALSE
  tbl2 <- TblItemProps(
    vars = vars,
    select = c("Easy test" = "easy",
               "Medium test" = "medium",
               "Difficult test" = "difficult"),
    prop = "txttyp",
    propname = "Text types",
    na.rm = FALSE
  )
  expect_s3_class(tbl2, "flextable")
  expect_equal(names(tbl2$body$dataset), c("Text types", "Easy test",
                                           "Medium test", "Difficult test"))
  expect_equal(tbl2$body$dataset[["Text types"]], c(
    "Complex multiple-choice items",
    "Short constructed responses",
    "Highlighting tasks",
    "Total number of items"
  ))
  expect_equal(tbl2$body$dataset[["Easy test"]], c("0", "1", "0", "1"))
  expect_equal(tbl2$body$dataset[["Difficult test"]], c("1", "0", "0", "1"))

  expect_error(TblItemProps(vars = vars, select = "easy", prop = "unknown"),
               "Unknown item property unknown!")

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


test_that("TblMvi() without booklets works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # Create output for missing values analyses
  data("ex1")
  outdir <- withr::local_tempdir()
  mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    position = "pos",
    plots = FALSE,
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = outdir,
    path_table = outdir,
    overwrite = TRUE,
    warn = FALSE,
    verbose = FALSE
  )
  tab <- Import(outdir, "mv_item.xlsx")
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

})


test_that("TblMvi() with booklets works", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # Create output for missing values analyses
  data("ex3")
  outdir <- withr::local_tempdir()
  mv_item(
    resp = ex3$resp,
    vars = ex3$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    position = c(booklet1 = "pos1", booklet2 = "pos2"),
    grouping = c("Booklet 1" = "booklet1", "Booklet 2" = "booklet2"),
    plots = FALSE,
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = outdir,
    path_table = outdir,
    overwrite = TRUE,
    warn = FALSE,
    verbose = FALSE
  )
  tab <- Import(outdir, "mv_item.xlsx")
  tbl <- TblMvi(tab, select = "booklet2")

  expect_s3_class(tbl, "flextable")
  expect_equal(names(tbl$body$dataset), c(
    "Nr.", "Item", "Pos.", "N", "OM", "NV", "NR"
  ))
  expect_equal(tbl$body$dataset[["Nr."]][1:3], c(1, 2, 3))
  expect_equal(tbl$body$dataset[["Item"]][1:3], c(
    "reg70001_c", "reg70003_c", "reg70005_c"
  ))
  expect_equal(tbl$body$dataset[["N"]][1:3], c(689, 689, 669))
  expect_equal(tbl$body$dataset[["Pos."]][1:3], c(1, 3, 5))

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

  # DIF analyses
  data(ex1)
  outdir <- withr::local_tempdir()
  dif_analysis(
   resp = ex1$resp,
   vars = ex1$vars,
   select = "dich",
   valid = "valid",
   dif_vars = c("sex", "mig"),
   include_mv = 100,
   print = FALSE,
   save = TRUE,
   return = FALSE,
   path_results = outdir,
   path_table = outdir,
   overwrite = TRUE,
   verbose = FALSE,
   warn = FALSE
  )
  dif <- Import(outdir, regexp = "^dif_dich_([^_]+\\.xlsx)")

  tbl <- TblDif(
    dif$TR,
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
  expect_equal(tbl$body$dataset[["sex.0-1"]][1], "0.02 (0.02)")
  expect_equal(TblDIF(dif$TR)$body$dataset, TblDif(dif$TR)$body$dataset)

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


test_that("specialized table functions forward ... to Tbl()", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "mv_item.xlsx")

  # TblMvi() does not set `align`/`align_head` itself, so both flow through `...`
  # to Tbl(); several Tbl() arguments can be forwarded in a single call.
  tbl <- TblMvi(tab, align = "left", align_head = "left")
  expect_s3_class(tbl, "flextable")
  expect_true(all(tbl$body$styles$pars$text.align$data == "left"))
  expect_true(all(tbl$header$styles$pars$text.align$data == "left"))

  # Without `...`, Tbl()'s default centered alignment is retained.
  tbl_default <- TblMvi(tab)
  expect_true(all(tbl_default$body$styles$pars$text.align$data == "center"))

  # An argument Tbl() does not accept errors at the Tbl() boundary rather than
  # being silently swallowed (Tbl() has a closed signature, no `...`).
  expect_error(TblMvi(tab, nonexistent_arg = 1), "unused argument")

  # Wrappers that hard-code structural Tbl() arguments raise a duplicate-argument
  # error if the same name is also passed via `...` (the intended guardrail).
  data("ex2")
  expect_error(
    TblItemProps(ex2$vars, select = c("Number of items" = "mixed"),
                 prop = "type", align = "left"),
    "matched by multiple"
  )

  # A valid, unbound Tbl() argument still forwards through a wrapper that
  # hard-codes a different argument (TblDifFit fixes `digits`).
  dif <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")
  expect_s3_class(TblDifFit(dif, merge = FALSE), "flextable")
  expect_error(TblDifFit(dif, digits = 0), "matched by multiple")

})


# Regression tests for #115: excl/select must match whole names exactly, not as
# unanchored regular expressions / substrings.

test_that("TblMvi() excl matches column names exactly (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "mv_item.xlsx")

  # Substring / regex collisions are ignored: "dministered" is inside the
  # internal "N_administered" and "ota" is inside the "Total" label, but neither
  # is an exact name or label, so nothing is excluded. The old grepl() dropped
  # N_administered, N_valid, NV and NR for a bare "N". (A bare "N" now excludes
  # the column shown as "N" -- see the display-label test below.)
  keep_all <- names(TblMvi(tab, excl = NULL)$body$dataset)
  expect_equal(names(TblMvi(tab, excl = "dministered")$body$dataset), keep_all)
  expect_equal(names(TblMvi(tab, excl = "ota")$body$dataset), keep_all)
  expect_true(all(c("OM", "NV", "NR") %in% keep_all))

  # The documented defaults still drop their exact columns.
  def <- names(TblMvi(tab)$body$dataset)
  expect_false(any(c("Total", "ALL") %in% def))  # N_administered, ALL excluded
  expect_true(all(c("OM", "NV", "NR") %in% def))

})


test_that("TblMvi() select matches the group suffix exactly (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # Two groups whose suffixes collide as substrings ("_g" is contained in
  # "_g2"). select = "g" must keep only the "_g" columns.
  obj <- data.frame(
    x = c("", ""),
    item = c("i1", "i2"),
    position_g = c(1, 2),
    N_valid_g = c(10, 20),
    position_g2 = c(3, 4),
    N_valid_g2 = c(30, 40),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(obj)[1] <- ""  # mimic the unnamed first column from openxlsx

  tbl <- TblMvi(obj, select = "g")
  nms <- names(tbl$body$dataset)
  # Old grepl("_g") also matched "_g2", leaking "position2"/"N_valid2" columns.
  expect_equal(nms, c("Nr.", "Item", "Pos.", "N"))
  expect_false(any(grepl("2$", nms)))

})


test_that("TblMvi() strips only a trailing '_collapsed' suffix from item names", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  obj <- data.frame(
    x = c("", "", ""),
    # Third item carries "_collapsed" mid-name: only the trailing suffix is
    # stripped, so the internal occurrence must be preserved.
    item = c("i1_collapsed", "i2", "i3_collapsed_c"),
    position = c(1, 2, 3),
    N_valid = c(10, 20, 30),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(obj)[1] <- ""  # mimic the unnamed first column from openxlsx

  # Only the trailing suffix is stripped by default
  tbl <- TblMvi(obj)
  expect_equal(tbl$body$dataset[["Item"]], c("i1", "i2", "i3_collapsed_c"))

  # Preserved when disabled
  tbl_keep <- TblMvi(obj, rename_collapsed = FALSE)
  expect_equal(
    tbl_keep$body$dataset[["Item"]],
    c("i1_collapsed", "i2", "i3_collapsed_c")
  )

})


test_that("TblMvi() select strips only the trailing group suffix (#121)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  # A measure name that itself contains the suffix token ("_g") must keep its
  # internal "_g"; the rename must strip only the *trailing* "_g" (anchored),
  # not the first occurrence.
  obj <- data.frame(
    x = c("", ""),
    item = c("i1", "i2"),
    position_g = c(1, 2),
    x_g_score_g = c(10, 20),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(obj)[1] <- ""

  nms <- names(TblMvi(obj, select = "g", excl = NULL)$body$dataset)
  expect_true("x_g_score" %in% nms)   # anchored: only the trailing "_g" removed
  expect_false("x_score_g" %in% nms)  # unanchored sub() would have produced this

})


test_that("TblPars() excl matches column names exactly (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex2", "tables"), "irt_poly.xlsx")

  # Substring / regex collisions are ignored: "WMNS" is inside "WMNSQ" and
  # "otal" is inside the "Total" label, but neither is an exact name or label,
  # so nothing is excluded. The old grepl() dropped N_administered, N_valid and
  # even WMNSQ for "N". (A bare "N" now excludes the column shown as "N" -- see
  # the display-label test below.)
  keep_all <- names(TblPars(tab, excl = NULL)$body$dataset)
  expect_equal(names(TblPars(tab, excl = "WMNS")$body$dataset), keep_all)
  expect_equal(names(TblPars(tab, excl = "otal")$body$dataset), keep_all)
  expect_true(all(c("WMNSQ", "N") %in% keep_all))

  # Exact name is still excluded.
  expect_false("N_administered" %in% names(TblPars(tab, excl = "N_administered")$body$dataset))

})


test_that("TblDif() excl matches column names exactly (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  dif <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")
  hdr1 <- function(ft) as.character(unlist(ft$header$dataset[1, ]))

  all_cols <- hdr1(TblDif(dif))
  # "mig" is not an exact column name; the old grepl() dropped mig.1-2/1-3/2-3.
  expect_equal(hdr1(TblDif(dif, excl = "mig")), all_cols)
  expect_true(all(c("mig.1-2", "mig.1-3", "mig.2-3") %in% all_cols))

  # Exact names (with "." and "-" treated literally) are excluded.
  dropped <- hdr1(TblDif(dif, excl = c("mig.1-2", "mig.1-3")))
  expect_false(any(c("mig.1-2", "mig.1-3") %in% dropped))
  expect_true(all(c("item", "sex.0-1", "mig.2-3") %in% dropped))

})


test_that("TblDifFit() excl matches DIF variables exactly (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")

  # "se" is a substring of "sex" but not an exact DIF variable, so the sex rows
  # are retained (the old grepl() removed them).
  collide <- TblDifFit(tab, excl = "se")$body$dataset[["DIF variable"]]
  expect_true(any(grepl("Sex", collide)))

  # The exact name still drops the rows.
  dropped <- TblDifFit(tab, excl = "sex")$body$dataset[["DIF variable"]]
  expect_false(any(grepl("Sex", dropped)))

})


# Regression tests for #115 (follow-up to #121): excl also accepts the display
# labels shown in the rendered table, in addition to the internal column /
# DIF-variable names it matches already. Renamed columns (e.g. N_administered ->
# "Total", N_valid -> "N") were previously impossible to exclude by what the
# user sees.

test_that("TblMvi() excl also accepts display labels (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "mv_item.xlsx")

  # Excluding by display label is equivalent to excluding by the internal name.
  by_label <- names(TblMvi(tab, excl = c("Total", "N"))$body$dataset)
  by_internal <- names(TblMvi(tab, excl = c("N_administered", "N_valid"))$body$dataset)
  expect_equal(by_label, by_internal)
  expect_false(any(c("Total", "N") %in% by_label))

  # The user's original mix of labels now drops each named column exactly.
  nms <- names(TblMvi(tab, excl = c("Nr.", "Pos.", "Total", "N"))$body$dataset)
  expect_false(any(c("Nr.", "Pos.", "Total", "N") %in% nms))
  expect_true(all(c("Item", "OM", "NV", "NR") %in% nms))

})


test_that("TblPars() excl also accepts display labels (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex2", "tables"), "irt_poly.xlsx")

  by_label <- names(TblPars(tab, excl = c("Total", "N"))$body$dataset)
  by_internal <- names(TblPars(tab, excl = c("N_administered", "N_valid"))$body$dataset)
  expect_equal(by_label, by_internal)
  expect_false(any(c("Total", "N") %in% by_label))

  # "Difficulty" is the label for the internal "xsi" column.
  expect_true("Difficulty" %in% names(TblPars(tab, excl = NULL)$body$dataset))
  expect_false("Difficulty" %in% names(TblPars(tab, excl = "Difficulty")$body$dataset))

})


test_that("TblDifFit() excl also accepts display labels (#115)", {

  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")

  tab <- Import(test_path("fixtures", "ex1", "tables"), "dif_dich_TR.xlsx")

  # "Migration" is the display label for the raw DIF variable "mig".
  by_label <- TblDifFit(tab, excl = "Migration")$body$dataset[["DIF variable"]]
  by_raw <- TblDifFit(tab, excl = "mig")$body$dataset[["DIF variable"]]
  expect_equal(by_label, by_raw)
  expect_false(any(grepl("Migration", by_label)))

  # A custom `label` value can also be used to exclude its rows.
  custom <- TblDifFit(tab, excl = "Custom",
                      label = c(mig = "Custom"))$body$dataset[["DIF variable"]]
  expect_false(any(grepl("Custom|Migration", custom)))

})


