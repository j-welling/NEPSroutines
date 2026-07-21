
test_that("Setup() with non-interactive input works", {

  path <- withr::local_tempdir()

  expect_error(
    Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
          wave = "x"),
    "wave must be numeric.",
    fixed = TRUE
  )
  expect_error(
    Setup(path = path, ask = FALSE, sc = 4, domain = "xx", study = "A104",
          wave = 14),
    "domain must be one of re, ma, dc.",
    fixed = TRUE
  )
  expect_error(
    Setup(path = path, ask = FALSE, sc = 9, domain = "re", study = "A104",
          wave = 14),
    "sc must fall between 1 and 8.",
    fixed = TRUE
  )

})


test_that("Setup() for reading works", {

  path <- withr::local_tempdir()

  expect_message(
    Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
          wave = 14),
    "technical report has been setup",
    fixed = TRUE
  )

  qmd <- file.path(path, "TR_A104_SC4_W14_RE.qmd")
  bib <- file.path(path, "TR_A104_SC4_W14_RE.bib")

  expect_true(file.exists(qmd))
  expect_true(file.exists(bib))
  expect_true(dir.exists(file.path(path, "_extensions", "neps-paper")))

  txt <- paste(readLines(qmd, warn = FALSE), collapse = "\n")
  expect_match(txt, 'bibliography: "TR_A104_SC4_W14_RE.bib"', fixed = TRUE)
  expect_match(txt, "NEPS technical report for reading", fixed = TRUE)
  expect_match(txt, "Starting Cohort 4", fixed = TRUE)
  expect_match(txt, "Wave 14", fixed = TRUE)
  expect_match(txt, "ninth grade", fixed = TRUE)
  expect_false(grepl("\\{\\{/?if(re|ma|dc)\\}\\}", txt))
  expect_false(grepl("\\{\\{(sc|wave|scname|domain|Domain)\\}\\}", txt))
  expect_match(txt, "Cognitive Requirements", fixed = TRUE)
  expect_false(grepl("Content areas \\{\\.unnumbered\\}", txt, fixed = TRUE))

})


test_that("Setup() for mathematics works", {

  path <- withr::local_tempdir()

  Setup(path = path, ask = FALSE, sc = 3, domain = "ma", study = "B123",
        wave = 2)

  qmd <- file.path(path, "TR_B123_SC3_W2_MA.qmd")
  txt <- paste(readLines(qmd, warn = FALSE), collapse = "\n")

  expect_match(txt, 'bibliography: "TR_B123_SC3_W2_MA.bib"', fixed = TRUE)
  expect_match(txt, "NEPS technical report for mathematics", fixed = TRUE)
  expect_match(txt, "Starting Cohort 3", fixed = TRUE)
  expect_match(txt, "Wave 2", fixed = TRUE)
  expect_match(txt, "fifth grade", fixed = TRUE)
  expect_match(txt, "Content areas", fixed = TRUE)
  expect_false(grepl("Cognitive Requirements", txt, fixed = TRUE))
  expect_false(grepl("\\{\\{/?if(re|ma|dc)\\}\\}", txt))

})


test_that("Setup() for digital competence works", {

  path <- withr::local_tempdir()

  Setup(path = path, ask = FALSE, sc = 8, domain = "dc", study = "C999",
        wave = 1)

  qmd <- file.path(path, "TR_C999_SC8_W1_DC.qmd")
  txt <- paste(readLines(qmd, warn = FALSE), collapse = "\n")

  expect_true(file.exists(file.path(path, "Fig_DC_facets.png")))
  expect_match(txt, "NEPS technical report for digital competence", fixed = TRUE)
  expect_match(txt, "Facets of the  Digital Competence Framework", fixed = TRUE)
  expect_false(grepl("\\{\\{/?if(re|ma|dc)\\}\\}", txt))

})


test_that("Setup() does not overwrite files unless requested", {

  path <- withr::local_tempdir()

  Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
        wave = 14)

  qmd <- file.path(path, "TR_A104_SC4_W14_RE.qmd")
  writeLines("custom report content", qmd)

  Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
        wave = 14, overwrite = FALSE)
  expect_equal(readLines(qmd, warn = FALSE), "custom report content")

  Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
        wave = 14, overwrite = TRUE)
  expect_match(
    paste(readLines(qmd, warn = FALSE), collapse = "\n"),
    "NEPS technical report for reading",
    fixed = TRUE
  )

})

test_that("Update() works", {

  path <- withr::local_tempdir()

  expect_error(
    Update(path),
    paste0("Couldn't find the path ", file.path(path, "_extensions")),
    fixed = TRUE
  )

  Setup(path = path, ask = FALSE, sc = 4, domain = "re", study = "A104",
        wave = 14)

  marker <- file.path(path, "_extensions", "neps-paper", "obsolete.txt")
  writeLines("obsolete", marker)

  expect_message(
    Update(path),
    "Quarto extension",
    fixed = TRUE
  )

  expect_true(dir.exists(file.path(path, "_extensions", "neps-paper")))
  expect_true(file.exists(file.path(path, "_extensions", "neps-paper",
                                    "_extension.yml")))
  expect_false(file.exists(marker))

})


