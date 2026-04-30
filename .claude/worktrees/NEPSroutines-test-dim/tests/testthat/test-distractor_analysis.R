
test_that("conduct_dis_analysis() works", {

  # Temporary output directory
  path <- withr::local_tempdir()

  ####### Data example 2 #######

  # Run distractor analysis
  data(ex2)
  results <- try({
    dis_analysis(
      resp = ex2$resp,
      vars = ex2$vars,
      valid = "valid",
      select_raw = "raw",
      correct = "correct",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = TRUE,
      overwrite = TRUE,
      path_results = path,
      path_table = path
    )
  })

  # Estimated without error
  expect_false(inherits(results, "try-error"))

  # All files created
  expect_true(file.exists(paste0(path, "/distractors_items.xlsx")))
  expect_true(file.exists(paste0(path, "/distractors_summary.xlsx")))
  expect_true(file.exists(paste0(path, "/distractors.rds")))

  # All generated files are identical to the precomputed files
  tabitem_fix <- Import(
    test_path("fixtures/ex2/tables"), "distractors_items.xlsx"
  )
  tabsum_fix <- Import(
    test_path("fixtures/ex2/tables"), "distractors_summary.xlsx"
  )
  res_fix <- readRDS(test_path("fixtures/ex2/results/distractors.rds"))
  tabitem <- Import(path, "distractors_items.xlsx")
  tabsum <- Import(path, "distractors_summary.xlsx")
  res <- readRDS(paste0(path, "/distractors.rds"))
  expect_true(identical(tabsum, tabsum_fix))
  expect_true(identical(tabitem, tabitem_fix))
  expect_true(identical(res, res_fix))

  ####### Data example 3 #######

  # Run distractor analysis
  data(ex3)
  results <- try({
    dis_analysis(
      resp = ex3$resp,
      vars = ex3$vars,
      valid = "valid",
      select_raw = "raw",
      select_score = "mixed",
      correct = "correct",
      use_wle = TRUE,
      scoring = "scoring",
      mvs = -999:-1,
      print = FALSE,
      save = TRUE,
      overwrite = TRUE,
      name_group = "ex3",
      path_results = path,
      path_table = path
    )
  })

  # Estimated without error
  expect_false(inherits(results, "try-error"))

  # All files created
  expect_true(file.exists(paste0(path, "/distractors_ex3_items.xlsx")))
  expect_true(file.exists(paste0(path, "/distractors_ex3_summary.xlsx")))
  expect_true(file.exists(paste0(path, "/distractors_ex3.rds")))

  # All generated files are identical to the precomputed files
  tabitem_fix <- Import(
    test_path("fixtures/ex3/tables"), "distractors_items.xlsx"
  )
  tabsum_fix <- Import(
    test_path("fixtures/ex3/tables"), "distractors_summary.xlsx"
  )
  res_fix <- readRDS(test_path("fixtures/ex3/results/distractors.rds"))
  tabitem <- Import(path, "distractors_ex3_items.xlsx")
  tabsum <- Import(path, "distractors_ex3_summary.xlsx")
  res <- readRDS(paste0(path, "/distractors_ex3.rds"))
  expect_true(identical(tabsum, tabsum_fix))
  expect_true(identical(tabitem, tabitem_fix))
  expect_true(identical(res, res_fix))

})

