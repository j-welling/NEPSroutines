
test_that("mvi_analysis() runs without error", {

  data(ex1)

  result <- try({
    mvi_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      position = "pos",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("list" %in% names(result))
  expect_true("summary" %in% names(result))

})


test_that("mvi_analysis() produces valid list output", {

  data(ex1)

  result <- mvi_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  mvlist <- result$list

  expect_true(is.data.frame(mvlist))
  expect_true("item" %in% names(mvlist))
  expect_true("position" %in% names(mvlist))
  expect_true("N_administered" %in% names(mvlist))
  expect_true("N_valid" %in% names(mvlist))
  expect_true("OM" %in% names(mvlist))
  expect_true("NV" %in% names(mvlist))
  expect_true("NR" %in% names(mvlist))
  expect_true("ALL" %in% names(mvlist))

})


test_that("mvi_analysis() produces valid summary", {

  data(ex1)

  result <- mvi_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  summary <- result$summary

  expect_true(is.data.frame(summary))
  expect_true("Mean" %in% names(summary))
  expect_true("SD" %in% names(summary))
  expect_true("Median" %in% names(summary))
  expect_true("Min" %in% names(summary))
  expect_true("Max" %in% names(summary))

})


test_that("mv_item() produces expected structure", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- try({
    mv_item(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      position = "pos",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("list" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_true("summary_table" %in% names(result))

})


test_that("mv_item() matches fixture", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/mv_item.rds"))

  # Compare structure
  expect_equal(names(result$list), names(fixture$list))
  expect_equal(nrow(result$list), nrow(fixture$list))

  # Compare summary structure
  expect_equal(names(result$summary), names(fixture$summary))

})


test_that("mv_item() saves files correctly", {

  data(ex1)
  path <- withr::local_tempdir()

  mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  expect_true(file.exists(paste0(path, "/mv_item.rds")))
  expect_true(file.exists(paste0(path, "/mv_item.xlsx")))

})


test_that("mvp_analysis() runs without error", {

  data(ex1)

  result <- try({
    mvp_analysis(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      save = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))

})


test_that("mvp_analysis() produces valid output structure", {

  data(ex1)

  result <- mvp_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  # Should have entries for each MV type plus ALL and summary
  expect_true("OM" %in% names(result))
  expect_true("NV" %in% names(result))
  expect_true("NR" %in% names(result))
  expect_true("ALL" %in% names(result))
  expect_true("summary" %in% names(result))

  # Summary should be a matrix with descriptive stats
  expect_true(is.matrix(result$summary) || is.data.frame(result$summary))

})


test_that("mv_person() produces expected structure", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- try({
    mv_person(
      resp = ex1$resp,
      vars = ex1$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true("mv_p" %in% names(result))
  expect_true("summary" %in% names(result))

})


test_that("mv_person() matches fixture", {

  data(ex1)
  path <- withr::local_tempdir()

  result <- mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = TRUE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/mv_person.rds"))

  # Compare structure
  expect_equal(names(result$mv_p), names(fixture$mv_p))

})


test_that("mv_person() saves files correctly", {

  data(ex1)
  path <- withr::local_tempdir()

  mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    print = FALSE,
    save = TRUE,
    return = FALSE,
    path_results = path,
    path_table = path,
    overwrite = TRUE,
    warn = FALSE
  )

  expect_true(file.exists(paste0(path, "/mv_person.rds")))
  expect_true(file.exists(paste0(path, "/mv_person.xlsx")))

})


test_that("mvi_table() produces valid output", {

  data(ex1)

  mv_i <- mvi_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  table <- mvi_table(
    mv_i = mv_i,
    vars = ex1$vars,
    select = "dich",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  expect_true(is.list(table))

})


test_that("mvp_table() produces valid output", {

  data(ex1)

  mv_p <- mvp_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  table <- mvp_table(
    mv_p = mv_p,
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE
  )

  expect_true(is.list(table))
  expect_true("summary" %in% names(table))

})


test_that("mv_item() handles grouping correctly", {

  data(ex2)
  path <- withr::local_tempdir()

  # Skip if ex2 doesn't have appropriate grouping variables
  skip_if(!all(c("position", "valid") %in% names(ex2$vars)))

  result <- try({
    mv_item(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      position = "pos",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))

})


test_that("mv_person() handles grouping correctly", {

  data(ex2)

  result <- try({
    mv_person(
      resp = ex2$resp,
      vars = ex2$vars,
      select = "mixed",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      print = FALSE,
      save = FALSE,
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))

})


test_that("mvi_analysis() handles NAs in resp", {

  data(ex1)

  # Add some actual NAs to response data
  ex1_mod <- ex1
  ex1_mod$resp[1, ex1_mod$vars$item[1]] <- NA

  # Should run without error (may or may not warn depending on internal checks)
  result <- try({
    suppressWarnings(mvi_analysis(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      position = "pos",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      save = FALSE,
      warn = FALSE
    ))
  })

  expect_false(inherits(result, "try-error"))

})


test_that("mvp_analysis() handles NAs in resp", {

  data(ex1)

  # Add some actual NAs to response data
  ex1_mod <- ex1
  ex1_mod$resp[1, ex1_mod$vars$item[1]] <- NA

  # Should run without error (may or may not warn depending on internal checks)
  result <- try({
    suppressWarnings(mvp_analysis(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      valid = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      save = FALSE,
      warn = FALSE
    ))
  })

  expect_false(inherits(result, "try-error"))

})
