
# `grouping` expects, for each group, the name of a logical column that exists
# in both `vars` (does the item belong to the group?) and `resp` (did the person
# receive it?). Neither ex1 nor ex2 ships such a variable, so the tests below
# build a synthetic two group design: the selected items are split between the
# groups and persons are assigned alternatingly.
add_grouping <- function(data, select = "dich") {

  items <- which(data$vars[[select]])
  first_half <- items[seq_along(items) <= length(items) / 2]

  data$vars$g1 <- seq_len(nrow(data$vars)) %in% first_half
  data$vars$g2 <- data$vars[[select]] & !data$vars$g1

  data$resp$g1 <- rep(c(TRUE, FALSE), length.out = nrow(data$resp))
  data$resp$g2 <- !data$resp$g1

  data
}


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

  # The fixture was generated with digits = 2, which differs from the current
  # default of 3. Rounding happens before the results are stored, so digits
  # must be pinned here for the comparison to be meaningful.
  result <- mv_item(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    digits = 2,
    print = FALSE,
    save = FALSE,
    return = TRUE,
    warn = FALSE
  )

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/mv_item.rds"))

  # Results are identical to the precomputed ones
  expect_identical(result, fixture)

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

  # As for mv_item(), the fixture was generated with digits = 2
  result <- mv_person(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    digits = 2,
    print = FALSE,
    save = FALSE,
    return = TRUE,
    warn = FALSE
  )

  # Load fixture
  fixture <- readRDS(test_path("fixtures/ex1/results/mv_person.rds"))

  # Results are identical to the precomputed ones
  expect_identical(result, fixture)

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
  expect_named(table, c("list", "summary"))

  # One row per selected item, one summary row per reported statistic
  expect_equal(nrow(table$list), sum(ex1$vars$dich))
  expect_named(
    table$list,
    c("item", "position", "N_administered", "N_valid", "OM", "NV", "NR", "ALL")
  )
  expect_named(table$summary, c("Mean", "SD", "Median", "Min", "Max"))
  expect_equal(
    rownames(table$summary),
    c("N_administered", "N_valid", "OM", "NV", "NR", "ALL")
  )

  # Each response can only carry one missing value type, so ALL is their total
  # (up to the rounding applied to each column separately)
  expect_true(all(abs(
    table$list$ALL - (table$list$OM + table$list$NV + table$list$NR)
  ) < 0.01))

  # Percentages stay within bounds, valid responses never exceed administered
  for (type in c("OM", "NV", "NR", "ALL")) {
    expect_true(all(table$list[[type]] >= 0 & table$list[[type]] <= 100))
  }
  expect_true(all(table$list$N_valid <= table$list$N_administered))

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
  expect_named(table, c("OM", "NV", "NR", "ALL", "summary"))

  # Each missing value type is a distribution over persons
  for (type in c("OM", "NV", "NR", "ALL")) {
    expect_named(
      table[[type]], c("Number of missing responses", "Percentage")
    )
    # Percentages over all persons have to add up to 100
    expect_equal(sum(table[[type]]$Percentage), 100, tolerance = 0.01)
  }

  # Summary carries one row per statistic and one column per missing value type
  expect_named(table$summary, c("statistics", "OM", "NV", "NR", "ALL"))
  expect_equal(
    table$summary$statistics, c("mean", "sd", "median", "min", "max")
  )

})


test_that("mv_item() works with mixed item types", {

  data(ex2)

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
  expect_equal(nrow(result$list), sum(ex2$vars$mixed))

})


test_that("mv_person() works with mixed item types", {

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


test_that("mvi_analysis() treats NAs as not administered", {

  data(ex1)
  mvs <- c(OM = -97, NV = -95, NR = -94)

  baseline <- mvi_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    position = "pos",
    valid = "valid",
    mvs = mvs,
    save = FALSE,
    warn = FALSE
  )

  # Set a single response of the first item to NA. The person is part of the
  # valid sample, so the NA has to show up in the results.
  item <- ex1$vars$item[1]
  expect_true(ex1$resp$valid[1])

  ex1_mod <- ex1
  ex1_mod$resp[1, item] <- NA

  result <- try({
    suppressWarnings(mvi_analysis(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      position = "pos",
      valid = "valid",
      mvs = mvs,
      save = FALSE,
      warn = FALSE
    ))
  })

  expect_false(inherits(result, "try-error"))

  # NAs count as not administered rather than as a user defined missing value
  expect_equal(
    result$list$N_administered[1], baseline$list$N_administered[1] - 1
  )
  expect_equal(result$list$N_valid[1], baseline$list$N_valid[1] - 1)

  # Only the affected item changes
  expect_equal(result$list[-1, ], baseline$list[-1, ])

})


test_that("mvp_analysis() does not count NAs as missing values", {

  data(ex1)
  mvs <- c(OM = -97, NV = -95, NR = -94)

  baseline <- mvp_analysis(
    resp = ex1$resp,
    vars = ex1$vars,
    select = "dich",
    valid = "valid",
    mvs = mvs,
    save = FALSE,
    warn = FALSE
  )

  # Replace a valid response by NA
  ex1_mod <- ex1
  ex1_mod$resp[1, ex1$vars$item[1]] <- NA

  result <- try({
    suppressWarnings(mvp_analysis(
      resp = ex1_mod$resp,
      vars = ex1_mod$vars,
      select = "dich",
      valid = "valid",
      mvs = mvs,
      save = FALSE,
      warn = FALSE
    ))
  })

  expect_false(inherits(result, "try-error"))

  # NAs are no user defined missing values, so the number of missing values
  # per person and with it the distributions stay the same
  for (type in c("OM", "NV", "NR", "ALL")) {
    expect_equal(result[[type]], baseline[[type]])
    expect_equal(sum(result[[type]]), 100, tolerance = 0.01)
  }

})


test_that("mvi_analysis() splits results by grouping", {

  data(ex1)
  grouped <- add_grouping(ex1)
  mvs <- c(OM = -97, NV = -95, NR = -94)

  # A single position variable for several groups is accepted, but warns that
  # the item positions are assumed to be identical across groups
  expect_warning(
    result <- mvi_analysis(
      resp = grouped$resp,
      vars = grouped$vars,
      select = "dich",
      position = "pos",
      valid = "valid",
      grouping = c("g1", "g2"),
      mvs = mvs,
      save = FALSE,
      warn = FALSE
    ),
    "Only one position variable"
  )

  # One entry per group plus the whole sample
  expect_named(result$list, c("g1", "g2", "all"))
  expect_named(result$summary, c("g1", "g2", "all"))

  # Each group only contains its own items, "all" contains every selected item
  expect_equal(nrow(result$list$g1), sum(grouped$vars$g1))
  expect_equal(nrow(result$list$g2), sum(grouped$vars$g2))
  expect_equal(nrow(result$list$all), sum(grouped$vars$dich))
  expect_equal(
    result$list$g1$item, grouped$vars$item[grouped$vars$g1]
  )

  # Only the persons of a group are counted for its items
  expect_true(all(result$list$g1$N_administered <= sum(grouped$resp$g1)))

})


test_that("mvi_analysis() accepts one position variable per group", {

  data(ex1)
  grouped <- add_grouping(ex1)

  # Providing one position variable per group suppresses the warning. The
  # vector has to be named with the group names, as the positions are looked
  # up by group rather than by order.
  expect_no_warning(
    result <- mvi_analysis(
      resp = grouped$resp,
      vars = grouped$vars,
      select = "dich",
      position = c(g1 = "pos", g2 = "pos"),
      valid = "valid",
      grouping = c("g1", "g2"),
      mvs = c(OM = -97, NV = -95, NR = -94),
      save = FALSE,
      warn = FALSE
    )
  )

  expect_named(result$list, c("g1", "g2", "all"))

})


test_that("mvi_analysis() rejects mismatching position and grouping", {

  data(ex1)
  grouped <- add_grouping(ex1)
  mvs <- c(OM = -97, NV = -95, NR = -94)

  # More position variables than groups
  expect_error(
    mvi_analysis(
      resp = grouped$resp,
      vars = grouped$vars,
      select = "dich",
      position = c("pos", "pos"),
      valid = "valid",
      grouping = "g1",
      mvs = mvs,
      save = FALSE,
      warn = FALSE
    ),
    "do not match"
  )

  # Several position variables without any grouping
  expect_error(
    mvi_analysis(
      resp = grouped$resp,
      vars = grouped$vars,
      select = "dich",
      position = c("pos", "pos"),
      valid = "valid",
      mvs = mvs,
      save = FALSE,
      warn = FALSE
    ),
    "No grouping variable provided"
  )

})


test_that("mvp_analysis() splits results by grouping", {

  data(ex1)
  grouped <- add_grouping(ex1)

  result <- mvp_analysis(
    resp = grouped$resp,
    vars = grouped$vars,
    select = "dich",
    valid = "valid",
    grouping = c("g1", "g2"),
    mvs = c(OM = -97, NV = -95, NR = -94),
    save = FALSE,
    warn = FALSE
  )

  # One entry per group plus the whole sample
  expect_named(result, c("g1", "g2", "all"))

  for (group in c("g1", "g2", "all")) {
    expect_named(result[[group]], c("OM", "NV", "NR", "ALL", "summary"))
    # Percentages within a group add up to 100
    for (type in c("OM", "NV", "NR", "ALL")) {
      expect_equal(sum(result[[group]][[type]]), 100, tolerance = 0.01)
    }
  }

  # A person can never have more missing values than the group has items
  expect_true(
    max(as.numeric(names(result$g1$ALL))) <= sum(grouped$vars$g1)
  )

})
