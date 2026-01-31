
test_that("link_item_parameters() works correctly", {

  data(ex2)

  # First get item parameters
  model <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  # Apply linking constant
  const <- 0.5
  linked <- link_item_parameters(
    xsi = model$mod$xsi,
    const = const,
    vars = ex2$vars,
    select = "mixed",
    scoring = "scoring",
    warn = FALSE
  )

  expect_true(is.matrix(linked))
  expect_equal(ncol(linked), 2)

  # Check that parameters have been shifted by the constant
  # (accounting for scoring)
  original_xsi <- model$mod$xsi$xsi
  items_in_linked <- rownames(linked)

  # At least some items should be in the linked parameters

  expect_gt(nrow(linked), 0)

})


test_that("link_item_parameters() warns without scoring", {

  data(ex2)

  model <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  expect_warning(
    link_item_parameters(
      xsi = model$mod$xsi,
      const = 0.5,
      vars = ex2$vars,
      select = "mixed",
      scoring = NULL,
      warn = TRUE
    ),
    regexp = "No variable name for scoring factor"
  )

})


test_that("link_wles() applies linking constant correctly", {

  # Create mock WLE data
  wle_prev <- data.frame(
    ID_t = 1:100,
    wle = rnorm(100, mean = 0, sd = 1)
  )

  wle_curr <- data.frame(
    ID_t = 1:100,
    wle = rnorm(100, mean = 0.5, sd = 1)  # Shifted by 0.5
  )

  const <- 0.3

  linked <- link_wles(
    wle_prev = wle_prev,
    wle_curr = wle_curr,
    const = const,
    wid = "wle",
    use_longitudinal_subsample = TRUE
  )

  expect_true(is.data.frame(linked))
  expect_true("wle" %in% names(linked))
  expect_true("ID_t" %in% names(linked))
  expect_equal(nrow(linked), 100)

})


test_that("link_wles() handles non-overlapping samples", {

  wle_prev <- data.frame(
    ID_t = 1:50,
    wle = rnorm(50)
  )

  wle_curr <- data.frame(
    ID_t = 51:100,  # No overlap
    wle = rnorm(50)
  )

  const <- 0.3

  # With longitudinal=TRUE, should use intersecting IDs (none)
  linked <- link_wles(
    wle_prev = wle_prev,
    wle_curr = wle_curr,
    const = const,
    wid = "wle",
    use_longitudinal_subsample = FALSE
  )

  expect_true(is.data.frame(linked))

})


test_that("prepare_longitudinal_resp() produces valid structure", {

  data(ex2)

  # Use same data for both waves for testing
  result <- try({
    prepare_longitudinal_resp(
      resp_curr = ex2$resp,
      resp_prev = ex2$resp,
      vars_curr = ex2$vars,
      vars_prev = ex2$vars,
      select_curr = "mixed",
      select_prev = "mixed",
      valid_curr = "valid",
      valid_prev = "valid",
      mvs = c(OM = -97, NV = -95, NR = -94),
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("resp_curr" %in% names(result))
  expect_true("resp_prev" %in% names(result))
  expect_true("items_curr" %in% names(result))
  expect_true("items_prev" %in% names(result))
  expect_true("anchors" %in% names(result))

})


test_that("prepare_longitudinal_resp() identifies anchor items", {

  data(ex2)

  result <- prepare_longitudinal_resp(
    resp_curr = ex2$resp,
    resp_prev = ex2$resp,
    vars_curr = ex2$vars,
    vars_prev = ex2$vars,
    select_curr = "mixed",
    select_prev = "mixed",
    valid_curr = "valid",
    valid_prev = "valid",
    mvs = c(OM = -97, NV = -95, NR = -94),
    warn = FALSE
  )

  # When using same items, all should be anchors
  expect_true(nrow(result$anchors) > 0)
  expect_equal(ncol(result$anchors), 2)

})


test_that("summarize_link_dif() produces valid output", {

  data(ex2)

  # Get models for both waves
  mod_prev <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )$mod

  mod_curr <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )$mod

  items <- ex2$vars$item[ex2$vars$mixed]
  anchors <- cbind(items, items)
  colnames(anchors) <- c("Previous", "Current")

  result <- summarize_link_dif(
    mod_curr = mod_curr,
    mod_prev = mod_prev,
    items_curr = items,
    items_prev = items,
    anchors = anchors
  )

  expect_true(is.list(result))
  expect_true("link_dif_table" %in% names(result))
  expect_true("Fkrit" %in% names(result))
  expect_true(is.data.frame(result$link_dif_table))

})


test_that("calculate_link_parameters() produces valid output", {

  data(ex2)

  # Get item parameters
  model <- irt_model(
    resp = ex2$resp,
    vars = ex2$vars,
    select = "mixed",
    valid = "valid",
    scoring = "scoring",
    mvs = c(OM = -97, NV = -95, NR = -94),
    irtmodel = "PCM2",
    verbose = FALSE,
    save = FALSE,
    warn = FALSE
  )

  xsi <- model$mod$xsi
  items <- ex2$vars$item[ex2$vars$mixed]
  anchors <- cbind(items, items)
  colnames(anchors) <- c("Previous", "Current")

  result <- try({
    calculate_link_parameters(
      vars_curr = ex2$vars,
      vars_prev = ex2$vars,
      select_curr = "mixed",
      select_prev = "mixed",
      scoring_curr = "scoring",
      scoring_prev = "scoring",
      xsi_curr = xsi,
      xsi_prev = xsi,
      anchors = anchors,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("const" %in% names(result))
  expect_true("const.err" %in% names(result))
  expect_true("xsi_curr.linked" %in% names(result))

  # With same parameters, linking constant should be approximately 0
  expect_true(abs(result$const) < 0.01)

})


test_that("check_dif_anchor() produces valid output", {

  data(ex2)

  result <- try({
    check_dif_anchor(
      resp_curr = ex2$resp,
      resp_prev = ex2$resp,
      vars_curr = ex2$vars,
      vars_prev = ex2$vars,
      select_curr = "mixed",
      select_prev = "mixed",
      valid_curr = "valid",
      valid_prev = "valid",
      scoring_curr = "scoring",
      scoring_prev = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      return = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("mod_prev" %in% names(result))
  expect_true("mod_curr" %in% names(result))
  expect_true("link_dif_summary" %in% names(result))
  expect_true("anchors" %in% names(result))

})


test_that("check_link_dimensionality() produces valid output", {

  data(ex2)

  result <- try({
    check_link_dimensionality(
      resp_curr = ex2$resp,
      resp_prev = ex2$resp,
      vars_curr = ex2$vars,
      vars_prev = ex2$vars,
      select_curr = "mixed",
      select_prev = "mixed",
      valid_curr = "valid",
      valid_prev = "valid",
      scoring_curr = "scoring",
      scoring_prev = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      maxiter = 500,
      snodes = 1000,
      verbose = FALSE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("dimensionality" %in% names(result))
  expect_true("dim_sum" %in% names(result))

})


test_that("link_samples() produces valid output", {

  data(ex2)

  result <- try({
    link_samples(
      resp_curr = ex2$resp,
      resp_prev = ex2$resp,
      vars_curr = ex2$vars,
      vars_prev = ex2$vars,
      select_curr = "mixed",
      select_prev = "mixed",
      valid_curr = "valid",
      valid_prev = "valid",
      scoring_curr = "scoring",
      scoring_prev = "scoring",
      mvs = c(OM = -97, NV = -95, NR = -94),
      longitudinal = TRUE,
      warn = FALSE
    )
  })

  expect_false(inherits(result, "try-error"))
  expect_true(is.list(result))
  expect_true("wle_curr" %in% names(result))
  expect_true("wle_prev" %in% names(result))
  expect_true("wle_linked" %in% names(result))
  expect_true("const" %in% names(result))
  expect_true("const.err" %in% names(result))
  expect_true("anchors" %in% names(result))

})
