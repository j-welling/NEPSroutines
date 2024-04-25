

test_that("dichotomous_scoring() works", {

  data(ex1)
  resp <- ex1$resp[ex1$resp$valid, c("ID_t", ex1$vars$item[ex1$vars$raw])]
  vars <- ex1$vars[ex1$vars$raw, ]

  resp_new <- tryCatch({
    dichotomous_scoring(resp = resp, vars = vars,
                        old_names = vars$item, correct = "correct")
  }, error = \(e) e, warning = \(w) w)

  expect_contains(colnames(resp_new), paste0(vars$item, "_c"))
  expect_equal(sum(resp_new[, paste0(vars$item, "_c")] -
               ex1$resp[ex1$resp$valid, paste0(vars$item, "_c")]), 0L)

})



test_that("pc_scoring() works", {

  poly_items <- list(
    "mag9100s_c" = c("mag91001_c", "mag91002_c", "mag91003_c"),
    "mag9200s_c" = c("mag92001_c", "mag92002_c")
  )
  resp <- data.frame(
    mag91001_c = rep(1, 10),
    mag91002_c = c(rep(1, 8), -97, -97),
    mag91003_c = c(rep(1, 8), -95, -97),
    mag92001_c = rep(c(0, 1), 5),
    mag92002_c = rep(1, 10)
  )

  expect_no_error( pc_scoring(resp = resp, poly_items = poly_items, warn = FALSE))
  expect_warning( pc_scoring(resp = resp, poly_items = poly_items, warn = TRUE))
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items, warn = FALSE)$mag9100s_c[9],
    -55)
  expect_equal(
    pc_scoring(resp = resp, poly_items = poly_items, warn = FALSE)$mag9100s_c[10],
    -97)
  expect_true(
    all(pc_scoring(resp = resp, poly_items = poly_items,
                   warn = FALSE)$mag9100s_c[-c(9:10)] == 3)
  )


})


