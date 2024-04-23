
test_that("only_valid() works", {

  resp <- data.frame(var1 = runif(10), var2 = runif(10),
                     valid = c(rep(TRUE, 8), rep(FALSE, 2)))

  expect_no_error(only_valid(resp = resp, valid = NULL, warn = FALSE))
  expect_warning(only_valid(resp = resp, valid = NULL, warn = TRUE),
                 regexp = "^No variable with valid cases provided.+")
  expect_no_warning(only_valid(resp = resp, valid = NULL, warn = FALSE))
  expect_equal(nrow(only_valid(resp = resp, valid = NULL, warn = FALSE)), 10)
  expect_error(only_valid(resp = resp, valid = "something", warn = TRUE),
               regexp = "^Data.frame resp does not include any variable.+")
  expect_equal(nrow(only_valid(resp = resp, valid = "valid", warn = TRUE)), 8)

})


test_that("convert_mv() works", {

  resp <- data.frame(
    var1 = c(runif(10), rep(c(-94, -97, -54), 2)),
    var2 = c(runif(4), rep(c(-94, -97, -54), 4)),
    var3 = runif(16)
  )
  vars <- data.frame(
    item = paste0("var", 1:5),
    use = c(rep(TRUE, 3), rep(FALSE, 2)),
    donotuse = TRUE,
    incorrect = 0
  )

  # mvs not set
  expect_warning(convert_mv(resp = resp, vars = vars, select = NULL,
                            mvs = NULL, warn = TRUE),
                 regexp = "^No user defined missing values provided.+")
  expect_no_warning(convert_mv(resp = resp, vars = vars, select = NULL,
                               mvs = NULL, warn = FALSE))

  # select is incorrect
  expect_error(convert_mv(resp = resp, vars = vars, select = "notdefined",
                          warn = FALSE),
               regexp = "^Data.frame vars does not include any variable with.+")
  expect_error(convert_mv(resp = resp, vars = vars, select = "incorrect",
                          warn = FALSE),
               regexp = "^Variable 'incorrect' in data.frame vars is no logical.+")
  expect_error(convert_mv(resp = resp, vars = vars, select = "donotuse",
                          warn = FALSE),
               regexp = "^Data.frame resp does not include any variable with.+")

  # all values < 0 replaced with NA
  expect_equal(sum(convert_mv(resp = resp, vars = vars, select = "use",
                              warn = FALSE) < 0, na.rm = TRUE), 0)
  expect_equal(sum(is.na(convert_mv(resp = resp, vars = vars, select = "use",
                                    warn = FALSE))), 18)

  # only -97 replaced with NA
  expect_equal(sum(convert_mv(resp = resp, vars = vars, select = "use",
                              mvs = -97) < 0, na.rm = TRUE), 12)
  expect_equal(sum(is.na(convert_mv(resp = resp, vars = vars, select = "use",
                                    mvs = -97))), 6)

})


test_that("prepare_resp() works", {

  resp <- data.frame(
    var1 = c(runif(10), rep(c(-94, -97, -54), 2)),
    var2 = c(runif(4), rep(c(-94, -97, -54), 4)),
    var3 = runif(16),
    valid = c(rep(FALSE, 2), rep(TRUE, 14))
  )
  vars <- data.frame(
    item = paste0("var", 1:5),
    use = c(rep(TRUE, 3), rep(FALSE, 2)),
    donotuse = TRUE,
    incorrect = 0
  )

  # vars is missing
  expect_error(prepare_resp(resp = resp, select = "use"),
               regexp = "^To create a data frame \\(resp\\) with only the.+")

  # select not set or incorrect
  expect_warning(prepare_resp(resp = resp, select = NULL, warn = TRUE),
                 regexp = "^No variable provided indicating the items.+")
  expect_no_warning(prepare_resp(resp = resp, select = NULL, warn = FALSE))
  expect_error(prepare_resp(resp = resp, vars = vars, select = "dontuse"),
               regexp = "^Data.frame vars does not include any variable.+")

  # all correctly set
  expect_equal(dim(prepare_resp(resp = resp, vars = vars, select = "use",
                                use_only_valid = TRUE, valid = "valid")),
               c(14, 3))

})


test_that("save_results() works", {

  resp <- data.frame(one = rep(1, 10), two = rep(2, 10))
  path <- withr::local_tempdir()

  expect_no_error(save_results(results = resp, filename = "test.rds",
                               path = path))
  expect_true(file.exists(paste0(path, "/test.rds")))
  expect_true(identical(
    readRDS(paste0(path, "/test.rds")),
    resp
  ))

})


test_that("save_table() works", {

  resp <- data.frame(one = rep(1, 10), two = rep(2, 10))
  path <- withr::local_tempdir()

  expect_no_error(save_table(results = resp, filename = "test.xlsx",
                             path = path, overwrite = TRUE,
                             show_rownames = FALSE))
  expect_true(file.exists(paste0(path, "/test.xlsx")))
  expect_true(identical(
    openxlsx::read.xlsx(paste0(path, "/test.xlsx")),
    resp
  ))

})


test_that("check_folder() works", {

  path <- withr::local_tempdir()

  expect_no_error(check_folder(path))
  expect_no_warning(check_folder(path))
  expect_warning(check_folder(paste0(path, "/emptyfolder")))
  expect_true(dir.exists(paste0(path, "/emptyfolder")))

})


test_that("check_pid() works", {

  expect_error(check_pid(c(1:10, 10)))
  expect_no_error(check_pid(c(1:10, 11)))
  expect_warning(check_pid(c(1:10, NA)))

})


test_that("check_items() works", {

  expect_error(check_items(c(paste0("var", 1:10), "var10")))
  expect_no_error(check_items(c(paste0("var", 1:10), "var11")))
  expect_error(check_items(c(paste0("var", 1:10), NA)))

})


test_that("check_variables() works", {

  df <- data.frame(var1 = 1:10, var2 = 1:10)

  expect_no_error(check_variables(df = df, name_df = "myname",
                                  variables = NULL))
  expect_no_error(check_variables(df = df, name_df = "myname",
                                  variables = c("var1", "var2")))
  expect_no_error(check_variables(df = df, name_df = NULL,
                                  variables = c("var1", "var2")))
  expect_no_error(check_variables(df = df, name_df = "myname",
                                  variables = "var1"))
  expect_error(check_variabes(df = df, name_df = "myname",
                              variables = c("var1", "var2", "var3")))

})


test_that("check_logicals() works", {

  df <- data.frame(var1 = rep(TRUE, 10), var2 = 1:10,
                   var3 = c(rep(FALSE, 9), NA))

  expect_no_error(check_logicals(df = df, name_df = "myname",
                                 logicals = "var1"))
  expect_error(check_logicals(df = df, name_df = "myname",
                              logicals = "var2"))
  expect_warning(check_logicals(df = df, name_df = NULL,
                                logicals = "var3"))
  expect_no_warning(check_logicals(df = df, name_df = NULL, warn = FALSE,
                                   logicals = "var3"))
  expect_error(check_logicals(df = df, name_df = "myname",
                              logicals = c("var1", "var2", "var3")))

})


test_that("check_numerics() works", {

  df <- data.frame(var1 = rep(TRUE, 10), var2 = 1:10,
                   var3 = c(1:9, NA))

  expect_no_error(check_numerics(df = df, name_df = "myname",
                                 numerics = "var2"))
  expect_no_error(check_numerics(df = df, name_df = NULL,
                                 numerics = "var3"))
  expect_error(check_numerics(df = df, name_df = NULL,
                              numerics = "var1"))
  expect_error(check_numerics(df = df, name_df = "myname"))

})


test_that("check_invalid_values() works", {

  df <- data.frame(var1 = 100:109, var2 = c(NA, 1:9),
                   var3 = -2:7)

  expect_no_error(check_invalid_values(df = df, name_df = "myname",
                                       items = c("var1", "var2")))
  expect_error(check_invalid_values(df = df, name_df = NULL,
                                    items = "var3"))
  expect_error(check_invalid_values(df = df, name_df = "myname"))

})


test_that("check_dich() works", {

  df <- data.frame(var1 = 1:10, var2 = c(NA, rep(1, 9)),
                   var3 = c(rep(1, 5), rep(0, 5)))

  expect_no_error(check_dich(df = df, name_df = "myname",
                             dich_items = c("var2", "var3")))
  expect_error(check_dich(df = df, name_df = NULL, items = "var1"))
  expect_error(check_dich(df = df, name_df = "myname"))

})


test_that("reached_maxiter() works", {

  mod <- readRDS(test_path("fixtures/ex2/results/irt_poly.rds"))
  mod <- mod$model.pcm$mod

  expect_no_error(reached_maxiter(mod, "My Model"))
  expect_no_warning(reached_maxiter(mod, "My Model"))
  expect_equal(reached_maxiter(mod, "My Model"), NULL)
  mod$control$maxiter <- 1
  expect_warning(reached_maxiter(mod, "My Model"),
                 regexp = "^Maximum number of iterations were reached.+")

})


test_that("is_poly() works", {

  data(ex2)
  expect_false(is_poly(ex2$resp, ex2$vars, "dich"))
  expect_true(is_poly(ex2$resp, ex2$vars, "mixed"))
  ex2$resp$mag120001_c[1:100] <- NA
  expect_true(is_poly(ex2$resp, ex2$vars, "mixed"))

})


test_that("create_q() works", {

  data(ex2)
  expect_equal(
    suppressWarnings(create_q(
      vars = ex2$vars, select = "dich", scoring = NULL, poly = TRUE
    )),
    NULL)
  expect_warning(
    create_q(vars = ex2$vars, select = "dich", scoring = NULL, poly = TRUE),
    regexp = "^No variable name for scoring factor.+"
  )
  expect_true(
    all(create_q(vars = ex2$vars, select = "dich", scoring = "scoring",
                 poly = FALSE) == 1),
  )
  expect_equal(
    sum(create_q(vars = ex2$vars, select = "mixed", scoring = "scoring",
                 poly = FALSE) == 0.5),
    4
  )

})


test_that("create_name() works", {

  expect_equal(create_name(start = "first"), "first")
  expect_equal(create_name(start = "first", end = "second"), "firstsecond")
  expect_equal(create_name(start = "first", end = "second",
                           name_group = "group", sep = "."),
               "first.groupsecond")

})


# argument include_step incorrect?
test_that("order_xsi_fixed() works", {

  data(ex2)
  resp <- ex2$resp[ex2$resp$valid, c("mag120001_c", "mag120002_c", "mag120003_c")]
  resp[resp < 0] <- NA

  xsi_fixed <- c("mag120002_c" = -0.5,
                 "mag120001_c" = 0,
                 "mag120003_c" = 0.5,
                 "mag120003_c_step1" = 1)
  expect_equal(
    order_xsi_fixed(xsi_fixed = xsi_fixed, resp = resp, irtmodel = "PCM2"),
    matrix(c(2, 1, 3, 4, -0.5, 0, 0.5, 1), ncol = 2,
           dimnames = list(names(xsi_fixed)))
  )

  xsi_fixed <- c("mag120002_c_Cat1" = -0.5,
                 "mag120001_c_Cat1" = 0,
                 "mag120003_c_Cat1" = 0.5,
                 "mag120003_c_Cat3" = 1)
  expect_equal(
    order_xsi_fixed(xsi_fixed = xsi_fixed, resp = resp, irtmodel = "GPCM"),
    matrix(c(2, 1, 3, 5, -0.5, 0, 0.5, 1), ncol = 2,
           dimnames = list(names(xsi_fixed)))
  )

  xsi_fixed <- c("mag120002_c" = -0.5,
                 "mag120001_c" = 0,
                 "mag120003_c" = 0.5,
                 "mag120004" = 1)
  expect_error(
    order_xsi_fixed(xsi_fixed = xsi_fixed, resp = resp, irtmodel = "PCM2")
  )

})


# Argument resp wird nie verwendet, vars wird immer nur als vector verwendet
test_that("create_suf_names() works", {

  varnames <- c(paste0("var", 1:2), paste0("var", 3:4, "_collapsed"))
  expect_error(create_suf_names(vars = varnames, resp = "resp"))
  expect_equal(create_suf_names(vars = varnames), paste0("var", 1:4))

})


test_that("describe() works", {

  data <- data.frame(
    sex = factor(rep(1:2, 5), labels = c("male", "female")),
    age = seq(10, 100, 10),
    weight = c(seq(60, 90, 4), NA, NA)
  )

  expect_equal(dim(describe(data)), c(4, 6))
  expect_contains(describe(data)$n, c(8, 10))
  expect_equal(rownames(describe(data)),
               c("age", "weight", "sexmale", "sexfemale"))

})


test_that("rnd() works", {

  expect_equal(rnd(0.1459, digits = 3), "0.146")
  expect_equal(rnd(99), "99.00")
  expect_equal(rnd(c(0.042, 0.1459)), c("0.04", "0.15"))
  expect_equal(rnd(c(0.042, -0.1459), d0 = TRUE), c(".04", "-.15"))

})
