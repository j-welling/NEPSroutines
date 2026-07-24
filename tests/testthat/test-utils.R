
test_that("only_valid() works", {

  resp <- data.frame(var1 = runif(10), var2 = runif(10),
                     valid = c(rep(TRUE, 8), rep(FALSE, 2)))

  expect_no_error(only_valid(resp = resp, valid = NULL, warn = FALSE))
  expect_message(only_valid(resp = resp, valid = NULL, warn = TRUE),
                 regexp = "^No variable with valid cases provided.+")
  expect_no_message(only_valid(resp = resp, valid = NULL, warn = FALSE))
  expect_equal(nrow(only_valid(resp = resp, valid = NULL, warn = FALSE)), 10)
  expect_error(only_valid(resp = resp, valid = "something", warn = TRUE),
               regexp = "^Variable 'something' not found in 'resp'.+")
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
  expect_message(convert_mv(resp = resp, vars = vars, select = NULL,
                            mvs = NULL, warn = TRUE),
                 regexp = "^No user-defined missing values provided for item responses.+")
  expect_no_message(convert_mv(resp = resp, vars = vars, select = NULL,
                               mvs = NULL, warn = FALSE))

  # select is incorrect
  expect_error(convert_mv(resp = resp, vars = vars, select = "notdefined",
                          warn = FALSE),
               regexp = "^Variable 'notdefined' not found in 'vars'.+")
  expect_error(convert_mv(resp = resp, vars = vars, select = "incorrect",
                          warn = FALSE),
               regexp = "^Variable 'incorrect' in 'vars' is not logical.+")
  expect_error(convert_mv(resp = resp, vars = vars, select = "donotuse",
                          warn = FALSE),
               regexp = "^Variables? .+ not found in 'resp'.+")

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
               regexp = "^To create a data frame \\(resp\\) containing only the.+")

  # select not set or incorrect
  expect_message(prepare_resp(resp = resp, select = NULL, warn = TRUE),
                 regexp = "^No variable was provided that indicates which of the items.+")
  expect_no_message(prepare_resp(resp = resp, select = NULL, warn = FALSE))
  expect_error(prepare_resp(resp = resp, vars = vars, select = "dontuse"),
               regexp = "^Variable 'dontuse' not found in 'vars'.+")

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
  expect_no_message(check_folder(path))
  expect_message(check_folder(paste0(path, "/emptyfolder")))
  expect_true(dir.exists(paste0(path, "/emptyfolder")))

})


test_that("check_pid() works", {

  expect_error(check_pid(c(1:10, 10)))
  expect_no_error(check_pid(c(1:10, 11)))
  expect_warning(check_pid(c(1:10, NA)))

})


test_that("fmt_names() formats and truncates name lists", {

  expect_equal(fmt_names(character(0)), "<none>")
  expect_equal(fmt_names("x"), "'x'")
  expect_equal(fmt_names(c("a", "b")), "'a', 'b'")
  expect_equal(fmt_names(letters[1:5]), "'a', 'b', 'c', 'd', 'e'")
  expect_equal(fmt_names(letters[1:6]),
               "'a', 'b', 'c', 'd', 'e' and 1 more")
  expect_equal(fmt_names(letters[1:8]),
               "'a', 'b', 'c', 'd', 'e' and 3 more")

})


test_that("validation_msg() handles singular and plural", {

  single <- validation_msg("Variable", "x", "df", "is", "are", "not numeric.")
  expect_equal(single, "Variable 'x' in 'df' is not numeric.")

  plural <- validation_msg("Variable", c("x", "y"), "df", "is", "are",
                           "not numeric.")
  expect_equal(plural, "Variables 'x', 'y' in 'df' are not numeric.")

})


test_that("check_items() works", {

  expect_error(check_items(c(paste0("var", 1:10), "var10")),
               regexp = "Duplicate item names found in 'vars\\$item': 'var10'")
  expect_no_error(check_items(c(paste0("var", 1:10), "var11")))
  expect_error(check_items(c(paste0("var", 1:10), NA)),
               regexp = "Missing values \\(NA\\) found in 'vars\\$item'")

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
  expect_error(check_variables(df = df, name_df = "myname",
                               variables = c("var1", "var2", "var3")),
               regexp = "'var3' not found in 'myname'")
  # name_df defaults to "<unknown>" when omitted or NULL
  expect_error(check_variables(df = df, name_df = NULL,
                               variables = "missing_var"),
               regexp = "'missing_var' not found in '<unknown>'")
  # Plural branch: multiple missing variables
  expect_error(check_variables(df = df, name_df = "myname",
                               variables = c("var3", "var4")),
               regexp = "Variables 'var3', 'var4' not found in 'myname'")
  # Long list is truncated
  many <- paste0("z", 1:8)
  expect_error(check_variables(df = df, name_df = "myname", variables = many),
               regexp = "and 3 more")

})


test_that("check_logicals() works", {

  df <- data.frame(var1 = rep(TRUE, 10), var2 = 1:10,
                   var3 = c(rep(FALSE, 9), NA))

  expect_no_error(check_logicals(df = df, name_df = "myname",
                                 logicals = "var1"))
  expect_error(check_logicals(df = df, name_df = "myname",
                              logicals = "var2"),
               regexp = "'var2' in 'myname' is not logical")
  expect_warning(check_logicals(df = df, name_df = "myname",
                                logicals = "var3"),
                 regexp = "'var3' in 'myname' contains values other than TRUE/FALSE")
  expect_no_warning(check_logicals(df = df, name_df = NULL, warn = FALSE,
                                   logicals = "var3"))
  # Multiple non-logical variables trigger plural grammar
  df_multi <- data.frame(v1 = rep(TRUE, 5), v2 = 1:5, v3 = letters[1:5])
  expect_error(check_logicals(df = df_multi, name_df = "myname",
                              logicals = c("v2", "v3")),
               regexp = "Variables 'v2', 'v3' in 'myname' are not logical")

})


test_that("check_numerics() works", {

  df <- data.frame(var1 = rep(TRUE, 10), var2 = 1:10,
                   var3 = c(1:9, NA))

  expect_no_error(check_numerics(df = df, name_df = "myname",
                                 numerics = "var2"))
  expect_no_error(check_numerics(df = df, name_df = NULL,
                                 numerics = "var3"))
  expect_error(check_numerics(df = df, name_df = NULL,
                              numerics = "var1"),
               regexp = "'var1' in '<unknown>' is not numeric")
  expect_error(check_numerics(df = df, name_df = "myname"),
               regexp = "'var1' in 'myname' is not numeric")
  # Plural branch: multiple non-numeric variables
  df_multi <- data.frame(a = letters[1:5], b = rep(TRUE, 5), c = 1:5)
  expect_error(check_numerics(df = df_multi, name_df = "test",
                              numerics = c("a", "b")),
               regexp = "Variables 'a', 'b' in 'test' are not numeric")

})


test_that("check_invalid_values() works", {

  df <- data.frame(var1 = 100:109, var2 = c(NA, 1:9),
                   var3 = -2:7)

  expect_no_error(check_invalid_values(df = df, name_df = "myname",
                                       items = c("var1", "var2")))
  expect_error(check_invalid_values(df = df, name_df = NULL,
                                    items = "var3"),
               regexp = "Data frame '<unknown>' contains invalid values")
  expect_error(check_invalid_values(df = df, name_df = "myname"),
               regexp = "Data frame 'myname' contains invalid values")

  # Test that error message includes the invalid values (issue #47)
  df2 <- data.frame(var1 = c(-5, -2, 1:8))
  expect_error(check_invalid_values(df = df2, name_df = "myname",
                                    items = "var1"),
               regexp = "-5 and -2")

})


test_that("check_dich() works", {

  df <- data.frame(var1 = 1:10, var2 = c(NA, rep(1, 9)),
                   var3 = c(rep(1, 5), rep(0, 5)))

  expect_no_error(check_dich(df = df, name_df = "myname",
                             dich_items = c("var2", "var3")))
  expect_error(check_dich(df = df, name_df = "myname", dich_items = "var1"),
               regexp = "'var1' in 'myname' contains values > 1 \\(max: 'var1'=10\\)")
  expect_error(check_dich(df = df, name_df = "myname"),
               regexp = "'var1' in 'myname' contains values > 1")

  # Multi-item: both unlabelled max values and plural grammar tested
  df2 <- data.frame(a = 1:3, b = c(0, 2, 1))
  expect_error(check_dich(df = df2, name_df = "test", dich_items = c("a", "b")),
               regexp = "Items 'a', 'b' in 'test' contain values > 1")

  # All-NA column is caught rather than silently passing
  df_na <- data.frame(ok = c(0, 1, 0), bad = c(NA, NA, NA))
  expect_error(check_dich(df = df_na, name_df = "test", dich_items = "bad"),
               regexp = "'bad' in 'test' is entirely NA")
  # All-NA plural branch
  df_multi_na <- data.frame(a = c(NA, NA), b = c(NA, NA))
  expect_error(check_dich(df = df_multi_na, name_df = "test"),
               regexp = "Items 'a', 'b' in 'test' are entirely NA")

  # name_df defaults to "<unknown>"
  expect_error(check_dich(df = df, name_df = NULL, dich_items = "var1"),
               regexp = "'var1' in '<unknown>' contains values > 1")

})


test_that("check_max_zero() works", {

  df_ok    <- data.frame(item1 = c(0, 1, 0, 1), item2 = c(NA, 1, 0, 1))
  df_bad   <- data.frame(item1 = c(0, 1, 0, 1), item2 = c(0, 0, NA, 0))
  df_all_na <- data.frame(item1 = c(0, 1),       item2 = c(NA, NA))

  expect_no_error(check_max_zero(df_ok, "resp"))

  # item2 is flagged, message contains key phrase
  expect_error(check_max_zero(df_bad, "resp"), regexp = "item2")
  expect_error(check_max_zero(df_bad, "resp"), regexp = "maximum observed score of 0")

  # item1 is fine and must NOT appear in the error
  err_bad <- tryCatch(check_max_zero(df_bad, "resp"), error = conditionMessage)
  expect_false(grepl("item1", err_bad))

  # all-NA column is also caught
  expect_error(check_max_zero(df_all_na, "resp"), regexp = "item2")

  # group name is included when provided
  expect_error(check_max_zero(df_bad, "resp", name_group = "isRegular"),
               regexp = "group 'isRegular'")

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
  skip_if_not_installed("MASS")

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


test_that("create_suf_names() works", {

  varnames <- c(paste0("var", 1:2), paste0("var", 3:4, "_collapsed"))
  expect_equal(create_suf_names(vars_name = varnames), paste0("var", 1:4))
  expect_null(create_suf_names(vars_name = NULL))

})


test_that("irt_model() rejects invalid irtmodel argument", {

  # Minimal data — irtmodel check fires before data validation
  resp <- data.frame(ID_t = 1:3, x = c(0, 1, 0))
  vars <- data.frame(item = "x", use = TRUE)

  expect_error(irt_model(resp, vars, "use", irtmodel = "3PL", test = FALSE),
               regexp = "Invalid irtmodel '3PL'")
  expect_error(irt_model(resp, vars, "use", irtmodel = NULL, test = FALSE),
               regexp = "must be a single character string")
  expect_error(irt_model(resp, vars, "use", irtmodel = c("1PL", "2PL"),
                         test = FALSE),
               regexp = "must be a single character string")

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


test_that("parse_conditions() parses operators, values and connectors", {

  # single comparison; no logical connectors
  expect_equal(
    parse_conditions(">1.15"),
    list(operators = ">", values = 1.15, logicals = character(0))
  )

  # "=" is normalised to "=="; two-character operators are supported
  expect_equal(parse_conditions("=3")$operators, "==")
  expect_equal(parse_conditions(">=1.15")$operators, ">=")
  expect_equal(parse_conditions("<=1.2")$operators, "<=")
  expect_equal(parse_conditions("!=0")$operators, "!=")

  # multiple conditions: operators, values and connectors captured in order
  p <- parse_conditions("<1 | >1.2 & =1.1")
  expect_equal(p$operators, c("<", ">", "=="))
  expect_equal(p$values, c(1, 1.2, 1.1))
  expect_equal(p$logicals, c("|", "&"))

  # whitespace around operators and connectors is tolerated
  expect_equal(parse_conditions(" > 1.15 & <= 1.20 "),
               parse_conditions(">1.15&<=1.20"))

  # a vector of conditions is rejected with a helpful message
  expect_error(parse_conditions(c(">1.15", "<1.20")), "single condition")

  # an unknown operator keeps the historical error message
  expect_error(parse_conditions("+1"), "Unknown stat function.", fixed = TRUE)

  # the reversed forms => and =< are rejected with a hint (R uses >= / <=)
  expect_error(parse_conditions("=>1.15"), "did you mean '>='", fixed = TRUE)
  expect_error(parse_conditions("=<1.15"), "did you mean '<='", fixed = TRUE)

  # a malformed operator or missing number errors instead of coercing to NA
  expect_error(parse_conditions("><1"), "Invalid number", fixed = TRUE)
  expect_error(parse_conditions(">"), "Invalid number", fixed = TRUE)

})


test_that("eval_conditions() folds comparisons left-to-right", {

  x <- c(0.90, 1.00, 1.10, 1.16, 1.18, 1.25)

  expect_equal(eval_conditions(parse_conditions(">1.15"), x),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))

  # AND keeps only the in-band values
  expect_equal(eval_conditions(parse_conditions(">1.15 & <1.20"), x),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))

  # >= includes the boundary value (1.16)
  expect_equal(eval_conditions(parse_conditions(">=1.16"), x),
               c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))

  # spaces give the same result as no spaces
  expect_equal(eval_conditions(parse_conditions("<1 | >1.2"), x),
               eval_conditions(parse_conditions("<1|>1.2"), x))

})


test_that("neps_palette() returns the package-wide blue scheme", {

  # returns n hex colors from the shared 'Blues 2' palette
  expect_length(neps_palette(1), 1)
  expect_length(neps_palette(4), 4)
  expect_match(neps_palette(1), "^#[0-9A-Fa-f]{6}$")

  # single source of truth: identical to colorspace default (Blues 2)
  expect_equal(neps_palette(3), colorspace::sequential_hcl(3, palette = "Blues 2"))

})


test_that("check_color() validates or generates colors", {

  # NULL color falls back to the package palette
  expect_equal(check_color(NULL, 3), neps_palette(3))

  # user-supplied colors of matching length pass through untouched
  cols <- c("red", "green", "blue")
  expect_equal(check_color(cols, 3), cols)

  # mismatched length errors
  expect_error(check_color(c("red", "blue"), 3),
               regexp = "number of provided colors does not match")

})
