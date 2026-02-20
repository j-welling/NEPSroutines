library(testthat)

################################################################################

test_that("dichotomous_scoring works", {

  # Load data
  data(b129)
  inp_resp <- resp[resp$valid, c("ID_t", vars$item[vars$raw])]
  inp_vars <- vars[vars$raw, ]

  # Run the function
  resp_new <- expect_error(
    dichotomous_scoring(resp = inp_resp,
                        vars = inp_vars,
                        old_names = inp_vars$item,
                        correct = "correct_response"),
    NA
  )

  # -----------------------------------
  # Check the correctness of the function

  # Check that _c columns are created
  new_vars <- paste0(inp_vars$item, "_c")
  expect_true(
    all(new_vars %in% colnames(resp_new)),
    info = "All _c columns are created."
  )

  # Check that all values in _c variables are allowed (mvs + 0 and 1)
  allowed_values <- c(mvs, 0, 1)
  for (var in new_vars) {
    expect_true(
      all(resp_new[[var]] %in% allowed_values),
      info = paste("Variable", var, "contains disallowed values")
    )
  }

  # Check that _c columns are numeric
  for (var in new_vars) {
    expect_true(
      is.numeric(resp_new[[var]]),
      info = paste("Variable", var, "should be numeric")
    )
  }

  # -----------------------------
  # Compare to with previous version

  # Check that all _c columns are identical between the previous and current version
  resp_old <- resp[resp$valid, c("ID_t", vars$item[vars$dich])]
  expect_true(
    all(new_vars %in% colnames(resp_old)),
    info = "All _c columns are the same in the previous and current resp."
  )

  # Check that all values in the newly scored _c columns match the existing scored values
  expect_equal(
    sum(resp_new[, new_vars] -
          resp_old[, new_vars]),
    0L
  )
})


################################################################################

test_that("duplicate_items() works", {

  # Load data
  data(b129)
  inp_resp <- resp[resp$valid, c("ID_t", vars$item[vars$raw])]
  inp_vars <- vars[vars$raw, c("item", "raw") ]
  inp_vars$dich <- FALSE

  old_names <- inp_vars$item
  new_names <- paste0(inp_vars$item, "_c")

  # Run function
  vars_new <- duplicate_items(
    vars = inp_vars,
    old_names = old_names,
    new_names = new_names,
    change = list(raw = FALSE, dich = TRUE)
    )

  # -----------------------------
  # Check that number of rows increased correctly
  expect_equal(
    nrow(vars_new),
    nrow(inp_vars) + length(old_names),
    info = "Number of rows in vars_new is not old_names + duplicated rows"
  )

  # Check that new items are present
  expect_true(
    all(new_names %in% vars_new$item),
    info = "Not all new_names are present in vars_new"
  )

  # Order of new items matches old_names
  duplicated_rows <- vars_new[vars_new$item %in% new_names, ]
  expect_equal(
    duplicated_rows$item,
    new_names,
    info = "Duplicated rows are not in the same order as new_names"
    )

  # 'change' argument: Check that changes applied to new items
  expect_true(
    all(vars_new[vars_new$item %in% new_names, "raw"] == FALSE),
    info = "Values in column 'raw' are not set to FALSE for all new items"
    )
  expect_true(
    all(vars_new[vars_new$item %in% new_names, "dich"] == TRUE),
    info = "Values in column 'dich' are not set to TRUE for all new items"
    )

  # 'change' argument: Check that old items remain unchanged
  expect_equal(
    vars_new[vars_new$item %in% old_names, "raw"],
    vars[vars$item %in% old_names, "raw"],
    info = "Values in column 'raw' differ for old_names between vars and vars_new"
    )
  expect_equal(
    vars_new[vars_new$item %in% old_names, "dich"],
    vars[vars$item %in% old_names, "dich"],
    info = "Values in column 'dich' differ for old_names between vars and vars_new"
    )

  # Classes of columns 'raw' and 'dich' remain the same
  for (col in c("raw", "dich")) {
    expect_identical(
      class(vars_new[[col]]),
      class(vars[[col]]),
      info = paste("Column class for", col, "differs between vars and vars_new")
      )
  }

  # 'old_names' argument
  # Error handling: old_names not in vars
  expect_error(
    duplicate_items(vars,
                    old_names = c("not_in_vars"),
                    new_names = "X"),
    regexp = " is/are not included in vars! Please check again."
    )

  # -----------------------------
  # Compare to with previous version
  # Check that columns are identical between the previous and current version
  required_cols <- c("item", "raw", "dich")
  expect_true(
    all(required_cols %in% colnames(vars)),
    info = "Some required columns are missing in vars"
  )
  expect_true(
    all(required_cols %in% colnames(vars_new)),
    info = "Some required columns are missing in vars_new"
  )

  # Compare item names between old and new vars
  # --- Old items ---
  expect_true(
    all(old_names %in% vars$item),
    info = "Some old items are missing in vars_old"
  )
  expect_true(
    all(old_names %in% vars_new$item),
    info = "Some old items are missing in vars_new"
  )

  # --- New items ---
  expect_true(
    all(new_names %in% vars$item),
    info = "Some new items are missing in vars_old"
  )
  expect_true(
    all(new_names %in% vars_new$item),
    info = "Some new items are missing in vars_new"
  )

  # Compare values of 'raw' and 'dich' between old and new vars
  # --- Check old items ---
  old_vars_old <- vars[vars$item %in% old_names, c("item", "raw", "dich")]
  old_vars_new <- vars_new[vars_new$item %in% old_names, c("item", "raw", "dich")]

  expect_equal(
    old_vars_new$raw,
    old_vars_old$raw,
    info = "Column 'raw' differs for old items between old and new vars"
  )
  expect_equal(
    old_vars_new$dich,
    old_vars_old$dich,
    info = "Column 'dich' differs for old items between old and new vars"
  )

  # --- Check new items ---
  new_vars_old <- vars[vars$item %in% new_names, c("item", "raw", "dich")]
  new_vars_new <- vars_new[vars_new$item %in% new_names, c("item", "raw", "dich")]

  expect_equal(
    new_vars_new$raw,
    new_vars_old$raw,
    info = "Column 'raw' differs for new items between old and new vars"
  )
  expect_equal(
    old_vars_new$dich,
    old_vars_old$dich,
    info = "Column 'dich' differs for new items between old and new vars"
  )
})


################################################################################
# Optimize tests for non-imputed subitems (analogous to the imputed ones)

test_that("pc_scoring() works", {

  # Load data
  data(b129)
  resp_old <- resp[, !grepl("s_sc2g7_c_d", colnames(resp))]
  resp <- resp_old[, !grepl("s_sc2g7_c", colnames(resp_old))]
  poly_items <- list(reg7013s_sc2g7_c = paste0("reg7013", c(1:3), "_sc2g7_c"),
                     reg7015s_sc2g7_c = paste0("reg7015", c(1:2), "_sc2g7_c"),
                     reg7016s_sc2g7_c = paste0("reg7016", c(1:4), "_sc2g7_c"),
                     reg7023s_sc2g7_c = paste0("reg7023", c(1:3), "_sc2g7_c"),
                     reg7024s_sc2g7_c = paste0("reg7024", c(1:3), "_sc2g7_c"),
                     reg7026s_sc2g7_c = paste0("reg7026", c(1:5), "_sc2g7_c"),
                     reg7033s_sc2g7_c = paste0("reg7033", c(1:4), "_sc2g7_c"),
                     reg7045s_sc2g7_c = paste0("reg7045", c(1:3), "_sc2g7_c"),
                     reg7051s_sc2g7_c = paste0("reg7051", c(1:3), "_sc2g7_c"),
                     reg7053s_sc2g7_c = paste0("reg7053", c(1:3), "_sc2g7_c"),
                     reg7055s_sc2g7_c = paste0("reg7055", c(1:3), "_sc2g7_c"),
                     reg7063s_sc2g7_c = paste0("reg7063", c(1:3), "_sc2g7_c"),
                     reg7066s_sc2g7_c = paste0("reg7066", c(1:4), "_sc2g7_c"),
                     reg7071s_sc2g7_c = paste0("reg7071", c(1:3), "_sc2g7_c"),
                     reg7075s_sc2g7_c = paste0("reg7075", c(1:4), "_sc2g7_c"))

  # -------------------------
  # Error handling
  expect_error(
    pc_scoring(resp,
               poly_items = "notalist",
               save = FALSE, overwrite = FALSE),
    regexp = "The argument 'poly_items' must be a list."
  )
  expect_error(
    pc_scoring(resp,
               poly_items = poly_items,
               threshold = 2,
               save = FALSE, overwrite = FALSE),
    regexp = "The argument 'treshold' must be numeric in the interval between 0 and 1."
  )

  # -------------------------
  # Scoring without imputation
  resp_no_imputed <- pc_scoring(resp,
                                poly_items = poly_items,
                                impute = FALSE,
                                warn = FALSE,
                                save = FALSE, overwrite = FALSE)

  # Run without error when imputation is disabled
  expect_no_error(
    pc_scoring(resp = resp,
               poly_items = poly_items,
               impute = FALSE,
               warn = FALSE,
               save = FALSE, overwrite = FALSE)
  )

  # Produces a warning when warn = TRUE
  expect_warning(
    pc_scoring(resp = resp, poly_items = poly_items,
               impute = FALSE, warn = TRUE,
               save = FALSE, overwrite = FALSE)
  )

  # Warning if mvs is NULL
  expect_warning(
    pc_scoring(resp,
               poly_items = poly_items,
               impute = FALSE,
               mvs = NULL,
               warn = TRUE,
               save = FALSE, overwrite = FALSE),
    regexp = "No missing values provided."
  )

  # Check expected scoring values for specific items/rows
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[507], 0)
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[255], 1)
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[958], 2)
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[23], 3)
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[1], -54)
  expect_equal(resp_no_imputed$reg7013s_sc2g7_c[167], -97)

  # Check that new items were added
  expect_true(
    all(names(poly_items) %in% colnames(resp_no_imputed)),
    info = "Polytomously scored items were not added to 'resp'"
    )

  # -------------------------
  # Scoring with imputation
  resp_imputed <- pc_scoring(resp, vars, select = "dich",
                             poly_items = poly_items,
                             impute = TRUE, warn = FALSE,
                             path_results = "tests/testthat/fixtures/results_curr",
                             path_table = "tests/testthat/fixtures/tables_curr")

  # Run without error
  expect_no_error(
    pc_scoring(resp, vars, select = "dich",
               poly_items = poly_items,
               impute = TRUE, warn = FALSE,
               save = FALSE, overwrite = FALSE)
  )

  # Produces a warning when warn = TRUE
  expect_warning(
    pc_scoring(resp, vars, select = "dich",
               poly_items = poly_items,
               impute = TRUE, warn = TRUE,
               save = FALSE, overwrite = FALSE)
  )

  # Warning if mvs is NULL
  expect_warning(
    pc_scoring(resp, vars, select = "dich",
               poly_items = poly_items,
               impute = TRUE, mvs = NULL, warn = TRUE,
               save = FALSE, overwrite = FALSE),
    regexp = "No missing values provided."
  )

  # Check expected scoring values for specific items/rows
  expect_equal(resp_imputed$reg7013s_sc2g7_c[507], 0)
  expect_equal(resp_imputed$reg7013s_sc2g7_c[255], 1)
  expect_equal(resp_imputed$reg7013s_sc2g7_c[958], 2)
  expect_equal(resp_imputed$reg7013s_sc2g7_c[23], 3)
  expect_equal(resp_imputed$reg7013s_sc2g7_c[1], -54)
  expect_equal(resp_imputed$reg7013s_sc2g7_c[167], 2)

  # Check that new items were added
  expect_true(
    all(names(poly_items) %in% colnames(resp_imputed)),
    info = "Polytomously scored items were not added to 'resp'"
  )

  # -------------------------
  # Compare indicators of imputation in two versions
  ind_new <- readRDS("tests/testthat/fixtures/results_curr/pc_subitems_mv_indicators.rds")$indicators
  ind_old <- readRDS("tests/testthat/fixtures/results_prev/pc_subitems_mv_indicators.rds")$indicators
  subitems <- unlist(poly_items, use.names = FALSE)
  ind_new <- ind_new[, subitems, drop = FALSE]
  ind_old <- ind_old[, subitems, drop = FALSE]

  expect_true(
    identical(ind_new, ind_old),
    info = "Indicators differ in two versions"
  )

  # Compare summary of imputation in two versions
  summary_imp_new <- readRDS("tests/testthat/fixtures/results_curr/pc_subitems_mv_indicators.rds")$summary_items_impMV
  summary_imp_old <- readRDS("tests/testthat/fixtures/results_prev/pc_subitems_mv_indicators.rds")$summary_items_impMV
  summary_imp_new <- summary_imp_new[summary_imp_new$Item %in% names(poly_items), ]
  summary_imp_old <- summary_imp_old[summary_imp_old$Item %in% names(poly_items), ]

  expect_true(
    identical(summary_imp_new, summary_imp_new),
    info = "Summary of imputed values differ in two versions"
  )

  # -------------------------
  # Compare polytomously scored items in two versions
  any_diffs <- FALSE

  for (item in names(poly_items)) {
    diffs <- which(resp_old[[item]] != resp_imputed[[item]])

    if (length(diffs) > 0) {
      any_diffs <- TRUE
      cat("\nItem:", item, "\n")
      cat("Different at indices:", paste(head(diffs, 10), collapse = ", "), "\n")
      cat("Old:", paste(resp_old[[item]][head(diffs, 10)], collapse = ", "), "\n")
      cat("Imputed:", paste(resp_imputed[[item]][head(diffs, 10)], collapse = ", "), "\n")
    }
  }

  expect_false(any_diffs, info = "Some polytomous items differ between versions.")

})


################################################################################

test_that("collapse_response_categories() works", {

  # Load data
  data(b129)
  resp_old <- resp[, !grepl("s_sc2g7_c_d", colnames(resp))]
  resp <- resp_old[, !grepl("s_sc2g7_c", colnames(resp_old))]
  poly_items <- list(reg7013s_sc2g7_c = paste0("reg7013", c(1:3), "_sc2g7_c"),
                     reg7015s_sc2g7_c = paste0("reg7015", c(1:2), "_sc2g7_c"),
                     reg7016s_sc2g7_c = paste0("reg7016", c(1:4), "_sc2g7_c"),
                     reg7023s_sc2g7_c = paste0("reg7023", c(1:3), "_sc2g7_c"),
                     reg7024s_sc2g7_c = paste0("reg7024", c(1:3), "_sc2g7_c"),
                     reg7026s_sc2g7_c = paste0("reg7026", c(1:5), "_sc2g7_c"),
                     reg7033s_sc2g7_c = paste0("reg7033", c(1:4), "_sc2g7_c"),
                     reg7045s_sc2g7_c = paste0("reg7045", c(1:3), "_sc2g7_c"),
                     reg7051s_sc2g7_c = paste0("reg7051", c(1:3), "_sc2g7_c"),
                     reg7053s_sc2g7_c = paste0("reg7053", c(1:3), "_sc2g7_c"),
                     reg7055s_sc2g7_c = paste0("reg7055", c(1:3), "_sc2g7_c"),
                     reg7063s_sc2g7_c = paste0("reg7063", c(1:3), "_sc2g7_c"),
                     reg7066s_sc2g7_c = paste0("reg7066", c(1:4), "_sc2g7_c"),
                     reg7071s_sc2g7_c = paste0("reg7071", c(1:3), "_sc2g7_c"),
                     reg7075s_sc2g7_c = paste0("reg7075", c(1:4), "_sc2g7_c"))
  resp_imputed <- pc_scoring(resp, vars, select = "dich",
                             poly_items = poly_items,
                             impute = TRUE, warn = FALSE,
                             save = FALSE, overwrite = FALSE)
  vars     <- vars[!grepl("s_sc2g7_c_d", vars$item), ]

  # Run function
  resp_new <- collapse_response_categories(resp = resp_imputed,
                                           vars = vars, select = 'poly',
                                           per_cat = 200, save = TRUE,
                                           path_table = "tests/testthat/fixtures/tables_curr")

  #--------------------------------------------
  # Compare table with collapsed items in two versions
  collapsed_items_old <- openxlsx::read.xlsx("tests/testthat/fixtures/tables_prev/collapsed_items.xlsx")
  collapsed_items_new <- openxlsx::read.xlsx("tests/testthat/fixtures/tables_curr/collapsed_items.xlsx")

  expect_equal(
    colnames(collapsed_items_new), colnames(collapsed_items_old),
    info = "Column names differ between old and new collapsed_items"
    )

  expect_equal(
    collapsed_items_new, collapsed_items_old,
    info = "Values in collapsed_items differ between old and new versions"
    )

  # -------------------------------------------
  # Compare values for each collapsed variable in two versions
  for (var in collapsed_items_new$collapsed_item) {
    var_old <- gsub("_collapsed$", "", var)
    diffs <- which(resp_new[[var]] != resp_old[[var_old]])
    if (length(diffs) > 0) {
      cat("\nVariable:", var, "\n")
      cat("Different at indices:", paste(head(diffs, 10), collapse = ", "), "\n")
      cat("New:", paste(resp_new[[var]][head(diffs, 10)], collapse = ", "), "\n")
      cat("Old:", paste(resp_old[[var_old]][head(diffs, 10)], collapse = ", "), "\n")
    }
  }
})


################################################################################

test_that("calculate_num_cat() works", {

  # Load data
  data(b129)
  resp_old <- resp[, !grepl("s_sc2g7_c_d", colnames(resp))]
  resp <- resp_old[, !grepl("s_sc2g7_c", colnames(resp_old))]
  poly_items <- list(reg7013s_sc2g7_c = paste0("reg7013", c(1:3), "_sc2g7_c"),
                     reg7015s_sc2g7_c = paste0("reg7015", c(1:2), "_sc2g7_c"),
                     reg7016s_sc2g7_c = paste0("reg7016", c(1:4), "_sc2g7_c"),
                     reg7023s_sc2g7_c = paste0("reg7023", c(1:3), "_sc2g7_c"),
                     reg7024s_sc2g7_c = paste0("reg7024", c(1:3), "_sc2g7_c"),
                     reg7026s_sc2g7_c = paste0("reg7026", c(1:5), "_sc2g7_c"),
                     reg7033s_sc2g7_c = paste0("reg7033", c(1:4), "_sc2g7_c"),
                     reg7045s_sc2g7_c = paste0("reg7045", c(1:3), "_sc2g7_c"),
                     reg7051s_sc2g7_c = paste0("reg7051", c(1:3), "_sc2g7_c"),
                     reg7053s_sc2g7_c = paste0("reg7053", c(1:3), "_sc2g7_c"),
                     reg7055s_sc2g7_c = paste0("reg7055", c(1:3), "_sc2g7_c"),
                     reg7063s_sc2g7_c = paste0("reg7063", c(1:3), "_sc2g7_c"),
                     reg7066s_sc2g7_c = paste0("reg7066", c(1:4), "_sc2g7_c"),
                     reg7071s_sc2g7_c = paste0("reg7071", c(1:3), "_sc2g7_c"),
                     reg7075s_sc2g7_c = paste0("reg7075", c(1:4), "_sc2g7_c"))
  vars     <- vars[!grepl("s_sc2g7_c_d", vars$item), ]

  # Run function
  vars_new <- vars[,!grepl("num_cat", colnames(vars))]
  vars_new$num_cat <- calculate_num_cat(vars = vars_new,
                                        poly_items = poly_items,
                                        select_suf = 'suf')

  #---------------------------------------
  # Compare length of "num-cat" in two versions
  expect_equal(
    length(vars_new$num_cat), length(vars$num_cat),
    info = "Length of num_cat differ between old and new versions"
  )

  # Compare values for "num-cat" in two versions
  differences <- which(vars_new$num_cat != vars$num_cat |
                         xor(is.na(vars_new$num_cat), is.na(vars$num_cat)))

  if (length(differences) > 0) {
    cat("\nDifferences found in num_cat between versions:\n")
    for (i in differences) {
      cat(
        sprintf("  Item: %-25s | old: %-6s | new: %-6s\n",
                vars$item[i],
                ifelse(is.na(vars$num_cat[i]), "NA", vars$num_cat[i]),
                ifelse(is.na(vars_new$num_cat[i]), "NA", vars_new$num_cat[i]))
      )
    }
    fail(paste(length(differences), "items differ in num_cat"))
  } else {
    succeed("All num_cat values identical between versions")
  }
})


################################################################################

test_that("min_val() works", {

  # Load data
  data(b129)
  resp <- resp[, !grepl("s_sc2g7_c_d", colnames(resp))]
  vars <- vars[!grepl("s_sc2g7_c_d", vars$item), ]

  # Run function
  resp_new <- resp[, !grepl("valid", colnames(resp))]
  resp_new$valid <- min_val(resp = resp_new,
                            vars = vars,
                            select = 'mixed')

  # Create frequency tables for 'valid' indicator in both versions
  tab_new <- table(resp_new$valid, useNA = "always")
  tab_old <- table(resp$valid, useNA = "always")

  # Compare the two tables element by element
  if (!identical(tab_new, tab_old)) {

  # Build a small summary of the differences
  diff_df <- data.frame(
    value = names(tab_new),
    old = as.vector(tab_old),
    new = as.vector(tab_new),
    diff = as.vector(tab_new) - as.vector(tab_old)
    )

  # Print the differences to console for inspection
  cat("\nDifferences found in 'valid' indicator between versions:\n")
  print(diff_df)

  # Mark the test as failed with an informative message
  fail(paste(
    "Tables differ for 'valid' cases — see printed differences above"
    ))

  } else {
    # If tables are exactly identical, mark the test as successful
    succeed("Tables identical for 'valid' cases between versions")
    }
})

################################################################################

test_that("pos_new() works", {

  # Load data
  data(b129)
  resp <- resp[, !grepl("s_sc2g7_c_d", colnames(resp))]
  vars <- vars[!grepl("s_sc2g7_c_d", vars$item), ]

  # Run function
  vars_new <- vars
  #vars_new$position_mixed <- as.numeric(c(NA))
  vars_new <- pos_new(vars = vars_new,
                      select = "mixed",
                      position = "position_mixed")

  #----------------------------------------
  # Compare positions between old and new
  diff_position <- which(vars_new$position_mixed.y != vars$position_mixed |
                           xor(is.na(vars_new$position_mixed.y), is.na(vars$position_mixed)))

  if (length(diff_position) > 0) {
    cat("\nDifferences found in 'position_mixed':\n")
    for (i in diff_position) {
      cat(sprintf("  Item: %-25s | old: %-6s | new: %-6s\n",
                  vars$item[i],
                  ifelse(is.na(vars$position_mixed[i]), "NA", vars$position_mixed[i]),
                  ifelse(is.na(vars_new$position_mixed.y[i]), "NA", vars_new$position_mixed.y[i])
      ))
    }
    # Fail the test in testthat
    fail(paste(length(diff_position), "items differ in position_mixed"))
  } else {
    succeed("All position_mixed values identical between versions")
  }
})
