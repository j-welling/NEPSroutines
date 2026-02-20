library(testthat)

################################################################################

test_that("mv_person() works", {

  # Load data
  data(b129)
  result_old <- readRDS("tests/testthat/fixtures/results_prev/mv_person.rds")

  # Run function
  mv_person(resp = resp, vars = vars, select = "mixed", valid = "valid",
            grouping = c("easy", "difficult"),
            mvs = mvs, labels_mvs = labels_mvs,
            save = TRUE, overwrite = TRUE, return = FALSE,
            path_results = "tests/testthat/fixtures/results_curr",
            path_table = "tests/testthat/fixtures/tables_curr",
            show_all = FALSE,
            warn = TRUE, verbose = FALSE
            )
  result_new <- readRDS("tests/testthat/fixtures/results_curr/mv_person.rds")

  #----------------------------------------
  # Basic structure comparison
    expect_true(
      all(names(result_new$summary$all) %in% names(result_old$summary$all)),
      info = "Some sub-lists in new summary$all are missing in the old version"
    )

  #----------------------------------------
  # Loop over each sub-list and compare
  common_sublists <- intersect(names(result_new$summary$all),
                               names(result_old$summary$all))

  for (sublist_name in common_sublists[-6]) {

    sublist_new <- result_new$summary$all[[sublist_name]]
    sublist_old <- result_old$summary$all[[sublist_name]]

    diffs <- which(sublist_new != sublist_old |
                     xor(is.na(sublist_new), is.na(sublist_old)),
                   arr.ind = TRUE)
    if (length(diffs) > 0) {
      cat("\nDifferences found in summary$all element:", sublist_name, "\n")
      for (i in seq_len(nrow(diffs))) {
        r <- diffs[i, 1]
        c <- diffs[i, 2]
        cat(sprintf(
          "  Row: %-20s | Col: %-20s | old: %-8s | new: %-8s\n",
          rownames(sublist_old)[r],
          colnames(sublist_old)[c],
          as.character(sublist_old[r, c]),
          as.character(sublist_new[r, c])
          ))
      }
      fail(paste(nrow(diffs), "differences found in", sublist_name))
      } else {
      cat(paste("✔", sublist_name, "identical between versions\n"))
      }
  }

  expect_equal(
    result_new$summary$all$summary, result_old$summary$all$summary,
    info = "Sub-list summary differ between versions"
    )
})
