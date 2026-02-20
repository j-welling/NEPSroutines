library(testthat)

################################################################################

test_that("mv_item() works", {

  # Load data
  data(b129)
  list_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/mv_item.xlsx",
                                 sheet = "list")
  summary_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/mv_item.xlsx",
                                    sheet = "summary_all")

  # Run function
  mv_item(resp = resp, vars = vars, select = "mixed", valid = "valid",
          grouping = c("easy", "difficult"),
          position = c(easy = "position_easy_mixed", difficult = "position_difficult_mixed"),
          mvs = mvs, labels_mvs = labels_mvs,
          suf_item_names = TRUE,
          save = TRUE, overwrite = TRUE, return = FALSE,
          path_results = "tests/testthat/fixtures/results_curr",
          path_table = "tests/testthat/fixtures/tables_curr",
          show_all = FALSE,
          warn = TRUE, verbose = FALSE, print = FALSE)
  list_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/mv_item.xlsx",
                                 sheet = "list")
  summary_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/mv_item.xlsx",
                                    sheet = "summary_all")

  #----------------------------------------------------------
  # Check structure of list sheet
  expect_true(
    all(names(list_new) %in% names(list_old)),
    info = "Column names of list sheet differ between versions"
  )

  #----------------------------------------------------------
  # Compare each common column in list sheet
  common_parts <- intersect(names(list_new), names(list_old))
  all_passed <- TRUE

  for (part_name in common_parts) {
    col_new <- list_new[[part_name]]
    col_old <- list_old[[part_name]]

    diffs <- which(
      col_new != col_old | xor(is.na(col_new), is.na(col_old)),
      arr.ind = TRUE
      )

    if (length(diffs) > 0) {
      all_passed <- FALSE
      cat("\nDifferences found in:", part_name, "\n")
      for (i in seq_len(nrow(diffs))) {
        r <- diffs[i, 1]
        c <- diffs[i, 2]
        cat(sprintf(
          "  Row: %-20s | Col: %-20s | old: %-10s | new: %-10s\n",
          rownames(col_old)[r],
          colnames(col_old)[c],
          as.character(col_old[r, c]),
          as.character(col_new[r, c])
          ))
      }
      fail(paste(nrow(diffs), "differences found in", part_name))
      } else {
        cat(paste("✔", part_name, "identical between versions\n"))
    }
  }

  # Final message if all identical
  if (all_passed) {
    cat("\n All components of mv_item() are identical between versions\n")
  }

  #----------------------------------------------------------
  # Check structure of summary sheet
  expect_true(
    all(names(summary_new) %in% names(summary_old)),
    info = "Colum names of summary sheet differ between versions"
  )

  #----------------------------------------------------------
  # Helper function for comparing summary sheet
  compare_summary <- function(summary_new, summary_old, name = "summary sheet") {

    common_cols <- intersect(
      colnames(summary_new), colnames(summary_old)
      )

    key_col <- colnames(summary_new)[1]
    common_rows <- intersect(summary_new[[key_col]], summary_old[[key_col]])

    new_aligned <- summary_new[match(common_rows, summary_new[[key_col]]), common_cols, drop = FALSE]
    old_aligned <- summary_old[match(common_rows, summary_old[[key_col]]), common_cols, drop = FALSE]

    diffs <- which(
      as.matrix(new_aligned[,-1]) != as.matrix(old_aligned[,-1]) |
        xor(is.na(as.matrix(new_aligned[,-1])), is.na(as.matrix(old_aligned[,-1]))),
      arr.ind = TRUE)

    if (length(diffs) > 0) {
      cat("\n Differences found in", name, "after alignment:\n")
      for (i in seq_len(nrow(diffs))) {
        r <- diffs[i, 1]
        c <- diffs[i, 2] + 1
        cat(sprintf(
          "  Row: %-15s | Col: %-10s | old: %-10s | new: %-10s\n",
          as.character(old_aligned[[1]][r]),
          colnames(old_aligned)[c],
          as.character(old_aligned[r, c]),
          as.character(new_aligned[r, c])
        ))
      }
      fail(paste(nrow(diffs), "differences found in", name))
    } else {
      cat(paste("✔", name, "identical between versions (after alignment)\n"))
    }
  }

  #----------------------------------------------------------
  # Compare summary sheet
  compare_summary(
    summary_new = summary_new,
    summary_old = summary_old,
    name = "mv_item summary_sheet"
  )
})
