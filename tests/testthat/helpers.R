#' Compare two data tables from different package versions
#'
#' Performs a two-step comparison:
#' 1) Quick global object comparison
#' 2) Detailed comparison of columns, rows, and cell values (if differences found)
#'
#' @param tab_new Data frame from the new package version
#' @param tab_old Data frame from the old package version
#' @param analysis_type Character, type of analysis (e.g., "irt_dich", "dif", "distractors")
#' @param table_name Character, name of the result table (e.g., "summary", "descriptives", "model_fit")
#' @param key_column Character, key column containing names of rows (e.g., "Item")
#' @param tolerance Numeric, allowed numeric tolerance (default = 1e-8)
#' @return A list with `status` (TRUE/FALSE) and `details` (differences summary)
#' @export
compare_table_objects <- function(tab_new,
                                  tab_old,
                                  analysis_type = NULL,
                                  table_name = NULL,
                                  key_column = NULL,
                                  tolerance = 1e-8) {
  # --- Input validation ---
  if (!is.data.frame(tab_new)) tab_new <- as.data.frame(tab_new)
  if (!is.data.frame(tab_old)) tab_old <- as.data.frame(tab_old)

  message("🔍 Global object comparison for ", table_name, " (", analysis_type, ") ...")

  # --- Global object comparison ---
  comp_result <- all.equal(tab_new, tab_old, tolerance = tolerance, check.attributes = TRUE)

  if (isTRUE(comp_result)) {
    message("✅ Tables are fully identical.")
    return(invisible(list(status = TRUE, details = NULL)))
  } else {
    message("⚠️ Differences detected in global comparison.")
  }

  # --- Determine key column ---
  if (is.null(key_column)) {
    if (analysis_type == "irt_dich" && table_name %in% c("summary")) {
      key_column <- "Item"
    } else if (analysis_type == "irt_dich" && table_name %in% c("model_fit", "steps")) {
      key_column <- NULL
    } else if ("item" %in% names(tab_new)) {
      key_column <- "item"
    } else if ("...1" %in% names(tab_new)) {
      key_column <- "...1"
    }
  }

  message("\n🔎 Detailed comparison for ", table_name, " (", analysis_type, ") ...")

  result <- compare_tables(tab_new = tab_new,
                           tab_old = tab_old,
                           table_name = table_name,
                           key_column = key_column,
                           tolerance = tolerance)

  return(invisible(result))
}


#' Detailed comparison of two data frames
#'
#' @return A list with:
#'   - `status`: TRUE/FALSE
#'   - `differences`: Data frame with detected differences
#'   - `summary`: Text summary of comparison
#' @export
compare_tables <- function(tab_new, tab_old, table_name, key_column, tolerance) {
  # ------------------------------------------------
  # 1. Column comparison
  cols_new <- names(tab_new)
  cols_old <- names(tab_old)
  cols_common <- intersect(cols_new, cols_old)

  summary_log <- character()

  if (setequal(cols_new, cols_old)) {
    summary_log <- c(summary_log, "✅ Column names match.")
  } else {
    summary_log <- c(summary_log, "⚠️ Column sets differ — aligning by shared columns only.")
  }

  # Align by shared columns
  tab_new <- tab_new[, cols_common, drop = FALSE]
  tab_old <- tab_old[, cols_common, drop = FALSE]

  # ------------------------------------------------
  # 2. Row alignment
  if (!is.null(key_column) && key_column %in% names(tab_new) && key_column %in% names(tab_old)) {
    rows_new <- as.character(tab_new[[key_column]])
    rows_old <- as.character(tab_old[[key_column]])
  } else {
    rows_new <- rownames(tab_new)
    rows_old <- rownames(tab_old)
    key_column <- NULL
  }

  rows_common <- intersect(rows_new, rows_old)

  if (setequal(rows_new, rows_old)) {
    summary_log <- c(summary_log, "✅ Row identifiers match.")
  } else {
    summary_log <- c(summary_log, "⚠️ Row identifiers differ — aligning by shared rows only.")
  }

  if (!is.null(key_column)) {
    tab_new <- tab_new[match(rows_common, tab_new[[key_column]]), , drop = FALSE]
    tab_old <- tab_old[match(rows_common, tab_old[[key_column]]), , drop = FALSE]
  } else {
    tab_new <- tab_new[match(rows_common, rownames(tab_new)), , drop = FALSE]
    tab_old <- tab_old[match(rows_common, rownames(tab_old)), , drop = FALSE]
  }

  # ------------------------------------------------
  # 3. Cell-by-cell comparison
  all_identical <- TRUE
  diff_list <- list()

  for (col in cols_common) {
    x_new <- tab_new[[col]]
    x_old <- tab_old[[col]]

    if (is.numeric(x_new) && is.numeric(x_old)) {
      diff_idx <- which(abs(x_new - x_old) > tolerance | xor(is.na(x_new), is.na(x_old)))
    } else {
      diff_idx <- which(x_new != x_old | xor(is.na(x_new), is.na(x_old)))
    }

    if (length(diff_idx) > 0) {
      all_identical <- FALSE
      summary_log <- c(summary_log, sprintf("❌ %d differences in column '%s'.", length(diff_idx), col))
      diff_list[[col]] <- data.frame(
        Column = col,
        Row = diff_idx,
        Old = x_old[diff_idx],
        New = x_new[diff_idx],
        stringsAsFactors = FALSE
      )
    } else {
      summary_log <- c(summary_log, "✅ Row identifiers match." )
    }
  }

  differences_df <- if (length(diff_list) > 0) do.call(rbind, diff_list) else NULL

  # ------------------------------------------------
  # 4. Summary + Return
  if (all_identical) {
    message("✅ Table is fully identical between versions.")
    status <- TRUE
  } else {
    message("❌ Differences detected in ", table_name, " table.")
    status <- FALSE
  }

  message(paste(summary_log, collapse = "\n"))
  message("──────────────────────────────────────────────")

  return(list(
    status = status,
    summary = summary_log,
    differences = differences_df
  ))
}
