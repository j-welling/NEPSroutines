#' Select only valid cases
#'
#' @param resp  data.frame with item responses
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no argument 'valid' is provided
#'
#' @return resp without invalid cases.
#' @export

only_valid <- function(resp, valid = NULL, warn = TRUE) {

  if (!is.null(valid)) {
      check_logicals(resp, "resp", valid, warn = warn)
      resp <- resp[resp[[valid]], ]
  } else if (warn) {
      message("No variable with valid cases provided. ",
              "All cases are used for analysis.")
  }

  return(resp)
}


#' Convert user-defined missing values to NAs
#'
#' User defined missing values (usually coded as negative numbers) must be
#' converted to NAs so that they do not break the IRT analysis
#'
#' @param resp  data.frame containing the item responses
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param mvs  named integer vector; contains user-defined missing values
#' @param warn logical; whether warnings are to be printed to the console
#'
#' @return resp without user-defined missing values.
#' @export

convert_mv <- function(resp, vars, select = NULL, mvs = NULL, warn = TRUE) {

  if (is.null(mvs)) {

    mvs <- -999:-1

    if (warn) {
      message("No user-defined missing values provided for item responses. ",
              "Default of '-999 to -1' is used.")
    }
  }

  if (is.null(select)) {
      items <- names(resp)
  } else {
      check_logicals(vars, "vars", select)
      items <- vars$item[vars[[select]]]
      check_variables(resp, "resp", variables = items)
  }

  for (i in items) {
    resp[[i]] <- base::replace(resp[[i]], resp[[i]] %in% mvs, NA)
  }

  return(resp)
}


#' Prepare resp for analysis
#'
#' @param resp  data.frame with item responses
#' @param use_only_valid logical; whether to check valid cases
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param convert  logical; whether to convert custom missing values to NA
#' @param mvs  named integer vector; contains user-defined missing values
#' @param warn logical; whether warnings are to be printed to the console
#' @param zap_labels logical; whether to convert haven_labelled to normal
#'
#' @return resp with some changes, depending on function arguments.
#'
#' @export

prepare_resp <- function(
    resp,
    vars = NULL,
    select = NULL,
    use_only_valid = FALSE,
    valid = NULL,
    convert = FALSE,
    mvs = NULL,
    warn = TRUE,
    zap_labels = TRUE
  ) {

    # Use only valid cases
    if (use_only_valid) {
      resp <- only_valid(
        resp = resp,
        valid = valid,
        warn = warn
      )
    }

    # Select only certain variables
    if (!is.null(select)) {
        if (is.null(vars)) {
            stop("To create a data frame (resp) with only the indicated items, ",
                 "please also provide vars.")
        } else {
            check_logicals(vars, "vars", select, warn = warn)
            items <- vars$item[vars[[select]]]
            check_variables(resp, "resp", variables = items)
            resp <- resp[ , items]
        }
    } else if (warn) {
        message("No variable provided indicating the items to keep. ",
                "All items are kept.")
    }

    # Convert missing values to NA
    if (convert) resp <- convert_mv(resp = resp, mvs = mvs, warn = warn)

    # Zap labels of variables
    if (zap_labels) {
      resp <- haven::zap_labels(resp)
    }

    # Return resp
    return(resp)
}


#' Warning message if mvs is not provided
#'
#' @param mvs named numeric vector; contains user defined missing values
#' @param valid string; defines name of variable in resp indicating valid cases
#' @returns NULL invisibly
#' @noRd

is_null_mvs_valid <- function(mvs = NA, valid = NA) {
  if (is.null(mvs)) {
    message("No user-defined missing values provided for item responses. ",
            "Default of '-999 to -1' is used.")
  }

  if (is.null(valid)) {
    message("No variable with valid cases provided. ",
            "All cases are used for analysis.")
  }
  return(invisible())
}

#' Save table
#'
#' @param results table to be saved
#' @param filename string; defines name of table file
#' @param path string; indicates the folder location where the tables
#' are stored on the hard drive
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param show_rownames logical; whether to show rownames
#' @returns NULL invisibly
#' @noRd

save_table <- function(
    results,
    filename,
    path,
    overwrite = FALSE,
    show_rownames = TRUE
  ) {

  if (!is.null(filename)) {

    # Check and create directory for table
    check_folder(path)

    # Write table
    openxlsx::write.xlsx(
        results,
        file = paste0(path, "/", filename),
        showNA = FALSE,
        rowNames = show_rownames,
        overwrite = overwrite
    )
  }

  return(invisible())
}


#' Save rds results
#'
#' @param results results to be saved
#' @param filename string with name of results file
#' @param path string; indicates the folder location where the tables
#' are stored on the hard drive
#' @returns NULL invisibly
#' @noRd

save_results <- function(results, filename, path) {

  if (!is.null(filename)) {

    # Check and create directory for data
    check_folder(path)

    saveRDS(results, file = paste0(path, "/", filename))
  }
  return(invisible())

}


#' Check if folder exists and if not, create new one
#'
#' @param path    string; indicates the folder location that shall be checked
#' @returns NULL invisibly
#' @export

check_folder <- function(path) {
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
        message("The location ", path, " did not exist. New folder created.")
    }
  return(invisible())
}


#' Check pid for duplicates
#'
#' @param pid  character vector with person identifiers
#' @returns NULL invisibly
#' @export

check_pid <- function(pid) {
    if (length(pid) != length(unique(pid))) {
        stop("There are duplicates in the person identifiers.")
    }

    if (any(is.na(pid))) {
        warning("There are missing values in the person identifiers.")
    }
  return(invisible())
}


#' Format a list of names for error messages
#'
#' Quotes each name and collapses with commas. If the list is longer than
#' \code{max_show}, only the first \code{max_show} items are shown and the
#' remainder is summarised as "and N more".
#'
#' @param x character vector of names
#' @param max_show integer; maximum number of names to display
#' @returns A single string like "'a', 'b' and 3 more"
#' @noRd
fmt_names <- function(x, max_show = 5L) {
  if (length(x) > max_show) {
    shown <- paste0("'", x[seq_len(max_show)], "'", collapse = ", ")
    paste0(shown, " and ", length(x) - max_show, " more")
  } else {
    paste0("'", x, "'", collapse = ", ")
  }
}

#' Build a validation error message with singular/plural grammar
#'
#' @param label string; noun to use (e.g. "Variable", "Item")
#' @param bad character vector; offending names
#' @param name_df string; data frame name shown in the message
#' @param verb_singular string; verb for a single item (e.g. "is")
#' @param verb_plural string; verb for multiple items (e.g. "are")
#' @param predicate string; the rest of the sentence (e.g. "not numeric")
#' @returns A single formatted string
#' @noRd
validation_msg <- function(label, bad, name_df, verb_singular, verb_plural,
                           predicate) {
  sprintf(
    "%s%s %s in '%s' %s %s",
    label,
    if (length(bad) > 1) "s" else "",
    fmt_names(bad),
    name_df,
    if (length(bad) > 1) verb_plural else verb_singular,
    predicate
  )
}

#' Check item names for duplicates
#'
#' @param items  character vector with item names
#' @returns NULL invisibly
#' @export

check_items <- function(items) {
    if (length(items) != length(unique(items))) {
        dupes <- unique(items[duplicated(items)])
        stop("Duplicate item names found in 'vars$item': ",
             fmt_names(dupes), ".")
    }

    if (any(is.na(items))) {
        stop("Missing values (NA) found in 'vars$item'. ",
             "Check that all selected items have a name in vars.")
    }
  return(invisible())
}


#' Check variables in df for existence
#'
#' @param df  data.frame
#' @param name_df  string; contains name of data.frame
#' @param variables  character vector; contains names of variables that shall be
#' checked for inclusion in df
#' @returns NULL invisibly
#' @export

check_variables <- function(df, name_df = "<unknown>", variables) {

  if (is.null(name_df)) name_df <- "<unknown>"

  if (!is.null(variables)) {

    not_included <- !variables %in% names(df)

    if (any(not_included)) {
      missing <- variables[not_included]
      stop(sprintf(
        "Variable%s %s not found in '%s'. ",
        if (length(missing) > 1) "s" else "",
        fmt_names(missing),
        name_df
      ), "Check that the column name is spelled correctly.")
    }
  }
  return(invisible())
}


#' Check logical variable(s) for correctness
#'
#' @param df  data.frame; contains at least the indicated logical variable(s)
#' @param name_df  string; contains name of data.frame
#' @param logicals  character vector; contains names of variables that shall be
#' checked for correctness
#' @param warn  logical; whether to warn if logicals include NA
#' @returns NULL invisibly
#' @export

check_logicals <- function(df, name_df = "<unknown>", logicals, warn = TRUE) {

  if (is.null(name_df)) name_df <- "<unknown>"

  if (!is.null(logicals)) {

    # Check whether variables are included in dataframe
    check_variables(df, name_df, logicals)

    no_logical <- sapply(df[ , logicals, drop = FALSE], function(x) !is.logical(x))

    if (any(no_logical)) {
      bad <- logicals[no_logical]
      stop(
        validation_msg("Variable", bad, name_df, "is", "are",
                       "not logical (TRUE/FALSE)."),
        " Convert to logical before passing to the function."
      )
    }

    other_value <- sapply(df[ , logicals, drop = FALSE], function(x) any(!x %in% c(TRUE, FALSE)))

    if (warn & any(other_value)) {
      bad <- logicals[other_value]
      warning(
        validation_msg("Logical variable", bad, name_df, "contains", "contain",
                       "values other than TRUE/FALSE (e.g. NA)."),
        " NA rows will be excluded from the analysis."
      )
    }
  }

  return(invisible())
}


#' Check numeric variable(s) for correctness
#'
#' @param df  data.frame; contains at least the indicated numeric variable(s)
#' @param name_df  string; contains name of df
#' @param numerics  character vector; contains names of variables that shall be
#' checked for correctness; if NULL, all items will be used
#' @param check_invalid  logical; whether to check df for invalid values
#' @param dich  logical; whether to check items for non-dichotomous values
#' @returns NULL invisibly
#' @export

check_numerics <- function(df, name_df = "<unknown>", numerics = NULL,
                           check_invalid = FALSE, dich = FALSE) {

  if (is.null(name_df)) name_df <- "<unknown>"
  if (is.null(numerics)) numerics <- names(df)

  # Check whether variables are included in dataframe
  check_variables(df, name_df, numerics)

  no_numeric <- sapply(df[ , numerics, drop = FALSE], function(x) !is.numeric(x))

  if (any(no_numeric)) {
    bad <- numerics[no_numeric]
    stop(
      validation_msg("Variable", bad, name_df, "is", "are", "not numeric."),
      " Convert the column to numeric before passing to the function."
    )
  }

  # Check whether variables contain invalid values
  if (check_invalid) check_invalid_values(df, name_df, items = numerics)

  # Check whether variables are dichotomous
  if (dich) check_dich(df, name_df, dich_items = numerics)

  return(invisible())
}


#' Check data.frame for invalid values
#'
#' @param df  data.frame; contains at least the indicated variable(s)
#' @param name_df  string; contains name of df
#' @param items  character vector; contains names of items to be checked;
#' if NULL, all items will be used
#' @returns NULL invisibly
#' @noRd

check_invalid_values <- function(df, name_df = "<unknown>", items = NULL) {

  if (is.null(name_df)) name_df <- "<unknown>"
  if (is.null(items)) items <- names(df)

  df_items <- df[, items, drop = FALSE]
  invalid_values <- sort(unique(unlist(df_items[df_items < 0 & !is.na(df_items)])))

  if (length(invalid_values) > 0) {
    stop(sprintf(
      "Data frame '%s' contains invalid values (< 0): %s. ",
      name_df,
      paste(invalid_values, collapse = ", ")
    ), "Include all user-defined missing values via the `mvs` argument.")
  }
  return(invisible())
}


#' Check data.frame for non-dichotomous values
#'
#' @param df  data.frame; contains at least the indicated variable(s)
#' @param name_df  string; contains name of df
#' @param dich_items  character vector; contains names of items to be checked;
#' if NULL, all items will be used
#' @returns NULL invisibly
#' @noRd

check_dich <- function(df, name_df = "<unknown>", dich_items = NULL) {

  if (is.null(name_df)) name_df <- "<unknown>"
  if (is.null(dich_items)) dich_items <- names(df)

  item_max <- sapply(df[, dich_items, drop = FALSE], function(x) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0L) return(NA_real_)
    max(vals)
  })

  all_na <- dich_items[is.na(item_max)]
  if (length(all_na) > 0) {
    stop(sprintf(
      "Item%s %s in '%s' %s entirely NA. Cannot verify dichotomous coding.",
      if (length(all_na) > 1) "s" else "",
      fmt_names(all_na),
      name_df,
      if (length(all_na) > 1) "are" else "is"
    ))
  }

  no_dich <- dich_items[item_max > 1]
  if (length(no_dich) > 0) {
    max_label <- paste(paste0("'", no_dich, "'=", item_max[no_dich]),
                       collapse = ", ")
    stop(
      validation_msg("Item", no_dich, name_df, "contains", "contain",
                     paste0("values > 1 (max: ", max_label,
                            "). Dichotomous responses (0/1) are required.")),
      " Use a polytomous model (PCM2/GPCM) for these items, or recode to 0/1."
    )
  }
  return(invisible())
}


#' Check data.frame for items with maximum score of 0
#'
#' Items where all observed values (after missing-value conversion) are 0 or NA
#' cause TAM to crash with an uninformative internal error.  This function
#' detects such items early and raises a descriptive error.
#'
#' @param df  data.frame; contains item responses (MVs already converted to NA)
#' @param name_df  string; name of df shown in the error message
#' @param name_group  string or NULL; group name included in the error message
#'   when running a multi-group analysis (e.g. from \code{grouped_irt_analysis})
#' @returns NULL invisibly
#' @noRd

check_max_zero <- function(df, name_df, name_group = NULL) {

  max_score <- sapply(df, function(x) {
    vals <- x[!is.na(x)]
    if (length(vals) == 0L) return(-Inf)
    max(vals)
  })
  zero_items <- names(max_score[max_score <= 0])

  if (length(zero_items) > 0) {
    group_info <- if (!is.null(name_group)) paste0(" (group '", name_group, "')") else ""
    stop(paste0(
      "The following items in ", name_df, group_info, " have a maximum observed score of 0 ",
      "(all responses are 0 or missing after missing-value conversion). ",
      "TAM cannot fit a model to such items. ",
      "Please exclude them from the analysis or verify the data and missing-value ",
      "specification (mvs):\n  ",
      paste(zero_items, collapse = "\n  ")
    ))
  }
  return(invisible())
}


#' Minimum effect hypothesis test
#' (following Murphy & Myors, 1999)
#'
#' @param stat    empirical F statistic
#' @param df1     degrees of freedom (= number of groups - 1)
#' @param df2     degrees of freedom (= sample size - number of groups)
#' @param eta2    percentage of explained variance (= minimum effect size)
#' @param delta   standardized mean difference (= minimum effect size)
#' @param alpha   error probability
#' @param digits  number of decimal places
#' @param verbose   print results to console
#'
#' @return  list
#' @importFrom stats qf pf
#' @export

meht <- function(stat, df1, df2, eta2 = NULL, delta = .40,
                 alpha = .05, digits = 3, verbose = TRUE) {

    # Determine effect size
    if (is.null(eta2) & is.null(delta)) {
        stop("Please provide an effect size in eta2 or delta!")
    }
    if (is.null(eta2)) {
        # eta2 = f2 / (1 + f2) = (d/2)^2 / (1 + (d/2)^2), as d = 2*f
        eta2 <- (delta / 2)^2 / (1 + (delta / 2)^2)
    }

    ncp <- df2 * eta2 / (1 - eta2) # non-centrality parameter
    Fmin <- qf(1 - alpha, df1, df2, ncp = ncp) # critical F for minimum effects test
    Fnil <- qf(1 - alpha, df1, df2) # critical F for nil hypotheses test
    pmin <- pf(stat, df1, df2, ncp = ncp, lower.tail = FALSE) # p for minimum effects test
    pnil <- pf(stat, df1, df2, lower.tail = FALSE) # p for nil hypothesis test

    if (verbose) {
        message("\nNil hypothesis test:")
        message("   Critical F-value: F(", df1, ",", df2, ") = ", round(Fnil, digits))
        message("   p for F = ", stat, ": p = ", round(pnil, digits))
        message("\nMinimum effect hypothesis test:")
        message("   Critical F-value: F(", df1, ", ", df2, ", ", round(ncp, digits), ") = ",
                round(Fmin, digits))
        message("   p for F = ", stat, ": p = ", round(pmin, digits))
    }

    out <- list(
        ncp = ncp, Fmin = Fmin, Fnil = Fnil,
        pmin = pmin, pnil = pnil,
        df1 = df1, df2 = df2, alpha = alpha,
        eta2 = eta2, ncp = ncp
    )
    class(out) <- "meht"
    invisible(out)
}



#' Get name of R object as string
#' @param object R object (e.g., vector, data.frame, ...)
#' @returns name of object as string
#' @noRd

get_object_name <- function(object) {
  deparse(substitute(object))
}


#' Test whether iterations have reached the maximum
#' @param mod  return object of TAM-functions that calculate IRT models
#' (e.g., TAM::tam.mml)
#' @param name_model  string; defines name of model
#' @returns warning if iter is equal or greater than maxiter
#' @noRd

reached_maxiter <- function(mod, name_model) {
    if (mod$iter >= mod$control$maxiter) {
        warning(paste0("Maximum number of iterations were reached for the IRT model ",
                       name_model, "! Model did not converge.\n"))
    }
}


#' Test whether data contains polytomous items (responses > 1)
#' @param resp  data.frame with item responses
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#'
#' @returns logical; whether data contains polytomous items.
#' @noRd

is_poly <- function(resp, vars, select) {
  max(resp[ , vars$item[vars[[select]]]], na.rm = TRUE) > 1
}


#' Create Q matrix for TAM-functions
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param scoring string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param poly logical; whether items include polytomous items
#'
#' @returns Q matrix (or NULL, if no scoring variable is provided).
#' @noRd

create_q <- function(vars, select, scoring, poly) {

    if (!is.null(scoring)) {
        Q = as.matrix(vars[[scoring]][vars[[select]]])
    } else {
        Q <- NULL
        if (poly) {
            warning("No variable name for scoring factor for polytomous analysis ",
                    "provided. Therefore no loading matrix is used for analysis.")
        }
    }

    Q
}


#' Create object depending on condition
#' @param condition if-clause on which depends decision
#' @param a return a if condition is TRUE
#' @param b return b if condition is FALSE
#'
#' @returns Object x (a or b, depending on condition).
#' @noRd
create_ifelse <- function(condition, a, b) {

    if(condition) {
        x <- a
    } else {
        x <- b
    }

    return(x)
}


#' Create group name
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param start string; first part of name
#' @param end string; last part of name
#' @param sep string; separator between first part and group name (Default is "_")
#'
#' @returns string with filename.
#' @noRd
create_name <- function(start, name_group = NULL, end = NULL, sep = "_") {

    name <- create_ifelse(
        is.null(name_group),
        paste0(start, end),
        paste0(start, sep, name_group, end)
    )

    return(name)
}

#' Match item parameters by item names
#' @param xsi_fixed named numerical vector; contains fixed item difficulties as
#'   elements and item names as names of elements
#' @param resp data.frame (containing item responses) as passed to TAM functions
#' @param irtmodel string; "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#'   "GPCM" for GPCM analysis
#' @param rename_steps logical; whether step parameters shall be renamed
#'   (from "_step" to ":step")
#' @param Q Q matrix as passed to TAM functions
#' @param A A array as passed to TAM functions
#' @param B B array as passed to TAM functions
#'
#' @returns xsi_fixed with correct order
#' @noRd
order_xsi_fixed <- function(
    xsi_fixed,
    resp,
    irtmodel,
    Q = NULL, A = NULL, B = NULL,
    rename_steps = FALSE
  ) {

  if (irtmodel %in% c("1PL", "PCM2")) {
    xsi_arg <- TAM::tam.mml(
      resp = resp,
      Q = Q,
      A = A,
      B = B,
      irtmodel = irtmodel,
      verbose = FALSE,
      control = list(maxiter = 1)
    )$xsi.fixed.estimated
  } else if (irtmodel %in% c("2PL", "GPCM")) {
    xsi_arg <- TAM::tam.mml.2pl(
      resp = resp,
      Q = Q,
      A = A,
      B = B,
      irtmodel = irtmodel,
      verbose = FALSE,
      control = list(maxiter = 1)
    )$xsi.fixed.estimated
  }

 if (rename_steps) names(xsi_fixed) <- gsub("_step", ":step", names(xsi_fixed))

  if (any(!names(xsi_fixed) %in% rownames(xsi_arg)))
    stop(paste0("Items in xsi_fixed do not match items in ", irtmodel, " model!"))

  xsi_new <- cbind(
    xsi_arg[names(xsi_fixed), 1], xsi_fixed[names(xsi_fixed)] # reorder xsi
  )

  return(xsi_new)
}


#' Create names for output as used in suf (this concerns the variables with collapsed categories)
#' @param vars_name  character vector of item names (possibly with '_collapsed'
#'   suffixes), or a data.frame with an 'item' column. If NULL, returns NULL.
#' @noRd
create_suf_names <- function(vars_name = NULL) {

  if (is.null(vars_name)) return(NULL)

  if (is.data.frame(vars_name)) {
    for (item in seq_along(vars_name$item)) {
      vars_name$item[[item]] <- gsub("_collapsed", "",vars_name$item[[item]])
    }
    return(vars_name$item)

  } else {
    for (item in seq_along(vars_name)) {
      vars_name[[item]] <- gsub("_collapsed", "",vars_name[[item]])
    }
    return(vars_name)
  }
}


#' Calculate basic descriptive statistics for variables
#'
#' @param x A data frame with variables to calculate the statistics for
#' @param digits A number for rounding
#' @returns A data frame with the calculated statistics for each variable
describe <- function(x, digits = 2) {

  if (!is.data.frame(x)) x <- as.data.frame(x)

  # Dummy coding of factors
  for (i in names(x)) {
    if (!is.factor(x[[i]])) next
    d <- model.matrix(as.formula(paste0("~ -1 + ", i)), x)
    names(d) <- paste0(i, levels(x[[i]]))
    x[[i]] <- NULL
    x <- cbind(x, d)
  }

  # Calculate statistics
  stats <- t(apply(x, 2, \(v) {
    c(n = sum(!is.na(v)),
      mean = mean(v, na.rm = TRUE),
      sd = sd(v, na.rm = TRUE),
      median = median(v, na.rm = TRUE),
      min = min(v, na.rm = TRUE),
      max = max(v, na.rm = TRUE))
  }))
  if (ncol(x) == 1L) {
    stats <- data.frame(t(stats))
  } else {
    stats <- as.data.frame(stats)
  }
  stats <- round(stats, digits = digits)

  return(stats)

}

#' Rounding with proper formatting for NEPS Survey Papers
#'
#' @param x numeric vector; the numbers to be formatted
#' @param digits integer vector; the number of decimal places for rounding
#' @param d0 logical vector; remove leading zeros (TRUE) or keep them (FALSE)
#' @returns character vector; the formatted numbers
#' @export
#' @examples
#' # Round with 2 decimals
#' rnd(0.1459)
#'
#' # Round with 3 decimals and remove leading 0
#' rnd(0.1459, digits = 3, d0 = TRUE)
rnd <- function(x, digits = 2, d0 = FALSE) {

  x <- base::formatC(x, digits = digits[1], format = "f", big.mark = ",")
  if (d0[1])
    x <- base::sub("^(-?)0+\\.", "\\1.", x)
  return(x)

}












