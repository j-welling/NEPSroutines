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
      warning("No variable with valid cases provided. ",
              "All cases are used for analysis.\n")
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
      warning("No user defined missing values provided for item responses. ",
              "Default of '-999 to -1' is used.\n")
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
            scaling:::check_logicals(vars, "vars", select, warn = warn)
            items <- vars$item[vars[[select]]]
            scaling:::check_variables(resp, "resp", variables = items)
            resp <- resp[ , items]
        }
    } else if (warn) {
        warning("No variable provided indicating the items to keep. ",
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
    warning("No user defined missing values provided. ",
            "Default of '-999 to -1' is used.\n")
  }

  if (is.null(valid)) {
    warning("No variable with valid cases provided. ",
            "All cases are used for analysis.\n")
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

save_table <- function(results, filename, path,
                       overwrite = FALSE,
                       show_rownames = TRUE) {

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
        warning("The location ", path, " did not exist. New folder created.\n")
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


#' Check item names for duplicates
#'
#' @param items  character vector with item names
#' @returns NULL invisibly
#' @export

check_items <- function(items) {
    if (length(items) != length(unique(items))) {
        stop("There are duplicates in the item names.")
    }

    if (any(is.na(items))) {
        stop("There are missing values in the item names.")
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

check_variables <- function(df, name_df, variables) {

  if (!is.null(variables)) {

    not_included <- !variables %in% names(df)

    if (sum(not_included) > 0) {
      stop(paste0("Data.frame ", name_df, " does not include any variable with the",
                  " name '", variables[not_included], "'. Please check again.\n"))
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

check_logicals <- function(df, name_df, logicals, warn = TRUE) {

  if (!is.null(logicals)) {

    # Check whether variables are included in dataframe
    check_variables(df, name_df, logicals)

    no_logical <- sapply(df[ , logicals, drop = FALSE], function(x) !is.logical(x))

    if (sum(no_logical) > 0) {
      stop(paste0("Variable '", logicals[no_logical], "' in data.frame ",
                  name_df, " is no logical. Please check again.\n"))
    }

    other_value <- sapply(df[ , logicals, drop = FALSE], function(x) any(!x %in% c(TRUE, FALSE)))

    if (warn & (sum(other_value) > 0)) {
      warning(paste0("Logical variable '", logicals[other_value], "' in ",
                     "data.frame ", name_df, " contains other values than TRUE or ",
                     "FALSE (e.g., NA). Please check again.\n"))
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

check_numerics <- function(df, name_df, numerics = NULL, check_invalid = FALSE,
                           dich = FALSE) {

  if (is.null(numerics)) numerics <- names(df)

  # Check whether variables are included in dataframe
  check_variables(df, name_df, numerics)

  no_numeric <- sapply(df[ , numerics, drop = FALSE], function(x) !is.numeric(x))

  if (sum(no_numeric) > 0) {
    stop(paste0("Variable '", numerics[no_numeric], "' in data.frame ", name_df,
                " is no numeric variable. Please check again.\n"))
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

check_invalid_values <- function(df, name_df, items = NULL) {

  if (is.null(items)) items <- names(df)

  if (any(df[ , items, drop = FALSE][!is.na(df[ , items, drop = FALSE])] < 0)) {
    stop(paste0("Data.frame ", name_df, " contains invalid values (< 0) in ",
                "specified items. Please check again and be sure to include all ",
                "user defined missing values in the vector mvs. Default is ",
                "-999 to -1."))
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

check_dich <- function(df, name_df, dich_items = NULL) {

  if (is.null(dich_items)) dich_items <- names(df)

  no_dich <- sapply(df[ , dich_items, drop = FALSE], function(x) {
    max(x, na.rm = TRUE) > 1})

  if (sum(no_dich) > 0) {
    stop(paste0("Variable '", dich_items[no_dich], "' in data.frame ", name_df,
                " contains values greater than 1, although specified as",
                " dichotomous. Please check again.\n"))
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
        cat("\nNil hypothesis test:\n")
        cat("   Critical F-value: F(", df1, ",", df2, ") = ",
            round(Fnil, digits), "\n",
            sep = ""
        )
        cat("   p for F = ", stat, ": p = ", round(pnil, digits), "\n", sep = "")

        cat("\nMinimum effect hypothesis test:\n")
        cat("   Critical F-value: F(", df1, ", ", df2, ", ", round(ncp, digits), ") = ",
            round(Fmin, digits), "\n",
            sep = ""
        )
        cat("   p for F = ", stat, ": p = ", round(pmin, digits), "\n\n", sep = "")
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


#' Create object depending on condition
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
#' @param vars_name  string; defines name of dataset vars
#' @param resp_name string; defines name of dataset resp
#' @noRd
create_suf_names <- function(vars_name = NULL) {

  if (!is.null(vars_name)) {

    if(is.data.frame(vars_name)) {
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
}


#'
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
