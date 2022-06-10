#' Select only valid cases
#'
#' @param resp  data.frame with item responses
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no parameter 'valid' provided
#'
#' @return data.frame as resp, but only with valid cases
#' @export

only_valid <- function(resp, valid = NULL, warn = TRUE) {

  if(!is.null(valid)) {
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
#' @return data.frame like resp, but without user-defined mvs
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
    resp[[i]] <- replace(resp[[i]], resp[[i]] %in% mvs, NA)
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
#' @return data.frame as resp, but only with valid cases
#'
#' @export

prepare_resp <- function(resp, vars = NULL, select = NULL,
                         use_only_valid = FALSE,  valid = NULL,
                         convert = FALSE, mvs = NULL,
                         warn = TRUE, zap_labels = TRUE) {

    if (use_only_valid) resp <- only_valid(resp = resp,
                                           valid = valid,
                                           warn = warn)

    if (!is.null(select)) {
        if (is.null(vars)) {
            stop("To create dataframe (resp) with only the indicated items ",
                 "please also provide vars.")
        } else {
            check_logicals(vars, "vars", select, warn = warn)
            items <- vars$item[vars[[select]]]
            check_variables(resp, "resp", variables = items)
            resp <- resp[ , items]
        }
    } else {
        warning("No variable provided indicating the items to keep. ",
                "All items are kept.")
    }

    if (convert) resp <- convert_mv(resp = resp, mvs = mvs, warn = warn)

    if (zap_labels) {
      resp <- haven::zap_labels(resp)
    }

    return(resp)
}


#' Save table
#'
#' @param results table to be saved
#' @param filename string; defines name of table file
#' @param path string; indicates the folder location where the tables
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param show_rownames logical; whether to show rownames
#'
#' @noRd

save_table <- function(results, filename, path,
                       overwrite = FALSE,
                       show_rownames = TRUE) {

  if (!is.null(filename)) {

    # Check and create directory for table
    check_folder(path)

    openxlsx::write.xlsx(results, file = paste0(path, "/", filename),
                         showNA = FALSE, rowNames = show_rownames,
                         overwrite = overwrite)
  }
}


#' Save rds results
#'
#' @param results results to be saved
#' @param filename string with name of results file
#' @param path string; indicates the folder location where the tables
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#'
#' @noRd

save_results <- function(results, filename, path) {

  if (!is.null(filename)) {

    # Check and create directory for data
    check_folder(path)

    saveRDS(results, file = here::here(paste0(path, "/", filename)))
  }
}


#' Check if folder exists and if not, create new one
#'
#' @param path    string; indicates the folder location that shall be checked;
#' please note that the path is relative to the current working path set by
#' here::i_am()
#'
#' @export

check_folder <- function(path) {
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
        warning("The location ", path, " did not exist. New folder created.\n")
    }
}


#' Check pid for duplicates
#'
#' @param pid  character vector with person identifiers
#'
#' @export

check_pid <- function(pid) {
    if (length(pid) != length(unique(pid))) {
        stop("There are duplicates in the person identifiers.")
    }

    if (any(is.na(pid))) {
        warning("There are missing values in the person identifiers.")
    }
}


#' Check variables in df for existence
#'
#' @param df  data.frame
#' @param name_df  string; contains name of data.frame
#' @param variables  character vector; contains names of variables that shall be
#' checked for inclusion in df
#'
#' @export

check_variables <- function(df, name_df, variables) {

  if (!is.null(variables)) {

    not_included <- !variables %in% names(df)

    if (sum(not_included) > 0) {
      stop(paste0("Data.frame ", name_df, " does not include any variable with the",
                  " name '", variables[not_included], "'. Please check again.\n"))
    }
  }
}


#' Check logical variable(s) for correctness
#'
#' @param df  data.frame; contains at least the indicated logical variable(s)
#' @param name_df  string; contains name of data.frame
#' @param logicals  character vector; contains names of variables that shall be
#' checked for correctness
#' @param warn  logical; whether to warn if logicals include NA
#'
#' @export

check_logicals <- function(df, name_df, logicals, warn = TRUE) {

  if (!is.null(logicals)) {

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
}


#' Check numeric variable(s) for correctness
#'
#' @param df  data.frame; contains at least the indicated numeric variable(s)
#' @param name_df  string; contains name of df
#' @param numerics  character vector; contains names of variables that shall be
#' checked for correctness; if NULL, all items will be used
#' @param check_invalid  logical; whether to check df for invalid values
#' @param dich  logical; whether to check items for non-dichotomous values
#'
#' @export

check_numerics <- function(df, name_df, numerics = NULL, check_invalid = FALSE,
                           dich = FALSE, poly = FALSE, collapsed = TRUE) {

  if(is.null(numerics)) numerics <- names(df)

  check_variables(df, name_df, numerics)

  no_numeric <- sapply(df[ , numerics, drop = FALSE], function(x) !is.numeric(x))

  if (sum(no_numeric) > 0) {
    stop(paste0("Variable '", numerics[no_numeric], "' in data.frame ", name_df,
                " is no numeric variable. Please check again.\n"))
  }

  if (check_invalid) check_invalid_values(df, name_df, items = numerics)

  if (dich) check_dich(df, name_df, dich_items = numerics)
}


#' Check data.frame for invalid values
#'
#' @param df  data.frame; contains at least the indicated variable(s)
#' @param name_df  string; contains name of df
#' @param items  character vector; contains names of items to be checked;
#' if NULL, all items will be used
#'
#' @noRd

check_invalid_values <- function(df, name_df, items = NULL) {

  if (is.null(items)) items <- names(df)

  if(any(df[ , items, drop = FALSE][!is.na(df[ , items, drop = FALSE])] < 0)) {
    stop(paste0("Data.frame ", name_df, " contains invalid values (< 0) in ",
                "specified items. Please check again and be sure to include all ",
                "user defined missing values in the vector mvs. Default is ",
                "-999 to -1."))
  }
}


#' Check data.frame for non-dichotomous values
#'
#' @param df  data.frame; contains at least the indicated variable(s)
#' @param name_df  string; contains name of df
#' @param dich_items  character vector; contains names of items to be checked;
#' if NULL, all items will be used
#'
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
                 alpha = .05, digits = 2, verbose = TRUE) {

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
#' @returns logical; whether data contains polytomous items
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
#' @returns Q matrix (or NULL, if no scoring variable is provided)
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
