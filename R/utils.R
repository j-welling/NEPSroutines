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

    if (!is.null(valid)) {
        resp <- resp[resp[[valid]], ]
    } else if (warn) {
        warning("No variable with valid cases provided. All cases are used ",
                "for analysis.")
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
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param convert  logical; whether to convert custom missing values to NA
#' @param mvs  named integer vector; contains user-defined missing values
#' @param warn logical; whether warnings are to be printed to the console
#' @param zap_labels logical; whether to convert haven_labelled to normal
#'
#' @return data.frame as resp, but only with valid cases
#'
#' @export

prepare_resp <- function(resp, vars = NULL, items = NULL,
                         use_only_valid = FALSE,  valid = NULL,
                         convert = FALSE, mvs = NULL,
                         warn = TRUE, zap_labels = TRUE) {

    if (use_only_valid) resp <- only_valid(resp = resp, valid = valid)

    if (!is.null(items)) {
        if (is.null(vars)) {
            stop("To create dataframe (resp) with only the indicated items ",
                 "please also provide vars.")
        } else {
            if (items %in% colnames(vars)) {
              resp <- resp[ , vars$items[vars[[items]]]]
            } else {
              stop(paste0("No variable with name '", items, "' exists in vars."))
            }
        }
    } else {
        warning("No variable provided indicating the items to keep. All items ",
                "are kept.")
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
        warning("The location ", path, " did not exist. New folder created.")
    }
}


#' Check pid for duplicates
#'
#' @param pid  character vector with person identifiers
#'
#' @noRd

check_pid <- function(pid) {
    if (length(pid) != length(unique(pid))) {
        stop("There are duplicates in the person identifiers.")
    }
}


#' Convert user-defined missing values to NAs
#'
#' User defined missing values (usually coded as negative numbers) must be
#' converted to NAs so that they do not break the IRT analysis
#'
#' @param resp  data.frame containing the item responses
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param mvs  named integer vector; contains user-defined missing values
#' @param warn logical; whether warnings are to be printed to the console
#'
#' @return              data.frame like resp, but without user-defined mvs
#' @export

convert_mv <- function(resp, items = NULL, mvs = NULL, warn = TRUE) {

    if (is.null(mvs)) {
        mvs <- -999:-1
        if (warn) {
          warning("No user defined missing values provided. Default of ",
                  "'-999 to -1' is used.\n")
        }
    }

    if (is.null(items)) items <- colnames(resp)

    for (i in items) {
      resp[[i]] <- replace(resp[[i]], resp[[i]] %in% mvs, NA)
    }

    return(resp)
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
