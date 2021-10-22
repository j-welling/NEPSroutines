#' Select only valid cases
#'
#' @param resp    data.frame with responses
#' @param valid   character string. defines name of boolean variable in resp,
#'                indicating (in)valid cases.
#' @return data.frame as resp, but only with valid cases
#'
#' @export

only_valid <- function(resp, valid = NULL) {

    if (!is.null(valid)) {
        resp <- resp[resp[[valid]], ]
    } else {
        warning("No variable with valid cases provided. All cases are used for analysis.")
    }

    return(resp)
}


#' Prepare resp for analysis
#'
#' @param resp    data.frame with responses
#' @param valid   character string. defines name of boolean variable in resp,
#'                indicating (in)valid cases.
#' @param without_valid boolean; indicates whether not to check valid cases
#' @param vars    data.frame. contains information about all competence items
#'                and includes the following columns:
#'                  items: character indicating names of items.
#'                  variable indicated by argument 'items'
#' @param items   character. contains name of variable (boolean) in vars that
#'                indicates which items to use for analysis.
#'
#' @return data.frame as resp, but only with valid cases
#'
#' @export

prepare_resp <- function(resp, valid = NULL, without_valid = FALSE,
                         vars = NULL, items = NULL, convert = FALSE) {

    if (!without_valid) resp <- only_valid(resp = resp, valid = valid)

    if (!is.null(items)) {
        if (is.null(vars)) {
            stop("To create dataframe (resp) with only the indicated items please also provide vars.")
        } else {
            if (items %in% colnames(vars)) {
              resp <- resp[ , vars$items[vars[[items]]]]
            } else {
              stop(paste0("No variable with name '", items, "' exists in vars."))
            }
        }
    } else {
        warning("No variable provided indicating the items to keep. All items are kept.")
    }

    if (convert) resp <- convert_mv(resp)

    return(resp)
}


#' Check if folder exists and if not, create new one
#'
#' @param path    path to folder
#'
#' @noRd

check_folder <- function(path) {
    if (!file.exists(path)) {
        dir.create(path, recursive = TRUE)
        warning("The location ", path, " did not exist. New folder created.")
    }
}

# if (!file.exists(save_at)) {
#     stop("The location ", save_at, " does not exist. Please provide a ",
#          "valid folder path to save the distractor analyses.")
# }

#' Check pid for duplicates
#'
#' @param pid    vector with person identifiers
#'
#' @noRd

check_pid <- function(pid) {
    if (length(pid) != length(unique(pid))) {
        stop("There are duplicates in the person identifiers.")
    }
}


#' Select sample with a minimum number of valid values
#'
#' @param resp    data.frame with responses
#' @param vars    variables to check for valid values;
#                 if NULL, all variables will be selected
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param invalid vector of invalid values
#' @param warn    print warnings
#' @return        boolean vector with length = nrow(resp),
#' indicating whether case is valid.
#' @export

min_val <- function(resp, vars, items, min.val = NULL,
                    invalid = NA, warn = TRUE) {

    vrs <- vars$items[vars[[items]]]
    resp_ <- resp[ , vrs]

    # Set minimum number of valid values
    if (is.null(min.val)) {
        min.val <- 3
        warning("No valid (=> 0) number of minimum valid responses per person (min.val) provided. Default of 3 valid responses applies.")
    } else if (min.val < 0) {
        min.val <- 3
        warning("No valid (=> 0) number of minimum valid responses per person (min.val) provided. Default of 3 valid responses applies.")
    }

    # Number of valid values by respondent
    nval <- rowSums(apply(
        subset(resp_, select = vrs), 2,
        function(x) {
            !(x %in% invalid)
        }
    ))

    # Create indicator
    valid <- (nval >= min.val)
    attr(valid, "label") <- paste0("Case with at least ",
                                   min.val,
                                   " valid responses")

    # Return results
    return(valid)
}


#' Convert user-defined missing values to NAs
#'
#' Testing for differential item functioning for binary and polytomous data.
#' Main effects and DIF effects models are estimated.
#'
#' @param resp          data.frame containing the responses, scored items,
#'                      sociodemographic and design variables
#' @param variables     character vector with names of all competence variables
#' @param mvs           numeric vector including all user-defined missing values
#'
#' @return              data.frame like resp, but without user-defined mvs
#' @export

convert_mv <- function(resp, variables = NULL, mvs = NULL) {

    if (is.null(mvs)) {
        mvs <- -999:-1
        warning("No user defined missing values provided. Default of '-999 to -1' is used.")
    }

    if (is.null(variables)) variables <- colnames(resp)

    for (i in variables) {
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


#' Calculate new position variable with only a set of variables
#'
#' @param vars      data.frame with information about all items
#' @param items     character. contains name of variable in vars (boolean) that
#'                  indicates which variables to use.
#' @param position  (named) character vector. contains name(s) of variable(s) in
#'                  vars that indicate the position of subitems;
#'                  if grouping, then for each group one variable name is necessary;
#'                  names represent names of groups (e.g. easy = "position_easy")
#'
#' @return   data.frame as input, with one extra variable indicating position of chosen variables.
#' @importFrom rlang .data
#' @export

pos_new <- function(vars, items = 'final', position = NULL) {

    if (is.null(position)) {

        stop("No position variable(s) provided.")

    } else if (length(position) == 1) {

        vars_ <- vars[vars[[items]], ]
        pos <- data.frame(items = vars_[['items']],
                          position = vars_[[position]])
        pos <- dplyr::arrange(pos, .data$position)
        pos[[paste0("position_", items)]] <- seq(1, nrow(pos))
        vars <- merge(vars, pos[ , c('items', paste0("position_", items))],
                      by = 'items', all = TRUE)

    } else {

        for (g in names(position)) {
            vars_ <- vars[vars[[items]] & !is.na(vars[[position[g]]]), ]
            pos <- data.frame(items = vars_[['items']],
                              position = vars_[[position[g]]])
            pos <- dplyr::arrange(pos, .data$position)
            pos[[paste0("position_", g, "_", items)]] <- seq(1, nrow(pos))
            vars <- merge(vars, pos[ , c('items', paste0("position_", g, "_", items))],
                          by = 'items', all = TRUE)
        }

        return(vars)
    }
}
