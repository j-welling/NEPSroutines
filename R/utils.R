#' Select sample with a minimum number of valid values
#'
#' @param x       data.frame with responses
#' @param vars    variables to check for valid values;
#                 if NULL, all variables will be selected
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param invalid vector of invalid values
#' @param append  boolean indicating whether to filter data.frame (FALSE)
#'                or append new variable (TRUE)
#' @param warn    print warnings
#' @return        filtered data.frame or list of results
#' @export

min_val <- function(x, vars = NULL, min.val = 3, # zu x: sollte hier auf ID_t hingewiesen/geprüft werden? Damit nicht versehentlich nur 2 valide Antworten bestehen
                    invalid = NA, append = FALSE,
                    warn = TRUE) {

    # Set variables to check
    if (is.null(vars)) {
        vars <- colnames(x)
    }

    # Remove missing variables
    if (!all(vars %in% colnames(x))) {
        if (warn) {
            warning("Some variables in vars are not included in x and, thus, are ignored!")
        }
        vars <- subset(vars, vars %in% colnames(x))
    }

    # Set minimum number of valid values
    if (min.val < 0) {
        min.val <- 3
    }

    # Number of valid values by respondent
    nval <- rowSums(apply(
        subset(x, select = vars), 2,
        function(x) {
            !(x %in% invalid)
        }
    ))

    # Create indicator
    if (append) {
        x$valid <- as.numeric(nval >= min.val)
        attr(x$valid, "label") <- paste0(
            "Case with at least ",
            min.val,
            " valid responses"
        )
        attr(x$valid, "labels") <- c("valid" = 1, "not valid" = 0)

        # Filter cases
    } else {
        x <- subset(x, nval >= min.val)
    }

    # Return results
    return(x)
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

convert_mv <- function(resp, variables = NULL, mvs = c(-97:-21)) {

    if (!is.null(mvs)) {
        if (is.null(variables)) variables <- colnames(resp)
        for (i in variables) {
            resp[[i]] <- replace(resp[[i]], resp[[i]] %in% mvs, NA)
        }
        return(resp)
    } else {
        stop("No user defined missing values provided.")
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

        return(vars[[paste0("position_", g, "_", items)]])
    }
}
