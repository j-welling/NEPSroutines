#' Number of valid cases
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#'
#' @return   character string with answer.
#'
#' @export

n_valid <- function(dat, valid = NULL) {

    check_logicals(resp, "resp", valid, warn = TRUE)

    if (!is.null(valid)) {
        n_inval <- sum(!dat[[valid]])
        n_val <- sum(dat[[valid]])
    } else {
        n_inval <- 0
        n_val <- nrow(dat)
        warn("No variable to identify (in)valid cases provided. Thus, all cases are counted as valid.")
    }

    message("There are ", n_val, " valid cases and ", n_inval, " invalid cases ",
    "in the dataset.")
}


#' Descriptives of continuous variables
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param digits  integer; number of decimals for rounding
#' @param print  logical; whether results shall be printed to console
#' @param return  logical; whether results shall be returned
#'
#' @return   list with descriptives.
#'
#' @export

desc_con <- function(dat, desc, valid = NULL, digits = 2,
                     print = TRUE, return = FALSE) {

    dat <- only_valid(dat, valid = valid)

    stats <- data.frame(sapply(dat[ , desc, drop = FALSE], function(x) {
             d <- data.frame(psych::describe(x))
             d[, -c(1:2)] <- format(round(d[, -c(1:2)], digits), nsmall = digits)
             d
             }))

    if (print) {
        message("\nSummary statistics of the continuous variables:\n")
        print(stats)
    }

    if (return) return(stats)
}


#' Descriptives of nominal variables
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param digits  integer; number of decimals for rounding of percentages
#' @param print  logical; whether results shall be printed to console
#' @param return  logical; whether results shall be returned
#'
#' @return   list with descriptives.
#'
#' @export

desc_nom <- function(dat, desc, valid = NULL, digits = 1,
                     print = TRUE, return = FALSE) {

    dat <- only_valid(dat, valid = valid)

    descriptives <- list()
    descriptives$frequency_abs <- desc_abs(dat, desc = desc, valid = valid,
                                           warn = FALSE)
    descriptives$frequency_perc_NA <- desc_perc(dat, desc = desc, valid = valid,
                                                warn = FALSE, useNA = 'always',
                                                digits = digits)
    descriptives$frequency_perc_noNA <- desc_perc(dat, desc = desc, valid = valid,
                                                  warn = FALSE, useNA = 'no',
                                                  digits = digits)

    if (print) {
        message("\nSample size by groups:\n")
        print(descriptives$frequency_abs)

        message("\nFrequency of groups including missing values:\n")
        print(descriptives$frequency_perc_NA)

        message("\nFrequency of groups excluding missing values:\n")
        print(descriptives$frequency_perc_noNA)
    }

    if (return) return(descriptives)
}


#' Show frequency (absolute) of answers of list of variables
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no parameter 'valid' provided
#'
#' @return   list with  frequency (absolute) of answers for each variable in desc.
#'
#' @export

desc_abs <- function(dat, desc, valid = NULL, warn = TRUE) {

    dat <- only_valid(dat, valid = valid, warn = warn)

    sapply(dat[ , desc, drop = FALSE], function(x) {table(x, useNA = "always")})
}


#' Show frequency (relative) of answers of list of variables
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no parameter 'valid' provided
#' @param useNA  string; "no" if NAs should not be included in table, "always"
#' (default) if NAs should be included in table
#' @param digits  integer; number of decimals for rounding
#'
#' @return   list with frequency (relative) of answers for each variable in desc.
#'
#' @export

desc_perc <- function(dat, desc, valid = NULL, warn = TRUE, useNA = 'always',
                      digits = 1) {

    dat <- only_valid(dat, valid = valid, warn = warn)

    sapply(dat[ , desc, drop = FALSE], function(x) {
        d <- round(prop.table(table(x, useNA = useNA))*100, digits)
        paste0(d, " %")
        })
}


#' Show attributes of selected variables
#'
#' @param dat  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @export

show_attributes <- function(dat, desc) {
    for (var in desc) {
        message("\nThe attributes for variable ", var, " are:\n")
        print(attributes(dat[[var]])$labels)
    }
}


#' Table with sample size by test version
#'
#' @param dat  data.frame; contains version variable
#' @param versions string; defines name of variable in dat that identifies test versions
#' @param versions_lbls named character vector; links a label each value of versions
#' (e.g. versions_lbls = c(version1 = 1, version2 = 2))
#' @param save  logical; whether to save the table in Excel
#' @param overwrite logical; whether to overwrite an existing table with the same name
#' @param path string; defines path for saving the table
#'
#' @return table with sample size by test version
#' @export

sample_by_version <- function(dat, versions, versions_lbls = NULL, save = FALSE,
                              overwrite = FALSE, path = here::here("Tables")) {

    # Create table with results
    df <- as.data.frame.AsIs(table(dat[[versions]]))
    df <- rbind(df,sum(df))
    names(df) <- 'N'
    rownames(df)[nrow(df)] <- c("Total")

    # Add labels as row names
    if(!is.null(versions_lbls) | !is.null(attributes(vars[[versions]])$labels)) {
        lbls <- create_ifelse(is.null(versions_lbls),
                              attributes(vars[[versions]])$labels,
                              versions_lbls)
        for (v in seq(nrow(df)-1)) {
            rownames(df)[v] <- names(which(rownames(df)[v] == lbls))
        }
    }

    # Convert results
    res <- data.frame(t(df))

    # Save results
    if (save) {
        save_table(res, filename = paste0("samplesize_by_", versions, ".xlsx"),
                   path = path, overwrite = overwrite, show_rownames = FALSE)
    }

    # Return results
    return(res)
}


#' Table with item properties by test version
#'
#' @param vars data.frame; contains information about the competence items
#' @param versions string; defines name of variable in vars that identifies test versions
#' @param props string; defines name of variable in vars that identifies item properties
#' @param versions_lbls named character vector; links a label each value of versions
#' (e.g. versions_lbls = c(version1 = 1, version2 = 2))
#' @param versions_lbls named character vector; links a label each value of props
#' (e.g. props_lbls = c(prop1 = 1, prop2 = 2))
#' @param save  logical; whether to save the table in Excel
#' @param overwrite logical; whether to overwrite an existing table with the same name
#' @param path string; defines path for saving the table
#'
#' @return table with item properties by test version
#' @export

props_by_version <- function(vars, select, versions, props,
                             versions_lbls = NULL, props_lbls = NULL,
                             save = FALSE, overwrite = FALSE,
                             path = here::here("Tables")) {

    # Create empty data frame
    vars <- subset(vars, vars[[select]])
    tbl <- addmargins(table(vars[[props]], vars[[versions]]))

    # Add property labels as row names
    if(!is.null(props_lbls) | !is.null(attributes(vars[[props]])$labels)) {
        lbls <- create_ifelse(is.null(props_lbls),
                              attributes(vars[[props]])$labels,
                              props_lbls)
        for (v in seq(nrow(tbl))) {
            rownames(tbl)[v] <- names(which(rownames(tbl)[v] == lbls))
        }
    }

    # Add version labels as column names
    if(!is.null(versions_lbls) | !is.null(attributes(vars[[versions]])$labels)) {
        lbls <- create_ifelse(is.null(versions_lbls),
                              attributes(vars[[versions]])$labels,
                              versions_lbls)
        for (v in seq(nrow(tbl))) {
            colnames(tbl)[v] <- names(which(colnames(tbl)[v] == lbls))
        }
    }

    # Save results
    if (save) {
        save_table(tbl, filename = paste0(props, "_by_", versions, ".xlsx"),
                   path = path, overwrite = overwrite, show_rownames = TRUE)
    }

    # Return results
    return(tbl)
}
