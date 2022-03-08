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

    dat <- only_valid(dat, valid = valid)

    message("There are ", nrow(dat[dat[[valid]], ]), " valid cases in the dataset.")
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

    data.frame(sapply(dat[ , desc, drop = FALSE], function(x) {
        d <- round(prop.table(table(x, useNA = useNA))*100, digits)
        paste0(d, " %")
        }))
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
