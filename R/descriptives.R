#' Descriptives of continuous variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param digits  number of decimals for rounding
#'
#' @return   list with descriptives.
#'
#' @export

desc_con <- function(dat, desc, min.val = 3, digits = 2) {
    dat <- min_val(dat, min.val = min.val)
    sapply(dat[ , desc, drop = FALSE], function(x) {
        format(round(psych::describe(x), digits), nsmall = digits)
        })
}


#' Descriptives of nominal variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param digits  number of decimals for rounding percentages
#'
#' @return   list with descriptives.
#'
#' @export

desc_nom <- function(dat, desc, min.val = 3, digits = 1,
                      attributes = TRUE, absolute = TRUE, percentage = TRUE) {
    dat <- min_val(dat, min.val = min.val)
    descriptives <- list()

    if (attributes) {
        descriptives$attributes <- desc_attr(dat, desc = desc,
                                            min.val = min.val)
        }
    if (absolute) {
        descriptives$frequency_absolute <- desc_abs(dat, desc = desc,
                                                   min.val = min.val)
        }
    if (percentage) {
        descriptives$frequency_percentage <- desc_perc(dat, desc = desc,
                                                      min.val = min.val,
                                                      digits = digits)
    }

    return(descriptives)
}


#' Show attributes of list of variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#'
#' @return   list with attributes for each variable in desc.
#'
#' @export

desc_attr <- function(dat, desc, min.val = 3) {
    dat <- min_val(dat, min.val = min.val)
    sapply(dat[ , desc, drop = FALSE], function(x) {attributes(x)})
}



#' Show frequency (absolute) of answers of list of variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param digits  number of decimals for rounding
#'
#' @return   list with  frequency (absolute) of answers for each variable in desc.
#'
#' @export

desc_abs <- function(dat, desc, min.val = 3) {
    dat <- min_val(dat, min.val = min.val)
    sapply(dat[ , desc, drop = FALSE], function(x) {table(x, useNA = "always")})
}



#' Show frequency (relative) of answers of list of variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param min.val minimum number of valid values;
#'                if negative, set to the default of 3
#' @param digits  number of decimals for rounding
#'
#' @return   list with frequency (relative) of answers for each variable in desc.
#'
#' @export

desc_perc <- function(dat, desc, min.val = 3, digits = 1) {
    dat <- min_val(dat, min.val = min.val)
    sapply(dat[ , desc, drop = FALSE], function(x) {
        format(round(prop.table(table(x, useNA = "always"))*100, digits),
               nsmall = digits)
        })
}
