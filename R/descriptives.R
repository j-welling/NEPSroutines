#' Number of valid cases
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet).
#' @param valid   character string. defines name of boolean variable in dat,
#' indicating (in)valid cases.
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
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param valid   character string. defines name of boolean variable in dat,
#' indicating (in)valid cases.
#' @param digits  number of decimals for rounding
#'
#' @return   list with descriptives.
#'
#' @export

desc_con <- function(dat, desc, valid = NULL, digits = 2) {

    dat <- only_valid(dat, valid = valid)

    sapply(dat[ , desc, drop = FALSE], function(x) {
            d <- round(psych::describe(x), digits)
            d[, -c(1:2)] <- format(d[, -c(1:2)], nsmall = digits)
            d
            })
}


#' Descriptives of nominal variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param valid   character string. defines name of boolean variable in dat,
#' indicating (in)valid cases.
#' @param digits  number of decimals for rounding percentages
#' @param show_absolute boolean; indicates whether to show frequency in
#' absolute numbers.
#' @param show_percentage boolean; indicates whether to show frequency in
#' relative numbers (percentage).
#'
#' @return   list with descriptives.
#'
#' @export

desc_nom <- function(dat, desc, valid = NULL, digits = 1,
                     show_absolute = TRUE, show_percentage = TRUE) {

    dat <- only_valid(dat, valid = valid)

    descriptives <- list()

    if (show_absolute) {
        descriptives$frequency_abs <- desc_abs(dat, desc = desc,
                                                valid = valid)
        }
    if (show_percentage) {
        descriptives$frequency_perc <- desc_perc(dat, desc = desc,
                                                 valid = valid,
                                                 digits = digits)
    }

    return(descriptives)
}


#' Show frequency (absolute) of answers of list of variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param valid   character string. defines name of boolean variable in dat,
#' indicating (in)valid cases.
#'
#' @return   list with  frequency (absolute) of answers for each variable in desc.
#'
#' @export

desc_abs <- function(dat, desc, valid = NULL) {

    dat <- only_valid(dat, valid = valid)

    sapply(dat[ , desc, drop = FALSE], function(x) {table(x, useNA = "always")})
}



#' Show frequency (relative) of answers of list of variables
#'
#' @param dat     data.frame with item responses (user-defined missing values)
#'                and logical grouping variables, named after the grouping
#'                factor (e.g. "easy" and "difficult" in the case of testlet.
#' @param desc    character. contains names of variables for descriptive analysis.
#' @param valid   character string. defines name of boolean variable in dat,
#' indicating (in)valid cases.
#' @param digits  number of decimals for rounding
#'
#' @return   list with frequency (relative) of answers for each variable in desc.
#'
#' @export

desc_perc <- function(dat, desc, valid = NULL, digits = 1) {

    dat <- only_valid(dat, valid = valid)

    data.frame(sapply(dat[ , desc, drop = FALSE], function(x) {
        d <- format(round(prop.table(table(x, useNA = "always"))*100, digits), nsmall = digits)
        paste0(d, " %")
        }))
}

#' Show attributes of selected variables
#'
#' @param dat data.frame containing variables
#' @param desc character vectors containing variables to be shown

show_attributes <- function(dat, desc) {
    for (var in desc) {
        print(attributes(dat[[var]])$labels)
    }
}
