#' Score partial credit items
#'
#' @param resp  data.frame; contains original item responses
#' @param subtasks  character vector; contains variable names of subtasks
#' @param mvs  integer vector; contains user-defined missing values
#'
#' @export
pc_scoring <- function(resp, subtasks, mvs = NULL) {

    pci <- rowSums(resp[, subtasks] == 1)
    s <- rowSums(resp[, subtasks] < 0) > 0
    pci[s] <- -55

    if (is.null(mvs)) {
        mvs <- c(-99:-1)
        warning("No missing values provided. c(-99:-1) used as default.")
    }

    for (mv in mvs) {
        s <- rowSums(resp[, subtasks] == mv) == rowSums(resp[, subtasks] < 0) &
            rowSums(resp[, subtasks] < 0) > 0
        pci[s] <- mv
    }

    return(pci)
}



#' Collapse response categories with N < 200
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param pcitems character; indicates the logical variable in vars which
#'   contains the item names of the polytomous items
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @return resp with collapsed categories. Note that this function changes the
#'   given PC items IN PLACE. If you want to keep the original data, please
#'   copy and rename the items to be collapsed first.
#' @export
collapse_response_categories <- function(resp, vars, pcitems,
                                         path_table = here::here("Tables"),
                                         print = TRUE, save = FALSE) {
    collapsed_items <- c()
    for (item in vars$items[[pcitems]]) {
        response <- resp[[item]]
        tab <- table(response[response >= 0])
        collapse <- which(tab < 200)
        if (length(collapse) > 0) {
            collapsed_items <- c(collapsed_items, item)
        }
        while (length(collapse) > 0) {
            # if frequency of 0 too low, left shift for all values larger than 0
            if (names(tab)[collapse[1]] == "0") {
                j <- which(response > 0)
                # if frequency of 1 or higher too low, left shift for all values
                # larger than or equal to original value
            } else if (as.numeric(names(tab)[collapse[1]]) >= 1) {
                j <- which(response >= as.numeric(names(tab)[collapse[1]]))
            }
            resp[j, item] <- response[j] - 1
            response <- resp[[item]]
            tab <- table(response[response >= 0])
            collapse <- which(tab < 200)
        }
    }
    if (print) {
        # Which items have been collapsed?
        cat(paste(collapsed_items, collapse = ", "))
    }
    if (save) {
        collapsed_items <- as.matrix(collapsed_items)
        save_table(collapsed_items, filename = "collapsed_items.rds",
                   path = path_table, overwrite = TRUE, show_rownames = TRUE)
    }

    return(resp)
}



#' Dichotomous scoring of MC items
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which (unscored) items to use for the analysis
#' @param correct_response character; indicates the logical variable in vars
#'   which contains the correct response of the MC items
#'
#' @return resp with dichotomously scored MC items. Note that this function
#'   changes the given MC items IN PLACE. If you want to keep the original data,
#'   please copy and rename the items to be scored first.
#' @export
dichotomous_scoring <- function(resp, vars, select, correct_response) {
    for (k in unique(vars[[correct_response]][vars[[select]]])) {
        for (i in vars$items[vars[[correct_response]][vars[[select]]] == k]) {
            if (is.double(resp[[i]])) {
                resp[[i]] <- as.numeric(resp[[i]])
            }
            resp[[i]] <- ifelse(resp[[i]] == k, 1,
                                ifelse(resp[[i]] < 0, resp[[i]], 0))
            resp[[i]] <- as.numeric(resp[[i]])
        }
    }

    return(resp)
}
