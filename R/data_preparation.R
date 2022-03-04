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
#' @param old_names  character vector; contains the names of the original items
#' @param new_names  character vector; contains the names of the new items,
#' must be in same order as parameter "old_names"
#'
#' @return resp with dichotomously scored MC items.
#' @export
dichotomous_scoring <- function(resp, vars, old_names, new_names = NULL) {
    if (is.null(new_names)) {
        new_names <- paste0(old_names, "_c")
    }

    for (i in seq_along(old_names)) {
        line <- which(vars$items == old_names[i])
        item <- vars$items[line]
        if (is.double(resp[[item]])) {
            resp[[item]] <- as.numeric(resp[[item]])
        } else if (is.factor(resp[[item]])) {
            resp[[item]] <- as.character(resp[[item]])
        }
        correct <- vars$correct_response[vars$items == item]
        resp[[new_names[i]]] <- ifelse(resp[[item]] == correct, 1,
                                       ifelse(resp[[item]] < 0, resp[[item]], 0)
        ) %>% as.numeric()
    }

    return(resp)
}


#' Duplicate item information in vars
#'
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param old_names  character vector; contains the names of the original items
#' @param new_names  character vector; contains the names of the new items (must
#' be in same order as parameter "old_names"), default is old name + "_c"
#' @param change named character vector; if some information in vars about the
#' new items shall be changed, include the respective variable as the name and the new
#' value as the value of the vector (e.g. change = c(raw = FALSE, dich = TRUE)),
#' if raw shall be set to FALSE and dich to TRUE for all new items.
#'
#' @return vars with new rows including the duplicated items.
#' @export
duplicate_items <- function(vars, old_names, new_names, change = NULL) {

        vars_new <- vars[vars$items %in% old_names, ]
    vars_new$items <- new_names
    if (!is.null(change)) {
        for (c in seq_along(change)) {
            variable <- names(change[c])
            new_value <- change[c]
            vars_new[[variable]] <- new_value
        }
    }

    vars <- rbind(vars, vars_new)

    return(vars)
}



#' Score partial credit items
#'
#' @param resp  data.frame; contains original item responses
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param mvs  integer vector; contains user-defined missing values
#'
#' @export

pc_scoring <- function(resp, poly_items, mvs = NULL) {

    for (item in names(poly_items)) {
        subitems <- poly_items[[item]]

        pci <- rowSums(resp[, subitems] == 1)
        s <- rowSums(resp[, subitems] < 0) > 0
        pci[s] <- -55

        if (is.null(mvs)) {
            mvs <- c(-99:-1)
            warning("No missing values provided. c(-99:-1) used as default.")
        }

        for (mv in mvs) {
            s <- rowSums(resp[, subitems] == mv) == rowSums(resp[, subitems] < 0) &
                rowSums(resp[, subitems] < 0) > 0
            pci[s] <- mv
        }

        resp[[item]] <- pci
    }

    return(resp)
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
#' @param per_cat integer; minimum number of persons per category; defaults to 200
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @return resp with collapsed categories. Note that this function changes the
#'   given PC items IN PLACE. If you want to keep the original data, please
#'   copy and rename the items to be collapsed first.
#' @export
collapse_response_categories <- function(resp, vars, pcitems, per_cat = 200,
                                         path_table = here::here("Tables"),
                                         print = TRUE, save = FALSE) {
    collapsed_items <- c()
    for (item in vars$items[[pcitems]]) {
        response <- resp[[item]]
        tab <- table(response[response >= 0])
        collapse <- which(tab < per_cat)
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
            collapse <- which(tab < per_cat)
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


#' Select sample with a minimum number of valid values
#'
#' @param resp  data.frame with item responses
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param min.val minimum number of valid values; if negative, set to the default of 3
#' @param invalid vector of invalid values (defaults to NA)
#'
#' @return   logical vector with length = nrow(resp), indicating whether case is valid
#' @export
min_val <- function(resp, vars, items, min.val = NULL, invalid = NA) {

    vrs <- vars$items[vars[[items]]]
    resp_ <- resp[ , vrs]

    # Set minimum number of valid values
    if (is.null(min.val) || min.val < 0) {
        min.val <- 3
        warning("No valid (=> 0) number of minimum valid responses per person ",
                "(min.val) provided. Default of 3 valid responses applies.")
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



#' Calculate new position variable with only a set of variables
#'
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param position  (named) character vector; defines name(s) of integer
#' variable(s) in vars that indicate position of items; if groups with differing
#' item positions in testlets exist, then vector must be named with names of
#' groups (as in "grouping") as names of elements and names of variables as elements
#'
#' @return   data.frame as input, with one or more extra variable(s) containing
#' the (relative) position of chosen items.
#' @importFrom rlang .data
#' @export
pos_new <- function(vars, items, position) {

    if (length(position) == 1) {

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

    }
    return(vars)
}


#' Calculate age from birth and test date
#'
#' @param resp  data.frame with birth and test date variables
#' @param birth_year  string; contains name of variable with year of birth
#' @param birth_month  string; contains name of variable with month of birth
#' @param birth_day  string; contains name of variable with day of birth (default is 15)
#' @param test_year  string; contains name of variable with year of test
#' @param tets_month  string; contains name of variable with month of test
#' @param test_day  string; contains name of variable with day of test (default is 15)
#'
#' @return   numeric vector with approximate age in years
calculate_age <- function(resp,
                          birth_year, birth_month, birth_day = NULL,
                          test_year, test_month, test_day = NULL) {

    if (is.null(birth_day)) {
        bday <- 15
    } else {
        bday <- resp[[birth_day]]
    }

    if (is.null(test_day)) {
        tday <- 15
    } else {
        tday <- resp[[test_day]]
    }

    birth <- strptime(paste(resp[[birth_year]], resp[[birth_month]], bday, sep = "-"), "%Y-%m-%d")
    test <- strptime(paste(resp[[test_year]], resp[[test_month]], tday, sep = "-"), "%Y-%m-%d")
    age <- as.numeric(difftime(birth, test, units = "weeks")) / 52.1429
    return(age)
}

# dichotomize_dif_variables <- function(variable, criteria, labels) {}
# geht das wirklich? z.B. migration kategorisch, books kontinuierlich
# lohnt sich auch kaum, da eh nur ein bis zwei Zeilen Code und ein Befehl
