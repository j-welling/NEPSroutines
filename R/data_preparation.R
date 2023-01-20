#' Dichotomous scoring of MC items
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param old_names  character vector; contains the names of the original items
#' @param new_names  character vector; contains the names of the new items. Must
#' be in same order as parameter "old_names"! (default is old name + "_c")
#'
#' @return resp with dichotomously scored MC items.
#' @export
dichotomous_scoring <- function(resp, vars, old_names, new_names = NULL) {

    # Check whether variables are indeed contained in data.frames
    scaling::check_variables(resp, "resp", old_names) ### add the name of package before the function

    # Check for duplicates
    scaling::check_items(old_names)     ### add the name of package before the function
    if(!is.null(new_names)) scaling::check_items(new_names)  ### add the name of package before the function

    # Create new names if no names are provided
    if (is.null(new_names)) {
        new_names <- paste0(old_names, "_c")
    }

    # Scoring
    for (i in seq_along(old_names)) {
        item <- old_names[i]
        if (is.double(resp[[item]])) {
            resp[[item]] <- as.numeric(resp[[item]])
        } else if (is.factor(resp[[item]])) {
            resp[[item]] <- as.character(resp[[item]])
        }
        correct <- vars$correct_response[vars$item == item]
        resp[[new_names[i]]] <- ifelse(resp[[item]] == correct, 1,
                                       ifelse(resp[[item]] < 0, resp[[item]], 0)
        ) %>% as.numeric()
    }

    return(resp)
}


#' Duplicate item information in vars
#'
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param old_names  character vector; contains the names of the original items
#' @param new_names  character vector; contains the names of the new items. Must
#' be in same order as parameter "old_names"!
#' @param change named character vector; if some information in vars about the
#' new items shall be changed, include the respective variable as the name and the new
#' value as the value of the vector (e.g. change = c(raw = FALSE, dich = TRUE)),
#' if raw shall be set to FALSE and dich to TRUE for all new items.
#'
#' @return vars with new rows including the duplicated items.
#' @export
duplicate_items <- function(vars, old_names, new_names, change = NULL) {

    vars_new <- vars[vars$item %in% old_names, ]
    vars_new$item <- new_names
    if (!is.null(change)) {
        for (c in seq_along(change)) {
            variable <- names(change[c])
            new_value <- change[c]
            vars_new[[variable]] <- new_value
            class(vars_new[[variable]]) <- class(vars[[variable]])
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

    # Check whether variables are indeed contained in data.frames
    scaling::check_numerics(resp, "resp", unlist(poly_items), dich = TRUE) ### add the name of package before the function

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
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select character; indicates the logical variable in vars which
#'   contains the item names of the polytomous items
#' @param per_cat integer; minimum number of persons per category; defaults to 200
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @return resp with collapsed categories. Note that this function changes the
#'   given PC items IN PLACE. If you want to keep the original data, please
#'   copy and rename the items to be collapsed first.
#'
#' @export
collapse_response_categories <- function(resp, vars, select, per_cat = 200,
                                         path_table = here::here("Tables"),
                                         save = FALSE) {

    # Check whether variables are indeed contained in data.frames
   ### check_logicals(vars, "vars", select) ### Deaktivate check_logicals for 'vars' because the variables from 'resp'(not from 'vars') are needed
   ### polyt_items <- vars$item[vars[[select]]] ### Changed (see line below), because the variables are not taken from the record 'vars', but from the vars$item
    polyt_items <- vars$item[(vars$item %in% select) == TRUE]
    scaling::check_numerics(resp, "resp", polyt_items) ### replace 'poly_items' by 'polyt_items' to avoid bug, because workspace already contains an object named poly_item
                                                       ### add the name of package befor the function
    scaling::check_items(polyt_items) ### replace 'poly_items' by 'polyt_items' to avoid bug, because workspace already contains an object named poly_item
                                                       ### add the name of package befor the function

    collapsed_items <- c()
    dichotomous_items <- c()
    problematic_items <- c()

    for (item in polyt_items) { ### replace 'poly_items' by 'polyt_items' to avoid bug, because workspace already contains an object named poly_item

        response <- resp[[item]]
        tab <- table(response[response >= 0])

        # More than one response category > per_cat?
        if (sum(tab >= per_cat) < 2) {
            if (max(resp[[item]], na.rm = TRUE) <= 1) {
                dichotomous_items <- c(dichotomous_items, item)
            } else {
                problematic_items <- c(problematic_items, item)
            }

        } else {

            collapse <- which(tab < per_cat)

            if (length(collapse) > 0) {
                collapsed_items <- c(collapsed_items, item)

                while (length(collapse) > 0) {

                    # if frequency of 0 too low, left shift for all values larger than 0
                    if (names(tab)[collapse[1]] == "0") {
                        j <- which(response > 0)
                        # if frequency of 1 or higher too low, left shift for all values
                        # larger than or equal to original value
                    } else if (as.numeric(names(tab)[collapse[1]]) >= 1) {
                        j <- which(response >= as.numeric(names(tab)[collapse[1]]))
                    }

                    response[j] <- response[j] - 1
                    tab <- table(response[response >= 0])
                    collapse <- which(tab < per_cat)
                }

                resp[ , paste0(item, "_collapsed")] <- response
            }
        }
    }

    # Which items have been collapsed?
    item_names <- data_frame(
        original_item = collapsed_items,
        collapsed_item = paste0(collapsed_items, "_collapsed")
    )

    # Print results
    if (!is.null(problematic_items)) {
      message("\nThe following items have less than two response categories with ",
              "more than ", per_cat, " cases and were thus not collapsed. ",
              "Please check these items manually:\n",
              paste(problematic_items, collapse = ", "))
    }

    if (!is.null(dichotomous_items)) {
      message("\nDichotomous items were not considered for collapsing. ",
              "The following items are dichotomous:\n",
              paste(dichotomous_items, collapse = ", "))
    }

    if (!is.null(collapsed_items)) {
      message("\nThe following items have been collapsed:\n")
      print(item_names)
    } else {
      message("\nNo items have been collapsed.")
    }

    # Save results
    if (save) {
        save_table(results = list(collapsed = item_names,
                                  dichotomous = dichotomous_items,
                                  problematic = problematic_items),
                   filename = "collapsed_items.rds",
                   path = path_table,
                   overwrite = TRUE,
                   show_rownames = TRUE)
    }

    return(resp)
}


#' Select sample with a minimum number of valid values
#'
#' @param resp  data.frame with item responses
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param min.val minimum number of valid values; if negative, set to the default of 3
#' @param invalid vector of invalid values (if not specified,
#' function defaults to NA and negative values)
#'
#' @return   logical vector with length = nrow(resp), indicating whether case is valid
#' @export
min_val <- function(resp, vars, select, min.val = NULL, invalid = NULL) {

    # Check whether variables are indeed contained in data.frames
    scaling::check_logicals(vars, "vars", select) ### add the name of package before the function
    items <- vars$item[vars[[select]]]
    scaling::check_numerics(resp, "resp", items)  ### add the name of package before the function
    resp_ <- resp[ , items]

    # Set minimum number of valid values
    if (is.null(min.val) || min.val < 0) {
        min.val <- 3
        warning("No valid (=> 0) number of minimum valid responses per person ",
                "(min.val) provided. Default of 3 valid responses applies.")
    }

    # Number of valid values by respondent
    nval <- rowSums(apply(
        subset(resp_, select = items), 2,
        function(x) {
            if (!is.null(invalid)) {
                !(x %in% invalid)
            } else (x >= 0 & !is.na(x))
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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
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
pos_new <- function(vars, select, position) {

    # Check whether variables are indeed contained in data.frames
    scaling::check_logicals(vars, "vars", select) ### add the name of package before the function
    scaling::check_numerics(vars, "vars", position, check_invalid = TRUE) ### add the name of package before the function

    if (length(position) == 1) {

        vars_ <- vars[vars[[select]], ]
        pos <- data.frame(item = vars_[['item']],
                          position = vars_[[position]])
        pos <- dplyr::arrange(pos, .data$position)
        pos[[paste0("position_", select)]] <- seq(1, nrow(pos))
        vars <- merge(vars, pos[ , c('item', paste0("position_", select))],
                      by = 'item', all = TRUE)

    } else {

        for (g in names(position)) {
            vars_ <- vars[vars[[select]] & !is.na(vars[[position[g]]]), ]
            pos <- data.frame(item = vars_[['item']],
                              position = vars_[[position[g]]])
            pos <- dplyr::arrange(pos, .data$position)
            pos[[paste0("position_", g, "_", select)]] <- seq(1, nrow(pos))
            vars <- merge(vars, pos[ , c('item', paste0("position_", g, "_", select))],
                          by = 'item', all = TRUE)
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
#' @export
calculate_age <- function(resp,
                          birth_year, birth_month, birth_day = NULL,
                          test_year, test_month, test_day = NULL) {

    # Check whether variables are indeed contained in data.frames
    scaling::check_variables(resp, "resp", c(birth_year, birth_month, ### add the name of package before the function
                                    test_year, test_month))

    # Check whether birth and test day exist and if not, replace with default 15
    if (is.null(birth_day)) {
        bday <- 15
    } else {
        scaling::check_variables(resp, "resp", birth_day) ### add the name of package before the function
        bday <- resp[[birth_day]]
    }

    if (is.null(test_day)) {
        tday <- 15
    } else {
        scaling::check_variables(resp, "resp", test_day) ### add the name of package before the function
        tday <- resp[[test_day]]
    }

    # Replace missing values in birth and test date with the sample median
    na_by <- is.na(resp[[birth_year]])
    na_bm <- is.na(resp[[birth_month]])
    na_ty <- is.na(resp[[test_year]])
    na_tm <- is.na(resp[[test_month]])

    if (sum(na_by) > 0) {
        message(sum(na_by), " missing value(s) in birth year were replaced by the sample median.")
        resp[[birth_year]][na_by] <- round(median(resp[[birth_year]], na.rm = TRUE))
    }

    if (sum(na_bm) > 0) {
        message(sum(na_bm), " missing value(s) in birth month were replaced by the sample median.")
        resp[[birth_month]][na_bm] <- round(median(resp[[birth_month]], na.rm = TRUE))
    }

    if (sum(na_ty) > 0) {
        message(sum(na_ty), " missing value(s) in test year were replaced by the sample median.")
        resp[[test_year]][na_ty] <- round(median(resp[[test_year]], na.rm = TRUE))
    }

    if (sum(na_tm) > 0) {
        message(sum(na_tm), " missing values in test month were replaced by the sample median.")
        resp[[test_month]][na_tm] <- round(median(resp[[test_month]], na.rm = TRUE))
    }

    # Calculate age
    birth <- strptime(paste(resp[[birth_year]], resp[[birth_month]],
                            bday, sep = "-"), "%Y-%m-%d")
    test <- strptime(paste(resp[[test_year]], resp[[test_month]],
                           tday, sep = "-"), "%Y-%m-%d")
    age <- as.numeric(difftime(test, birth, units = "weeks")) / 52.1429
    return(age)
}
