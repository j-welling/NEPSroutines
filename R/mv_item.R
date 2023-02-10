#' Missing values analysis by item - all in one function
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param position  (named) character vector; defines name(s) of integer
#' variable(s) in vars that indicate position of items; if groups with differing
#' item positions in testlets exist, then vector must be named with names of
#' groups (as in "grouping") as names of elements and names of variables as elements
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param plots  logical; whether plots shall be created and saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
# #' @param color  character calar or vector; defines color(s) of the bar in plots
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#' @param labels_legend character vector; contains legend labels
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be better set to true)
#' @param verbose  logical; whether to print processing information to console
#'
#' @return (if return = TRUE) list with results:
#'          mvlist: percentages for each item
#'          mvsum: summary statistics across items
#'          summary_table: table with summary statistics for TR
#' @export

mv_item <- function(resp, vars, select, valid = NULL,
                    position = NULL, grouping = NULL,
                    mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                            UM = -90, ND = -55, NAd = -54, AZ = -21),
                    labels_mvs = c(
                        ALL = "total missing items",
                        OM = "omitted items",
                        NV = "not valid items",
                        NR = "not reached items",
                        TA = "missing items due to test abortion",
                        UM = "unspecific missing items",
                        ND = "not determinable items",
                        NAd = "not administered items",
                        AZ = "missing items due to ''Angabe zurueckgesetzt''"
                    ),
                    plots = FALSE, print = TRUE, save = TRUE, return = FALSE,
                    path_results = here::here("Results"),
                    path_table = here::here("Tables"),
                    path_plots = here::here("Plots/Missing_Responses/by_item"),
                    show_all = TRUE, name_grouping = 'test version', #color = NULL,
                    overwrite = FALSE, digits = 3, warn = TRUE, verbose = TRUE,
                    labels_legend = NULL) {

    # Test data
    scaling:::check_logicals(resp, "resp", c(valid, grouping), warn = warn)
    scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
    scaling:::check_items(vars$item[vars[[select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    if (warn) scaling:::is_null_mvs_valid(valid = valid)

    # Conduct analysis
    mv_item <- scaling:::mvi_analysis(
        resp = resp,
        vars = vars,
        select = select,
        valid = valid,
        position = position,
        grouping = grouping,
        show_all = show_all,
        mvs = mvs,
        digits = digits,
        warn = warn,
        save = FALSE,
        test = FALSE
    )

    # Write grouped table
    mv_item$summary_table <- scaling:::mvi_table(
      mv_i = mv_item,
      vars = vars,
      select = select,
      mvs = mvs,
      grouping = grouping,
      warn = warn,
      test = FALSE,
      save = FALSE
    )

    # Write plots
    if (plots) scaling:::mvi_plots(
      mv_i = mv_item,
      vars = vars,
      select = select,
      grouping = grouping,
      mvs = mvs,
      labels_mvs = labels_mvs,
      show_all = show_all,
      verbose = verbose,
      #color = color,
      name_grouping = name_grouping,
      labels_legend = labels_legend,
      path = path_plots,
      warn = warn,
      test = FALSE
    )

    # Save results
    if (save) {
        scaling:::save_results(mv_item, filename = "mv_item.rds", path = path_results)
        scaling:::save_table(
          mv_item$summary_table,
          overwrite = overwrite,
          filename = "mv_item.xlsx",
          path = path_table
        )
    }

    # Print results
    if (print) {
        message("\nTable 1 shows missing values by item position and missing type.\n",
                "All other tables show summary statistics over all items.\n")
        print(mv_item$summary_table)
        message("\nSummary for TR\n")
        scaling:::print_mvi_results(
            mv_item,
            name_grouping = name_grouping,
            labels_mvs = labels_mvs
        )
    }

    # Return results
    if (return) return(mv_item)

}

#' Percentage of missing responses by item
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param position  (named) character vector; defines name(s) of integer
#' variable(s) in vars that indicate position of items; if groups with differing
#' item positions in testlets exist, then vector must be named with names of
#' groups (as in "grouping") as names of elements and names of variables as elements
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param path  string; defines path to folder where results shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return   list with percentages:
#'            mvlist: percentages for each item
#'            mvsum: summary statistics across items
#' @importFrom stats median sd na.omit
#' @export

mvi_analysis <- function(resp, vars, select, position, valid = NULL,
                         grouping = NULL, show_all = TRUE,
                         mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                                 UM = -90, ND = -55, NAd = -54, AZ = -21),
                         path = here::here("Results"), save = TRUE,
                         digits = 3, warn = TRUE, test = TRUE) {

    # Test data
    scaling:::check_numerics(vars, "vars", position, check_invalid = TRUE)

    if (test) {
        scaling:::check_logicals(resp, "resp", c(valid, grouping), warn = warn)
        scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
        scaling:::check_items(vars$item[vars[[select]]])
        scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
        if (warn) scaling:::is_null_mvs_valid(valid = valid)
    }

    if (is.null(grouping)) {
        if (length(position) > 1) {
            stop("No grouping. Please provide only one position variable.")
        }
    } else {
        if (length(position) == 1 & warn) {
            warning("Only one position variable provided. ",
                    "The items of each group are therefore set to the same positions.")
        } else if (length(position) != length(grouping)) {
            stop("Position and grouping variables do not match. ",
                 "Please provide either only one position variable (if positions of items do not differ between groups) ",
                 "or matching position and grouping variables (if positions of items do differ between groups).")
        }
    }


    # Prepare data
    vars_c <- vars[vars[[select]], ]
    resp <- scaling:::only_valid(resp, valid = valid)
    resp_c <- scaling:::prepare_resp(resp, vars = vars, select = select, warn = warn,
                           zap_labels = FALSE)

    # NAs are not acknowledged in mvs-argument
    if (warn & !(NA %in% mvs) & any(resp_c %in% NA)) {
        warning("NAs found in resp! These values are ignored.")
    }

    # Create results
    if (is.null(grouping)) {

        # Create list with results
        mvlist <- scaling:::create_mvlist(
            item = vars_c$item,
            position = vars_c[[position]],
            responses = resp_c,
            mvs = mvs,
            digits = digits
        )

        # Summary across items
        mvsum <- scaling:::mvi_summary(mvlist[ , -c(1:2)], digits = digits)

    } else {

        # Create list with results
        mvlist <- list()
        mvsum <- list()

        for (g in grouping) {

            # Select administered items for the group
            vars_g <- vars_c[vars_c[[g]], ]
            resp_g <- resp_c[resp[[g]], vars_g$item]

            # Number of valid responses and position
            if (length(position) == 1) {
                pos <- vars_g[[position]]
            } else {
                pos <- vars_g[[position[g]]]
            }

            # Create list with results
            mvlist[[g]] <- scaling:::create_mvlist(
              item = vars_g$item[!is.na(pos)],
              position = na.omit(pos),
              responses = resp_g[, vars_g$item[!is.na(pos)]],
              mvs = mvs,
              digits = digits
            )

            # Summary across items
            mvsum[[g]] <- scaling:::mvi_summary(mvlist[[g]][ , -c(1:2)], digits = digits)
        }

        if (length(position) > 1) {

            # Creating new dataframe with responses by position, not by item
            resp_p <- data.frame(position = NA)

            for (g in grouping) {

                r <- data.frame(t(resp_c[resp[[g]], ]))
                r$item <- rownames(r)
                r <- merge(r, vars_c[, c('item', position[g])], by = "item")
                r <- dplyr::rename(r, position = as.character(position[g]))
                r <- dplyr::select(r,-.data$item)
                resp_p <- dplyr::full_join(resp_p, r, by = "position")
            }

            resp_p <- dplyr::arrange(resp_p[!is.na(resp_p$position), ], position)

            # Number of valid responses and position
            pos <- resp_p$position
            resp_p <- dplyr::select(resp_p, -.data$position) %>% t() %>% data.frame()
            mvlist$all <- data.frame(
              position = pos,
              N = apply(resp_p, 2, function(x) sum(x >= 0, na.rm = TRUE))
            )

            # Determine percentage of missing values for each missing type
            results <- data.frame(mvi_calc(resp_p, mvs = mvs, digits = digits))
            mvlist$all <- cbind(mvlist$all, results)

            # Summary across items
            mvsum$all <- scaling:::mvi_summary(mvlist$all[ , -1], digits = digits)
        }
    }

    # List with results
    mv_i <- list(list = mvlist, summary = mvsum)

    # Save results
    if (save) {
        scaling:::save_results(mv_i, filename = "mv_item.rds", path = path)
    }

    # Return results
    return(mv_i)
}


#' Write table of user defined missing values per item (and position)
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param mv_i  list; return object of mvi_analysis()
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param path  string; defines path to folder where table shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return table with results
#' @export

mvi_table <- function(mv_i, vars, select, grouping = NULL,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      save = TRUE, path = here::here("Tables"),
                      overwrite = FALSE, test = TRUE, warn = TRUE) {

    # Test data
    if (test) {
        scaling:::test_mvi_data(mv_i, mvs = mvs, grouping = grouping)
        scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
        scaling:::check_items(vars$item[vars[[select]]])
    }

    # Create table

    if (is.null(grouping)) {

        results <- mv_i

    } else {

        results <- list()
        results$list <- data.frame(item = vars$item[vars[[select]]])
        results$summary_all <- mv_i$summary$all

        for (g in grouping) {
            mv <- mv_i$list[[g]]
            names(mv)[-1] <- apply(data.frame(names(mv)[-1]), 2, function(x) {
                paste0(x, "_", g)
            })
            results$list <- dplyr::full_join(results$list, mv, by = "item")
            results[[paste0("summary_", g)]] <- mv_i$summary[[g]]
        }
    }

    # Save table
    if (save) {
        scaling:::save_table(
          results,
          filename = "mv_item.xlsx",
          path = path,
          overwrite = overwrite,
          show_rownames = FALSE
        )
    }

    # Return table
    return(results)
}


#' Plot percentages of user-defined missing values by item position
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param mv_i  list; return object of mvi_analysis()
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param path  string; defines path to folder where plots shall be saved
# #' @param color  character calar or vector; defines color(s) of the bar in plots
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#' @param labels_legend character vector; contains legend labels
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @importFrom rlang .data
#' @export

mvi_plots <- function(mv_i, vars, select, grouping = NULL,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      labels_mvs = c(
                          ALL = "total missing items",
                          OM = "omitted items",
                          NV = "not valid items",
                          NR = "not reached items",
                          TA = "missing items due to test abortion",
                          UM = "unspecific missing items",
                          ND = "not determinable items",
                          NAd = "not administered items",
                          AZ = "missing items due to ''Angabe zurueckgesetzt''"
                      ),
                      path = here::here("Plots/Missing_Responses/by_item"),
                      name_grouping = 'test version', #color = NULL,
                      show_all = TRUE, verbose = TRUE, warn = TRUE,
                      test = TRUE, labels_legend = NULL) {

    # Test data
    if (test) {
        scaling:::test_mvi_data(mv_i, mvs = mvs, grouping = grouping)
        scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
        scaling:::check_items(vars$item[vars[[select]]])
    }

    # Pepare data
    mv_i <- mv_i$list
    k <- scaling:::create_ifelse(
        is.null(grouping),
        sum(vars[[select]]),
        max(sapply(grouping, function(x) {sum(vars[[select]] & vars[[x]])}), na.rm = TRUE)
    )

    if(!is.null(grouping))
        groups <- scaling:::create_ifelse(show_all, c(grouping, 'all'), grouping)

    # Create directory for plots
    scaling:::check_folder(path)

    # Create plots

    for (i in names(mvs)) {

        if (is.null(grouping)) {

            # create plot
            y <- mv_i[[i]]
            ylim <- ceiling(max(y, na.rm = TRUE)/10)*10

            gg <- ggplot2::ggplot(data = mv_i,
                                  mapping = ggplot2::aes(x = .data$position, y = y)
            ) +
                ggplot2::labs(
                    title = paste0(Hmisc::capitalize(labels_mvs[i]), " by item position"),
                    x = "Item position", y = "Percentage"
                )

        } else {

            mv <- data.frame(position = NA)

            for (g in groups) {
                mv_i[[g]][[g]] <- mv_i[[g]][[i]]
                mv <- merge(mv, mv_i[[g]], by = 'position', all = TRUE)
            }

            mv <- dplyr::filter(dplyr::select(mv, c("position",
                                                    tidyselect::all_of(groups))),
                                !is.na(mv$position))

            mv_wide <- tidyr::gather(mv, key = "group", value = "MV",
                                     tidyselect::all_of(groups))
            ylim <- ceiling(max(mv_wide$MV, na.rm = TRUE)/10)*10

            # create plot
            gg <- ggplot2::ggplot(
                data = mv_wide,
                mapping = ggplot2::aes(x = .data$position, y = .data$MV, fill = .data$group)
            ) +
                ggplot2::labs(
                    title = paste0(Hmisc::capitalize(labels_mvs[i]), " by item position and ",
                                   name_grouping),
                    x = "Item position", y = "Percentage"
                ) +
                if (is.null(labels_legend)) {
                    scale_fill_discrete(name = Hmisc::capitalize(name_grouping))
                } else {
                    if (length(labels_legend) != length(groups)) {
                        warning("Number of provided legend labels does not ",
                                "correspond to number of groups. ",
                                "Group labels are used instead.")
                        scale_fill_discrete(name = Hmisc::capitalize(name_grouping))
                    } else {
                        scale_fill_discrete(
                            name = Hmisc::capitalize(name_grouping),
                            labels = labels_legend
                        )
                    }
                }
        }

        gg <- gg +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::scale_y_continuous(breaks = seq(0, ylim, 10),
                                        labels = paste0(seq(0, ylim, 10), " %"),
                                        limits = c(0, ylim)) +
            ggplot2::scale_x_continuous(breaks = seq(0, k, ifelse(k < 20, 1, floor(k/20)))) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.justification = c(0, 1),
                           legend.position = c(0.01, 0.99))

        # save plot
        ggplot2::ggsave(
            filename = paste0("Missing_responses_by_item (", i,").png"),
            plot = gg, path = path, width = 2000, height = 1200, units = "px",
            dpi = 300
        )

        # Print progress
        if (verbose) cat("Missing plot", i, "created.\n")
    }
}


#' Print mvi results to console
#'
#' @param mv_i  list; return object of mvi_analysis()
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#'
#' @export

print_mvi_results <- function(mv_i,
                              name_grouping = 'test version',
                              labels_mvs = c(
    ALL = "total missing items",
    OM = "omitted items",
    NV = "not valid items",
    NR = "not reached items",
    TA = "missing items due to test abortion",
    UM = "unspecific missing items",
    ND = "not determinable items",
    NAd = "not administered items",
    AZ = "missing items due to ''Angabe zurueckgesetzt''"
)) {
    if (is.data.frame(mv_i$list)) {
        for (lbl in names(mv_i$list[-c(1:3)])) {
            mv_min <- min(mv_i$list[[lbl]], na.rm = TRUE)
            mv_max <- max(mv_i$list[[lbl]], na.rm = TRUE)
            item_min <- mv_i$list$item[mv_i$list[[lbl]] == mv_min]
            item_max <- mv_i$list$item[mv_i$list[[lbl]] == mv_max]
            message("The proportion of ", labels_mvs[lbl], " varied between ",
                    mv_min, "%", if(length(item_min) <= 3) {
                        paste0(" (", ifelse(length(item_min) > 1, "items ", "item "),
                               paste(item_min, collapse = ", "), ")")
                    }, " and ",
                    mv_max, "%", if(length(item_max) <= 3) {
                        paste0(" (", ifelse(length(item_max) > 1, "items ", "item "),
                               paste(item_max, collapse = ", "), ")")
                    }, ".")
        }
    } else {
        for (g in names(mv_i$list)[-length(names(mv_i$list))]) {
            message("\n", Hmisc::capitalize(g), ":\n")
            for (lbl in names(mv_i$list[[g]][-c(1:3)])) {
                mv_min <- min(mv_i$list[[g]][[lbl]], na.rm = TRUE)
                mv_max <- max(mv_i$list[[g]][[lbl]], na.rm = TRUE)
                item_min <- mv_i$list[[g]]$item[mv_i$list[[g]][[lbl]] == mv_min]
                item_max <- mv_i$list[[g]]$item[mv_i$list[[g]][[lbl]] == mv_max]
                message("The proportion of ", labels_mvs[lbl], " in the ", g,
                        " ", name_grouping, " varied between ",
                        mv_min, "%", if(length(item_min) <= 3) {
                            paste0(" (", ifelse(length(item_min) > 1, "items ", "item "),
                                   paste(item_min, collapse = ", "), ")")
                        }, " and ",
                        mv_max, "%", if(length(item_max) <= 3) {
                            paste0(" (", ifelse(length(item_max) > 1, "items ", "item "),
                                   paste(item_max, collapse = ", "), ")")
                        }, ".")
            }
        }
    }
}


#' Calculate and round frequency (in percentage) of one missing value type (by item)
#'
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param digits  integer; number of decimals for rounding
#'
#' @return table with frequency of missing values for one missing value type
#' (by item)
#' @noRd

mvi_perc <- function(responses, mvs, digits = 3) {
    perc <- data.frame(apply(responses, 2, function(x) {
        ifelse(x %in% mvs, 1, ifelse(!is.na(x), 0, NA))
    }))
    round(apply(perc, 2, mean, na.rm = TRUE) * 100, digits)
}


#' Create list with frequency of missing responses by item for each missing value type
#'
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param digits  integer; number of decimals for rounding
#'
#' @return list with frequency of missing values by item for each missing value
#' type in mvs
#' @noRd

mvi_calc <- function(responses, mvs, digits = 3) {

    result <- list()

    # Determine percentage of missing values for each missing type
    for (i in names(mvs)) {
        result[[i]] <- scaling:::mvi_perc(responses = responses, mvs = mvs[[i]], digits = digits)
    }

    # Percentage of total missing responses for each item
    result$ALL <- scaling:::mvi_perc(responses = responses, mvs = mvs, digits = digits)

    return(result)
}


#' Create list with missing values per item
#'
#' @param item  character vector; contains names of items
#' @param position  integer vector; contains position of items
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param digits  integer; number of decimals for rounding
#'
#' @return   list with results of missing values per item.
#' @noRd

create_mvlist <- function(item, position, responses, mvs, digits = 3) {

    if (length(item) != length(position) |
        ncol(responses) != length(item) |
        ncol(responses) != length(position)) {
        stop("Number of items in data.frame responses, in vector item and in vector position do not match. ",
             "Please provide matching arguments to function create_mvlist().")
    }

    # Create list
    mvlist <- data.frame(
        item = item,
        position = position,
        N = colSums(apply(responses, 2, function(x) !(x %in% mvs)))
    )

    # Merge with percentage of missing values for each missing type
    results <- data.frame(mvi_calc(responses, mvs = mvs, digits = digits))
    results$item <- row.names(results)
    mvlist <- merge(mvlist, results, by = 'item')

    # Return list
    return(mvlist)
}


#' MVI summary
#'
#' @param mvlist  list; return object of create_mvlist()
#' @param digits  integer; number of decimals for rounding
#'
#' @return  data.frame with mean, median, SD, min and max of missing values
#' @noRd

mvi_summary <- function(mvlist, digits = 3) {
    mvsum <- data.frame(t(apply(mvlist, 2, function(x) {
        round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
    })))
    mvsum[1, ] <- round(mvsum[1, ])
    colnames(mvsum) <- c("Mean", "SD", "Median", "Min", "Max")
    return(mvsum)
}

#' Test mv_i for completeness
#'
#' @param mv_i  list; return object of mvi_analysis()
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#'
#' @noRd

test_mvi_data <- function(mv_i, grouping, mvs) {

    if (is.null(grouping)) {
        nms <- names(mv_i$list)[-length(mv_i$list)]
    } else {
        nms <- names(mv_i$list$all)[-length(mv_i$list$all)]
    }

    if (any(!(names(mvs) %in% nms))) {
        stop("Please provide mv_i with all specified missing values.")
    }
}
