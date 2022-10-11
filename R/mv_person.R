#' Missing values by person analysis
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
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param plots  logical; whether plots shall be created and saved to hard drive
#' @param save  logical; whether results shall be saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
# #' @param color  character scalar or vector; defines color(s) of the bar in plots
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#' @param labels_legend character vector; contains legend labels
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be set to TRUE to
#' avoid problems with the data structure)
#' @param verbose  logical; whether to print processing information to console
#'
#' @return   (if return = TRUE) list with missing values per person and
#' each missing value type (in percentage)
#'
#' @export

mv_person <- function(resp, vars, select, valid = NULL, grouping = NULL,
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
                      path_plots = here::here("Plots/Missing_Responses/by_person"),
                      show_all = FALSE, overwrite = FALSE, #color = NULL,
                      name_grouping = 'test version', labels_legend = NULL,
                      digits = 2, warn = TRUE, verbose = TRUE) {

    # Test data
    check_logicals(resp, "resp", c(valid, grouping), warn = warn)
    check_logicals(vars, "vars", c(select, grouping), warn = warn)
    check_items(vars$item[vars[[select]]])
    if (warn) is_null_mvs_valid(valid = valid)

    # Create list for results
    mv_person <- list()

    # Conduct analysis
    mv_person$mv_p <- mvp_analysis(resp = resp, vars = vars, select = select,
                                   valid = valid, mvs = mvs, grouping = grouping,
                                   digits = digits, warn = warn, test = FALSE)

    # Create plots
    if (plots) {
        mvp_plots(mv_p = mv_person$mv_p, vars = vars, select = select, mvs = mvs,
                  labels_mvs = labels_mvs, grouping = grouping, show_all = show_all,
                  path = path_plots, verbose = verbose, warn = warn, #color = color,
                  name_grouping = name_grouping, labels_legend = labels_legend,
                  test = FALSE)
    }

    # Create table
    if (save) {
        mv_person$summary <- mvp_table(mv_p = mv_person$mv_p, grouping = grouping,
                                       filename = "mv_person", path = path_table,
                                       overwrite = overwrite, mvs = mvs)

        save_results(mv_person, filename = "mv_person.rds", path = path_results)
        # dass hier kein save_table steht ist Absicht
    } else {
        mv_person$summary <- mvp_table(mv_p = mv_person$mv_p,
                                       grouping = grouping,
                                       mvs = mvs)
    }

    # Print results
    if (print) print_mvp_results(mv_person$mv_p)

    # Return results
    if (return) return(mv_person)
}


#' Percentage of missing responses by person
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
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param path  string; defines path to folder where results shall be saved
#' @param filename  string; defines name of file that shall be saved
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return   list with missing values per person and
#' for each missing value type (in percentage) is returned
#'
#' @export

mvp_analysis <- function(resp, vars, select, valid = NULL, grouping = NULL,
                         mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                                 UM = -90, ND = -55, NAd = -54, AZ = -21),
                         filename = NULL, path = here::here("Results"),
                         digits = 2, warn = TRUE, test = TRUE) {

    # Test data
    if (test) {
        check_logicals(vars, "vars", c(grouping, select), warn = warn)
        check_logicals(resp, "resp", c(grouping, valid), warn = warn)
        check_items(vars$item[vars[[select]]])
        if (warn) is_null_mvs_valid(valid = valid)
    }

    # NAs are not acknowledged in mvs-argument
    if (!(NA %in% mvs) & any(resp %in% NA)) {
        warning("NAs found in resp! These values are ignored.")
    }

    # Create list for results
    mv_p <- list()

    # Prepare data
    resp <- only_valid(resp, valid = valid)
    resp_c <- prepare_resp(resp, vars = vars, select = select, warn = warn,
                           zap_labels = FALSE)

    # Calculate mvs per person
    if (is.null(grouping)) {

        # Determine percentage of missing values for each missing type
        results <- mvp_calc(responses = resp_c, mvs = mvs)
        mv_p <- mvp_summary(results, digits = digits)

    } else {

        results <- list()
        results$all <- NA

        for (g in grouping) {

            resp_g <- resp_c[resp[[g]], vars$item[vars[[select]] & vars[[g]]]]

            # Create dataframe with missing values per person and missing value type
            results[[g]] <- mvp_calc(responses = resp_g, mvs = mvs)
            results$all <- rbind(results$all, results[[g]])

            # Calculate summaries
            mv_p[[g]]<- mvp_summary(results[[g]], digits = digits)
        }

        # Calculate summaries for whole sample
        mv_p$all<- mvp_summary(results$all, digits = digits)

    }

    # Save results
    save_results(mv_p, filename = filename, path = path)

    # Return results
    return(mv_p)
}


#' Write table of user defined missing values per item (and position)
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param mv_p  list; return object of mv_person()
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#' @param filename  string; defines name of table that shall be saved
#' @param path  string; defines path to folder where table shall be saved
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return   table with missing values per person and
#' for each missing value type (in percentage)
#'
#' @export

mvp_table <- function(mv_p, grouping = NULL, overwrite = FALSE,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      filename = NULL, path = here::here("Tables"),
                      test = TRUE) {

    # Test data
    if (test) {
        test_mvp_data(mv_p, mvs = mvs, grouping = grouping)
    }

    # Create table
    if (is.null(grouping)) {
        results <- write_mvp_table(mv_p)
    } else {
        results <- list()
        for (g in names(mv_p)) {
            results[[g]] <- write_mvp_table(mv_p[[g]])
        }
    }

    # Save table
    if (!is.null(filename)) {

        # Create directory for table
        check_folder(path)

        # Write table as Excel sheet
        if (is.null(grouping)) {
            openxlsx::write.xlsx(results,
                                 file = paste0(path, "/", filename, ".xlsx"),
                                 showNA = FALSE, rowNames = FALSE, overwrite = overwrite)
        } else {
            for (g in names(mv_p)) {
                openxlsx::write.xlsx(results[[g]],
                                     file = paste0(path, "/", filename, "_", g, ".xlsx"),
                                     showNA = FALSE, rowNames = FALSE, overwrite = overwrite)
            }
        }
    }

    # Return table
    return(results)
}


#'
#' Plot percentage of missing values for persons
#'
#' @param mv_p  list; return object of mv_person()
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
#' @param path  string; defines path to folder where X shall be saved
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
# #' @param color  character calar or vector; defines color(s) of the bar in plots
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#' @param labels_legend character vector; contains legend labels
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @importFrom rlang .data
#' @export

mvp_plots <- function(mv_p, vars, select, grouping = NULL,
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
                      path = here::here("Plots/Missing_Responses/by_person"),
                      show_all = FALSE, verbose = TRUE, # color = NULL,
                      name_grouping = 'test version', labels_legend = NULL,
                      warn = TRUE, test = TRUE) {

    # Test data
    if (test) {
        test_mvp_data(mv_p, mvs = mvs, grouping = grouping)
        check_logicals(vars, "vars", c(select, grouping), warn = warn)
        check_items(vars$item[vars[[select]]])
    }

    # Prepare data
    mv_all <- create_ifelse(is.null(grouping), mv_p, mv_p$all)
    k <- create_ifelse(is.null(grouping), sum(vars[[select]]),
                       max(sapply(grouping, function(x) {
                           sum(vars[[select]] & vars[[x]])
                       }), na.rm = TRUE))

    if(!is.null(grouping))
        groups <- create_ifelse(show_all, c(grouping, 'all'), grouping)

    # Test labels
    if (any(!names(mv_all) %in% names(labels_mvs))) {
        stop("Please provide labels for each missing value type specified in mv_p.")
    }

    # Create directory for plots
    check_folder(path)

    # for each missing type
    for (i in names(mv_all)) {

        if (is.null(grouping)) {

            end <- max(as.double(names(mv_p[[i]])))
            mv <- data.frame(number = seq(0, end), y = rep(0, end + 1))
            mv$y[mv$number %in% names(mv_p[[i]])] <- mv_p[[i]]
            ylim <- ceiling(max(mv$y, na.rm = TRUE)/10)*10

            gg <- ggplot2::ggplot(data = mv,
                                  mapping = ggplot2::aes(x = .data$number, y = .data$y)
            ) +
                ggplot2::labs(
                    title = paste0(Hmisc::capitalize(labels_mvs[i]), " by person"),
                    x = paste0("Number of ", labels_mvs[i]),
                    y = "Percentage"
                )

        } else {

            end <- max(as.double(names(mv_all[[i]])))
            mv <- data.frame(matrix(0, nrow = end + 1, ncol = length(groups)))
            names(mv) <- groups
            mv$number <- seq(0, end)

            for (g in groups) {
                mv[[g]][mv$number %in% names(mv_p[[g]][[i]])] <- mv_p[[g]][[i]]
            }

            mv_wide <- tidyr::gather(mv, key = "group", value = "MV",
                                     tidyselect::all_of(groups))
            ylim <- ceiling(max(mv_wide$MV, na.rm = TRUE)/10)*10

            # create plot
            gg <- ggplot2::ggplot(
                data = mv_wide,
                mapping = ggplot2::aes(x = .data$number, y = .data$MV, fill = .data$group)
            ) +
                ggplot2::labs(
                    title = paste0(Hmisc::capitalize(labels_mvs[i]), " by person and ",
                                   name_grouping),
                    x = paste0("Number of ", labels_mvs[i]), y = "Percentage"
                ) +
                if (is.null(labels_legend)) {
                    scale_fill_discrete(name = Hmisc::capitalize(name_grouping))
                } else {
                    scale_fill_discrete(name = Hmisc::capitalize(name_grouping),
                                        labels = labels_legend)
                }
        }

        gg <- gg +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::scale_y_continuous(breaks = seq(0, ylim, 10),
                                        labels = paste0(seq(0, ylim, 10), " %"),
                                        limits = c(0, ylim)) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.justification = c(1, 1),
                           legend.position = c(0.99, 0.99)) +
            ggplot2::scale_x_continuous(breaks = seq(0, end, ifelse(end > 10, 2, 1)))

        # save plot
        ggplot2::ggsave(
            filename = paste0("Missing_responses_by_person (", i,").png"),
            plot = gg, path = path, width = 2000, height = 1200, units = "px",
            dpi = 300
        )

        # Print progress
        if (verbose) cat("Missing plot", i, "created.\n")
    }
}



#' Count missing values per person and missing value type
#'
#' @param responses  data.frame; contains item responses with items as
#' variables and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#'
#' @return  data.frame with missing values per person and missing value type
#' @noRd

mvp_calc <- function(responses, mvs) {

    result <- data.frame(matrix(NA, nrow(responses), length(mvs)+1))
    names(result) <- c(names(mvs), 'ALL')

    # Determine percentage of missing values for each missing type
    for (i in names(mvs)) {
        result[[i]] <- rowSums(apply(responses, 2, function(x) x %in% mvs[[i]]))
    }

    # Percentage of total missing responses for each person
    result$ALL <- rowSums(result, na.rm = TRUE)

    return(result)
}


#' Descriptive statistics per missing value type
#'
#' @param results  data.frame; contains sum of missing values per person and
#' missing value type; return object of mvp_calc()
#' @param digits  integer; number of decimals for rounding
#'
#' @return list with frequency tables per missing value type and a summary table
#' with descriptive statistics
#' @noRd

mvp_summary <- function(results, digits = 2) {

    # Create tables with frequencies per missing value type
    out <- apply(results, 2, function(x) {
        round(prop.table(table(x)) * 100, digits)
    })

    # Create table with descriptive statistics for all misisng value types
    out$summary <- sapply(results, function(x) {
        round(psych::describe(x), digits)[c(3:5, 8:9)]
    })

    return(out)
}


#' Convert results of missing values by person to table
#'
#' @param mv_p  list; return object of mv_person()
#'
#' @return   table with frequency of missing values for one missing value type
#' @export

write_mvp_table <- function(mv_p) {

    results <- list()

    for (i in names(mv_p)) {
        results[[i]] <- data.frame(mv_p[[i]])
        names(results[[i]]) <- c("Number of missing responses", "Percentage")
    }

    results$summary <- mv_p$summary

    return(results)
}


#' Test mv_p for completeness
#'
#' @param mv_p  list; return object of mvi_analysis()
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#' @param mvs  named integer vector; contains user-defined missing values
#'
#' @noRd


test_mvp_data <- function(mv_p, grouping, mvs) {

    if (is.null(grouping)) {
        nms <- names(mv_p)[-length(mv_p)]
    } else {
        nms <- names(mv_p$all)[-length(mv_p$all)]
    }

    if (any(!(names(mvs) %in% nms))) {
        stop("Please provide mv_p with all specified missing values.")
    }
}


#' Print mvp results to console
#'
#' @param mv_p  list; return object of mvp_analysis()
#'
#' @export

print_mvp_results <- function(mv_p) {

    if ('summary' %in% names(mv_p)) {
        print(mv_p$summary)
    } else {
        for (group in names(mv_p)) {
            message("\nMissing values per person for ", group, " items:\n")
            print(mv_p[[group]]$summary)
        }
    }
}
