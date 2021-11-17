#' Missing values analysis by item - all in one function
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
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
#' @param plots  logical; whether plots shall be created and saved to hard disc
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard disc
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
#' @param color  character calar or vector; defines color(s) of the bar in plots
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print a warning if NAs were found in resp
#' @param verbose  logical; whether to print processing information to console
#'
#' @return (if return = TRUE) list with results:
#'          mvlist: percentages for each item
#'          mvsum: summary statistics across items
#'          summary_table: table with summary statistics for TR
#' @importFrom ?
#' @export

mv_item <- function(resp, vars, items, valid = NULL,
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
                    path_plots = here::here("Plots/Missing_Responses/by_person"),
                    show_all = FALSE, color = NULL, overwrite = FALSE,
                    digits = 2, warn = TRUE, verbose = TRUE) {

  # Conduct analysis
  mv_item <- mvi_analysis()

  # Write grouped table
  mv_item$summary_table <- mvi_table()

  # Write plots
  if (plots) mvi_plots()

  # Save results
  if (save) {
    save_results(mv_item, filename = "mv_item.Rdata", path = path_results)
    save_table(mv_item$summary_table,
               filename = "mv_item.xlsx", path = path_table)
  }

  # Print results
  if (print) {
    print(mv_item$summary_table)
    print_mvi_results()
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
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
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
#' @param filename  string; defines name of file that shall be saved
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print a warning if NAs were found in resp
#'
#' @return   list with percentages:
#'            mvlist: percentages for each item
#'            mvsum: summary statistics across items
#' @importFrom stats median sd na.omit
#' @export

mvi_analysis <- function(resp, vars, items, valid = NULL,
                         position = NULL, grouping = NULL, show_all = FALSE,
                         mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                                 UM = -90, ND = -55, NAd = -54, AZ = -21),
                         path = here::here("Data"), filename = NULL,
                         digits = 2, warn = TRUE) {

  # Test data
  if (is.null(grouping)) {
    if (length(position) > 1) {
      stop("No grouping. Please provide only one position variable.")
    }
  } else if (length(position) == 1) {
        warn("Only one position variable provided. ",
        "The items of each group are therefore set to the same positions.")
    } else if (length(position) != length(grouping)) {
          stop("Position and grouping variables do not match. ",
          "Please provide either only one position variable (if positions of items do not differ between groups) ",
          "or matching position and grouping variables (if positions of items do differ between groups).")
  }


  # Prepare data
  vars_c <- vars[vars[[items]], ]
  resp <- only_valid(resp, valid = valid)
  resp_c <- prepare_resp(resp, vars = vars, items = items)

  # NAs are not acknowledged in mvs-argument
  if (warn & !(NA %in% mvs) & any(resp_c %in% NA)) {
    warning("NAs found in resp! These values are ignored.")
  }

  # Create results
  if (is.null(grouping)) {

    # Create list with results
    mvlist <- create_mvlist(items = vars_c$items,
                            position = vars_c[[position]],
                            responses = resp_c,
                            mvs = mvs,
                            digits = digits)

    # Summary across items
    mvsum <- mvi_summary(mvlist[ , -c(1:2)], digits = digits)

  } else {

    # Create list with results
    mvlist <- list()
    mvsum <- list()

    for (g in grouping) {

      # Select administered items for the group
      resp_g <- resp_c[resp[[g]], vars$items[vars[[items]] & vars[[g]]]]

      # Number of valid responses and position
      if (length(position) == 1) {
        pos <- vars_c[[position]]
      } else {
        pos <- vars_c[[position[g]]]
      }

      # Create list with results
      mvlist[[g]] <- create_mvlist(items = vars_c$items[!is.na(pos)],
                                   position = na.omit(pos),
                                   responses = resp_g,
                                   mvs = mvs,
                                   digits = digits)

      # Summary across items
      mvsum[[g]] <- mvi_summary(mvlist[[g]][ , -c(1:2)], digits = digits)
    }

    if (length(position) > 1) {

      # Creating new dataframe with responses by position, not by item
      resp_p <- data.frame(position = NA)

        for (g in grouping) {

          r <- data.frame(t(resp_c[resp[[g]], ]))
          r$items <- rownames(r)
          r <- merge(r, vars_c[, c('items', position[g])], by = "items")
          r <- dplyr::rename(r, position = as.character(position[g]))
          r <- dplyr::select(r,-.data$items)
          resp_p <- dplyr::full_join(resp_p, r, by = "position")
        }

      resp_p <- dplyr::arrange(resp_p[!is.na(resp_p$position), ], position)

      # Number of valid responses and position
      pos <- resp_p$position
      resp_p <- dplyr::select(resp_p, -.data$position) %>% t() %>% data.frame()
      mvlist$all <- data.frame(position = pos,
                               N = apply(resp_p, 2, function(x) {
                                 sum(x >= 0, na.rm = TRUE)
                                 }))

      # Determine percentage of missing values for each missing type
      results <- data.frame(mvi_calc(resp_p, mvs = mvs, digits = digits))
      mvlist$all <- cbind(mvlist$all, results)

      # Summary across items
      mvsum$all <- mvi_summary(mvlist$all[ , -1], digits = digits)
    }
  }

  # List with results
  mv_i <- list(list = mvlist, summary = mvsum)

  # Save results
  save_results(mv_i, filename = filename, path = path)

  # Return results
  return(mv_i)
}


#' Write table of user defined missing values per item (and position)
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param mv_i  list; return object of mvi_analysis()
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
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
#' @param path  string; defines path to folder where table shall be saved
#' @param filename  string; defines name of table that shall be saved
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print a warning if NAs were found in resp
#'
#' @return table with results
#' @export

mvi_table <- function(vars, items, mv_i = NULL, resp = NULL, valid = NULL,
                      position = NULL, grouping = NULL, show_all = FALSE,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      filename = NULL, path = here::here("Tables"),
                      digits = 2, warn = TRUE, overwrite = FALSE) {

  # Test data
  test_mvi_data(mv_i, resp = resp, vars = vars, mvs = mvs, items = items,
                position = position, grouping = grouping, show_all = show_all,
                valid = valid, digits = digits, warn = warn)

  # Create table

  if (is.null(grouping)) {

    results <- mv_i

    } else {

    results <- list()
    results$list <- data.frame(items = vars$item[vars[[items]]])
    results$summary_all <- mv_i$summary$all

    for (g in grouping) {
      mv <- mv_i$list[[g]]
      names(mv)[-1] <- apply(data.frame(names(mv)[-1]), 2, function(x) {
        paste0(x, "_", g)
      })
      results$list <- dplyr::full_join(results$list, mv, by = "items")
      results[[paste0("summary_", g)]] <- mv_i$summary[[g]]
    }
  }

  # Save table
  save_table(results, filename = filename, path = path,
             overwrite = overwrite, show_rownames = TRUE)

  # Return table
  return(results)
}


#' Plot percentages of user-defined missing values by item position
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param mv_i  list; return object of mvi_analysis()
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
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
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param path  string; defines path to folder where plots shall be saved
#' @param color  character calar or vector; defines color(s) of the bar in plots
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print a warning if NAs were found in resp
#' @param verbose  logical; whether to print processing information to console
#'
#' @importFrom rlang .data
#' @export

mvi_plots <- function(vars, items, mv_i = NULL, resp = NULL, valid = NULL,
                      position = NULL, grouping = NULL, show_all = FALSE,
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
                      color = NULL, digits = 2, verbose = TRUE, warn = TRUE) {

  # Test data
  test_mvi_data(mv_i, resp = resp, vars = vars, mvs = mvs, items = items,
                position = position, grouping = grouping, show_all = show_all,
                valid = valid, digits = digits, warn = warn)

  # Pepare data
  mv_i <- mv_i$list

  if (is.null(grouping)) {
    k <- sum(vars[[items]]) # auch hier wäre es vielleicht übersichtlicher, das Argument "items" in "select" o.Ä. umzubenennen
  } else {
    k <- NA_integer_
    for (g in grouping) {
      k <- c(k, length(vars$items[vars[[items]] & vars[[g]]]))
    }
    k <- max(k, na.rm = TRUE)

    if (show_all) {
      groups <- c(grouping, "all")
    } else {
      groups <- grouping
    }
  }

  # Create directory for plots
  check_folder(path)

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
          title = paste0(Hmisc::capitalize(labels_mvs[i]), " by item position"),
          x = "Item position", y = "Percentage", fill = "Group"
        )
    }

    gg <- gg +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_continuous(breaks = seq(0, ylim, 10),
                                  labels = paste0(seq(0, ylim, 10), " %"),
                                  limits = c(0, ylim)) +
      ggplot2::scale_x_continuous(breaks = seq(0, k, floor(k/20))) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.justification = c(0, 1),
                     legend.position = c(0.01, 0.99))

    if (!is.null(color)) {
      gg <- gg + ggplot2::scale_fill_manual(values = .data[[color]])
    }

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
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in resp and vars that indicates to which group belongs a person or
#' an item
#'
#' @export

print_mvi_results <- function(mv_i, grouping = NULL, labels_mvs = c(
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
  if (is.null(grouping)) {
    for (lbl in names(labels_mvs)) {
      mv_min <- min(mv_i$list[[lbl]], na.rm = TRUE)
      mv_max <- max(mv_i$list[[lbl]], na.rm = TRUE)
      item_min <- mv_i$list$items[mv_i$list[[lbl]] == mv_min]
      item_max <- mv_i$list$items[mv_i$list[[lbl]] == mv_max]
      message("The number of ", labels_mvs[lbl], " varied between ",
              mv_min, " %", if(length(item_min) == 1) {paste0(" (item ", item_min, ")")}, " and ",
              mv_max, " %", if(length(item_max) == 1) {paste0(" (item ", item_max, ")")}, ".")
    }
  } else {
    for (g in grouping) {
      print(g)
      for (lbl in names(labels_mvs)) {
        mv_min <- min(mv_i$list[[g]][[lbl]], na.rm = TRUE)
        mv_max <- max(mv_i$list[[g]][[lbl]], na.rm = TRUE)
        item_min <- mv_i$list[[g]]$items[mv_i$list[[g]][[lbl]] == mv_min]
        item_max <- mv_i$list[[g]]$items[mv_i$list[[g]][[lbl]] == mv_max]
        message("The number of ", labels_mvs[lbl], " in the ", g, " test version varied between ",
                mv_min, " %", if(length(item_min) == 1) {paste0(" (item ", item_min, ")")}, " and ",
                mv_max, " %", if(length(item_max) == 1) {paste0(" (item ", item_max, ")")}, ".")
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

mvi_perc <- function(responses, mvs, digits = 2) {
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

mvi_calc <- function(responses, mvs, digits = 2) {
  result <- list()

  # Determine percentage of missing values for each missing type
  for (i in names(mvs)) {
    result[[i]] <- mvi_perc(responses = responses, mvs = mvs[[i]], digits = digits)
  }

  # Percentage of total missing responses for each item
  result$ALL <- mvi_perc(responses = responses, mvs = mvs, digits = digits)

  return(result)
}


#' Create list with missing values per item
#'
#' @param items  character vector; contains names of items
#' @param position  integer vector; contains position of items
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param digits  integer; number of decimals for rounding
#'
#' @return   list with results of missing values per item.
#' @noRd

create_mvlist <- function(items, position, responses, mvs, digits = 2) {

  if (length(items) != length(position) |
      ncol(responses) != length(items) |
      ncol(responses) != length(position)) {
        stop("Number of items in data.frame responses, in vector items and in vector position do not match. ",
        "Please provide matching arguments to function create_mvlist().")
      }

  # Create list
  mvlist <- data.frame(items = items,
                       position = position,
                       N = colSums(apply(responses, 2, function(x) !(x %in% mvs))))

  # Merge with percentage of missing values for each missing type
  results <- data.frame(mvi_calc(responses, mvs = mvs, digits = digits))
  results$items <- row.names(results)
  mvlist <- merge(mvlist, results, by = 'items')

  # Return list
  return(mvlist)
}


#' MVI summary
#'
#' @param mvlist  list; return object of create_mvlist()
#'
#' @return  data.frame with mean, median, SD, min and max of missing values
#' @noRd

mvi_summary <- function(mvlist, digits = 2) {
  mvsum <- data.frame(t(apply(mvlist, 2, function(x) {
    round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
  })))
  colnames(mvsum) <- c("Mean", "Median", "SD", "Min", "Max")
  return(mvsum)
}

#' Test mvi data and if not incomplete, create new mv_i
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' includes all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
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
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print a warning if NAs were found in resp
#'
#' @noRd


test_mvi_data <- function(mv_i, resp, vars, mvs, items, position, grouping,
                          show_all, valid, digits, warn) {

  if (is.null(mv_i)) {
    if (!is.null(resp) & !is.null(position)) {
      mv_i <- mvi_analysis(resp, vars = vars, mvs = mvs, items = items,
                           position = position, grouping = grouping,
                           show_all = show_all, valid = valid,
                           digits = digits, warn = warn)
    } else {
      stop("Please provide mv_i, or resp and position.")
    }
  } else {
    if (is.null(grouping)) {
      nms <- names(mv_i$list)
    } else {
      nms <- names(mv_i$list$all)
    }

    if (any(!(names(mvs) %in% nms))) {
      if (!is.null(resp) & !is.null(position)) {
        mv_i <- mvi_analysis(resp, vars = vars, mvs = mvs, items = items,
                             position = position, grouping = grouping,
                             show_all = show_all, valid = valid,
                             digits = digits, warn = warn)
      } else {
        stop("Please provide mv_i with specified missing values, or resp and position.")
      }
    }
  }
}
