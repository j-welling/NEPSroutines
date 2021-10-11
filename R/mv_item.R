#' Percentage of missing responses by item
#'
#' @param resp      data.frame. contains item responses (integer with user-defined missing values)
#'                  and grouping variables (boolean).
#' @param vars      data.frame. contains all competence items as rows,
#'                  and at least the following variables:
#'                    character vector named "items"; contains the names of the items.
#'                    boolean vector; indicates which items to use for analysis.
#'                    integer vector; contains position for each item; if grouping
#'                      exists and positions differ between groups, several variables necessary.
#' @param items     character. contains name of variable (boolean) in vars that
#'                  indicates which items to use for analysis.
#' @param position  (named) character vector. contains name(s) of variable(s) in
#'                  vars that indicate the position of subitems;
#'                  if grouping with differing positions in testlets,
#'                  then for each group one variable name is necessary,
#'                  as well as names for the vector that represent names of groups
#'                  and must be identical with items in variable "grouping"
#'                  (e.g. position = c(easy = "position_easy", diff = "position_diff"))
#' @param grouping  character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                  as used in resp.
#' @param show.all  boolean; only needed when groups exist, indicates whether
#'                  plots shall also include the whole sample as a "group"
#' @param mvs       named vector with definition of user-defined missing values.
#' @param path      folder path for data
#' @param filename  string with name of file that shall be saved
#' @param min.val   minimum number of valid values;
#'                  if negative, set to the default of 3
#' @param digits    number of decimals for rounding
#' @param warn      boolean whether to print a warning if NAs were found in resp
#' @param return_results  boolean. indicates whether to return results.
#'
#' @return   list with percentages:
#'            mvlist: percentages for each item
#'            mvsum: summary statistics across items
#' @importFrom stats median sd na.omit
#' @export

mvi_analysis <- function(resp, vars, items, position = NULL,
                    grouping = NULL, show.all = FALSE,
                    mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                            UM = -90, ND = -55, NAd = -54, AZ = -21),
                    path = here::here("Data"), filename = NULL,
                    min.val = 3, digits = 2, warn = TRUE, return_results = TRUE) {

  # Test data
  if (is.null(grouping)) {
    if (length(position) > 1) {
      stop("No grouping. Please provide only one position variable.")
    }
  } else if (length(position) == 1) {
        warn("Only one position variable provided. The items of each group are therefore set to the same positions.")
    } else if (length(position) != length(grouping)) {
          stop("Position and grouping variables do not match. Please provide either only one position variable (if positions of items do not differ between groups) or matching position and grouping variables (if positions of items do differ between groups).")
  }


  # Prepare data
  resp <- min_val(resp, min.val = min.val)
  vars_c <- vars[vars[[items]], ]
  resp_c <- resp[ , vars_c$items]

  # NAs are not acknowledged in mvs-argument
  if (warn & !(NA %in% mvs) & any(resp_c %in% NA)) {
    warning("NAs found in resp! These values are ignored.")
  }

  # Create results
  if (is.null(grouping)) {

    # Number of valid responses and position
    mvlist <- data.frame(items = vars_c$items,
                         position = vars_c[[position]],
                         N = colSums(apply(resp_c, 2, function(x) {!(x %in% mvs)}))
                         )

    # Determine percentage of missing values for each missing type
    results <- data.frame(mvi_calc(resp = resp_c, mvs = mvs, digits = digits))
    results$items <- row.names(results)
    mvlist <- merge(mvlist, results, by = 'items', all = TRUE)

    # Summary across items
    mvsum <- t(apply(mvlist[ , -1], 2, function(x) {
      round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
    }))
    colnames(mvsum) <- c("Mean", "Median", "SD", "Min", "Max")

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

      mvlist[[g]] <- data.frame(items = vars_c$items[!is.na(pos)],
                                position = na.omit(pos),
                                N = colSums(apply(resp_c[ , !is.na(pos)], 2,
                                function(x) !(x %in% mvs))))

      # Determine percentage of missing values for each missing type
      results <- data.frame(mvi_calc(resp = resp_c, mvs = mvs, digits = digits))
      results$items <- row.names(results)
      mvlist[[g]] <- merge(mvlist[[g]], results, by = 'items')

      # Summary across items
      mvsum[[g]] <- t(apply(mvlist[[g]][ , -1], 2, function(x) {
        round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
      }))
      colnames(mvsum[[g]]) <- c("Mean", "Median", "SD", "Min", "Max")
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
      results <- data.frame(mvi_calc(resp = resp_p, mvs = mvs, digits = digits))
      mvlist$all <- cbind(mvlist$all, results)

      # Summary across items
      mvsum$all <- t(apply(mvlist$all[ , -1], 2, function(x) {
        round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
      }))
      colnames(mvsum$all) <- c("Mean", "Median", "SD", "Min", "Max")
    }
  }

  # List with results
  mv_i <- list(list = mvlist, summary = mvsum)

  # Save results
  if (!is.null(filename)) {

      # Create directory for data
      if (!file.exists(path)) {
          dir.create(path, recursive = TRUE)
      }

      save(mv_i, file = here::here(paste0(path, "/", filename)))
  }

  # Return results
  if (return_results)  return(mv_i)
}


#' Write table of user defined missing values per item (and position)
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param vars      data.frame. contains all competence items as rows,
#'                  and at least the following variables:
#'                  character vector named "items"; contains the names of the items.
#'                  boolean vector; indicates which items to use for analysis.
#' @param items     character. contains name of variable (boolean) in vars that
#'                  indicates which items to use for analysis.
#' @param mv_i      list; return object of mvi_analysis(). mvi_analysis$list must contain
#'                  for each group and all groups together the name of the
#'                  user-defined missing values that are to be
#'                  analyzed and the names of the items as its row names.
#' @param resp      data.frame. contains item responses (integer with user-defined missing values)
#'                  and grouping variables (boolean).
#' @param position  (named) character vector. contains name(s) of variable(s) in
#'                  vars that indicate the position of subitems;
#'                  if grouping with differing positions in testlets,
#'                  then for each group one variable name is necessary,
#'                  as well as names for the vector that represent names of groups
#'                  and must be identical with items in variable "grouping"
#'                  (e.g. position = c(easy = "position_easy", diff = "position_diff"))
#' @param grouping  character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                  as used in resp.
#' @param show.all  boolean; only needed when groups exist, indicates whether
#'                  plots shall also include the whole sample as a "group"
#' @param mvs       named vector with definition of user-defined missing values.
#' @param filename  string with name of file that shall be saved (excluding type of file);
#'                  if NULL, no file will be saved.
#' @param path      folder path for plots
#' @param min.val   minimum number of valid values;
#'                  if negative, set to the default of 3
#' @param digits    number of decimals for rounding
#' @param warn      boolean whether to print a warning if NAs were found in resp
#' @param return_table  boolean. indicates whether to return table.
#' @export

mvi_table <- function(vars, items, mv_i = NULL, resp = NULL,
                      position = NULL, grouping = NULL, show.all = FALSE,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      filename = NULL, path = here::here("Tables"),
                      min.val = 3, digits = 2, warn = TRUE, return_table = FALSE) {

  # Test data
  if (is.null(mv_i)) {
    if (!is.null(resp) & !is.null(position)) {
      mv_i <- mvi_analysis(resp, vars = vars, mvs = mvs, items = items,
                      position = position, grouping = grouping,
                      show.all = show.all, min.val = min.val,
                      digits = digits, warn = warn)
    } else {
      stop("Please provide mv_i or resp and position.")
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
                        show.all = show.all, min.val = min.val,
                        digits = digits, warn = warn)
      } else {
        stop("Please provide mv_i with specified missing values or resp and position.")
      }
    }
  }

  # Create table

  if (is.null(grouping)) {

    results <- mv_i

    } else {

    results <- list()
    results$list <- data.frame(item = vars$item[vars[[items]]])
    results$summary_all <- mv_i$summary$all

    for (g in grouping) {
      mv <- mv_i$list[[g]]
      names(mv) <- apply(data.frame(names(mv)), 2, function(x) {
        return(paste0(x, "_", g))
      })
      mv$item <- rownames(mv)
      results$list <- dplyr::full_join(results$list, mv, by = "item")
      results[[paste0("summary_", g)]] <- mv_i$summary[[g]]
    }
  }

  # Save table
  if (!is.null(filename)) {
    # Create directory for table
    if (!file.exists(path)) dir.create(path, recursive = TRUE)

    openxlsx::write.xlsx(results,
                         file = paste0(path, "/", filename),
                         showNA = FALSE, rowNames = TRUE, overwrite = TRUE)
  } else {
    warn("No filename provided. The table will not be saved.")
  }

  # Return table
  if (return_table) return(results)
}


#' Plot percentages of user-defined missing values by item position
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param vars      data.frame. contains all competence items as rows,
#'                  and at least the following variables:
#'                  character vector named "items"; contains the names of the items.
#'                  boolean vector; indicates which items to use for analysis.
#' @param items     character. contains name of variable (boolean) in vars that
#'                  indicates which items to use for analysis.
#' @param mv_i      list; return object of mvi_analysis(). mvi_analysis$list must contain
#'                  for each group and all groups together the name of the
#'                  user-defined missing values that are to be
#'                  analyzed and the names of the items as its row names.
#' @param resp      data.frame. contains item responses (integer with user-defined missing values)
#'                  and grouping variables (boolean).
#' @param position  (named) character vector. contains name(s) of variable(s) in
#'                  vars that indicate the position of subitems;
#'                  if grouping with differing positions in testlets,
#'                  then for each group one variable name is necessary,
#'                  as well as names for the vector that represent names of groups
#'                  and must be identical with items in variable "grouping"
#'                  (e.g. position = c(easy = "position_easy", diff = "position_diff"))
#' @param grouping  character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                  as used in resp.
#' @param show.all  boolean; only needed when groups exist, indicates whether
#'                  plots shall also include the whole sample as a "group"
#' @param mvs       named vector; contains definition of user-defined missing
#'                  values that shall be analyzed in the plot
#' @param path      folder path for plots
#' @param filename  string with name of file that shall be saved (excluding type of file)
#' @param position  character vector. contains names of position variables
#'                  (one element per grouping, same order necessary)
#' @param color     character vector. defines bar color(s); if the data contains subgroups,
#'                  one color per subgroup must be specified
#' @param verbose   logical; print information to console
#' @param min.val   minimum number of valid values;
#'                  if negative, set to the default of 3
#' @param digits    number of decimals for rounding
#' @param warn      boolean whether to print a warning if NAs were found in resp
#'
#' @importFrom rlang .data
#' @export

mvi_plots <- function(vars, items, mv_i = NULL, resp = NULL,
                     position = NULL, grouping = NULL, show.all = FALSE,
                     mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                             UM = -90, ND = -55, NAd = -54, AZ = -21),
                     path = here::here("Plots/Missing_Responses/by_item"),
                     filename = "Missing_responses_by_item",
                     color = NULL, verbose = TRUE,
                     min.val = 3, digits = 2, warn = TRUE) {

  # Test data
  if (is.null(mv_i)) {
    if (!is.null(resp) & !is.null(position)) {
      mv_i <- mvi_analysis(resp, vars = vars, mvs = mvs, items = items,
                      position = position, grouping = grouping,
                      show.all = show.all, min.val = min.val,
                      digits = digits, warn = warn)
    } else {
        stop("Please provide mv_i or resp and position.")
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
                        show.all = show.all, min.val = min.val,
                        digits = digits, warn = warn)
      } else {
        stop("Please provide mv_i with specified missing values or resp and position.")
      }
    }
  }

  # Pepare data
  mv_i <- mv_i$list

  if (is.null(grouping)) {
    k <- sum(vars[[items]])
  } else {
    k <- NA_integer_
    for (g in grouping) {
      k <- c(k, length(vars$items[vars[[items]] & vars[[g]]]))
    }
    k <- max(k, na.rm = TRUE)

    if (show.all) {
      groups <- c(grouping, "all")
    } else {
      groups <- grouping
    }
  }

  # Create directory for plots
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Create plots

  for (i in names(mvs)) {

    if (is.null(grouping)) {

      # create plot
      y <- mv_i[[i]]
      ylim <- ceiling(max(y, na.rm = TRUE)/10)*10

      gg <- ggplot2::ggplot(data = mv_i,
                            mapping = ggplot2::aes(x = .data$position, y = .data$y)
      ) +
        ggplot2::labs(
          title = paste0("Missing responses by item position (", i,")"),
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

      mv_wide <- tidyr::gather(mv, .data$group, .data$MV, tidyselect::all_of(groups))
      ylim <- ceiling(max(mv_wide$MV, na.rm = TRUE)/10)*10

      # create plot
      gg <- ggplot2::ggplot(
        data = mv_wide,
        mapping = ggplot2::aes(x = .data$position, y = .data$MV, fill = .data$group)
      ) +
        ggplot2::labs(
          title = paste0("Missing responses by item position (", i,")"),
          x = "Item position", y = "Percentage", fill = "Group"
        )
    }

    gg <- gg +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_continuous(breaks = seq(0, ylim, 10),
                                  labels = paste0(seq(0, ylim, 10), " %"),
                                  limits = c(0, ylim)) +
      ggplot2::scale_x_continuous(breaks = seq(1, k, floor(k/20))) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.justification = c(0, 1),
                     legend.position = c(0.01, 0.99))

    if (!is.null(color)) {
      gg <- gg + ggplot2::scale_fill_manual(values = .data[[color]])
    }

    # save plot
    ggplot2::ggsave(
      filename = paste0(filename, " (", i,").png"),
      plot = gg, path = path, width = 2000, height = 1200, units = "px",
      dpi = 300
    )

    # Print progress
    if (verbose) cat("Missing plot", i, "created.\n")
  }
}



#' Calculate and round frequency (in percentage) of one missing value type (by item)
#'
#' @param resp      data.frame with item responses
#' @param mvs       named vector with definition of one user-defined missing values
#' @param digits    number of decimals for rounding
#'
#' @return   table with frequency of missing values for one missing value type (by item)
#' @noRd

mvi_perc <- function(resp, mvs, digits = 2) {
  perc <- data.frame(apply(resp, 2, function(x) {
    ifelse(x %in% mvs, 1, ifelse(!is.na(x), 0, NA))
  }))
  round(apply(perc, 2, mean, na.rm = TRUE) * 100, digits)
}



#' Create list with frequency of missing responses by item for each missing value type
#'
#' @param resp      data.frame with item responses
#' @param mvs       named vector with definition of all relevant user-defined missing values
#' @param digits    number of decimals for rounding
#'
#' @return   list with frequency of missing values by item for each missing value type in mvs
#' @noRd

mvi_calc <- function(resp, mvs, digits = 2) {
  result <- list()

  # Determine percentage of missing values for each missing type
  for (i in names(mvs)) {
    result[[i]] <- mvi_perc(resp = resp, mvs = mvs[[i]], digits = digits)
  }

  # Percentage of total missing responses for each item
  result$ALL <- mvi_perc(resp = resp, mvs = mvs, digits = digits)

  return(result)
}
