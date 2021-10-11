#' Missing values by person analysis
#'
#' @param resp          data.frame with item responses (user-defined missing values)
#'                      and logical grouping variables, named after the grouping
#'                      factor (e.g. "easy" and "difficult" in the case of testlet
#'                      difficulties) --> must be in separate columns!
#' @param vars          data.frame. contains all competence items as rows,
#'                      with the column "items" indicating the names of the items.
#' @param items         character. contains name of variable (boolean) in vars that
#'                      indicates which items to items in analysis.
#' @param grouping      character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                      as items in data.frame resp.
#' @param mvs           named vector with definition of user-defined missing values
#' @param name_data     string with name of data file that shall be saved (including type of file);
#'                      if NULL, no file will be saved.
#' @param path_data     folder path for file if it shall be saved
#' @param return_table  boolean. indicates whether table shall be returned.
#' @param name_table    string with name of table file that shall be saved (excluding type of file);
#'                      if NULL, no file will be saved.
#' @param path_table    folder path for file if it shall be saved
#' @param plots         boolean. indicates whether plotsshall be created.
#' @param name_plots    string with base of name for plot files (excluding type of file)
#' @param path_plots    folder path for plots
#' @param lbls_plots    lables for different types of user-defined missing values
#' @param color_plots   bar color
#' @param min.val       minimum number of valid values;
#'                      if negative, set to the default of 3
#' @param digits        number of decimals for rounding
#' @param warn          boolean. indicates whether to print a warning if NAs were found in resp.
#' @param verbose       print progress
#' @param show.all_plots logical; only needed when groups exist, indicates whether
#'                  plots shall also include the whole sample as a "group"
#'
#' @return   if return_table is set to TRUE, table with missing values per person and
#' for each missing value type (in percentage)
#'
#' @export

mv_person <- function(resp, vars, items = 'final', grouping = NULL,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      name_data = NULL, path_data = here::here("Data"),
                      return_table = TRUE, name_table = NULL,
                      path_table = here::here("Tables"),
                      plots = FALSE, name_plots = "Missing_responses_by_person",
                      path_plots = here::here("Plots/Missing_Responses/by_person"),
                      color_plots = NULL, show.all_plots = FALSE,
                      lbls_plots = c(
                        ALL = "total missing",
                        OM = "omitted",
                        NV = "not valid",
                        NR = "not reached",
                        TA = "test aborted",
                        UM = "unspecific missing",
                        ND = "not determinable",
                        NAd = "not administered",
                        AZ = "Angabe zurueckgesetzt"
                      ),
                      min.val = 3, digits = 2, warn = TRUE, verbose = TRUE) {

  mv_p <- mvp_analysis(resp = resp, vars = vars, items = items, grouping = grouping,
                  mvs = mvs, filename = name_data, path = path_data,
                  min.val = min.val, digits = digits, warn = warn)

  if (plots) {
    mvp_plots(mv_p = mv_p, vars = vars, items = items, grouping = grouping,
             mvs = mvs, lbls = lbls_plots, show.all = show.all_plots,
             filename = name_plots, path = path_plots, color = color_plots,
             verbose = verbose)
  }

  if (return_table | !is.null(name_table)) {
    mvp_table(mv_p = mv_p, grouping = grouping, mvs = mvs,
             filename = name_table, path = path_table,
             return_table = return_table)
  }
}


#' Percentage of missing responses by person
#'
#' @param resp      data.frame with item responses (user-defined missing values)
#'                  and logical grouping variables, named after the grouping
#'                  factor (e.g. "easy" and "difficult" in the case of testlet
#'                  difficulties) --> must be in separate columns!
#' @param vars      data.frame. contains all competence items as rows,
#'                  with the column "items" indicating the names of the items.
#' @param items       character. contains name of variable (boolean) in vars that
#'                  indicates which items to items in analysis.
#' @param grouping  character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                  as itemsd in data.frame resp.
#' @param mvs       named vector with definition of user-defined missing values
#' @param filename  string with name of file that shall be saved (including type of file);
#'                  if NULL, no file will be saved.
#' @param path      folder path for file if it shall be saved
#' @param min.val   minimum number of valid values;
#'                  if negative, set to the default of 3
#' @param digits    number of decimals for rounding
#' @param warn      boolean. indicates whether to print a warning if NAs were found in resp
#' @param return_results    boolean. indicates whether to return results.
#'
#' @return   if return is set to TRUE, list with missing values per person and
#' for each missing value type (in percentage)
#'
#' @export

mvp_analysis <- function(resp, vars, items = 'final', grouping = NULL,
                    mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                            UM = -90, ND = -55, NAd = -54, AZ = -21),
                    filename = NULL, path = here::here("Data"),
                    min.val = 3, digits = 2, warn = TRUE, return_results = TRUE) {

  # Testing prerequisites (data)
  if (is.null(vars) | is.null(resp)) {
    stop("Please provide vars and resp.")
  }

  # NAs are not acknowledged in mvs-argument
  if (warn & !(NA %in% mvs) & any(resp %in% NA)) {
    warning("NAs found in resp! These values are ignored.")
  }

  # Create list for results
  mv_p <- list()

  # Prepare data
  resp <- min_val(resp, min.val = min.val)
  resp_c <- resp[ , vars$items[vars[[items]]]]

  if (is.null(grouping)) {

    mv_p <- mvp_calc(resp = resp_c, mvs = mvs, digits = digits)

  } else {

    resp_all <- data.frame(NA)

    for (g in grouping) {

      resp_g <- resp_c[resp[[g]], vars$items[vars[[items]] & vars[[g]]]]
      resp_all <- merge(resp_all, resp_g, all = TRUE)

      # Determine percentage of missing values for each missing type
      mv_p[[g]] <- mvp_calc(resp = resp_g, mvs = mvs, digits = digits)
    }

    mv_p$all <- mvp_calc(resp = resp_all, mvs = mvs, digits = digits)
  }

  # Save results
  if (!is.null(filename)) {

    # Create directory for data
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    save(mv_p, file = here::here(paste0(path, "/", filename)))
  }

  # Return results
  if (return_results) return(mv_p)
}


#' Write table of user defined missing values per item (and position)
#'
#' Plots the percentages of not reached missing values for each item by its
#' item position. If not otherwise specified, the order in which the items are
#' given to the function is taken as the order the items appeared in the test.
#'
#' @param mv_p      list; return object of mv_person().
#' @param grouping  character vector. contains names of grouping variables.
#' @param resp      data.frame. contains item responses (user-defined mvs)
#' @param vars      data.frame, contains information about variables
#'                  (for minimum columns see function "mv_person")
#' @param mvs       named vector; contains definition of user-defined missing
#'                  values that shall be analyzed in the plot
#' @param filename  string with name of file that shall be saved (excluding type of file);
#'                  if NULL, no file will be saved.
#' @param path      folder path for plots
#' @param return_table  boolean. indicates whether to return table.
#'
#' @return   if return is set to TRUE, table with missing values per person and
#' for each missing value type (in percentage)
#'
#' @export

mvp_table <- function(mv_p = NULL, grouping = NULL, resp = NULL, vars = NULL,
                      mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                              UM = -90, ND = -55, NAd = -54, AZ = -21),
                      filename = NULL, path = here::here("Tables"),
                      return_table = FALSE) {

  # Percentage of missing values per item
  if (is.null(mv_p)) {
    if (!is.null(resp) & !is.null(vars)) {
      mv_p <- mv_person(resp, vars = vars, mvs = mvs, grouping = grouping)
    } else {
      stop("Please provide mv_p or resp and vars.")
    }
  }

  # Create table

  if (is.null(grouping)) {
    results <- write_results(mv_p)
  } else {
    results <- list()
    for (g in names(mv_p)) {
      results[[g]] <- write_results(mv_p[[g]])
    }
  }

  # Save table

  if (!is.null(filename)) {

    # Create directory for table
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    # Write table as Excel sheet
    if (is.null(grouping)) {
      openxlsx::write.xlsx(results,
                           file = paste0(path, "/", filename, ".xlsx"),
                           showNA = FALSE, rowNames = FALSE, overwrite = TRUE)
    } else {
      for (g in names(mv_p)) {
        openxlsx::write.xlsx(results[[g]],
                             file = paste0(path, "/", filename, "_", g, ".xlsx"),
                             showNA = FALSE, rowNames = FALSE, overwrite = TRUE)
      }
    }
  }

  # Return table
  if (return_table) return(results)
}


#'
#' Plot percentage of missing values for persons
#'
#' @param mv_p      percentage of missing responses by person as
#'                  returned by mv_person()
#' @param resp      data.frame with item responses (user-defined missing values)
#'                  and logical grouping variables, named after the grouping
#'                  factor (e.g. "easy" and "difficult" in the case of testlet
#'                  difficulties) --> must be in separate columns!
#' @param vars      data.frame. contains all competence items as rows,
#'                  with the column "items" indicating the names of teh items.
#' @param items     character. contains name of variable (boolean) in vars that
#'                  indicates which items to items in analysis.
#' @param grouping  character vector. contains names of groups (e.g. 'easy and 'difficult')
#'                  as itemsd in data.frame resp.
#' @param show.all  logical; only needed when groups exist, indicates whether
#'                  plots shall also include the whole sample as a "group"
#' @param mvs       named vector with definition of user-defined missing values
#' @param path      folder path for plots
#' @param filename  string with name of file that shall be saved (excluding type of file)
#' @param lbls      lables for different types of user-defined missing values
#' @param color     character vector. defines bar color(s); if the data contains subgroups,
#'                  one color per subgroup must be specified
#' @param verbose   print progress
#'
#' @importFrom rlang .data
#' @export

mvp_plots <- function(mv_p = NULL, resp = NULL, vars = NULL, items = 'final',
                     grouping = NULL, show.all = FALSE,
                     mvs = c(OM = -97, NV = -95, NR = -94, TA = -91,
                             UM = -90, ND = -55, NAd = -54, AZ = -21),
                     path = here::here("Plots/Missing_Responses/by_person"),
                     filename = "Missing_responses_by_person",
                     lbls = c(
                       ALL = "total missing",
                       OM = "omitted",
                       NV = "not valid",
                       NR = "not reached",
                       TA = "test aborted",
                       UM = "unspecific missing",
                       ND = "not determinable",
                       NAd = "not administered",
                       AZ = "Angabe zurueckgesetzt"
                     ),
                     color = NULL, verbose = TRUE) {

  # Percentage of missing values by persons
  if (is.null(mv_p)) {
    if (is.null(resp) | is.null(mvs)) {
      stop("Please provide mv_p or resp!")
    } else {
      mv_p <- mv_person(resp, vars = vars, grouping = grouping, mvs = mvs)
    }
  }

  # Prepare data
  if (is.null(grouping)) {
    mv_all <- mv_p
  } else {
    mv_all <- mv_p$all
    if (show.all) {
      groups <- c(grouping, "all")
    } else {
      groups <- grouping
    }
  }

  # Labels
  for (i in names(mv_all)) {
    if (!(i %in% names(lbls))) lbls[i] <- "missing"
  }

  # Approx. number of items
  if (!is.null(vars)) {
    if (is.null(grouping)) {
      k <- sum(vars[[items]])
    } else {
      k <- NA_integer_
      for (g in grouping) {
        k <- c(k, length(vars$items[vars[[items]] & vars[[g]]]))
      }
      k <- max(k, na.rm = TRUE)
    }
  } else {
    k <- max(sapply(mv_all, function(x) {max(as.numeric(names(x)))})) + 1
  }

  # Create directory for plots
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

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
          title = paste0(filename, " (", i,")"),
          x = paste0("Number of ", lbls[i], " items"),
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
          title = paste0("Missing responses by person (", i,")"),
          x = paste0("Number of ", lbls[i], " items"),
          y = "Percentage", fill = "Group"
        )
    }

    gg <- gg +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_continuous(breaks = seq(0, ylim, 10),
                                  labels = paste0(seq(0, ylim, 10), " %"),
                                  limits = c(0, ylim)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.justification = c(0, 1),
                     legend.position = c(0.85, 0.99))

    if (!is.null(color)) {
      gg <- gg + ggplot2::scale_fill_manual(values = .data[[color]])
    }

    if (end > 1) {
      gg <- gg + ggplot2::scale_x_continuous(breaks = seq(0, end, 2))
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


#' Create list with frequency of missing responses by person for each missing value type
#'
#' @param resp      data.frame with item responses
#' @param mvs       named vector with definition of all relevant user-defined missing values
#' @param digits    number of decimals for rounding
#'
#' @return   list with frequency of missing values by person for each missing value type in mvs
#' @noRd

mvp_calc <- function(resp, mvs, digits = 2) {
  result <- list()

  # Determine percentage of missing values for each missing type
  for (i in names(mvs)) {
    result[[i]] <- mvp_perc(resp = resp, mvs = mvs[[i]], digits = digits)
  }

  # Percentage of total missing responses for each person
  result$ALL <- mvp_perc(resp = resp, mvs = mvs, digits = digits)

  return(result)
}


#' Calculate and round frequency (in percentage) of one missing value type (by person)
#'
#' @param resp      data.frame with item responses
#' @param mvs       named vector with definition of one user-defined missing values
#' @param digits    number of decimals for rounding
#'
#' @return   table with frequency of missing values for one missing value type (by person)
#' @noRd

mvp_perc <- function(resp, mvs, digits = 2) {
  perc <- rowSums(apply(resp, 2, function(x) x %in% mvs))
  round(prop.table(table(perc)) * 100, digits)
}


#' Convert results of missing values by person to table
#'
#' @param res  list with results as returned by mv_person
#'
#' @return   table with frequency of missing values for one missing value type
#' @export

write_results <- function(res) {
  for (i in names(res)) {
    results <- data.frame(res[[i]])
    names(results) <- c("Number of missing responses", "Percentage")
    return(results)
  }
}
