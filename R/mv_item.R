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
#' @param stages  character vector; contains names of stage variables in resp and vars,
#' only applicable in multistage tests (otherwise NULL)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param missing_by_design  integer; user defined missing value for missing by
#' design
#' @param plots  logical; whether plots shall be created and saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param suf_item_names logical; whether to output SUF item names in the .xlsx file
#'  for items with collapsed categories
#' @param show_all  logical; whether whole sample shall be included as a "group"
#' (only applicable when grouping exists)
#' @param color  character scalar or vector; defines color(s) of the bar in plots
#' @param name_grouping  string; name of the grouping variable (e.g. test version)
#' for title and legend of plots (only needed when grouping exists)
#' @param labels_legend character vector; contains legend labels
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'settingA')
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be better set to true)
#' @param verbose  logical; whether to print processing information to console
#'
#' @return (if return = TRUE) list with results:
#'            list: percentages for each item
#'            summary: summary statistics across items
#'            summary_table: table with summary statistics for TR.
#' @export

mv_item <- function(
    resp,
    vars,
    select,
    valid = NULL,
    position = NULL,
    grouping = NULL,
    stages = NULL,
    mvs = c(
      OM = -97, NV = -95, NR = -94, TA = -91, UM = -90, ND = -55, MD = -54, AZ = -21
    ),
    labels_mvs = c(
      ALL = "total missing items",
      OM = "omitted items",
      NV = "not valid items",
      NR = "not reached items",
      TA = "missing items due to test abortion",
      UM = "unspecific missing items",
      ND = "not determinable items",
      MD = "items missing by design",
      AZ = "missing items due to 'Angabe zurueckgesetzt'"
    ),
    missing_by_design = -54,
    plots = FALSE,
    print = TRUE,
    save = TRUE,
    return = FALSE,
    path_results = "Results",
    path_table = "Tables",
    path_plots = "Plots/Missings_by_item",
    suf_item_names = FALSE,
    show_all = TRUE,
    name_grouping = 'test version',
    overwrite = FALSE,
    name_group = NULL,
    color = NULL,
    digits = 3,
    warn = TRUE,
    verbose = TRUE,
    labels_legend = NULL
  ) {

    # Test data
    scaling:::check_logicals(resp, "resp", c(valid, grouping), warn = warn)
    scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
    scaling:::check_items(vars$item[vars[[select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    if (warn) scaling:::is_null_mvs_valid(valid = valid)

    # Missing by design
    if (!is.null(missing_by_design)) mvs <- mvs[!(mvs %in% missing_by_design)]

    # Conduct analysis
    mv_item <- scaling:::mvi_analysis(
        resp = resp,
        vars = vars,
        select = select,
        valid = valid,
        position = position,
        grouping = grouping,
        stages = stages,
        mvs = mvs,
        missing_by_design = missing_by_design,
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
      missing_by_design = missing_by_design,
      grouping = grouping,
      suf_item_names = suf_item_names,
      warn = warn,
      test = FALSE,
      save = FALSE
    )

    # Write plots
    if (plots) scaling:::mvi_plots(
        resp = resp,
        valid = valid,
        position = position,
        stages = stages,
        digits = digits,
        vars = vars,
        select = select,
        grouping = grouping,
        mvs = mvs,
        labels_mvs = labels_mvs,
        missing_by_design = missing_by_design,
        show_all = show_all,
        verbose = verbose,
        color = color,
        name_grouping = name_grouping,
        labels_legend = labels_legend,
        path = path_plots,
        name_group = name_group,
        warn = warn,
        test = FALSE
    )

    # Save results
    if (save) {
        name <- scaling:::create_name("mv_item", name_group)
        scaling:::save_results(
            mv_item,
            filename = paste0(name, ".rds"),
            path = path_results
        )

        if (suf_item_names) {
            mv_item[["summary_table"]][["list"]][["item"]] <- scaling:::create_suf_names(
               vars_name = mv_item[["summary_table"]][["list"]][["item"]])
        }

        scaling:::save_table(
          mv_item$summary_table,
          overwrite = overwrite,
          filename = paste0(name, ".xlsx"),
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
#' @param stages  character vector; contains names of stage variables in resp and vars,
#' only applicable in multistage tests (otherwise NULL)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param missing_by_design  integer; user defined missing value for missing by
#' design
#' @param path  string; defines path to folder where results shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param name_group  string; defines name of group used in analysis (e.g. 'settingA')
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return    list with percentages:
#'              list: percentages for each item
#'              summary: summary statistics across items.
#' @importFrom stats median sd na.omit
#' @export

mvi_analysis <- function(
    resp,
    vars,
    select,
    position,
    valid = NULL,
    grouping = NULL,
    stages = NULL,
    mvs = c(
      OM = -97, NV = -95, NR = -94, TA = -91, UM = -90, ND = -55, MD = -54, AZ = -21
    ),
    missing_by_design = -54,
    path = "Results",
    use_for_plot = FALSE,
    save = TRUE,
    name_group = NULL,
    digits = 3,
    warn = TRUE,
    test = TRUE
  ) {

    # Test data
    scaling:::test_mvi_analysis(
      resp = resp,
      vars = vars,
      select = select,
      valid = valid,
      grouping = grouping,
      position = position,
      stages = stages,
      warn = warn,
      test = test
    )

    # Prepare data
    vars_c <- vars[vars[[select]], ]
    resp <- scaling:::only_valid(resp, valid = valid)
    resp_c <- scaling:::prepare_resp(
        resp,
        vars = vars,
        select = select,
        warn = warn,
        zap_labels = FALSE
    )

    # NAs are not acknowledged in mvs-argument
    if (warn & any(resp_c %in% NA)) {
      warning("NAs found in resp! These values are ignored.")
    }

    # Convert all values missing by design
    if (!is.null(missing_by_design)) {
        resp_c <- scaling:::convert_mv(resp_c, vars_c, mvs = missing_by_design)
        mvs <- mvs[!(mvs %in% missing_by_design)]
    }

    # Create results
    if (is.null(grouping)) {

        # Create list with results
        mvlist <- scaling:::create_mvlist(
            item = vars_c$item,
            position = vars_c[[position]],
            responses = resp_c,
            stages = stages,
            resp = resp,
            vars = vars_c,
            mvs = mvs,
            digits = digits,
            use_for_plot = use_for_plot
        )

        # Summary across items
        mvsum <- scaling:::mvi_summary(
          mvlist[ , -c(1:ifelse(is.null(stages), 2, 3))],
          digits = digits
        )

    } else {

        # Create list with results
        mvlist <- list()
        mvsum <- list()

        for (g in grouping) {

            # Select administered items for the group
            vars_g <- vars_c[vars_c[[g]], ]
            resp_g <- resp_c[resp[[g]], vars_g$item]

            # Number of valid responses and position
            pos <- scaling:::create_ifelse(
                length(position) == 1,
                vars_g[[position]],
                vars_g[[position[g]]]
            )

            # Create list with results
            mvlist[[g]] <- scaling:::create_mvlist(
              item = vars_g$item[!is.na(pos)],
              position = na.omit(pos),
              responses = resp_g[, vars_g$item[!is.na(pos)]],
              stages = stages,
              resp = resp[resp[[g]],],
              vars = vars_g,
              mvs = mvs,
              digits = digits,
              use_for_plot = use_for_plot
            )

            # Summary across items
            mvsum[[g]] <- scaling:::mvi_summary(
              mvlist[[g]][ , -c(1:ifelse(is.null(stages), 2, 3))],
              digits = digits
            )
        }

        if (length(position) > 1) { # This branch is only compatible with linear tests (see tests at function start)

            # Creating new dataframe with responses by position, not by item
            resp_p <- scaling:::resp_per_position(
              resp = resp,
              resp_c = resp_c,
              vars_c = vars_c,
              grouping = grouping,
              position = position
            )

            pos <- resp_p$position
            resp_p <- data.frame(t(dplyr::select(resp_p, -.data$position)))

            # Number of valid responses per position
            mvlist$all <- data.frame(
              position = pos,
              N_administered = colSums(apply(resp_p, 2, function(x) !is.na(x))),
              N_valid = colSums(apply(resp_p, 2, function(x) !(x %in% mvs | is.na(x))))
            )

            # Determine percentage of missing values for each missing type
            mvlist$all <- cbind(
              mvlist$all,
              data.frame(scaling:::mvi_calc(
                resp_p,
                mvs = mvs,
                digits = digits,
                use_for_plot = use_for_plot
              ))
            )

            # Summary across items
            mvsum$all <- scaling:::mvi_summary(mvlist$all[ , -1], digits = digits)

        } else {

          # Create list with results
          mvlist$all <- scaling:::create_mvlist(
            item = vars_c$item,
            position = vars_c[[position]],
            responses = resp_c[, vars_c$item],
            stages = stages,
            resp = resp,
            vars = vars_c,
            mvs = mvs,
            digits = digits,
            use_for_plot = use_for_plot
          )

          # Summary across items
          mvsum$all <- scaling:::mvi_summary(
            mvlist$all[ , -c(1:ifelse(is.null(stages), 2, 3))],
            digits = digits
          )
        }
    }

    # List with results
    mv_i <- list(list = mvlist, summary = mvsum)

    # Save results
    if (save) {
        name <- scaling:::create_name("mv_item", name_group, ".rds")
        scaling:::save_results(mv_i, filename = name, path = path)
    }

    # Return results
    return(mv_i)
}

#' Test data for mvi_analysis()
#'
#' # Function arguments see mvi_analysis()
#'
#' @noRd

test_mvi_analysis <- function(
    resp, vars, select, valid, grouping, position, stages, warn, test
    ) {

  scaling:::check_numerics(vars, "vars", position, check_invalid = TRUE)

  if (test) {
    scaling:::check_logicals(resp, "resp", c(valid, grouping, stages), warn = warn)
    scaling:::check_logicals(vars, "vars", c(select, grouping, stages), warn = warn)
    scaling:::check_items(vars$item[vars[[select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    if (warn) scaling:::is_null_mvs_valid(valid = valid)
  }

  if (is.null(grouping)) {
    if (length(position) > 1) {
      stop("No grouping. Please provide only one position variable.\n")
    }
  } else {
    if (length(position) == 1) {
      warning("Only one position variable provided. ",
              "The items of each group are therefore set to the same positions.\n")
    } else {
      if (length(position) != length(grouping)) {
        stop("Position and grouping variables do not match. ",
             "Please provide either only one position variable (if positions of items do not differ between groups) ",
             "or matching position and grouping variables (if positions of items do differ between groups).\n")
      } else if (!is.null(stages)) {
        stop("Scaling package cannot account for multiple stages and multiple item positions at once. ",
             "Please conduct missing values analysis manually.\n")
      }
    }
  }

}


#' Create list with missing values per item
#'
#' @param item  character vector; contains names of items
#' @param position  integer vector; contains position of items
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param stages  character vector; contains names of stage variables in resp and vars,
#' only applicable in multistage tests (otherwise NULL)
#' @param digits  integer; number of decimals for rounding
#'
#' @return   list with results of missing values per item.
#' @noRd

create_mvlist <- function(
    item,
    position,
    responses,
    mvs,
    resp,
    vars,
    stages = NULL,
    digits = 3,
    use_for_plot
) {

  if (length(item) != length(position) |
      ncol(responses) != length(item) |
      ncol(responses) != length(position)) {
    stop("Number of items in dataframe responses, in vector item and in vector position do not match. ",
         "Please provide matching arguments to function create_mvlist().")
  }

  # Create dataframe
  mvlist <- data.frame(
    item = item,
    position = position,
    N_administered = colSums(apply(responses, 2, function(x) !is.na(x))),
    N_valid = colSums(apply(responses, 2, function(x) !(x %in% mvs | is.na(x))))
  )

  # Merge with percentage of missing values for each missing type
  results <- data.frame(scaling:::mvi_calc(
    responses,
    mvs = mvs,
    digits = digits,
    use_for_plot = use_for_plot
  ))
  results$item <- row.names(results)
  mvlist <- merge(mvlist, results, by = 'item')
  mvlist <- mvlist[order(mvlist$position, mvlist$item), ]

  # Return list
  return(mvlist)
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
#' type in mvs.
#' @noRd

mvi_calc <- function(responses, mvs, digits = 3, use_for_plot) {

  result <- list()

  # Determine percentage of missing values for each missing type
  for (i in names(mvs)) {
    result[[i]] <- scaling:::mvi_perc(
      responses = responses,
      mvs = mvs[[i]],
      digits = digits,
      use_for_plot = use_for_plot
    )
  }

  # Percentage of total missing responses for each item
  result$ALL <- scaling:::mvi_perc(
    responses = responses,
    mvs = mvs,
    digits = digits,
    use_for_plot = use_for_plot
  )

  return(result)
}

#' Calculate and round frequency (in percentage) of one missing value type (by item)
#'
#' @param responses  data.frame; contains item responses with items as variables
#' and persons as rows; all responses ∈ ℕ0; user-defined missing values;
#' only the items administered to and the cases of the designated group
#' @param mvs  named integer vector; contains user-defined missing values
#' @param digits  integer; number of decimals for rounding
#' @param use_for_plot
#'
#' @return table with frequency of missing values for one missing value type
#' (by item).
#' @noRd

mvi_perc <- function(responses, mvs, digits = 3, use_for_plot) {
  perc <- data.frame(apply(responses, 2, function(x) {
    ifelse(x %in% mvs, 1, ifelse(!is.na(x), 0, if(use_for_plot) 0 else NA))
  }))
  round(apply(perc, 2, mean, na.rm = TRUE) * 100, digits)
}

#' MVI summary
#'
#' @param mvlist  list; return object of create_mvlist()
#' @param digits  integer; number of decimals for rounding
#'
#' @return  data.frame with mean, median, SD, min and max of missing values.
#' @noRd

mvi_summary <- function(mvlist, digits = 3) {
  mvsum <- data.frame(t(apply(mvlist, 2, function(x) {
    round(c(mean(x), sd(x), median(x), range(x)[1], range(x)[2]), digits)
  })))
  mvsum[1, ] <- round(mvsum[1, ])
  colnames(mvsum) <- c("Mean", "SD", "Median", "Min", "Max")
  return(mvsum)
}

#' Create dataframe with responses per position, not items
#'
#' @param resp  see mvi_analysis()
#' @param position  see mvi_analysis()
#' @param grouping  see mvi_analysis()
#' @param resp_c  prepared resp
#' @param vars_c  prepared vars
#'
#' @return dataframe with responses per position
#' @noRd

resp_per_position <- function(resp, resp_c, vars_c, grouping, position) {

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

  return(resp_p)
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
#' @param missing_by_design  integer; user defined missing value for missing by
#' design
#' @param path  string; defines path to folder where table shall be saved
#' @param suf_item_names logical; whether to output SUF item names in the .xlsx file
#'  for items with collapsed categories
#' @param save  logical; whether results shall be saved to hard drive
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'settingA')
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return table with summary statistics for TR.
#' @export

mvi_table <- function(
    mv_i,
    vars,
    select,
    grouping = NULL,
    mvs = c(
      OM = -97, NV = -95, NR = -94, TA = -91, UM = -90, ND = -55, MD = -54, AZ = -21
    ),
    missing_by_design = -54,
    save = TRUE,
    path = "Tables",
    suf_item_names = FALSE,
    overwrite = FALSE,
    name_group = NULL,
    test = TRUE,
    warn = TRUE
  ) {

  # Missing by design
  if (!is.null(missing_by_design)) mvs <- mvs[!(mvs %in% missing_by_design)]

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
        name <- scaling:::create_name("mv_item", name_group, ".xlsx")

        if (suf_item_names) {
            results[["list"]][["item"]] <- scaling:::create_suf_names(
              vars_name = results[["list"]][["item"]])
        }

        scaling:::save_table(
          results,
          filename = name,
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
#' @param stages  character vector; contains names of stage variables in resp and vars,
#' only applicable in multistage tests (otherwise NULL)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param labels_mvs  named character vector; contains labels for user-defined
#' missing values to use them in plot titles and printed results
#' @param missing_by_design  integer; user defined missing value for missing by
#' design
#' @param path  string; defines path to folder where plots shall be saved
#' @param color  character scalar or vector; defines color(s) of the bar in plots
#' @param name_group  string; defines name of group used in analysis (e.g. 'settingA')
#' @param name_grouping  string; name of the grouping variable (e.g. 'test version')
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

mvi_plots <- function(
    resp,
    vars,
    select,
    valid,
    position,
    stages = NULL,
    grouping = NULL,
    mvs = c(
      OM = -97, NV = -95, NR = -94, TA = -91, UM = -90, ND = -55, MD = -54, AZ = -21
    ),
    labels_mvs = c(
      ALL = "total missing items",
      OM = "omitted items",
      NV = "not valid items",
      NR = "not reached items",
      TA = "missing items due to test abortion",
      UM = "unspecific missing items",
      ND = "not determinable items",
      MD = "items missing by design",
      AZ = "missing items due to 'Angabe zurueckgesetzt'"
    ),
    missing_by_design = -54,
    path = "Plots/Missings_by_item",
    name_group = NULL,
    name_grouping = 'test version',
    show_all = TRUE,
    labels_legend = NULL,
    color = NULL,
    digits = 3,
    verbose = TRUE,
    warn = TRUE,
    test = TRUE
  ) {

  # Missing by design
  if(!is.null(missing_by_design)) mvs <- mvs[!(mvs %in% missing_by_design)]

  mv_i <- scaling:::mvi_analysis(
    resp = resp,
    vars = vars,
    select = select,
    valid = valid,
    position = position,
    grouping = grouping,
    stages = stages,
    mvs = mvs,
    missing_by_design = missing_by_design,
    digits = digits,
    warn = warn,
    save = FALSE,
    test = TRUE,
    use_for_plot = !is.null(stages)
  )


  # Test data
  if (test) {
    scaling:::test_mvi_data(mv_i, mvs = mvs, grouping = grouping)
    scaling:::check_logicals(vars, "vars", c(select, grouping), warn = warn)
    scaling:::check_items(vars$item[vars[[select]]])
  }

  # Prepare data
  mv_i <- mv_i$list
  k <- max(vars[vars[[select]], position], na.rm = TRUE)

  # Create groups vector
  if (!is.null(grouping))
    groups <- scaling:::create_ifelse(show_all, c(grouping, 'all'), grouping)

  # Check color argument
  grps <- scaling:::create_ifelse(is.null(grouping), 1, length(groups))
  color <- scaling:::check_color(color = color, grps = grps)

  # Create directory for plots
  path_ <- scaling:::create_name(path, name_group, sep = "/")
  scaling:::check_folder(path_)

  # Create plots

  for (i in names(mvs)) {

    add_missings_per_stage <- !is.null(stages) & "NR" %in% names(mvs) & i %in% c("NR", "ALL")

    if (is.null(grouping)) {

      # create plot
      mv_i <- scaling:::mv_per_position(
        mv_i,
        mv = i,
        resp = resp,
        vars = vars,
        stages = stages,
        position = position,
        add_missings_per_stage = add_missings_per_stage,
        digits = digits
      )
      y <- mv_i[[i]]
      ylim <- ceiling(max(y, na.rm = TRUE)/10)*10

      gg <- ggplot2::ggplot(
              data = mv_i,
              mapping = ggplot2::aes(x = position, y = y, fill = color)
            ) +
            ggplot2::scale_fill_manual(values = color) +
            ggplot2::labs(
              title = paste0(Hmisc::capitalize(labels_mvs[i]), " by item position"),
              x = "Item position", y = "Percentage"
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(legend.position = 'none')

    } else {

      mv_wide <- scaling:::create_wide_df_mvi(
        mv_i,
        groups = groups,
        mv = i,
        resp = resp,
        vars = vars,
        stages = stages,
        position = position,
        add_missings_per_stage = add_missings_per_stage,
        digits = digits
      )
      ylim <- ceiling(max(mv_wide$MV, na.rm = TRUE)/10)*10

      # create plot
      gg <- ggplot2::ggplot(
        data = mv_wide,
        mapping = ggplot2::aes(
          x = position,
          y = MV,
          fill = group
        )
      ) +
        ggplot2::labs(
          title = paste0(
            Hmisc::capitalize(labels_mvs[i]),
            " by item position and ",
            name_grouping
          ),
          x = "Item position",
          y = "Percentage"
        ) +
        if (is.null(labels_legend)) {
          ggplot2::scale_fill_manual(
            name = Hmisc::capitalize(name_grouping),
            values = color
          )
        } else {
          if (length(labels_legend) != length(groups)) {
            warning("Number of provided legend labels does not ",
                    "correspond to number of groups. ",
                    "Group labels are used instead.")
            ggplot2::scale_fill_manual(
              name = Hmisc::capitalize(name_grouping),
              values = color
            )
          } else {
            ggplot2::scale_fill_manual(
              name = Hmisc::capitalize(name_grouping),
              labels = labels_legend,
              values = color
            )
          }
        }
    }

    gg <- gg +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_y_continuous(
        breaks = seq(0, ylim, 10),
        labels = paste0(seq(0, ylim, 10), " %"),
        limits = c(0, ylim)
      ) +
      ggplot2::scale_x_continuous(
        breaks = seq(0, k, ifelse(k < 20, 1, floor(k/20)))
      ) +
      if (!is.null(grouping)) ggplot2::theme_bw() +
      if (!is.null(grouping)) ggplot2::theme(
        legend.justification = c(0, 1),
        legend.position = c(0.01, 0.99)
      )

    # save plot
    ggplot2::ggsave(
      filename = paste0("Missing_responses_by_item_", i,".png"),
      plot = gg, path = path_, width = 2000, height = 1200, units = "px",
      dpi = 300
    )

    # Print progress
    if (verbose) cat("Missing plot", i, "created.\n")
  }
}

#' Check color argument and create new if null
#'
#' @param color  vector; includes colors for plot
#' @param grps  character vector; includes groups for graph
#'
#' @return color argument
#' @noRd

check_color <- function(color, grps) {

  if (!is.null(color)) {
    if (length(color) != grps) {
      stop(paste0('The number of provided colors does not match the number ',
                  'of groups (', grps, ').'))
    }
  } else {
    color <- colorspace::sequential_hcl(grps)
  }

  return(color)

}

#' Create dataframe in wide format for all groups
#'
#' @param mv_i  list with missing values as variables
#' @param groups  character vector; includes names of groups for graph
#' @param mv string; name of missing value type for graph
#' @param resp see mvi_plots()
#' @param vars see mvi_plots()
#' @param stages see mvi_plots()
#' @param position see mvi_plots()
#' @param digits see mvi_plots()
#' @param add_missings_per_stage logical; indicates whether for this missing
#' value all missings per stage shall be added
#'
#' @return dataframe in wide format
#' @noRd

create_wide_df_mvi <- function(
    mv_i,
    groups,
    mv,
    resp,
    vars,
    stages,
    position,
    add_missings_per_stage,
    digits
  ) {

  df <- data.frame(position = NA)

  for (g in groups) {
    mv_pos <- scaling:::mv_per_position(
      mv_i[[g]],
      mv = mv,
      resp = resp[resp[[g]],],
      vars = vars[vars[[g]],],
      stages = stages,
      position = position,
      add_missings_per_stage = add_missings_per_stage,
      digits = digits
    )
    mv_pos[[g]] <- mv_pos[[mv]]
    df <- merge(df, mv_pos[, c("position", g)], by = 'position', all = TRUE)
  }

  df <- dplyr::filter(
    dplyr::select(df, c("position", tidyselect::all_of(groups))),
    !is.na(df$position)
  )

  mv_wide <- tidyr::gather(
    df, key = "group", value = "MV", tidyselect::all_of(groups)
  )

  mv_wide$group <- factor(mv_wide$group, levels = groups)

  return(mv_wide)
}

#' Create dataframe with missing values per position and not item
#'
#' @param mv_i  list with missing values as variables
#' @param mv string; name of missing value type for graph
#' @param resp see mvi_plots()
#' @param vars see mvi_plots()
#' @param stages see mvi_plots()
#' @param position see mvi_plots()
#' @param digits see mvi_plots()
#' @param add_missings_per_stage logical; indicates whether for this missing
#' value all missings per stage shall be added
#'
#' @return dataframe with missing values as variables and position as lines
#' @noRd

mv_per_position <- function(
    mv_i,
    mv,
    resp,
    vars,
    stages,
    position,
    add_missings_per_stage,
    digits
  ) {

  pos <- mv_i$position[!is.na(mv_i$position)]

  if (length(unique(pos)) != length(pos)) {

    mv_pos <- data.frame(position = unique(pos))

    for (p in pos) {

      df <- mv_i[mv_i$position == p, ]

      mv_pos[mv_pos$position == p, mv] <- sum(df[[mv]], na.rm = TRUE)

    }

  } else {

    mv_pos <- mv_i
  }

  # Multistage tests
  if (add_missings_per_stage)
    scaling:::add_missings_per_stage(mv_pos, mv, resp, vars, stages, position, digits)

  return(mv_pos)
}

#' Add missings per stage to dataframe
#'
#' @param mv_pos dataframe with missing values per position
#' @param mv string; name of missing value type for graph
#' @param resp see mvi_plots()
#' @param vars see mvi_plots()
#' @param stages see mvi_plots()
#' @param position see mvi_plots()
#' @param digits see mvi_plots()
#'
#' @return dataframe with missing values per position including missings per stage
#' @noRd

add_missings_per_stage <- function(
    mv_pos,
    mv,
    resp,
    vars,
    stages,
    position,
    digits
  ) {

  not_reached_stage <- sapply(
    resp[stages], function(x) {round((1 - mean(x)) * 100, digits)}
  )

  mv_pos$stage <- NA

  for (stage in stages) {

    s <- which(stages == stage)
    item_in_stage <- mv_pos$position %in% unique(vars[[position]][vars[[stage]]])
    mv_pos$stage[item_in_stage] <- s
    mv_pos[[mv]][item_in_stage] <- mv_pos[[mv]][item_in_stage] + not_reached_stage[s]

  }

  mv_pos <- mv_pos[
    order(mv_pos$stage, mv_pos$position),
    names(mv_pos[c(1, length(mv_pos), 2:(length(mv_pos)-1))])
  ]

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
                                  MD = "items missing by design",
                                  AZ = "missing items due to 'Angabe zurueckgesetzt'"
                                  )) {
    if (is.data.frame(mv_i$list)) {

      n <- ifelse(is.null(mv_i$list$stage), 4, 5)

        for (lbl in names(mv_i$list[-c(1:n)])) {

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

            n <- ifelse(is.null(mv_i$list[[g]]$stage), 4, 5)
            message("\n", Hmisc::capitalize(g), ":\n")

            for (lbl in names(mv_i$list[[g]][-c(1:n)])) {

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

    message("\n\n")
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
