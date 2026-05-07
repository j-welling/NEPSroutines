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
#' @param correct string; defines name of variable in vars that contains the
#' correct responses to the items
#' @param sep string; defines type of punctuation used for separating several
#' correct responses in the variable correct_response in vars (default is ";")
#'
#' @return resp with dichotomously scored MC items.
#' @export
dichotomous_scoring <- function(resp, vars, old_names, new_names = NULL,
                                correct = 'correct_response', sep = ";") {

    # Check whether variables are indeed contained in data.frames
    check_variables(resp, "resp", old_names)

    # Check whether variable with correct responses is available
    check_variables(vars, "vars", correct)

    # Check for duplicates
    check_items(old_names)
    if(!is.null(new_names)) check_items(new_names)

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
        correct_resp <- base::strsplit(
          as.character(vars[[correct]][vars$item == item]), sep
        )[[1]]
        resp[[new_names[i]]] <- ifelse(
          resp[[item]] %in% correct_resp, 1, ifelse(resp[[item]] < 0, resp[[item]], 0)
        )
        resp[[new_names[i]]] <- as.numeric(resp[[new_names[i]]])
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

  # Check whether all items are indeed included in vars
  if (any(!(old_names %in% vars$item))) {
    stop("Item/s '", paste(old_names[!(old_names %in% vars$item)], collapse = "', '"),
         "' is/are not included in vars! Please check again.")
  }

  # Create new dataframe
  vars_new <- vars[vars$item %in% old_names, ]
  vars_new <- vars_new[match(old_names, vars_new$item), ]
  vars_new$item <- new_names

  # Change variable values
    if (!is.null(change)) {
        for (c in seq_along(change)) {
            variable <- names(change[c])
            new_value <- change[[c]]
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
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines the name of the logical variable in vars
#' that indicates which items should be included in the imputation model.
#' It refers to all scored dichotomous items: multiple-choice items and
#' subitems of polytomous items.
#' @param mvs  integer vector; contains user-defined missing values
#' @param warn  logical; print warnings
#' @param missing_by_design  numeric; missing value indicating missing by design
#' @param impute logical; whether to impute missing values for
#' subitems of a polytomous item
#' @param threshold numeric; gives the threshold for the share of missing subitems
#'   used to impute missing responses
#' @param path_results  string; defines path to folder where results shall be
#' saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param verbose  logical; provides information on how polytomous items are scored
#'
#' @return resp including unscored (raw) and scored items
#' @export
pc_scoring <- function(resp, poly_items, vars = NULL, select = NULL,
                       mvs = NULL, warn = TRUE,
                       missing_by_design = -54,
                       impute = TRUE, threshold = .50,
                       path_results = "Results",  path_table = "Tables",
                       save = TRUE, overwrite = TRUE, verbose = TRUE) {

  # Test data
  if ( !is.list(poly_items) ) {
    stop( "The argument 'poly_items' must be a list. Please check your input." )
  }
  if ( !is.numeric(threshold) | threshold < 0 | threshold > 1) {
    stop( "The argument 'treshold' must be numeric in the interval ",
          "between 0 and 1. Please check your input." )
  }

  # Check whether variables are indeed contained in data.frames
  NEPSroutines:::check_numerics(resp, "resp", unlist(poly_items), dich = TRUE)

  # Check pc_item (should be marked with 's_c' or 's_[startingCohortTargetGroup]_c')
  if (warn) {
    for ( pc_name in names(poly_items) ) {
      is_pc_named_correctly <- grepl("s(_[a-zA-Z0-9]+)*_c$", pc_name)
      if ( !is_pc_named_correctly ) {
        message( pc_name, ": Variable name should contain a subitem marker like 's', e.g. '[item]s_c', '[item]s_sc3g9_c'.\n" )
      }
    }
  }

  # Set missing values
  if (is.null(mvs)) {
    mvs <- c(-99:-1)
    if (isTRUE(warn))
      warning("No missing values provided. c(-99:-1) used as default.")
  }

  # Impute missing subitems
  if (impute) {

    if (verbose) {
      message( "When scoring polytomous items, missing values of subitems are ",
               "imputed if the share of missing responses falls below the 'threshold'. ",
               "However, the returned dataset remains unchanged, that is, ",
               "it includes the original (non-imputed) responses for the ",
               "subitems.\n",
               "To skip subitem imputation, set 'impute = FALSE'." )
    }

    # Create indicators for missing subitems to impute
    indicators <- NEPSroutines:::pc_missing_subitems(
      resp = resp,
      mvs = mvs,
      missing_by_design = missing_by_design,
      poly_items = poly_items,
      threshold = threshold,
      path_results = path_results,
      path_table = path_table,
      save = save,
      overwrite = overwrite
    )

    # Impute missing values
    resp_full <- NEPSroutines:::pc_imputation(
      resp = resp,
      vars = vars,
      select = select,
      mvs = mvs,
      missing_by_design = missing_by_design,
      poly_items = poly_items,
      indicators = indicators,
      path_results = path_results,
      save = save
    )

  } else {

    resp_full <- resp

  }

  # Score polytomous items
  for (item in names(poly_items)) {
    subitems <- poly_items[[item]]

    pc_item <- rowSums(resp_full[, subitems] == 1)
    number_missing <- rowSums(resp_full[, subitems] < 0)
    any_missing <- number_missing > 0
    pc_item[any_missing] <- -55

    for (mv in mvs) {
      all_this_missing_type <-
        (rowSums(resp_full[, subitems] == mv) == number_missing) & any_missing
      pc_item[all_this_missing_type] <- mv
    }

    resp[[item]] <- pc_item

  }

  return(resp)

}


#' Create indicators for subitems with missing values
#' (criterion: < 50% of subitems of a pc-item with missing values, as defined with 'threshold')
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data; additionally includes ID_t
#' as a person identifier and all variables that are further defined in
#'the function arguments
#' @param mvs  named integer vector; contains user-defined missing values
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param missing_by_design  numeric; missing value indicating missing by design
#' @param threshold numeric; gives the threshold for the share of missing subitems
#'   used to impute missing responses
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @noRd
pc_missing_subitems <- function( resp, mvs, poly_items,
                                 missing_by_design, threshold,
                                 path_results, path_table,
                                 save, overwrite ) {

  # Step 1. Create indicators for missing values on subitems
  subitems <- unlist(poly_items)
  indicators <- resp[c("ID_t", subitems)]
  indicators[subitems] <- lapply(indicators[subitems], \(x) {
    ifelse(x %in% missing_by_design, NA,
           ifelse(x %in% mvs, 1, 0))
  })
  # test
  if ( sum(sapply(indicators[subitems], \(x) {
    all(range(x, na.rm = TRUE) %in% c(0, 1))
  })) != length(names(indicators)[-1]) ) {
    warning( "Recoding of subitems into indicator variables failed. ",
             "Please contact the package developers." )
  }

  # Step 2. Calculate number of missing values (coded as 1) within each
  # polytomous item
  for ( i in names(poly_items) ) {
    indicators[[paste0(i, "_sumMV")]] <-
      rowSums(indicators[poly_items[[i]]], na.rm = TRUE)
    indicators[[paste0(i, "_relMV")]] <-
      indicators[[paste0(i, "_sumMV")]] / length(poly_items[[i]])
    indicators[[paste0(i, "_impMV")]] <-
      as.numeric(
        indicators[[paste0(i, "_relMV")]] > 0 &
        indicators[[paste0(i, "_relMV")]] < threshold
      )
  }
  #test
  if ( sum(grepl("_sumMV$", names(indicators))) != length(poly_items) ) {
    stop( "Number of 'sumMV' variables does not match the number of 'pc_items'. ",
          "Please contact the package developers." )
  }
  rm(i)

  # Print results
  summary_items_impMV <- data.frame(
    Item = sub("_impMV.*", "", grep("_impMV", names(indicators), value = TRUE)),
    Freq_of_imputed_MV = sapply(indicators[grep("_impMV", names(indicators), value = TRUE)], function(x) sum(x == 1, na.rm = TRUE)),
    RelFreq_of_imputed_MV = sapply(indicators[grep("_impMV", names(indicators), value = TRUE)], function(x) round(mean(x == 1, na.rm = TRUE), 3)),
    row.names = NULL
  )
  message("\nOverview of the absolute and relative frequencies of imputed missing values " ,
          "for the polytomous items in the dataset: ")
  print(summary_items_impMV)
  desc_items_impMV <- psych::describe(summary_items_impMV["RelFreq_of_imputed_MV"])[c(2:5,8:10)]
  print(desc_items_impMV, digits = 3)

  Freq <- table(rowSums(indicators[grep("_impMV", names(indicators), value = TRUE)], na.rm = TRUE))
  RelFreq <- round(prop.table(Freq), 3)
  summary_cases_impMV <- data.frame(Nr_of_polytomous_items_with_imputed_MV = rownames(Freq),
                                    cbind(Freq = as.integer(Freq), RelFreq = as.numeric(RelFreq)),
                                    row.names = NULL)
  message("\nOverview of cases with imputed missing values for the polytomous items in the dataset:")
  print(summary_cases_impMV)

  # Save results
  if ( save ) {
    tab_sumMV <- apply(indicators[, names(indicators)[grepl("_sumMV", names(indicators))]], 2, table, useNA = "always")
    tab_impMV <- apply(indicators[, names(indicators)[grepl("_impMV", names(indicators))]], 2, table, useNA = "always")
    results = list(indicators = indicators,
                   tab_sumMV = tab_sumMV,
                   tab_impMV = tab_impMV,
                   summary_items_impMV = summary_items_impMV,
                   desc_items_impMV = desc_items_impMV,
                   summary_cases_impMV = summary_cases_impMV)
    NEPSroutines:::save_results(
      results,
      "pc_subitems_mv_indicators.rds",
      path_results
    )

    results <- results[4:6]
    NEPSroutines:::save_table(
      results,
      "summary_pc_subitems_mv_indicators.xlsx",
      path_table,
      overwrite = overwrite
    )
  }

  return( indicators )

}



#' Subitem imputation
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param mvs  named integer vector; contains user-defined missing values
#' @param missing_by_design  numeric; user defined missing value for missing by
#' design (is necessary for calculating N_administered)
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param indicators data.frame; contains indicators for missing values
#' that should be imputed
#' @param path_results  string; defines path to folder where results shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @noRd
pc_imputation <- function( resp, vars, select,
                           mvs, missing_by_design,
                           poly_items, indicators,
                           path_results, save ) {

  # Test
  if ( is.null(indicators) | !is.data.frame(indicators) ) {
    stop( "The imputation of missing values on subitems, requires a ",
          "data.frame with missing indicators. ",
          "This data.frame should have been generated automatically ",
          "using 'pc_scoring()' with 'impute = TRUE'. ",
          "Please contact the package developers." )
  }
  if ( length(intersect(resp$ID_t, indicators$ID_t)) != nrow(resp) ) {
    stop( "The number of respondents 'resp' does not match the ",
          "number of respondents in 'indicators'. These ",
          "data.frames should have generated automatically ",
          "using 'pc_scoring()' with 'impute = TRUE'. ",
          "Please contact the package developers." )
  }
  if ( is.null(vars) | !is.data.frame(vars) ) {
    stop( "The imputation of missing values on subitems, requires a ",
          "data.frame containing information on the competence items. ",
          "This should be specified in the 'vars' argument. ",
          "Please check your input." )
  }
  if ( is.null(select) ) {
    stop( "The imputation of missing values on subitems requires ",
          "the name of a logical variable in vars that indicates ",
          "the scored dichotomous items: multiple-choice items ",
          "and subitems of polytomous items. ",
          "This should be specified in the 'select' argument. ",
          "Please check your input." )
  }
  if ( !all(unlist(poly_items) %in% names(indicators)) ) {
    stop( "The subitems defined in 'poly_items' are not included in ",
                 "'indicators'. Please check your input." )
  }
  if ( !all(unlist(poly_items) %in% vars$item[vars[[select]]]) ) {
    stop( "The subitems defined in 'poly_items' are not included in ",
          "the selected item set. Please check your input." )
  }

  # Default valid cases
  resp_ <- NEPSroutines:::convert_mv(resp, vars = vars, select = select,
                                     warn = FALSE)
  resp$valid <- rowSums(!is.na(resp_[, vars$item[vars[[select]]]])) >= 3
  valid <- "valid"
  rm(resp_)

  # Fit Rasch model
  fit <- NEPSroutines::irt_analysis(
    resp = resp,
    vars = vars,
    select = select,
    valid = valid,
    mvs = mvs,
    missing_by_design = missing_by_design,
    scoring = NULL,
    plots = FALSE,
    save = FALSE,
    print = FALSE,
    return = TRUE,
    suf_item_names = FALSE,
    verbose = FALSE,
    overwrite = FALSE,
    warn = FALSE,
    test = TRUE,
    xsi_fixed_1p = NULL,
    xsi_fixed_2p = NULL,
    pweights = NULL,
    control_tam = NULL,
    control_wle = NULL
  )

  # Calculate predicted responses (threshold = .50 as criterion for predicted response)
  xsi <- fit$model.1pl$mod$xsi$xsi
  names(xsi) <- row.names(fit$model.1pl$mod$xsi)
  theta <- as.data.frame(fit$model.1pl$mod$person[, c("pid", "EAP", "SD.EAP")])
  P <- data.frame(ID_t = theta$pid, sapply(xsi, \(x) 1 / (1 + exp(-(theta$EAP - x)))))
  pred_resp <- data.frame(ID_t = theta$pid, ifelse(P[,-1] > 0.5, 1, 0))
  #test
  if ( !setequal(resp$ID_t[resp[[valid]] == TRUE], pred_resp$ID_t) |
       length(resp$ID_t[resp[[valid]] == TRUE]) != length(pred_resp$ID_t) ) {
    warning( "ID_ts in original data.frame and in data.frame with ",
             "predicted responses are different. ",
             "Please contact the package developer." )
  }

  # Calculate error rate (threshold = .50 as criterion for predicted response)
  merged <- merge(resp, pred_resp, by.x = "ID_t", by.y = "ID_t", suffixes = c("_true", "_pred"))
  error_rates <- data.frame(
    item = vars$item[vars[[select]]],
    error_rate = sapply(vars$item[vars[[select]]], \(item) {
      true_values <- merged[[paste0(item, "_true")]]
      pred_values <- merged[[paste0(item, "_pred")]]
      no_na <- !(true_values %in% mvs)
      mean(true_values[no_na] != pred_values[no_na]) # % of discrepancies between observed and predicted responses
    })
  )
  mean_error_rates <- mean(error_rates$error_rate)

  # Imputation of predicted responses for subitems with missing values
  resp_imp <- resp
  impMV <- names(indicators)[grepl("_impMV$", names(indicators))]
  for (ID_t in resp$ID_t) {
    for (imp in impMV) {
      sel <- indicators$ID_t == ID_t
      if (indicators[sel, imp] == 1) {
        item_stem <- sub("_impMV$", "", imp)
        for (subitem in poly_items[[item_stem]]) {
          if (indicators[sel, subitem] == 1) {
            resp_imp[resp_imp$ID_t == ID_t, subitem] <-
              pred_resp[pred_resp$ID_t == ID_t, subitem]
          }
        }
      }
    }
  }

  # Save results
  if (save) {
    pc_subitems_imputation <- list(
      fit = fit,
      pred_resp = pred_resp,
      error_rates = error_rates,
      mean_error_rates = mean_error_rates,
      resp_imp = resp_imp
    )
    NEPSroutines:::save_results(
      pc_subitems_imputation,
      "pc_subitems_imputations.rds",
      path_results
    )
  }

  return( resp_imp )

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
#' @return data.frame resp with collapsed and original items

collapse_response_categories <- function(resp, vars, select = 'poly',
                                         per_cat = 200, save = FALSE,
                                         path_table = "Tables") {

  # Check whether variables are indeed contained in data.frames
  NEPSroutines:::check_logicals(vars, "vars", select, warn = TRUE)
  polyt_items <- vars$item[vars[[select]]]
  NEPSroutines:::check_numerics(resp, "resp", polyt_items)
  NEPSroutines:::check_items(polyt_items)

  collapsed_items <- c()
  dichotomous_items <- c()
  problematic_items <- c()

  for (item in polyt_items) {

    response <- resp[[item]]

    # Create table with all possible categories (= from minimum to maximum value)
    vals <- unique(response[response >= 0 & !is.na(response)])
    values <- 0:max(vals)
    tab <- sapply(values, function (x) sum(response == x, na.rm = TRUE))
    names(tab) <- values

    # Skip dichotomous items with response categories 0 and 1
    if (length(values) <= 2) {

      dichotomous_items <- c(dichotomous_items, item)

    } else {

      collapse <- which(tab < per_cat)
      collapse_values <- as.numeric(names(collapse))

      if (length(collapse) > 0) {

        log <- matrix(values, nrow = 1, dimnames = list("", values))
        while (length(collapse) > 0) {

          # for score of 0: left shift all values larger than 0
          log <- rbind(log, NA)
          if (collapse_values[1] == 0) {

            j <- which(response > 0)

            # for highest score: left shift current value
            log[nrow(log), ] <-
              c(log[nrow(log) - 1, log[nrow(log) - 1, ] == 0],
                log[nrow(log) - 1, log[nrow(log) - 1, ] > 0] - 1)
          } else if (collapse_values[1] == max(response, na.rm = TRUE)) {

            j <- which(response == max(response, na.rm = TRUE))

            # for scores between lowest and highest score, if the next score has
            #  a smaller frequency than the previous score:
            #  left shift all values greater than the current value
            log[nrow(log), ] <-
              c(log[nrow(log) - 1, log[nrow(log) - 1, ] != max(log[nrow(log) - 1, ])],
                log[nrow(log) - 1, log[nrow(log) - 1, ] == max(log[nrow(log) - 1, ])] - 1)
          } else if (tab[collapse[1] - 1] > tab[collapse[1] + 1]) {

            j <- which(response > collapse_values[1])

            # for scores between 1 and highest score, if the previous score has
            #  a smaller frequency than the next score:
            #  left shift the current value and all values greater than the
            #  current value
            log[nrow(log), ] <-
              c(log[nrow(log) - 1, log[nrow(log) - 1, ] <= collapse_values[1]],
                log[nrow(log) - 1, log[nrow(log) - 1, ] > collapse_values[1]] - 1)
          } else if (tab[collapse[1] - 1] <= tab[collapse[1] + 1]) {

            j <- which(response >= collapse_values[1])

            log[nrow(log), ] <-
              c(log[nrow(log) - 1, log[nrow(log) - 1, ] < collapse_values[1]],
                log[nrow(log) - 1, log[nrow(log) - 1, ] >= collapse_values[1]] - 1)
          }

          response[j] <- response[j] - 1

          # Create table with all possible categories (= from minimum to maximum value)
          vals <- unique(response[response >= 0 & !is.na(response)])
          values <- 0:max(vals)
          tab <- sapply(values, function (x) sum(response == x, na.rm = TRUE))
          names(tab) <- values

          # Determine categories for collapsing
          collapse <- which(tab < per_cat)
          collapse_values <- as.numeric(names(collapse))

          if (length(tab) <= 1)
            break
        }

        if (length(collapse) == 0 & length(values) >= 2) {

          resp[ , paste0(item, "_collapsed")] <- response
          collapsed_items <- rbind(collapsed_items,
                                   c(item, paste0(log[1, ], "=", log[nrow(log),], collapse = ", ")))
        }
        else {

          problematic_items <- c(problematic_items, item)

        }
      }
    }
  }

  # Which items have been collapsed?
  colnames(collapsed_items) <- c("Item", "Scoring")
  item_names <- tibble::tibble(original_item = collapsed_items[, 1],
                               scoring = collapsed_items[, 2],
                               collapsed_item = paste0(collapsed_items[, 1], "_collapsed"))

  # Print results
  if (!is.null(problematic_items)) {
    message("\nThe following items resulted in less than two response categories ",
            "with more than ", per_cat, " cases and were thus not collapsed. ",
            "Please check these items manually:\n",
            paste(problematic_items, collapse = ", "))
  }

  if (!is.null(dichotomous_items)) {
    message("\nDichotomous items were not considered for collapsing. ",
            "The following items have less than three response categories::\n",
            paste(dichotomous_items, collapse = ", "))
  }

  if (!is.null(collapsed_items)) {
    message("\nThe following items have been collapsed:\n")
    print(item_names, n=nrow(item_names))
  } else {
    message("\nNo items have been collapsed.")
  }

  # Save results
  if (save) {

    NEPSroutines:::save_table(
        results = list(
            collapsed = item_names,
            dichotomous = dichotomous_items,
            problematic = problematic_items
        ),
        filename = "collapsed_items.xlsx",
        path = path_table,
        overwrite = TRUE,
        show_rownames = FALSE
    )
  }

  return(resp)
}



#' Create table for all possible response categories (even if n = 0)
#'
#' @param response responses for one item
#'
#' @return  table
#' @export
create_table <- function(response) {

  # Create table with all possible categories (= from minimum to maximum value)

  # # Different approaches for character or numeric variables
  # if (is.character(vals)) {
  #   if (vals[1] == toupper(vals[1])) {
  #     nums <- which(LETTERS %in% vals)
  #     values <- LETTERS[min(nums):max(nums)]
  #   } else {
  #     nums <- which(letters %in% vals)
  #     values <- letters[min(nums):max(nums)]
  #   }
  # } else {
  #   values <- min(vals):max(vals)
  # }

  return(tab)
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
    NEPSroutines:::check_logicals(vars, "vars", select)
    items <- vars$item[vars[[select]]]
    NEPSroutines:::check_numerics(resp, "resp", items)
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
    NEPSroutines:::check_logicals(vars, "vars", select)
    NEPSroutines:::check_numerics(vars, "vars", position, check_invalid = TRUE)

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
#' @param birth_day  string; contains name of variable with day of birth
#' (default is median)
#' @param test_year  string or integer; contains either the name of the variable
#' in resp that includes the test year for each participant or one number with
#' the test year if it's the same for all participants
#' @param test_month  string or integer; contains either the name of the variable
#' in resp that includes the test month for each participant or one number with
#' the test month if it's the same for all participants
#' @param test_day  string or integer; contains either the name of the variable
#' in resp that includes the test day for each participant or one number with
#' the test day if it's the same for all participants (default is median)
#'
#' @return   numeric vector with approximate age in years
#' @export
calculate_age <- function(resp,
                          birth_year = "birthy", birth_month = "birthm",
                          test_year = "testy", test_month = "testm",
                          birth_day = NULL, test_day = NULL) {

    # Check and create birth date variables
    NEPSroutines:::check_variables(resp, "resp", c(birth_year, birth_month)    )
    byear <- resp[[birth_year]]
    bmonth <- resp[[birth_month]]

    # Check and create test date variables
    if (is.numeric(test_year)) {
        tyear <- test_year
    } else {
        NEPSroutines:::check_variables(resp, "resp", test_year)
        tyear <- resp[[test_year]]
    }

    if (is.numeric(test_month)) {
        tmonth <- test_month
    } else {
        NEPSroutines:::check_variables(resp, "resp", test_month)
        tmonth <- resp[[test_month]]
    }

    # Check whether birth and test day exist and if not, replace with default 15
    if (is.null(birth_day)) {
        bday <- 15
    } else {
        NEPSroutines:::check_variables(resp, "resp", birth_day)
        bday <- resp[[birth_day]]
    }

    if (is.null(test_day)) {
        tday <- 15
    } else if (is.numeric(test_day)) {
        tday <- test_day
    } else {
        NEPSroutines:::check_variables(resp, "resp", test_day)
        tday <- resp[[test_day]]
    }

    # Replace missing values in birth and test date with the sample median
    na_by <- is.na(byear)
    na_bm <- is.na(bmonth)
    na_ty <- is.na(tyear)
    na_tm <- is.na(tmonth)

    if (sum(na_by) > 0) {
        message(sum(na_by), " missing value(s) in birth year were replaced by the sample median.")
        byear[na_by] <- round(median(byear, na.rm = TRUE))
    }

    if (sum(na_bm) > 0) {
        message(sum(na_bm), " missing value(s) in birth month were replaced by the sample median.")
        bmonth[na_bm] <- round(median(bmonth, na.rm = TRUE))
    }

    if (sum(na_ty) > 0) {
        message(sum(na_ty), " missing value(s) in test year were replaced by the sample median.")
        tyear[na_ty] <- round(median(tyear, na.rm = TRUE))
    }

    if (sum(na_tm) > 0) {
        message(sum(na_tm), " missing values in test month were replaced by the sample median.")
        tmonth[na_tm] <- round(median(tmonth, na.rm = TRUE))
    }

    # Calculate age
    birth <- strptime(paste(byear, bmonth, bday, sep = "-"), "%Y-%m-%d")
    test <- strptime(paste(tyear, tmonth, tday, sep = "-"), "%Y-%m-%d")
    age <- as.numeric(difftime(test, birth, units = "weeks")) / 52.1429
    return(age)
}


#' Calculate number of categories for each item
#'
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param select_suf string; defines name of logical variable in vars that indicates
#' which items to use for the calculation (e.g. 'suf')
#' @return   numeric vector with number of subitems/categories for the items to be included in suf
#' @export
calculate_num_cat <- function(vars, poly_items = NULL, select_suf) {

  # Test data
  NEPSroutines:::check_logicals(vars, "vars", select_suf, warn = TRUE)

  # Create vector with number of categories for items to be included in suf
  ## All items get a value of 1 as the number of categories
  num_cat <- c()
  for(item in vars$item[vars[[select_suf]]]) (num_cat[vars$item==item] <- 1)
  rm(item)

  if(!is.null(poly_items)) {

  	# Create named vector with number of categories for each polytomous item
  	poly_cat <- sapply(poly_items, function(x) length(x))
 	names(poly_cat) <- names(poly_items)

  	# Replace value of 1 by the correct number of categories for polytomous items
  	for (item in names(poly_cat)) num_cat[vars$item == item] <- poly_cat[[item]]
  }

  # Return vector
  return(num_cat)
}
