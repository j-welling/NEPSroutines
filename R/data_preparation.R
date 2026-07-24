#' Dichotomous scoring of MC items
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{stop}}{One or more names in \code{old_names} are not found in
#'     \code{vars$item}. Effect: the function halts; no new rows are added to
#'     \code{vars}.}
#' }
#' @export
duplicate_items <- function(vars, old_names, new_names, change = NULL) {

  # Check whether all items are indeed included in vars
  if (any(!(old_names %in% vars$item))) {
    stop("Item/s ", list_elements(old_names[!(old_names %in% vars$item)]),
         " is/are not included in vars! Please check again.")
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{stop}: \code{resp} is not a data.frame}{Triggered when
#'     \code{resp} is not a data.frame. Effect: the function halts.}
#'   \item{\code{stop}: \code{poly_items} is invalid}{Triggered when
#'     \code{poly_items} is not a non-empty named list of character vectors.
#'     Effect: the function halts; no scoring is performed.}
#'   \item{\code{stop}: logical parameter is invalid}{Triggered when
#'     \code{impute}, \code{warn}, \code{save}, \code{overwrite}, or
#'     \code{verbose} is not a single \code{TRUE}/\code{FALSE} value.
#'     Effect: the function halts.}
#'   \item{\code{stop}: \code{threshold} is out of range}{Triggered when
#'     \code{threshold} is not a single numeric value in the interval
#'     \[0, 1\]. Effect: the function halts; no scoring is performed.}
#'   \item{\code{stop}: \code{mvs} is invalid}{Triggered when \code{mvs}
#'     is provided but is not a numeric vector. Effect: the function halts.}
#'   \item{\code{stop}: \code{missing_by_design} is invalid}{Triggered when
#'     \code{missing_by_design} is not a single numeric value. Effect: the
#'     function halts.}
#'   \item{\code{stop}: string parameter is invalid}{Triggered when
#'     \code{path_results}, \code{path_table}, or \code{select} is not a
#'     single character string (or NULL for \code{select}). Effect: the
#'     function halts.}
#'   \item{\code{stop}: imputation prerequisites missing}{Triggered when
#'     \code{impute = TRUE} but \code{vars} is not a data.frame or
#'     \code{select} is \code{NULL}. Effect: the function halts with an
#'     actionable message suggesting to provide the argument or set
#'     \code{impute = FALSE}.}
#'   \item{\code{warning}: polytomous item naming convention}{Triggered when
#'     a polytomous item name in \code{poly_items} does not contain the
#'     expected subitem marker (e.g., \code{"s_c"} or \code{"s_sc3g9_c"}).
#'     Effect: scoring proceeds but downstream functions that rely on the
#'     naming convention (e.g., label lookup) may silently fail or produce
#'     unexpected results.}
#'   \item{\code{warning}: no \code{mvs} provided}{Triggered when
#'     \code{mvs = NULL}. Effect: the default \code{c(-99:-1)} is used;
#'     any user-defined missing values outside this range will not be
#'     converted to \code{NA}.}
#'   \item{\code{message}: verbose imputation info}{Triggered when
#'     \code{impute = TRUE} and \code{verbose = TRUE}. Effect: informational
#'     message printed to the console; no impact on results.}
#' }
#' @export
pc_scoring <- function(resp, poly_items, vars = NULL, select = NULL,
                       mvs = NULL, warn = TRUE,
                       missing_by_design = -54,
                       impute = TRUE, threshold = .50,
                       path_results = "Results",  path_table = "Tables",
                       save = TRUE, overwrite = TRUE, verbose = TRUE) {

  # --- Input validation ---

  # resp must be a data.frame
  if (!is.data.frame(resp)) {
    stop("The argument 'resp' must be a data.frame. Got '",
         class(resp)[1], "'. Please check your input.")
  }

  # poly_items must be a non-empty named list of character vectors
  if (!is.list(poly_items)) {
    stop("The argument 'poly_items' must be a list. Got '",
         class(poly_items)[1], "'. Please check your input.")
  }
  if (length(poly_items) == 0L) {
    stop("The argument 'poly_items' must have at least one element. ",
         "Please check your input.")
  }
  if (is.null(names(poly_items)) || any(names(poly_items) == "")) {
    stop("All elements of 'poly_items' must be named. ",
         "The names are used as column names for the scored items. ",
         "Please check your input.")
  }
  bad_elements <- names(which(!vapply(poly_items, is.character, logical(1))))
  if (length(bad_elements) > 0L) {
    stop("Each element of 'poly_items' must be a character vector of subitem ",
         "names. Element(s) ", fmt_names(bad_elements),
         " are not character vectors. Please check your input.")
  }

  # Scalar logical parameters
  logical_params <- list(impute = impute, warn = warn, save = save,
                         overwrite = overwrite, verbose = verbose)
  for (param_name in names(logical_params)) {
    if (!is.logical(logical_params[[param_name]]) ||
        length(logical_params[[param_name]]) != 1L ||
        is.na(logical_params[[param_name]])) {
      stop("The argument '", param_name, "' must be TRUE or FALSE ",
           "(single logical value). Please check your input.")
    }
  }

  # threshold must be a single numeric in [0, 1]
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      threshold < 0 || threshold > 1) {
    stop("The argument 'threshold' must be a single numeric value in the ",
         "interval [0, 1]. Please check your input.")
  }

  # mvs must be a numeric vector when provided
  if (!is.null(mvs) && (!is.numeric(mvs) || length(mvs) == 0L)) {
    stop("The argument 'mvs' must be a numeric vector of missing value codes ",
         "(or NULL to use the default). Got '", class(mvs)[1],
         "'. Please check your input.")
  }

  # missing_by_design must be a single numeric value
  if (!is.numeric(missing_by_design) || length(missing_by_design) != 1L ||
      is.na(missing_by_design)) {
    stop("The argument 'missing_by_design' must be a single numeric value. ",
         "Please check your input.")
  }

  # String parameters
  if (!is.character(path_results) || length(path_results) != 1L) {
    stop("The argument 'path_results' must be a single character string. ",
         "Please check your input.")
  }
  if (!is.character(path_table) || length(path_table) != 1L) {
    stop("The argument 'path_table' must be a single character string. ",
         "Please check your input.")
  }
  if (!is.null(select) && (!is.character(select) || length(select) != 1L)) {
    stop("The argument 'select' must be a single character string (or NULL). ",
         "Please check your input.")
  }

  # Early check: imputation prerequisites
  if (impute) {
    if (is.null(vars) || !is.data.frame(vars)) {
      stop("When 'impute = TRUE', the argument 'vars' must be a data.frame ",
           "containing information on the competence items. ",
           "Please provide 'vars' or set 'impute = FALSE'.")
    }
    if (is.null(select)) {
      stop("When 'impute = TRUE', the argument 'select' must specify the name ",
           "of a logical variable in 'vars' that indicates the scored ",
           "dichotomous items. Please provide 'select' or set 'impute = FALSE'.")
    }
  }

  # Check whether variables are indeed contained in data.frames
  check_numerics(resp, "resp", unlist(poly_items), dich = TRUE)

  # Check pc_item (should be marked with 's_c' or 's_[startingCohortTargetGroup]_c')
  if (warn) {
    for ( pc_name in names(poly_items) ) {
      is_pc_named_correctly <- grepl("s(_[a-zA-Z0-9]+)*_c$", pc_name)
      if ( !is_pc_named_correctly ) {
        warning( pc_name, ": Variable name should contain a subitem marker like 's', e.g. '[item]s_c', '[item]s_sc3g9_c'.\n" )
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
    indicators <- pc_missing_subitems(
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
    resp_full <- pc_imputation(
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
#' persons as rows; y in \{0, 1\} for binary data; additionally includes ID_t
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{stop}: recoding failure}{Triggered when recoding subitems to
#'     binary missing-value indicators produces values outside \{0, 1\} — an
#'     internal consistency check. Effect: the function halts; no indicator
#'     data.frame is returned.}
#'   \item{\code{stop}: \code{_sumMV} count mismatch}{Triggered when the
#'     number of \code{_sumMV} summary columns created does not match the
#'     number of polytomous items — an internal consistency check. Effect:
#'     the function halts; no indicator data.frame is returned.}
#'   \item{\code{message}: missing-value imputation summary}{Always printed
#'     (unless output is suppressed). Two tables are shown: absolute / relative
#'     frequencies of imputed missing values per item, and distribution of
#'     imputed items across persons. Effect: informational only; no impact on
#'     results.}
#' }
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
    stop( "Recoding of subitems into indicator variables failed. ",
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
  desc_items_impMV <- describe(summary_items_impMV["RelFreq_of_imputed_MV"])
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
    save_results(
      results,
      "pc_subitems_mv_indicators.rds",
      path_results
    )

    results <- results[4:6]
    save_table(
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
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{stop}: \code{indicators} is not a data.frame}{Triggered when
#'     \code{indicators} is \code{NULL} or not a data.frame — this object
#'     should be created automatically by \code{pc_scoring()} with
#'     \code{impute = TRUE}. Effect: the function halts; no imputed response
#'     data.frame is returned.}
#'   \item{\code{stop}: respondent count mismatch between \code{resp} and
#'     \code{indicators}}{Triggered when the person IDs in \code{resp} and
#'     \code{indicators} do not fully overlap. Effect: the function halts to
#'     prevent a corrupted merge that would produce wrong error-rate estimates
#'     and imputed values.}
#'   \item{\code{stop}: \code{vars} is not a data.frame}{Triggered when
#'     \code{vars} is \code{NULL} or not a data.frame. Effect: the function
#'     halts; no imputed data.frame is returned.}
#'   \item{\code{stop}: \code{select} is \code{NULL}}{Triggered when no
#'     selection variable is provided. Effect: the function halts; no imputed
#'     data.frame is returned.}
#'   \item{\code{stop}: subitems not in \code{indicators}}{Triggered when the
#'     subitems defined in \code{poly_items} are missing from the
#'     \code{indicators} data.frame. Effect: the function halts to prevent
#'     silent imputation of wrong items.}
#'   \item{\code{stop}: subitems not in selected item set}{Triggered when the
#'     subitems defined in \code{poly_items} are not included in the item set
#'     selected by \code{select}. Effect: the function halts to prevent
#'     imputation based on a mismatched item set.}
#'   \item{\code{stop}: person ID mismatch between \code{resp} and IRT
#'     predictions}{Triggered when the set of valid person IDs in \code{resp}
#'     does not match the person IDs in the IRT-predicted responses. Effect:
#'     the function halts to prevent wrong imputed values being assigned to
#'     the wrong persons.}
#' }
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
          "data.frames should have been generated automatically ",
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
  resp_ <- convert_mv(resp, vars = vars, select = select,
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
    stop( "ID_ts in original data.frame and in data.frame with ",
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
    save_results(
      pc_subitems_imputation,
      "pc_subitems_imputations.rds",
      path_results
    )
  }

  return( resp_imp )

}



#' Score highlighting items
#'
#' @param resp  data.frame; contains original item responses
#' @param hl_solutions  list; contains character vector with subitems for each
#' highlighting item with correct solutions, name of the vector is the name of the highlighting item (e.g.
#' hl_solutions = list(hl1 = c("subitem1", "subitem2"), hl2 = c("subitem1", "subitem2"))),
#' name of the vector should match the vector names in @hl_distractors
#' @param hl_distractors  list; contains character vector with subitems for each
#' highlighting item with distractors, name of the vector is the name of the highlighting item (e.g.
#' hl_distractors = list(hl1 = c("subitem1", "subitem2"), hl2 = c("subitem1", "subitem2"))),
#' name of the vector should match the vector names in @hl_solutions
#' @param mvs  integer vector; contains user-defined missing values
#' @param warn  logical; print warnings
#' @param verbose logical; print messages
#'
#' @return resp including unscored (raw) and scored items
#' @export
hl_scoring <- function(resp, hl_solutions, hl_distractors,
                       mvs = NULL, warn = TRUE, verbose = TRUE) {
  if (!is.list(hl_solutions)) {
    stop("The argument 'hl_solutions' must be a list. Please check your input.")
  }
  if (!is.list(hl_distractors)) {
    stop("The argument 'hl_distractors' must be a list. Please check your input.")
  }
  names_diff <- setdiff(names(hl_solutions), names(hl_distractors))
  if (length(names_diff) != 0L) {
    stop(paste0("The arguments 'hl_solutions' and 'hl_distractors' ",
                "must include the same names for their elements. ",
                "Please check your input. Problems found for ",
                list_elements(names_diff), "."))
  }
  check_numerics(resp, "resp", unlist(hl_solutions), dich = TRUE)
  check_numerics(resp, "resp", unlist(hl_distractors), dich = TRUE)
  if (warn) {
    for (hl_name in names(hl_solutions)) {
      is_hl_named_correctly <- grepl("s(_[a-zA-Z0-9]+)*_c$", hl_name)
      if (!is_hl_named_correctly) {
        message(hl_name, ": Variable name should contain a subitem marker like 's', e.g. '[item]s_c', '[item]s_sc3g9_c'.\n")
      }
    }
  }
  if (is.null(mvs)) {
    mvs <- c(-99:-1)
    if (isTRUE(warn))
      warning("No missing values provided. c(-99:-1) used as default.")
  }

  # for each highlighting item
  for (item in names(hl_solutions)) {

    # Sensitivity index A
    rp <- rowSums(resp[, hl_solutions[[item]], drop = FALSE])
    fp <- rowSums(resp[, hl_distractors[[item]], drop = FALSE])
    fn <- length(hl_solutions[[item]]) - rp
    rn <- length(hl_distractors[[item]]) - fp
    rpr <- rp / (rp + fn)
    fpr <- fp / (fp + rn)
    denom <- (4 * pmax(rpr, fpr) - 4 * rpr * fpr)
    hl_item <-
      0.5 + sign(rpr - fpr) * ((rpr - fpr)^2 + abs(rpr - fpr)) / denom
    hl_item <-
      cut(hl_item, c(-1, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2),
          labels = FALSE, right = FALSE) - 1
    hl_item[denom %in% 0] <- 0

    # Set missing values
    subitems <- c(hl_solutions[[item]], hl_distractors[[item]])
    number_missing <- rowSums(resp[, subitems] < 0)
    any_missing <- number_missing > 0
    hl_item[any_missing] <- -55
    for (mv in mvs) {
      all_this_missing_type <-
        (rowSums(resp[, subitems] == mv) == number_missing) & any_missing
      hl_item[all_this_missing_type] <- mv
    }

    # Set item score
    resp[[item]] <- hl_item

  }

  return(resp)

}



#' Collapse response categories with N < 200
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select character; indicates the logical variable in vars which
#'   contains the item names of the polytomous items
#' @param per_cat integer; minimum number of persons per category; defaults to 200
#' @param rules data.frame; collapsing rules for items;
#'    same format as the excel file saved via \code{save}; must include three
#'    variables (original_item, scoring, collapsed_item)
#' @param rules_file character; path to excel file with collapsing rules created
#'    via \code{save}
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param filename  string; file name for saved tables
#' @param save  logical; whether results shall be saved to hard drive
#' @return resp with collapsed categories. Note that this function changes the
#'   given PC items IN PLACE. If you want to keep the original data, please
#'   copy and rename the items to be collapsed first.
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{message}: problematic items}{Triggered when one or more
#'     polytomous items cannot be collapsed because fewer than two response
#'     categories have sufficient observations (>= \code{per_cat}). Effect:
#'     the affected items are omitted from collapsing; they appear in the
#'     printed list and should be reviewed manually.}
#'   \item{\code{message}: dichotomous items skipped}{Triggered when the
#'     selected item set contains items with only two response categories.
#'     Effect: these items are silently left unchanged; the message lists
#'     the skipped items.}
#'   \item{\code{message}: items collapsed}{Always printed when at least one
#'     item was successfully collapsed. Lists each item and its new scoring
#'     scheme. Effect: informational only.}
#'   \item{\code{message}: no items collapsed}{Printed when no items met the
#'     collapsing criterion. Effect: informational only; \code{resp} is
#'     returned unchanged.}
#' }
#' @export
#' @return data.frame resp with collapsed and original items

collapse_response_categories <- function(resp, vars, select = 'poly',
                                         per_cat = 200,
                                         rules = NULL,
                                         rules_file = NULL,
                                         save = FALSE,
                                         path_table = "Tables",
                                         filename = "collapsed_items") {

  if (!is.null(rules) | !is.null(rules_file)) {

    # Use rules
    results <- collapse_response_categories_with_rules(
      resp = resp, vars = vars, select = select, per_cat = per_cat,
      rules = rules, rules_file = rules_file
    )

  } else {

    # Create rules based on data
    results <- collapse_response_categories_without_rules(
      resp = resp, vars = vars, select = select, per_cat = per_cat
    )

  }

  # Save results
  if (save) {

    save_table(
      results = list(
        collapsed = results$collapsed_items,
        dichotomous = results$dichotomous_items,
        problematic = results$problematic_items
      ),
      filename = paste0(filename, ".xlsx"),
      path = path_table,
      overwrite = TRUE,
      show_rownames = FALSE
    )
  }

  return(results$resp)
}



#' Collapse response categories without existing collapsing rules
#'
#' @inheritParams collapse_response_categories

collapse_response_categories_without_rules <-
  function(resp, vars, select = 'poly', per_cat = 200) {

    # Check whether variables are indeed contained in data.frames
    check_logicals(vars, "vars", select, warn = TRUE)
    polyt_items <- vars$item[vars[[select]]]
    check_numerics(resp, "resp", polyt_items)
    check_items(polyt_items)

    collapsed_items <- matrix(NA, 0, 2)
    dichotomous_items <- c()
    problematic_items <- c()

    for (item in polyt_items) {

      response <- resp[[item]]

      # Create table with all possible categories (= from minimum to maximum value)
      vals <- unique(response[response >= 0 & !is.na(response)])
      values <- 0:max(vals)
      tab <- sapply(values, \(x) sum(response == x, na.rm = TRUE))
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
            tab <- sapply(values, \(x) sum(response == x, na.rm = TRUE))
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
    item_names <- data.frame(
      original_item = collapsed_items[, 1],
      scoring = collapsed_items[, 2],
      collapsed_item = paste0(collapsed_items[, 1], "_collapsed")
    )

    # Print results
    if (!is.null(problematic_items)) {
      message("\nThe following items resulted in less than two response categories ",
              "with more than ", per_cat, " cases and were thus not collapsed. ",
              "Please check these items manually:\n",
              list_elements(problematic_items))
    }

    if (!is.null(dichotomous_items)) {
      message("\nDichotomous items were not considered for collapsing. ",
              "The following items have less than three response categories::\n",
              list_elements(dichotomous_items))
    }

    if (nrow(collapsed_items) > 0L) {
      message("\nThe following items have been collapsed:\n")
      print(format(item_names, justify = "left"))
    } else {
      message("\nNo items have been collapsed.")
    }

    out <- list(
      resp = resp,
      dichotomous_items = dichotomous_items,
      collapsed_items = item_names,
      problematic_items = problematic_items
    )

    return(out)
  }


#' Collapse response categories with existing collapsing rules
#'
#' @inheritParams collapse_response_categories

collapse_response_categories_with_rules <-
  function(resp, vars, select = 'poly', per_cat = 200,
           rules = NULL, rules_file = NULL) {

    # Check whether variables are indeed contained in data.frames
    check_logicals(vars, "vars", select, warn = TRUE)
    polyt_items <- vars$item[vars[[select]]]
    check_numerics(resp, "resp", polyt_items)
    check_items(polyt_items)

    # Check supplied rules
    if (!is.null(rules)) {
      if (!("data.frame" %in% class(rules))) {
        stop("Argument 'rules' is not a data.frame.")
      }
      check_variables(rules, name_df = "rules",
                      variables = c("original_item", "scoring",
                                    "collapsed_item"))
    }

    # Import rules
    if (!is.null(rules_file)) {
      if (!is.character(rules_file) ||
          !length(rules_file) == 1L ||
          !file.exists(rules_file) ||
          !grepl("\\.(xlsx|xls)$", rules_file, ignore.case = TRUE))
        stop(sprintf("The path '%s' supplied for argument 'rules_file' is not a valid excel file.",
                     rules_file))
      rules2 <- openxlsx::read.xlsx(rules_file, sheet = 1)
      check_variables(rules2, name_df = "rules_file",
                      variables = c("original_item", "scoring", "collapsed_item"))
      if (is.null(rules)) {
        rules <- rules2
      } else {
        rules <- rbind(rules, rules2)
      }
    }

    # Check number of rules for items
    n_rules <- table(rules$original_item)
    duplicates <- names(n_rules)[n_rules > 1]
    if (length(duplicates) > 0)
      stop(paste(
        "Multiple rules found for items ",
        list_elements(duplicates)
      ))

    # Remove rules for non-existent items
    rm_rules <- c()
    for (i in seq_len(nrow(rules))) {
      if (is.null(resp[[rules$original_item[i]]]))
        rm_rules <- c(rm_rules, i)
    }
    if (length(rm_rules) > 0L)
      rules <- rules[(!seq_len(nrow(rules)) %in% rm_rules), ]

    # Apply rules to data
    for (i in seq_len(nrow(rules))) {
      rec_string <- trimws(strsplit(rules$scoring[i], ",")[[1]])
      resp[[paste0(rules$original_item[i], "_collapsed")]] <-
        recodeVar(
          resp[[rules$original_item[i]]],
          src = regmatches(rec_string, regexpr("^([0-9]+)", rec_string)),
          tgt = as.numeric(regmatches(rec_string, regexpr("([0-9]+)$", rec_string)))
        )
    }

    # Identify problematic items
    is_problematic <- apply(resp[, rules$collapsed_item], 2,
                            \(x) min(table(x[x >= 0])) < per_cat)
    if (sum(is_problematic) > 0L) {
      problematic_items <- rules$collapsed_item[is_problematic]
    } else {
      problematic_items <- NULL
    }

    # Print results
    message("\nThe following items have been collapsed:\n")
    print(rules)

    if (!is.null(problematic_items)) {
      message("\nThe following items resulted in response categories ",
              "with less than ", per_cat, " cases. ",
              "Please check these items carefully:\n",
              list_elements(problematic_items))
    }

    out <- list(
      resp = resp,
      dichotomous_items = NULL,
      collapsed_items = rules,
      problematic_items = problematic_items
    )
    return(out)
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{warning}: invalid \code{min.val}}{Triggered when
#'     \code{min.val} is \code{NULL} or negative. Effect: the default of
#'     3 valid responses per person is used; cases with fewer than 3 valid
#'     responses are marked as invalid.}
#' }
#' @export
min_val <- function(resp, vars, select, min.val = NULL, invalid = NULL) {

    # Check whether variables are indeed contained in data.frames
    check_logicals(vars, "vars", select)
    items <- vars$item[vars[[select]]]
    check_numerics(resp, "resp", items)
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
#' @export
pos_new <- function(vars, select, position) {

    # Check whether variables are indeed contained in data.frames
    check_logicals(vars, "vars", select)
    check_numerics(vars, "vars", position, check_invalid = TRUE)

    if (length(position) == 1) {

        vars_ <- vars[vars[[select]], ]
        pos <- data.frame(item = vars_[['item']],
                          position = vars_[[position]])
        pos <- dplyr::arrange(pos, position)
        pos[[paste0("position_", select)]] <- seq(1, nrow(pos))
        vars <- merge(vars, pos[ , c('item', paste0("position_", select))],
                      by = 'item', all = TRUE)

    } else {

        for (g in names(position)) {
            vars_ <- vars[vars[[select]] & !is.na(vars[[position[g]]]), ]
            pos <- data.frame(item = vars_[['item']],
                              position = vars_[[position[g]]])
            pos <- dplyr::arrange(pos, position)
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
#'
#' @section Notifications:
#' \describe{
#'   \item{\code{warning}: missing birth year replaced}{Triggered when
#'     \code{birth_year} contains \code{NA}s. Effect: the missing values are
#'     silently replaced by the sample median of \code{birth_year}; the
#'     number of replacements is reported. Age values for affected persons are
#'     approximate.}
#'   \item{\code{warning}: missing birth month replaced}{Triggered when
#'     \code{birth_month} contains \code{NA}s. Effect: the missing values are
#'     silently replaced by the sample median of \code{birth_month}; the
#'     number of replacements is reported. Age values for affected persons are
#'     approximate.}
#'   \item{\code{warning}: missing test year replaced}{Triggered when
#'     \code{test_year} contains \code{NA}s. Effect: the missing values are
#'     silently replaced by the sample median of \code{test_year}; the number
#'     of replacements is reported. Age values for affected persons are
#'     approximate.}
#'   \item{\code{warning}: missing test month replaced}{Triggered when
#'     \code{test_month} contains \code{NA}s. Effect: the missing values are
#'     silently replaced by the sample median of \code{test_month}; the
#'     number of replacements is reported. Age values for affected persons are
#'     approximate.}
#' }
#' @export
calculate_age <- function(resp,
                          birth_year = "birthy", birth_month = "birthm",
                          test_year = "testy", test_month = "testm",
                          birth_day = NULL, test_day = NULL) {

    # Check and create birth date variables
    check_variables(resp, "resp", c(birth_year, birth_month)    )
    byear <- resp[[birth_year]]
    bmonth <- resp[[birth_month]]

    # Check and create test date variables
    if (is.numeric(test_year)) {
        tyear <- test_year
    } else {
        check_variables(resp, "resp", test_year)
        tyear <- resp[[test_year]]
    }

    if (is.numeric(test_month)) {
        tmonth <- test_month
    } else {
        check_variables(resp, "resp", test_month)
        tmonth <- resp[[test_month]]
    }

    # Check whether birth and test day exist and if not, replace with default 15
    if (is.null(birth_day)) {
        bday <- 15
    } else {
        check_variables(resp, "resp", birth_day)
        bday <- resp[[birth_day]]
    }

    if (is.null(test_day)) {
        tday <- 15
    } else if (is.numeric(test_day)) {
        tday <- test_day
    } else {
        check_variables(resp, "resp", test_day)
        tday <- resp[[test_day]]
    }

    # Replace missing values in birth and test date with the sample median
    na_by <- is.na(byear)
    na_bm <- is.na(bmonth)
    na_ty <- is.na(tyear)
    na_tm <- is.na(tmonth)

    if (sum(na_by) > 0) {
        warning(sum(na_by), " missing value(s) in birth year were replaced by the sample median.")
        byear[na_by] <- round(median(byear, na.rm = TRUE))
    }

    if (sum(na_bm) > 0) {
        warning(sum(na_bm), " missing value(s) in birth month were replaced by the sample median.")
        bmonth[na_bm] <- round(median(bmonth, na.rm = TRUE))
    }

    if (sum(na_ty) > 0) {
        warning(sum(na_ty), " missing value(s) in test year were replaced by the sample median.")
        tyear[na_ty] <- round(median(tyear, na.rm = TRUE))
    }

    if (sum(na_tm) > 0) {
        warning(sum(na_tm), " missing value(s) in test month were replaced by the sample median.")
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
  check_logicals(vars, "vars", select_suf, warn = TRUE)

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
