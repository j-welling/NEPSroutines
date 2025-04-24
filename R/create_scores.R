#' Create scores
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param score_name character; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param xsi_fixed named numerical vector; contains fixed item difficulties as
#'   elements and item names as names of elements
#' @param rotation character vector; contains the variable name indicating the
#'   test rotation
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param missing_by_design  numeric; user defined missing value for missing by
#' design (is necessary for calculating N_administered)
#' @param wle logical; whether to estimate WLEs
#' @param imp_poly_items logical; whether to impute omitted missing values for
#' subitems of a polytomous item, when less than 50% of the subitems of a
#' polytomous item contain missing values
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param per_cat integer; minimum number of persons per category of a scored
#' polytomous item; defaults to 200
#' @param sum_score logical; whether to calculate sum scores
#' @param sum_select string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#'   (if identical to select, argument can be empty)
#' @param num_cat string; defines name of numeric variable in vars that indicates
#'  the maximum number of response options for the items
#' @param metap logical; whether to calculate metacognition score
#' @param meta_select string; defines name of logical variable in vars that
#'   indicates which items to use for the meta cognition analysis
#'   (if identical to select, argument can be empty)
#' @param meta_variable  string; defines name of meta competence variable in resp
#' @param meta_score_name string; name of the meta competence scores -- WITHOUT
#'   extension (e.g., reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param control_tam list; control argument as passed to tam.mml.mfr()
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights numeric vector; person weights for current measurement point
#'   passed to TAM-functions
#' @param poly2dich  logical; indicates whether count only correctly scored
#'   binary or FULLY correctly scored PC items
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param path_results  string; defines path to folder where results shall be
#'   saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param warn  logical; whether to print warnings
# #' @param resp_prev data.frame with responses of first measurement wave,
# #'    a person identifier; may also include a WLE as given in wid
# #' @param resp_link data.frame with responses of link sample,
# #'    a person identifier;
# #' @param vars_prev data.frame; contains information about items of previous
# #' measurement wave with items as rows; includes variable 'item' containing item
# #' names and may include variable with item scoring
# #' @param vars_link data.frame; contains information about items of link study
# #' with items as rows; includes variable 'item' containing item names and may
# #' include variable with item scoring; required if anchor group design is used
# #' @param select_prev character; contains name of logical variable in vars_prev
# #' identifying the item set for the previous measurement wave
# #' @param select_link character; contains name of logical variable in vars_link
# #' identifying the item set for the link study; required if anchor group design
# #' is used
# #' @param valid_prev string; defines name of logical variable in resp_prev that
# #' indicates (in)valid cases for the previous measurement wave
# #' @param valid_link string; defines name of logical variable in resp_link that
# #' indicates (in)valid cases for the link study; optional variable for anchor
# #' group design
# #' @param pweights_prev numeric vector; person weights for previous
# #'   measurement point passed to TAM-functions
# #' @param pweights_link numeric vector; person weights for link study
# #'   passed to TAM-functions
# #' @param scoring_prev  numeric; named vector with the scoring factors to
# #'   be applied to the loading matrix of the previous measurement point; can be
# #'   NULL for the Rasch model
# #' @param scoring_link  numeric; named vector with the scoring factors to
# #'   be applied to the loading matrix of the link study; can be NULL for the
# #'   Rasch model
# #' @param anchors  character; data.frame with two columns including the link
# #'   items; for anchor item designs the first column refers to the previous
# #'   measurement time point and the second column to the current measurement
# #'   time point; for anchor group designs the first column refers to the main
# #'   sample (for both time points) and the second column refers to the link
# #'   sample; if NULL, all common items are used
# #' @param longitudinal  logical; do within cohort linking (TRUE) or between
# #'   cohort linking (FALSE)
# #' @param print  logical; whether results shall be printed to console
# #' @param do_dim logical; whether to do dimensionality analysis for linking
# #' @param do_dif logical; whether to do dif analysis for linking
# #' @param dif_threshold numeric; threshold under which DIF in common link items
# #'   is accepted; defaults to .5
# #' @param wid variable name used as WLE identifier in first measurement wave
# #' @param snodes snodes as passed to the TAM function for the dimensionality
# #'   analyses
# #' @param maxiter maximum number of iterations as passed to the TAM function
# #'   for the dimensionality analyses
# #' @param digits  numeric; number of decimal places for rounding
# #' @param verbose  logical; verbose as passed to the TAM function
#'
#' @export
create_scores <- function(
    resp,
    vars,
    select,
    scoring = NULL,
    score_name = 'score',
    num_cat = 'num_cat',
    xsi_fixed = NULL,
    rotation = NULL,
    valid = NULL,
    mvs = NULL,
    missing_by_design = -54,
    wle = TRUE,
    imp_poly_items = TRUE,
    poly_items = NULL,
    per_cat = 200,
    sum_score = FALSE,
    sum_select = NULL,
    metap = FALSE,
    meta_variable = NULL,
    meta_score_name = NULL,
    meta_select = NULL,
    control_tam = NULL,
    control_wle = NULL,
    pweights = NULL,
    poly2dich = TRUE,
    save = TRUE,
    return = FALSE,
    name_group = NULL,
    overwrite = FALSE,
    path_results = "Results",
    path_table = "Tables",
    warn = TRUE
    ### The following function arguments were commented out
    ### because linking is not yet implemented
    # print = TRUE,
    # resp_prev = NULL,
    # resp_link = NULL,
    # vars_prev = NULL,
    # vars_link = NULL,
    # select_prev = NULL,
    # select_link = NULL,
    # valid_prev = NULL,
    # valid_link = NULL,
    # scoring_prev = NULL,
    # scoring_link = NULL,
    # pweights_prev = NULL,
    # pweights_link = NULL,
    # anchors = NULL,
    # longitudinal = TRUE,
    # do_dim = TRUE,
    # do_dif = TRUE,
    # dif_threshold = .5,
    # wid = NULL,
    # snodes = 5000,
    # maxiter = 10000,
    # digits = 3,
    # verbose = TRUE,
) {


  if (!is.null(scoring))
    NEPSroutines:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)

  if (sum_score | metap)
    NEPSroutines:::check_variables(vars, "vars", num_cat)

  if (warn) NEPSroutines:::is_null_mvs_valid(mvs = mvs, valid = valid)

  ################################## Linking ##################################

  # # Estimate linked WLEs and SEs
  # if (wle & !is.null(resp_prev)) {
  #   linked_scores <-
  #     NEPSroutines:::linking(
  #       resp_curr = resp,
  #       resp_prev = resp_prev,
  #       resp_link = resp_link,
  #       vars_curr = vars,
  #       vars_prev = vars_prev,
  #       vars_link = vars_link,
  #       select_curr = select,
  #       select_prev = select_prev,
  #       select_link = select_link,
  #       valid_curr = valid,
  #       valid_prev = valid_prev,
  #       valid_link = valid_link,
  #       scoring_curr = scoring,
  #       scoring_prev = scoring_prev,
  #       scoring_link = scoring_link,
  #       anchors = anchors,
  #       mvs = mvs,
  #       longitudinal = longitudinal,
  #       overwrite = overwrite,
  #       verbose = verbose,
  #       path_table = path_table,
  #       path_results = path_results,
  #       return = return, print = print, save = save,
  #       dif_threshold = dif_threshold,
  #       wid = wid, snodes = snodes, maxiter = maxiter,
  #       digits = digits,
  #       pweights_curr = pweights,
  #       pweights_prev = pweights_prev,
  #       pweights_link = pweights_link,
  #       control_tam = control_tam,
  #       do_dim = do_dim, do_dif = do_dif,
  #       warn = warn
  #     )
  #   wles_linked <- linked_scores$link_results$wle_linked
  #   names(wles_linked) <- c("ID_t", paste0(score_name, c("_sc1u", "_sc2u")))
  # } else {
  #   wles_linked <- NULL
  #   linked_scores <- NULL
  # }

  #############################################################################

  # Estimate (unrotated) WLEs and SEs
  if (wle) {

    # Test data
    NEPSroutines:::check_logicals(vars, "vars", select, warn = warn)
    NEPSroutines:::check_logicals(resp, "resp", valid, warn = warn)
    NEPSroutines:::check_items(vars$item[vars[[select]]])
    NEPSroutines:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    NEPSroutines:::check_pid(resp$ID_t)

    # Whether the estimation of wles should be executed after imputation of subitems
    if ( imp_poly_items ) {
      message( "\nBy default, WLE estimation includes subitem imputation for polytomous items.
      To ensure smooth processing, specify 'poly_items' as a list of polytomous items
      and their subitems in the 'create_score()' function.
      Category collapsing is then applied to the dataset with imputed subitems ('resp_imp')
      if the ≤ 50% missingness criterion is met. Categories with fewer than 200 cases
      are collapsed by default—adjustable via the 'per_cat' argument.
      Note that the original dataset ('resp') remains unchanged.
      \nIf you wish to skip subitem imputation, set 'imp_poly_items = FALSE'." )

      if ( !is.null(poly_items) ) {
        # Create indicators for OM values for subitems
        indicators <- NEPSroutines:::missings_subitems(
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          mvs = mvs,
          poly_items = poly_items,
          path_results = path_results,
          save = save
        )

        # Create 'resp_imp' containing imputed subitems if criterion met
        resp_imp <- NEPSroutines:::imputation_OM_subitems(
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          mvs = mvs,
          poly_items = poly_items,
          per_cat = per_cat,
          indicators = indicators,
          path_results = path_results,
          save = save
        )
        message( "\nDataset with imputed subitems has been successfully created.")

        # Estimation of wles based on 'resp_imp'
        if (is.null(rotation) | (!is.null(rotation) & is.null(xsi_fixed))) {
          fit <- NEPSroutines:::irt_analysis( # hier könnte man irt_model() anstatt irt_analysis() verwenden --> spart Berechnungszeit
            resp = resp_imp,
            vars = vars,
            select = select,
            valid = valid,
            mvs = mvs,
            missing_by_design = missing_by_design,
            scoring = scoring,
            xsi_fixed_1p = xsi_fixed,
            verbose = FALSE,
            warn = warn,
            return = TRUE,
            plots = FALSE,
            save = FALSE,
            print = FALSE,
            control_tam = control_tam,
            control_wle = control_wle,
            pweights = pweights,
            test = FALSE
          )
          if (is.null(fit$model.1pl)) {
            fit <- fit$model.pcm
          } else {
            fit <- fit$model.1pl
          }
          warn <- FALSE
        }

        if (!is.null(rotation)) {
          if (is.null(xsi_fixed)) {
            xsi_fixed <- fit$mod$xsi$xsi
            names(xsi_fixed) <- row.names(fit$mod$xsi)
          }
          mod_wles <- NEPSroutines:::estimate_rotated_wles(
            resp = resp_imp,
            vars = vars,
            select = select,
            valid = valid,
            rotation = rotation,
            mvs = mvs,
            scoring = scoring,
            xsi_fixed = xsi_fixed,
            wle_name = score_name,
            control_tam = control_tam,
            pweights = pweights
          )
          wles <- mod_wles[[2]]
        } else {
          wles <- as.data.frame(fit$wle[, c("pid", "theta", "error")])
          names(wles) <- c("ID_t", paste0(score_name, c("_sc1", "_sc2")))
        }

        # if (!is.null(wles_linked))                                                # commented out because linking is not yet implemented
        # wles <- merge(wles, wles_linked, by = "ID_t", all = TRUE)               # commented out because linking is not yet implemented

        message( "\nWLEs based on the dataset with imputed subitems have been successfully estimated." )

      } else { # when is.null(poly_items)
        # Check whether selected variables are poyltomous
        if ( NEPSroutines:::is_poly(resp, vars, select) ) {
          stop("\nNo list of pc-items with subitems (poly_items) was provided.
          To enable imputation before WLE estimation, please specify the poly_items argument.
          If WLE scores should be estimated based on the original dataset (without imputed subitems),
          please set 'imp_poly_items' to 'FALSE'.")
        }
      }
    } else { # when imp_poly_items = FALSE
      # Estimation of wles based on original dataset 'resp'
      if (is.null(rotation) | (!is.null(rotation) & is.null(xsi_fixed))) {
        fit <- NEPSroutines:::irt_analysis( # hier könnte man irt_model() anstatt irt_analysis() verwenden --> spart Berechnungszeit
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          mvs = mvs,
          missing_by_design = missing_by_design,
          scoring = scoring,
          xsi_fixed_1p = xsi_fixed,
          verbose = FALSE,
          warn = warn,
          return = TRUE,
          plots = FALSE,
          save = FALSE,
          print = FALSE,
          control_tam = control_tam,
          control_wle = control_wle,
          pweights = pweights,
          test = FALSE
        )
        if (is.null(fit$model.1pl)) {
          fit <- fit$model.pcm
        } else {
          fit <- fit$model.1pl
        }
        warn <- FALSE
      }

      if (!is.null(rotation)) {
        if (is.null(xsi_fixed)) {
          xsi_fixed <- fit$mod$xsi$xsi
          names(xsi_fixed) <- row.names(fit$mod$xsi)
        }
        mod_wles <- NEPSroutines:::estimate_rotated_wles(
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          rotation = rotation,
          mvs = mvs,
          scoring = scoring,
          xsi_fixed = xsi_fixed,
          wle_name = score_name,
          control_tam = control_tam,
          pweights = pweights
        )
        wles <- mod_wles[[2]]
      } else {
        wles <- as.data.frame(fit$wle[, c("pid", "theta", "error")])
        names(wles) <- c("ID_t", paste0(score_name, c("_sc1", "_sc2")))
      }

      # if (!is.null(wles_linked))                                                # commented out because linking is not yet implemented
      # wles <- merge(wles, wles_linked, by = "ID_t", all = TRUE)               # commented out because linking is not yet implemented

      message( "\nWLEs based on the original dataset (without imputed subitems) have been successfully estimated.")

    }
  }

  # Estimate sum scores
  if (sum_score) {

    # Select
    if (is.null(sum_select)) {
      sum_select <- select
      warning("\nNo variable 'sum_select' provided for sum scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    # Test data
    NEPSroutines:::check_logicals(vars, "vars", sum_select, warn = warn)
    NEPSroutines:::check_logicals(resp, "resp", valid, warn = warn)
    NEPSroutines:::check_items(vars$item[vars[[sum_select]]])
    NEPSroutines:::check_numerics(resp, "resp", vars$item[vars[[sum_select]]])
    NEPSroutines:::check_pid(resp$ID_t)

    sss <- NEPSroutines:::estimate_sum_scores(
      resp = resp,
      vars = vars,
      select = sum_select,
      num_cat = num_cat,
      valid = valid,
      mvs = mvs,
      scoring = scoring,
      score_name = score_name,
      poly2dich = poly2dich
    )
    if (wle) {
      wles <- merge(wles, sss, by = "ID_t", all = TRUE)
    } else {
      wles <- sss
    }
  }

  # Estimate metap scores
  if (metap) {

    # Test and prepare data
    if (is.null(meta_variable)) stop("No argument 'meta_variable' provided.")
    meta_score_name <- ifelse(is.null(meta_score_name), score_name, meta_score_name)

    if (is.null(meta_select)) {
      meta_select <- select
      warning("\nNo variable 'meta_select' provided for meta scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    NEPSroutines:::check_logicals(vars, "vars", meta_select, warn = warn)
    NEPSroutines:::check_logicals(resp, "resp", valid, warn = warn)
    NEPSroutines:::check_items(vars$item[vars[[meta_select]]])
    NEPSroutines:::check_numerics(resp, "resp", c(meta_variable, vars$item[vars[[meta_select]]]))
    NEPSroutines:::check_pid(resp$ID_t)

    metas <- NEPSroutines:::estimate_metap(
      resp = resp,
      vars = vars,
      select = meta_select,
      valid = valid,
      meta_variable = meta_variable,
      score_name = meta_score_name,
      num_cat = num_cat,
      mvs = mvs
    )
    if (wle | sum_score) {
      wles <- merge(wles, metas, by = "ID_t", all = TRUE)
    } else {
      wles <- metas
    }
  }

  # Create results object
  scores <- data.frame(wles) # !!delete this line when linking is implemeted!!
  # scores <- list(wle = wles, linking = linked_scores) # commented out because linking is not yet implemented

  # Create objects that obtain item parameters and TAM model used to estimate wles
  if (wle) {
    if (is.null(rotation)) {
        itemParamModel_wles <- fit$mod
        itemParam_wles <- fit$mod$xsi["xsi"]
    } else {
        itemParamModel_wles.position <- mod_wles[[1]]
        itemParam_wles.position <- itemParamModel_wles.position$xsi["xsi"]
      }
  }

  # Save results
  if (save) {
      name <- NEPSroutines:::create_name("scores", name_group, ".rds")
      NEPSroutines:::save_results(scores, filename = name, path = path_results)

      # Save item parameters and TAM model used to estimate wles
      if (wle) {
        if (is.null(rotation)) {

            name <- NEPSroutines:::create_name("itemParamModel_wles", name_group, ".rds")
            NEPSroutines:::save_results(
              itemParamModel_wles,
              filename = name,
              path = path_results
            )

            name <- NEPSroutines:::create_name("itemParam_wles", name_group, ".xlsx")
            NEPSroutines:::save_table(
              itemParam_wles,
              filename = name,
              path = path_table,
              overwrite = overwrite
            )

        } else {

            name <- NEPSroutines:::create_name("itemParamModel_wles.position", name_group, ".rds")
            NEPSroutines:::save_results(
              itemParamModel_wles.position,
              filename = name,
              path = path_results
            )

            name <- NEPSroutines:::create_name("itemParam_wles.position", name_group, ".xlsx")
            NEPSroutines:::save_table(
              itemParam_wles.position,
              filename = name,
              path = path_table,
              overwrite = overwrite
            )
        }
      }
    }


  # Return results
  if (return) return(scores)
}



#' Create indicators for subitems with OM (omitted missing) values
#' (criterion: ≤ 50% of subitems of a pc-item with OM values).
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
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param missing_by_design  numeric; user defined missing value for missing by
#' design (is necessary for calculating N_administered)
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param path_results  string; defines path to folder where results shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @noRd
missings_subitems <- function( resp, vars, select, valid,
                               mvs, missing_by_design = missing_by_design,
                               poly_items, path_results,
                               save ) {

  # Test data
  if ( !is.null(poly_items) ) {

    if ( !is.list(poly_items) ) {
      stop( "❌ 'poly_items' must be a list. Please check your input." )
    }
    NEPSroutines:::check_numerics(resp, "resp", unlist(poly_items), dich = TRUE)

    # Check pc_item and subitem names (should be marked with 's_c' and '[number]_c', respectively)
    for ( pc_name in names(poly_items) ) {
      is_pc_named_correctly <- grepl("s_c$", pc_name)
      subitems <- poly_items[[pc_name]]
      subitem_check <- grepl(paste0("^", sub("s_c$", "", pc_name), "_[0-9]+_c$"), subitems)

      if ( !is_pc_named_correctly ) {
        message( pc_name, ": ❌ pc-item name should end with 's_c'" )
      }

      if ( !all(subitem_check) ) {
        message( pc_name, ": ❌ One or more subitems are incorrectly named: ",
                 paste(subitems[!subitem_check], collapse = ", "),
                 ". Each subitem ends with _<number>_c." )
      }
    }
  }

  # Create indicators for OM values on subitems
  # Step 1. Identify subitems of poly_items included in the analysis
  pc_items <- vars$item[vars[[select]] == TRUE][grepl("s_c(_collapsed)?$", vars$item[vars[[select]] == TRUE])]
  subitems <- as.character(unlist(poly_items))[sapply(as.character(unlist(poly_items)), function(item) {
    any(startsWith(item, sub("s_c(_collapsed)?$", "", pc_items)))
  })]

  # Step 2. Recode responses and missing values in indicators (OM = 1)
  indicators <- resp[c("ID_t", subitems)]
  indicators[subitems] <- lapply(indicators[subitems], function(x) {
    ifelse(x == -97, 1,
           ifelse(x %in% -95:-21, NA,
                  0))
  })
  # test
  if ( sum(sapply(indicators[subitems], function(x) {
    all(range(x, na.rm = TRUE) == c(0, 1))
  })) != length(names(indicators)[-1]) ) {
    warning( "\nRecoding of subitems into indicator variables is incorrect,
              please estimate the wle values without imputation of missing values for subitems
              and contact package developers" )
  }

  # Step 3. Calculate total number of OM values (coded as 1) within each poly item
  sel <- unique(sub("_.*", "", subitems))
  for ( i in sel ) {
    indicators[[paste0(i, "_sumOM")]] <- rowSums(indicators[subitems[grepl(i, subitems)]] == 1, na.rm = TRUE)
  }
  #test
  if ( sum(grepl("_sumOM$", names(indicators))) != length(pc_items) ) {
    warning( "❌ Number of ‘_sumOM’ variables does not match the number of pc_items. Please contact package developer." )
  }
  rm(sel, i)

  # Step 4. Calculate relative frequencies of OM subitems
  num_subitems <- vars[vars$item %in% gsub("_collapsed", "", pc_items), c("item", "num_cat")]

  sel <- names(indicators)[grepl("_sumOM", names(indicators))]
  for( i in sel ) {
    stem <- sub("_.*", "", i)
    num_cat_value <- num_subitems$num_cat[grepl(stem, num_subitems$item)]
    indicators[[paste0(stem, "_relOM")]] <- ( indicators[[i]] / num_cat_value )
  }
  rm(sel, i, stem, num_cat_value)
  #test
  if ( sum(grepl("_relOM$", names(indicators))) != length(pc_items) ) {
    warning("❌ Number of ‘_relOM’ variables does not match the number of pc_items. Please contact package developer")
  }

  # Step 5. Check whether the relOM criterion (≤ 0.50), required for imputation, is met.
  sel <- names(indicators)[grepl("_relOM", names(indicators))]

  for ( i in sel ) {
    stem <- sub("_.*", "", i)
    indicators[[paste0(stem, "_indOM")]] <- ifelse(( indicators[[i]] > 0 & indicators[[i]] <= 0.5 ), 1, 0)
  }
  rm(sel, i, stem)
  #test
  if ( sum(grepl("_indOM$", names(indicators))) != length(pc_items) ) {
    warning( "❌ Number of ‘_indOM’ variables does not match the number of pc_items. Please contact package developer." )
  }



  # Save results
  if ( save ) {

    # Frequency tables
    tab_sumOM <- apply(indicators[, names(indicators)[grepl("_sumOM", names(indicators))]], 2, table, useNA = "always")
    tab_indOM <- apply(indicators[, names(indicators)[grepl("_indOM", names(indicators))]], 2, table, useNA = "always")

    results = list(indicators = indicators,
                   tab_sumOM = tab_sumOM,
                   tab_indOM = tab_indOM)

    NEPSroutines:::save_results(results, "subitems_indicators_OM.rds", "results")
  }

  return( results$indicators )
}


#' Subitem imputation
#' (criterion =< 50% of OM (omitted missing) values on subitems of a pc-item)
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
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param missing_by_design  numeric; user defined missing value for missing by
#' design (is necessary for calculating N_administered)
#' @param poly_items  list; contains character vector with subitems for each
#' polytomous item, name of the vector is the name of the polytomous item (e.g.
#' poly_items = list(poly1 = c("subitem1", "subitem2"), poly2 = c("subitem1", "subitem2")))
#' @param per_cat integer; minimum number of persons per category; defaults to 200
#' @param path_results  string; defines path to folder where results shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @noRd
imputation_OM_subitems <- function( resp, vars, select, valid,
                                    mvs, missing_by_design = missing_by_design,
                                    poly_items,
                                    per_cat,
                                    indicators,
                                    path_results,
                                    save ) {

  # Test
  if ( is.null(indicators) | !is.data.frame(indicators) ) {
    stop( "\nFor imputation of missing values on subitems, a data.frame with indicators is required.
             This data.frame should be generated automatically using the create_score function
             if poly_items are provided. Please contact package developers." )
  }

  if ( !is.numeric(per_cat) ) {
    stop( "\nPlease define 'per_cat' as an integer." )
  }


  # Prepare data (Select subitems and MC-items used in the analysis)
  items <- vars$item[vars[[select]] == TRUE]
  pc_items <- items[grepl("s_c(_collapsed)?$", items)]
  MC_items <- setdiff(items, pc_items)
  pc_subitems <- as.character(unlist(poly_items))[sapply(as.character(unlist(poly_items)), function(item) {
    stem <- sub("(_\\d+_c)$", "", item)
    stem %in% sub("s_c(_collapsed)?$", "", pc_items)
  })]
  rm(items)
  items <- c(MC_items, pc_subitems)
  rm(pc_items, MC_items)

  resp_ <- resp[, c("ID_t", valid, items)]
  vars_ <- vars[vars$item %in% items, ]


  # Fit Rasch model
  fit <- NEPSroutines::irt_analysis(
    resp = resp_,
    vars = vars_,
    select = "dich",
    valid = "valid",
    mvs = mvs,
    missing_by_design = -54,
    scoring = NULL,
    plots = FALSE,
    save = FALSE,
    print = FALSE,
    return = TRUE,
    suf_item_names = FALSE,
    verbose = FALSE,
    overwrite = FALSE,
    warn = TRUE,
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

  P <- data.frame(ID_t = theta$pid, sapply(xsi, function(x) 1 / (1 + exp(-(theta$EAP - x)))))
  pred_resp <- data.frame(ID_t = theta$pid, ifelse(P[,-1] > 0.5, 1, 0))

  # Calculate error rate (threshold = .50 as criterion for predicted response)
  #test
  if ( !setequal(resp_$ID_t[resp_[valid] == TRUE], pred_resp$ID_t) | length(resp_$ID_t[resp_[valid] == TRUE]) != length(pred_resp$ID_t) ) {
    warning( "\nID_ts in original data.frame and in data.frame with predicted responses are different.
             Please contact the package developer." )
  }

  merged <- merge(resp_, pred_resp, by.x = "ID_t", by.y = "ID_t", suffixes = c("_true", "_pred"))

  error_rates_.50 <- data.frame(
    item = items,
    error_rate = sapply(items, function(item) {
      true_values <- merged[[paste0(item, "_true")]]
      pred_values <- merged[[paste0(item, "_pred")]]
      no_na <- !is.na(true_values)
      mean(true_values[no_na] != pred_values[no_na]) # % der Unstimmigkeiten zw. beobachteten und vorhergesagten Responses
    })
  )
  mean_error_rates_.50 <- mean(error_rates_.50$error_rate)

  # Alternative calculation of predicted responses (empirical optimal cut-off obtained by ROC analysis)
  suppressMessages(suppressPackageStartupMessages({
    if (!requireNamespace("pROC", quietly = TRUE)) {
      invisible(capture.output(install.packages("pROC")))
    }
    if (!require("pROC", quietly = TRUE, character.only = TRUE)) {
      stop("\nPackage ‘pROC’ could not be installed or loaded.
           First install the pROC package and then execute the 'create_score' function again.")
    }
  }))

  optimal_cutoffs_roc <- data.frame(item = character(),
                                    threshold = numeric(),
                                    sensitivity = numeric(),
                                    specificity = numeric(),
                                    error_rate = numeric(),
                                    stringsAsFactors = FALSE)
  pred_resp_roc <- data.frame(matrix(NA, nrow = as.numeric(table(resp_[valid])["TRUE"]), ncol = length(items)))
  colnames(pred_resp_roc) <- items

  for (item in items) {
    inp_obs <- resp_[[item]][resp_[[valid]]== TRUE]
    inp_obs[inp_obs < 0] <- NA
    inp_pred <- P[[item]]

    roc_obj <- suppressMessages(roc(inp_obs, inp_pred))
    optimal_cutoff <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))

    out_pred <- ifelse(inp_pred >= optimal_cutoff$threshold, 1, 0)
    pred_resp_roc[[item]] <- out_pred

    error_rate <- mean(out_pred != inp_obs, na.rm = TRUE)
    optimal_cutoffs_roc <- rbind(optimal_cutoffs_roc,
                                 data.frame(item = item,
                                            threshold = optimal_cutoff$threshold,
                                            sensitivity = optimal_cutoff$sensitivity,
                                            specificity = optimal_cutoff$specificity,
                                            error_rate = error_rate))
    rm(inp_obs, inp_pred, roc_obj, out_pred, error_rate, optimal_cutoff)
  }

  pred_resp_roc <- data.frame(ID_t = resp_$ID_t[resp_[valid]==TRUE], pred_resp_roc)
  mean_error_rates_roc <- mean(optimal_cutoffs_roc$error_rate)


  # Imputation of predicted responses for subitems with OM values
  resp_imp <- resp
  indOM <- names(indicators)[grepl("_indOM$", names(indicators))]

  for (person in 1:nrow(resp)) {
    for (ind in indOM) {
      if (indicators[person, ind] == 1) {
        ind_stem <- sub("_indOM$", "", ind)
        subitems_one_pc <- pc_subitems[grepl(ind_stem, pc_subitems)]

        ind_subitems_one_pc <- indicators[person, subitems_one_pc]
        pred_resp_subitems_one_pc <- pred_resp[person, subitems_one_pc]

        if ( length(ind_subitems_one_pc) == length(pred_resp_subitems_one_pc) ) {
          for (subitem in subitems_one_pc) {
            if (ind_subitems_one_pc[[subitem]] == 1) {
              resp_imp[person, subitem] <- pred_resp_subitems_one_pc[[subitem]]
            }
          }
        } else {
          warning( "\nThe number of subitems of at least one pc-item in the data.frame 'indicators' differs
                   from the data.frame with predicted responses. Please contact the package developer." )
        }
      }
    }
  }


  # Score polytomous items
  resp_imp <- NEPSroutines:::pc_scoring(resp = resp_imp[ , !(names(resp_imp) %in% grep("s_c", names(resp_imp), value = TRUE))],
                                        poly_items = poly_items,
                                        mvs = mvs)

  # Collapse categories
  vars_imp <- vars[!grepl("s_c_collapsed", vars$item), ]
  resp_imp <-
    NEPSroutines:::collapse_response_categories(resp = resp_imp,
                                                vars = vars_imp,
                                                select = "poly",
                                                save = save,
                                                path_table = here::here("tables/", "collapsed_pc_items_resp_imp"),
                                                per_cat = per_cat)

  # Save results
  tab_pc_items_imputed <- sapply(resp_imp[ , (names(resp_imp) %in% grep("s_c", names(resp_imp), value = TRUE))], table, useNA="always")
  tab_pc_items_imputed_collapsed <- sapply(resp_imp[ , (names(resp_imp) %in% grep("s_c_collapsed", names(resp_imp), value = TRUE))], table, useNA="always")

  subitems_imputation_details = list(fit = fit,
                                     pred_resp = pred_resp,
                                     error_rates_.50 = error_rates_.50,
                                     mean_error_rates_.50 = mean_error_rates_.50,
                                     pred_resp_roc = pred_resp_roc,
                                     optimal_cutoffs_roc = optimal_cutoffs_roc,
                                     mean_error_rates_roc = mean_error_rates_roc,
                                     resp_imp = resp_imp,
                                     tab_pc_items_imputed = tab_pc_items_imputed,
                                     tab_pc_items_imputed_collapsed = tab_pc_items_imputed_collapsed)

  if (save) {

    save_results(subitems_imputation_details, "subitems_imputation_details.rds", "results")
  }

  return( subitems_imputation_details$resp_imp )
}


#' Create scores
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... , k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param num_cat string; defines name of numeric variable in vars that indicates
#'  the maximum number of response options for the items
#' @param mvs  named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param poly2dich  logical; indicates whether count only correctly scored
#'   binary or FULLY correctly scored PC items
#' @param score_name character; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#'
#' @noRd
estimate_sum_scores <- function(resp,
                                vars,
                                select,
                                num_cat = 'num_cat',
                                valid = NULL,
                                mvs = NULL,
                                scoring = NULL,
                                poly2dich = TRUE,
                                score_name = "score") {

  # Prepare data
  resp_ <- only_valid(resp, valid = valid, warn = FALSE)
  pid <- resp_$ID_t
  resp_ <- NEPSroutines:::prepare_resp(resp_, vars, select, convert = TRUE,
                                  mvs = mvs, warn = FALSE)
  resp_[is.na(resp_)] <- 0

  # Score polytomous items dichotomously
  if (poly2dich) {
    for (i in vars$item[vars[[select]]]) {
      score <- vars[[num_cat]][vars$item==i]
      resp_[[i]][resp_[[i]] != score] <- 0
      resp_[[i]][resp_[[i]] == score] <- 1
    }
    rm(i, score)
  }

  # Sum score
  if (!poly2dich) {
    scores <- NEPSroutines:::create_ifelse(
        is.null(scoring),
        rep(1, sum(vars[[select]])),
        vars[[scoring]][vars[[select]]]
    )
    n <- nrow(resp_)
    resp_ <- resp_ * matrix(rep(scores, n),nrow = n, byrow = TRUE)
  }
  sum_scores <- rowSums(resp_)

  resp <- data.frame(ID_t = pid, sum_scores)
  out <- data.frame(ID_t = resp$ID_t)
  out <- merge(out, resp, by = "ID_t", all.x = TRUE)
  names(out) <- c("ID_t", paste0(score_name, "_sc3"))

  # Return results
  return(out)

}


#' Estimated WLEs with test rotation
#' @param resp data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person
#'   identifier and all variables that are further defined in the function
#'   arguments; if special person sets are of interest, these persons have to
#'   be selected in resp beforehand
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param rotation character vector; contains the variable name indicating the
#'   test rotation
#' @param xsi_fixed named numerical vector; contains fixed item difficulties as
#'   elements and item names as names of elements
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param mvs  named integer vector; contains user-defined missing values
#' @param wle_name character; name of the wle -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param control_tam list; control argument as passed to tam.mml.mfr()
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights numeric vector; person weights for current measurement point
#'   passed to tam.mml.mfr()
#'
#' @return a data.frame containing ID_t, wle and se of wle (named like indicated
#'   in wle_name)
#' @noRd
estimate_rotated_wles <- function(resp,
                                  vars,
                                  select,
                                  valid = NULL,
                                  rotation,
                                  xsi_fixed = NULL,
                                  scoring = NULL,
                                  mvs = NULL,
                                  wle_name,
                                  control_wle = NULL,
                                  control_tam = NULL,
                                  pweights = NULL) {

  # Test data
  NEPSroutines:::check_variables(resp, "resp", rotation)

  if (is.null(xsi_fixed)) {
    warning("\nPlease provide the item parameters to ensure the correct",
            " results in the WLE estimation.")
  }

  # Identify IRT type
  irt_type <- ifelse(NEPSroutines:::is_poly(resp, vars, select), 'poly', 'dich')

  # Prepare data
  rotation <- resp[resp[[valid]], rotation, drop = FALSE]
  pid <- resp$ID_t[resp[[valid]]]
  NEPSroutines:::check_pid(pid)
  resp_ <- NEPSroutines:::prepare_resp(
      resp = resp,
      vars = vars,
      select = select,
      use_only_valid = TRUE,
      valid = valid,
      convert = TRUE,
      mvs = mvs,
      warn = FALSE
  )

  # Test resp
  NEPSroutines:::check_numerics(resp_, "resp", check_invalid = TRUE)

  # Conduct analyses
  frmA <- as.formula(paste0("~ item + ",
                            ifelse(irt_type == "poly", " item:step + ", ""),
                            names(rotation)))

  # Design matrix for model
  des <- TAM::designMatrices.mfr2(resp = resp_, facets = rotation, formulaA = frmA)
  resp2 <- des$gresp$gresp.noStep
  A <- des$A$A.3d[ , , -des$xsi.elim[, 2]]
  B <- des$B$B.3d

  # 0.5 scoring for PCMs
  if (irt_type == "poly" & !is.null(scoring)) {
    v <- sub(paste0('-', names(rotation)[1], '.+$'), "", rownames(B))
    v <- merge(data.frame(item = v), vars[vars[[select]], c("item", scoring)], by.x = "item")
    v[[2]][is.na(v[[2]])] <- 1
    B[, , 1] <- B[, , 1] * v[[2]]
  }

  # Match item parameters by item name
  xsi_fixed <- NEPSroutines:::order_xsi_fixed(
    xsi_fixed, resp2, irtmodel = '1PL', A = A, B = B, rename_steps = TRUE
  )

  # Fit model
  mod <- TAM::tam.mml(
    resp = resp2,
    A = A,
    B = B,
    xsi.fixed = xsi_fixed,
    verbose = FALSE,
    pid = pid,
    control = control_tam,
    pweights = pweights
  )


  if (is.null(control_wle)) control_wle <- list()
  if (is.null(control_wle$convM)) control_wle$convM <- .0001
  if (is.null(control_wle$Msteps)) control_wle$Msteps <- 50
  wles <- TAM::tam.wle(
      mod, convM = control_wle$convM,
      Msteps = control_wle$Msteps,
      progress = FALSE
  )
  wles <- data.frame(ID_t = wles$pid, wle = wles$theta, se = wles$error)
  names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))

  wles_mod <- list(mod, wles)

  # Return results
  return(wles_mod)
}

#' Create meta competence scores
#'
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
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param meta_variable  string; defines name of meta competence variable in resp
#' @param score_name string; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param num_cat string; defines name of numeric variable in vars that indicates
#'  the maximum number of response options for the items
#' @param mvs  named integer vector; contains user-defined missing values
#' @returns data.frame with the three variables ID_t, proportion correct ("_sc5")
#' & difference score ("_sc6")
#'
#' @noRd
estimate_metap <- function(resp,
                           vars,
                           select,
                           valid = NULL,
                           meta_variable,
                           score_name = 'score',
                           num_cat = 'num_cat',
                           mvs = NULL) {

  # Calculate sum scores
  sss <- NEPSroutines:::estimate_sum_scores(
    resp = resp,
    vars = vars,
    select = select,
    num_cat = num_cat,
    valid = valid,
    mvs = mvs,
    poly2dich = TRUE)

  # Estimated score
  es <- resp[, c("ID_t", meta_variable)]

  # Merge scores
  metap <- merge(sss, es, by = "ID_t", all = TRUE)

  # Recode meta-p string variables into numeric variables
  # Only integer numbers in the defined number range are considered valid
  k <- sum(vars[[select]]) # number of items
  metap[[meta_variable]] <- as.numeric(metap[[meta_variable]])
  metap[[meta_variable]][is.na(metap[[meta_variable]]) | metap[[meta_variable]] > k] <- -95
  metap$score_sc3[is.na(metap$score_sc3)] <- -95

  # Select valid cases
  f <- metap[[meta_variable]] >= 0 & metap$score_sc3 >= 0

  # Calculate proportion correct
  metap[[paste0(score_name, "_sc6")]][f] <- metap[[meta_variable]][f] / k

  # Calculate difference score
  metap[[paste0(score_name, "_sc5")]][f] <-
    (metap[[meta_variable]][f] - metap$score_sc3[f]) / k

  # Format results
  metap <- metap[, c("ID_t", paste0(score_name, "_sc5"), paste0(score_name, "_sc6"))]
  metap[is.na(metap)] <- -55

  # Return results
  return(metap)
}
