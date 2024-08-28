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
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
# #' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param path_results  string; defines path to folder where results shall be
#'   saved
# #' @param path_table  string; defines path to folder where tables shall be saved
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
#' @param warn  logical; whether to print warnings
#'
#' @export
create_scores <- function(resp, vars, select, scoring = NULL,
                          score_name = 'score', num_cat = 'num_cat',
                          xsi_fixed = NULL, rotation = NULL, valid = NULL,
                          mvs = NULL, missing_by_design = -54, wle = TRUE,
                          sum_score = FALSE, sum_select = NULL,
                          metap = FALSE, meta_variable = NULL,
                          meta_score_name = NULL, meta_select = NULL,
                          control_tam = NULL, control_wle = NULL,
                          pweights = NULL, poly2dich = TRUE,
                          # resp_prev = NULL, resp_link = NULL,                 # commented out because linking is not yet implemented
                          # vars_prev = NULL, vars_link = NULL,                 # commented out because linking is not yet implemented
                          # select_prev = NULL, select_link = NULL,             # commented out because linking is not yet implemented
                          # valid_prev = NULL, valid_link = NULL,               # commented out because linking is not yet implemented
                          # scoring_prev = NULL, scoring_link = NULL,           # commented out because linking is not yet implemented
                          # pweights_prev = NULL, pweights_link = NULL,         # commented out because linking is not yet implemented
                          # anchors = NULL, longitudinal = TRUE,                # commented out because linking is not yet implemented
                          save = TRUE, return = FALSE, #print = TRUE,           # commented out because linking is not yet implemented
                          name_group = NULL, #overwrite = FALSE,                # commented out because linking is not yet implemented
                          #path_table = "Tables",                   # commented out because linking is not yet implemented
                          path_results = "Results",
                          #do_dim = TRUE, do_dif = TRUE,                        # commented out because linking is not yet implemented
                          #dif_threshold = .5, wid = NULL,                      # commented out because linking is not yet implemented
                          #snodes = 5000, maxiter = 10000,                      # commented out because linking is not yet implemented
                          # digits = 3, verbose = TRUE,                         # commented out because linking is not yet implemented
                          warn = TRUE) {


  if (!is.null(scoring))
    scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)

  if (sum_score | metap)
    scaling:::check_variables(vars, "vars", num_cat)

  if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)

  ################################## Linking ##################################

  # # Estimate linked WLEs and SEs
  # if (wle & !is.null(resp_prev)) {
  #   linked_scores <-
  #     scaling:::linking(
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
    scaling:::check_logicals(vars, "vars", select, warn = warn)
    scaling:::check_logicals(resp, "resp", valid, warn = warn)
    scaling:::check_items(vars$item[vars[[select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    scaling:::check_pid(resp$ID_t)

    if (is.null(rotation) | (!is.null(rotation) & is.null(xsi_fixed))) {
      fit <- scaling:::irt_analysis( # hier könnte man irt_model() anstatt irt_analysis() verwenden --> spart Berechnungszeit
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
      mod_wles <- scaling:::estimate_rotated_wles(
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
  }

  # Estimate sum scores
  if (sum_score) {

    # Select
    if (is.null(sum_select)) {
      sum_select <- select
      warning("No variable 'sum_select' provided for sum scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    # Test data
    scaling:::check_logicals(vars, "vars", sum_select, warn = warn)
    scaling:::check_logicals(resp, "resp", valid, warn = warn)
    scaling:::check_items(vars$item[vars[[sum_select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[sum_select]]])
    scaling:::check_pid(resp$ID_t)

    sss <- scaling:::estimate_sum_scores(
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
      warning("No variable 'meta_select' provided for meta scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    scaling:::check_logicals(vars, "vars", meta_select, warn = warn)
    scaling:::check_logicals(resp, "resp", valid, warn = warn)
    scaling:::check_items(vars$item[vars[[meta_select]]])
    scaling:::check_numerics(resp, "resp", c(meta_variable, vars$item[vars[[meta_select]]]))
    scaling:::check_pid(resp$ID_t)

    metas <- scaling:::estimate_metap(
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
      name <- scaling:::create_name("scores", name_group, ".rds")
      scaling:::save_results(scores, filename = name, path = path_results)

      # Save item parameters and TAM model used to estimate wles
      if (wle) {
        if (is.null(rotation)) {
            name <- scaling:::create_name("itemParamModel_wles", name_group, ".rds")
            scaling:::save_results(itemParamModel_wles, filename = name, path = path_results)

            name <- scaling:::create_name("itemParam_wles", name_group, ".xlsx")
            scaling:::save_table(itemParam_wles, filename = name, path = path_results)
        } else {
            name <- scaling:::create_name("itemParamModel_wles.position", name_group, ".rds")
            scaling:::save_results(itemParamModel_wles.position, filename = name, path = path_results)

            name <- scaling:::create_name("itemParam_wles.position", name_group, ".xlsx")
            scaling:::save_table(itemParam_wles.position, filename = name, path = path_results)
        }
      }
    }


  # Return results
  if (return) return(scores)
}



#' Create scores
#'
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
  resp_ <- scaling:::prepare_resp(resp_, vars, select, convert = TRUE,
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
    scores <- scaling:::create_ifelse(
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
estimate_rotated_wles <- function(resp, vars, select, valid = NULL,
                                  rotation, xsi_fixed = NULL,
                                  scoring = NULL, mvs = NULL, wle_name,
                                  control_wle = NULL, control_tam = NULL,
                                  pweights = NULL) {

  # Test data
  scaling:::check_variables(resp, "resp", rotation)

  if (is.null(xsi_fixed)) {
    warning("Please provide the item parameters to ensure the correct",
            " results in the WLE estimation.")
  }

  # Identify IRT type
  irt_type <- ifelse(scaling:::is_poly(resp, vars, select), 'poly', 'dich')

  # Prepare data
  rotation <- resp[resp[[valid]], rotation, drop = FALSE]
  pid <- resp$ID_t[resp[[valid]]]
  scaling:::check_pid(pid)
  resp_ <- scaling:::prepare_resp(
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
  scaling:::check_numerics(resp_, "resp", check_invalid = TRUE)

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
  xsi_fixed <- scaling:::order_xsi_fixed(
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
  sss <- scaling:::estimate_sum_scores(
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
