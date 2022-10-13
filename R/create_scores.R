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
#' @param xsi_fixed matrix; fixed parameter estimates of final IRT scaling as
#'   returned by tam.mml()
#' @param facet character vector; contains the variable name indicating the
#'   test rotation
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param wle logical; whether to estimate WLEs
#' @param sum_score logical; whether to calculate sum scores
#' @param metap logical; whether to calculate metacognition score
#' @param meta_select string; defines name of logical variable in vars that
#'   indicates which items to use for the meta cognition analysis
#'   (if identical to select, argument can be empty)
#' @param meta_var_name  string; defines name of meta competence variable in resp
#' @param meta_score_name string; name of the meta competence scores -- WITHOUT
#'   extension (e.g., reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param control_tam list; control argument as passed to tam.mml.mfr()
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights numeric vector; person weights for current measurement point
#'   passed to TAM-functions
#' @param poly2dich  logical; indicates whether count only correctly scored
#'   binary or FULLY correctly scored PC items
#' @param resp_prev data.frame with responses of first measurement wave,
#'    a person identifier; may also include a WLE as given in wid
#' @param resp_link_study data.frame with responses of link sample,
#'    a person identifier;
#' @param pweights_previous numeric vector; person weights for previous
#'   measurement point passed to TAM-functions
#' @param pweights_link_study numeric vector; person weights for link study
#'   passed to TAM-functions
#' @param scoring_previous  numeric; named vector with the scoring factors to
#'   be applied to the loading matrix of the previous measurement point; can be
#'   NULL for the Rasch model
#' @param scoring_link_study  numeric; named vector with the scoring factors to
#'   be applied to the loading matrix of the link study; can be NULL for the
#'   Rasch model
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param longitudinal  logical; do within cohort linking (TRUE) or between
#'   cohort linking (FALSE)
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param overwrite logical; whether to overwrite existing file when saving
#'   table
#' @param path_results  string; defines path to folder where results shall be
#'   saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param do_dim logical; whether to do dimensionality analysis for linking
#' @param do_dif logical; whether to do dif analysis for linking
#' @param dif_threshold numeric; threshold under which DIF in common link items
#'   is accepted; defaults to .5
#' @param wid variable name used as WLE identifier in first measurement wave
#' @param snodes  snodes as passed to the TAM function for the dimensionality
#'   analyses
#' @param maxiter maximum number of iterations as passed to the TAM function
#'   for the dimensionality analyses
#' @param digits  numeric; number of decimal places for rounding
#' @param verbose  logical; verbose as passed to the TAM function
#' @param warn  logical; whether to print warnings
#'
#' @export
create_scores <- function(resp, vars, scoring = NULL,
                          score_name = "score", xsi_fixed = NULL,
                          facet = NULL, select, valid = NULL,
                          mvs = NULL, wle = TRUE, sum_score = FALSE,
                          metap = FALSE, meta_var_name = NULL,
                          meta_score_name = NULL, meta_select = NULL,
                          control_tam = NULL, control_wle = NULL,
                          pweights = NULL, poly2dich = TRUE,
                          resp_prev = NULL, resp_link = NULL,
                          vars_prev = NULL, vars_link = NULL,
                          select_prev = NULL, select_link = NULL,
                          valid_prev = NULL, valid_link = NULL,
                          scoring_prev = NULL, scoring_link = NULL,
                          pweights_prev = NULL, pweights_link = NULL,
                          anchors = NULL, longitudinal = TRUE,
                          print = TRUE, save = TRUE,
                          return = FALSE, overwrite = FALSE,
                          path_table = here::here("Tables"),
                          path_results = here::here("Results"),
                          do_dim = TRUE, do_dif = TRUE,
                          dif_threshold = .5, wid = NULL,
                          snodes = 0, maxiter = 1000,
                          digits = 3, verbose = TRUE, warn = TRUE) {

  # Test data
  scaling:::check_logicals(vars, "vars", select, warn = warn)
  scaling:::check_logicals(resp, "resp", valid, warn = warn)
  scaling:::check_items(vars$item[vars[[select]]])
  scaling:::check_pid(resp$ID_t[ifelse(is.null(valid), TRUE, resp[[valid]])])

  if (!is.null(scoring))
    scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)

  if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)

  # Estimate linked WLEs and SEs --> linking not yet implemented
  if (wle & !is.null(resp_prev)) {
    linked_scores <-
      scaling:::linking(
        resp_curr = resp,
        resp_prev = resp_prev,
        resp_link = resp_link,
        vars_curr = vars,
        vars_prev = vars_prev,
        vars_link = vars_link,
        select_curr = select,
        select_prev = select_prev,
        select_link = select_link,
        valid_curr = valid,
        valid_prev = valid_prev,
        valid_link = valid_link,
        scoring_curr = scoring,
        scoring_prev = scoring_prev,
        scoring_link = scoring_link,
        anchors = anchors,
        mvs = mvs,
        longitudinal = longitudinal,
        overwrite = overwrite,
        verbose = verbose,
        path_table = path_table,
        path_results = path_results,
        return = return, print = print, save = save,
        dif_threshold = dif_threshold,
        wid = wid, snodes = snodes, maxiter = maxiter,
        digits = digits,
        pweights_curr = pweights,
        pweights_prev = pweights_prev,
        pweights_link = pweights_link,
        control_tam = control_tam,
        do_dim = do_dim, do_dif = do_dif,
        warn = warn
      )
    wles_linked <- linked_scores$link_results$wle_linked
    names(wles_linked) <- c("ID_t", paste0(score_name, c("_sc1u", "_sc2u")))
  } else {
    wles_linked <- NULL
  }

  # Estimate (unrotated) WLEs and SEs
  if (wle) {

    if (is.null(facet) | (!is.null(facet) & is.null(xsi_fixed))) {
      fit <- scaling:::irt_analysis(
        resp = resp, vars = vars, select = select,
        valid = valid, mvs = mvs,
        scoring = scoring, xsi.fixed = xsi_fixed,
        verbose = FALSE, warn = warn, return = TRUE,
        plots = FALSE, save = FALSE, print = FALSE,
        control_tam = control_tam, control_wle = control_wle,
        pweights = pweights, test = FALSE
      )
      if (is.null(fit$model.1pl)) {
        fit <- fit$model.pcm
      } else {
        fit <- fit$model.1pl
      }
      warn <- FALSE
    }

    if (!is.null(facet)) {
      if (is.null(xsi_fixed)) xsi_fixed <- fit$mod$xsi.fixed.estimated
      wles <- scaling:::estimate_rotated_wles(
        resp = resp, vars = vars, select = select,
        valid = valid, facet = facet, mvs = mvs,
        scoring = scoring, xsi_fixed = xsi_fixed,
        wle_name = score_name, control_tam = control_tam,
        pweights = pweights
      )
    } else {
      wles <- as.data.frame(fit$wle[, c("pid", "theta", "error")])
      names(wles) <- c("ID_t", paste0(score_name, c("_sc1", "_sc2")))
    }
    if (!is.null(wles_linked))
      wles <- merge(wles, wles_linked, by = "ID_t", all = TRUE)
  }

  # Estimate sum scores
  if (sum_score) {
    sss <- scaling:::estimate_sum_scores(
      resp = resp, vars = vars, select = select,
      valid = valid, mvs = mvs, scoring = scoring,
      score_name = score_name, poly2dich = poly2dich
    )
    if (wle) {
      wles <- merge(wles, sss, by = "ID_t", all = TRUE)
    } else {
      wles <- sss
    }
  }

  if (metap) {
    meta_select <- ifelse(is.null(meta_select), select, meta_select)
    meta_score_name <- ifelse(is.null(meta_score_name), score_name, meta_score_name)
    metas <- scaling:::estimate_metap(
      resp = resp, vars = vars, select = meta_select, valid = valid,
      var_name = meta_var_name, score_name = meta_score_name, mvs = mvs
    )
    if (wle | sum_score) {
      wles <- merge(wles, metas, by = "ID_t", all = TRUE)
    } else {
      wles <- metas
    }
  }

  # Return results
  if (return & !is.null(wles_linked)) {
    return(list(wle = wles, linking = linked_scores))
  } else {
    return(wles)
  }

}



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
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
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
estimate_sum_scores <- function(resp, vars, select, valid = NULL,
                                mvs = NULL, scoring = NULL,
                                poly2dich = TRUE, score_name = "score") {

  # Prepare data
  resp_ <- scaling:::prepare_resp(resp, vars, select, use_only_valid = TRUE,
                                  valid = valid, convert = TRUE, mvs = mvs,
                                  warn = FALSE)
  resp_[is.na(resp_)] <- 0

  # Score polytomous items dichotomously
  if (poly2dich) {
    resp_ <- apply(resp_, 2, \(x) {
      ifelse(x %in% max(x), 1, 0)
    })
  }

  # Sum score
  if (!poly2dich) {
    scores <- scaling:::create_ifelse(is.null(scoring),
                                      rep(1, sum(vars[[select]])),
                                      vars[[scoring]][vars[[select]]])
    n <- nrow(resp_)
    resp_ <- resp_ * matrix(rep(scores, n),nrow = n, byrow = TRUE)
  }
  sum_scores <- rowSums(resp_)

  resp <- data.frame(ID_t = resp$ID_t[resp[[valid]]], sum_scores)
  out <- data.frame(ID_t = resp$ID_t)
  out <- merge(out, resp, by = "ID_t", all.x = TRUE)
  names(out) <- c("ID_t", paste0(score_name, "_sc3"))

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
#' @param facet character vector; contains the variable name indicating the
#'   test rotation
#' @param xsi_fixed matrix; fixed parameter estimates of final IRT scaling as
#'   returned by tam.mml.mfr()
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
                                  facet, xsi_fixed = NULL,
                                  scoring = NULL, mvs = NULL, wle_name,
                                  control_wle = NULL, control_tam = NULL,
                                  pweights = NULL) {

  # Test data
  scaling:::check_variables(resp, "resp", facet)

  if (is.null(xsi_fixed)) {
    warning("Please provide the item parameters to ensure the correct",
            " results in the WLE estimation.")
  }

  # Identify IRT type
  irt_type <- ifelse(scaling:::is_poly(resp, vars, select), 'poly', 'dich')

  # Prepare data
  facet <- resp[resp[[valid]], facet, drop = FALSE]
  pid <- resp$ID_t[resp[[valid]]]
  scaling:::check_pid(pid)
  resp_ <- scaling:::prepare_resp(resp, vars, select, use_only_valid = TRUE,
                                  valid = valid, convert = TRUE, mvs = mvs,
                                  warn = FALSE)

  # Test resp
  scaling:::check_numerics(resp_, "resp", check_invalid = TRUE)

  # Conduct analyses
  frmA <- as.formula(paste0("~ item + ",
                            ifelse(irt_type == "poly", "item*step +", ""),
                            names(facet)))

  if (irt_type == "poly") {
    # get design matrix for model with 0.5 scoring
    B <- TAM::designMatrices(modeltype = "PCM", resp = resp_)$B
    B[vars$item[vars[[select]]], , 1] <- scaling:::create_ifelse(is.null(scoring),
                                                                 rep(1, sum(vars[[select]])),
                                                                 vars[[scoring]][vars[[select]]])
  } else {
    B <- NULL
  }


  mod <- TAM::tam.mml.mfr(
    resp = resp_, facets = facet, xsi.fixed = xsi_fixed,
    verbose = FALSE, pid = pid, formulaA = frmA,
    irtmodel = ifelse(irt_type == "poly", "PCM2", "1PL"),
    B = B, control = control_tam, pweights = pweights
  )

  if (is.null(control_wle)) control_wle <- list()
  if (is.null(control_wle$convM)) control_wle$convM <- .0001
  if (is.null(control_wle$Msteps)) control_wle$Msteps <- 50
  wles <- TAM::tam.wle(mod, convM = control_wle$convM,
                       Msteps = control_wle$Msteps,
                       progress = FALSE)[, c("pid", "theta", "error")]
  names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
  return(wles)
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
#' @param var_name  string; defines name of meta competence variable in resp
#' @param score_name string; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param mvs  named integer vector; contains user-defined missing values
#'
#' @returns data.frame with the three variables ID_t, proportion correct ("_sc5")
#' & difference score ("_sc6")
#'
#' @noRd
estimate_metap <- function(resp, vars, select, valid = NULL,
                           var_name, score_name = NULL,
                           mvs = NULL) {

  if (is.null(var_name)) stop("No name for meta score variable provided.")
  if (is.null(score_name)) score_name <- "score"

  # Calclute sum scores
  sss <- scaling:::estimate_sum_scores(
    resp = resp, vars = vars, select = select,
    valid = valid, mvs = mvs, poly2dich = TRUE)

  # Estimated score
  es <- resp[, c("ID_t", var_name)]

  # Merge scores
  metap <- merge(sss, es, by = "ID_t", all = TRUE)

  # Recode meta-p string variables into numeric variables
  # Only integer numbers in the defined number range are considered valid
  k <- sum(vars[[select]]) # number of items
  metap[[var_name]] <- as.numeric(metap[[var_name]])
  metap[[var_name]][is.na(metap[[var_name]]) | metap[[var_name]] > k] <- -95
  metap$score_sc3[is.na(metap$score_sc3)] <- -95

  # Select valid cases
  f <- metap[[var_name]] >= 0 & metap$score_sc3 >= 0

  # Calculate proportion correct
  metap[[paste0(score_name, "sc_6")]][f] <- metap[[var_name]][f] / k

  # Calculate difference score
  metap[[paste0(score_name, "sc_5")]][f] <-
    (metap[[var_name]][f] - metap$score_sc3[f]) / k

  # Format results
  metap <- metap[, c("ID_t", paste0(score_name, "sc_5"), paste0(score_name, "sc_6"))]
  metap[is.na(metap)] <- -55

  # Return results
  return(metap)
}
