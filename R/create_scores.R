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
#' @param control_tam list; control argument as passed to tam.mml.mfr()
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights numeric vector; person weights for current measurement point
#'   passed to TAM-functions
#' @param poly2dich  logical; indicates whether count only correctly scored
#'   binary or FULLY correctly scored PC items
#' @param resp_previous data.frame with responses of first measurement wave,
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
#' @param diff_threshold numeric; threshold under which DIF in common link items
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
                          control_tam = NULL, control_wle = NULL,
                          pweights = NULL, poly2dich = TRUE,
                          #resp_previous = NULL, resp_link_study = NULL,
                          #scoring_previous = NULL, scoring_link_study = NULL,
                          #pweights_previous = NULL, pweights_link_study = NULL,
                          #anchors = NULL, longitudinal = TRUE,
                          print = TRUE, save = TRUE,
                          return = FALSE, overwrite = FALSE,
                          path_table = here::here("Tables"),
                          path_results = here::here("Results"),
                          do_dim = TRUE, do_dif = TRUE,
                          diff_threshold = .5, wid = NULL,
                          snodes = 0, maxiter = 1000,
                          digits = 3, verbose = TRUE, warn = TRUE) {

  # # Estimate linked WLEs and SEs --> linking not yet implemented
  # if (wle & !is.null(resp_previous)) {
  #   linked_scores <-
  #     linking(
  #       resp = resp,
  #       resp_previous = resp_previous,
  #       resp_link_study = resp_link_study,
  #       vars = vars,
  #       select = select,
  #       valid = valid,
  #       anchors = anchors,
  #       mvs = mvs,
  #       scoring = scoring,
  #       scoring_previous = scoring_previous,
  #       scoring_link_study = scoring_link_study,
  #       longitudinal = longitudinal,
  #       overwrite = overwrite,
  #       verbose = verbose,
  #       path_table = path_table,
  #       path_results = path_results,
  #       return = return, print = print, save = save,
  #       diff_threshold = diff_threshold,
  #       wid = wid, snodes = snodes, maxiter = maxiter,
  #       digits = digits,
  #       pweights = pweights,
  #       pweights_previous = pweights_previous,
  #       pweights_link_study = pweights_link_study,
  #       control_tam = control_tam,
  #       do_dim = do_dim, do_dif = do_dif
  #     )
  #   wles_linked <- linked_scores$link_results$wle_linked
  #   names(wles_linked) <- c("ID_t", paste0(score_name, c("_sc1u", "_sc2u")))
  # } else {
    wles_linked <- NULL
  #}

  # Estimate (unrotated) WLEs and SEs
  if (wle) {

    if (is.null(facet) | (!is.null(facet) & is.null(xsi_fixed))) {
      fit <- scaling::irt_analysis(
        resp = resp, vars = vars, select = select,
        valid = valid, mvs = mvs,
        scoring = scoring, xsi.fixed = xsi_fixed,
        verbose = FALSE, warn = warn, return = TRUE,
        plots = FALSE, save = FALSE, print = FALSE, test = FALSE,
        control_tam = control_tam, control_wle = control_wle,
        pweights = pweights
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
      wles <- estimate_rotated_wles(
        resp = resp, vars = vars, select = select,
        valid = valid, facet = facet, mvs = mvs,
        scoring = scoring, xsi_fixed = xsi_fixed,
        wle_name = score_name, control_tam = control_tam,
        pweights = pweights, warn = warn
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
    sss <- estimate_sum_scores(
      resp = resp, vars = vars, select = select,
      valid = valid, mvs = mvs, scoring = scoring,
      score_name = score_name, warn = !wle & warn,
      poly2dich = poly2dich
    )
    if (wle) {
      wles <- merge(wles, sss, by = "ID_t", all = TRUE)
    } else {
      wles <- sss
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
#' @param warn  logical; whether to print warnings
#' @param score_name character; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#'
#' @noRd
estimate_sum_scores <- function(resp, vars, select, valid = NULL,
                                mvs = NULL, scoring = NULL,
                                poly2dich = TRUE, warn = TRUE,
                                score_name = "score") {

  # Default values
  if (is.null(valid)) {
    valid <- "valid"
    resp[["valid"]] <- TRUE
  }
  if (is.null(scoring)) {
    scoring <- "scoring"
    vars[["scoring"]] <- 1
  }

  # Test data
  if (warn & !is.null(scoring))
    scaling:::check_numerics(vars, "vars", scoring)

  # IDs
  if (warn)
    scaling:::check_pid(resp$ID_t)

  # Prepare data
  resp_ <- scaling:::prepare_resp(resp, vars, select, use_only_valid = TRUE,
                        valid = valid, convert = TRUE, mvs = mvs,
                        warn = warn)
  resp_[is.na(resp_)] <- 0

  # Score polytomous items dichotomously
  if (poly2dich) {
    resp_ <- apply(resp_, 2, \(x) {
      ifelse(x %in% max(x), 1, 0)
    })
  }

  # Sum score
  if (!poly2dich) {
    scores <- vars[[scoring]][vars[[select]]]
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
#' @param warn  logical; whether to print warnings
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
                                  warn = TRUE, control_wle = NULL,
                                  control_tam = NULL, pweights = NULL) {

  # Test data
  scaling:::check_variables(resp, "resp", facet)
  scaling:::check_numerics(vars, "vars", scoring)
  if (is.null(scoring)) {
    scoring <- "scoring"
    vars[["scoring"]] <- 1
  }

  if (is.null(facet)) {
    stop("Please provide the facet indicating the testlet rotation.")
  }
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
                        warn = warn)

  # Test resp
  scaling:::check_numerics(resp_, "resp", check_invalid = TRUE)

  # Conduct analyses
  frmA <- as.formula(paste0("~ item + ",
                            ifelse(irt_type == "poly", "item*step +", ""),
                            names(facet)))

  if (irt_type == "poly") {
    # get design matrix for model with 0.5 scoring
    B <- TAM::designMatrices(modeltype = "PCM", resp = resp_)$B
    B[vars$item[vars[[select]]], , 1] <- vars[[scoring]][vars[[select]]]
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

