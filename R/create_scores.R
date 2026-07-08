#' Create scores
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
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
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param path_results  string; defines path to folder where results shall be
#'   saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param warn  logical; whether to print warnings
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
) {


  if (!is.null(scoring))
    check_numerics(vars, "vars", scoring, check_invalid = TRUE)

  if (sum_score | metap)
    check_variables(vars, "vars", num_cat)

  if (warn) is_null_mvs_valid(mvs = mvs, valid = valid)

  # Estimate (unrotated) WLEs and SEs
  if (wle) {

    # Test data
    check_logicals(vars, "vars", select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_items(vars$item[vars[[select]]])
    check_numerics(resp, "resp", vars$item[vars[[select]]])
    check_pid(resp$ID_t)

    # Estimation of wles
    if (is.null(rotation) | (!is.null(rotation) & is.null(xsi_fixed))) {
      fit <- irt_analysis( # hier könnte man irt_model() anstatt irt_analysis() verwenden --> spart Berechnungszeit
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
      mod_wles <- estimate_rotated_wles(
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

  }

  # Estimate sum scores
  if (sum_score) {

    # Select
    if (is.null(sum_select)) {
      sum_select <- select
      message("No variable 'sum_select' provided for sum scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    # Test data
    check_logicals(vars, "vars", sum_select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_items(vars$item[vars[[sum_select]]])
    check_numerics(resp, "resp", vars$item[vars[[sum_select]]])
    check_pid(resp$ID_t)

    sss <- estimate_sum_scores(
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
      message("No variable 'meta_select' provided for meta scores. All items as ",
              "specified in variable '", select, "' are used instead.")
    }

    check_logicals(vars, "vars", meta_select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_items(vars$item[vars[[meta_select]]])
    check_numerics(resp, "resp", c(meta_variable, vars$item[vars[[meta_select]]]))
    check_pid(resp$ID_t)

    metas <- estimate_metap(
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
  scores <- data.frame(wles)

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
      name <- create_name("scores", name_group, ".rds")
      save_results(scores, filename = name, path = path_results)

      # Save item parameters and TAM model used to estimate wles
      if (wle) {
        if (is.null(rotation)) {

            name <- create_name("itemParamModel_wles", name_group, ".rds")
            save_results(
              itemParamModel_wles,
              filename = name,
              path = path_results
            )

            name <- create_name("itemParam_wles", name_group, ".xlsx")
            save_table(
              itemParam_wles,
              filename = name,
              path = path_table,
              overwrite = overwrite
            )

        } else {

            name <- create_name("itemParamModel_wles.position", name_group, ".rds")
            save_results(
              itemParamModel_wles.position,
              filename = name,
              path = path_results
            )

            name <- create_name("itemParam_wles.position", name_group, ".xlsx")
            save_table(
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


#' Create scores
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... , k-1\} for
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
  resp_ <- prepare_resp(resp_, vars, select, convert = TRUE,
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
    scores <- create_ifelse(
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
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
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
  check_variables(resp, "resp", rotation)

  if (is.null(xsi_fixed)) {
    warning("\nPlease provide the item parameters to ensure the correct",
            " results in the WLE estimation.")
  }

  # Identify IRT type
  irt_type <- ifelse(is_poly(resp, vars, select), 'poly', 'dich')

  # Prepare data
  # Note: rotation needs to be a data.frame instead of a tibble for
  #       TAM::designMatrices.mfr2()
  rotation <- as.data.frame(resp[resp[[valid]], rotation, drop = FALSE])
  pid <- resp$ID_t[resp[[valid]]]
  check_pid(pid)
  resp_ <- prepare_resp(
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
  check_numerics(resp_, "resp", check_invalid = TRUE)

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
  if (!is.null(scoring)) {
    scoring_mat <- vars[vars[[select]], c("item", scoring)]
    for (i in seq(1, nrow(scoring_mat))) {
      if (is.na( scoring_mat$scoring[i])) next
      B[grepl(paste0("^", scoring_mat$item[i]), rownames(B)), , 1] <-
        B[grepl(paste0("^", scoring_mat$item[i]), rownames(B)), , 1] *
          scoring_mat$scoring[i]
    }
  }

  # Match item parameters by item name
  xsi_fixed_mat <- order_xsi_fixed(
    xsi_fixed, resp2, A = A, B = B, rename_steps = TRUE,
    irtmodel = ifelse(irt_type == "poly", "PCM2", "1PL")
  )

  # Fit model
  mod <- TAM::tam.mml(
    resp = resp2,
    A = A,
    B = B,
    xsi.fixed = xsi_fixed_mat,
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
#'   persons as rows; y in \{0, 1\} for binary data and y in \{0, 1, ... k-1\} for
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
  sss <- estimate_sum_scores(
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
