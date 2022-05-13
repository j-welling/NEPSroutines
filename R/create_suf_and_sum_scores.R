#' Create SUF
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param items_suf character; indicates the logical variable in vars which
#'   contains the item names designated for the SUF (original scored items
#'   before the first IRT analyses)
#' @param wle_name character; name of the wle -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param xsi_fixed matrix; fixed parameter estimates of final IRT scaling as
#'   returned by tam.mml()
#' @param rotated logical; whether the test design contains rotated tests; if
#'   TRUE, the WLEs have to be estimated again
#' @param linked logical; whether the test is linked to a previous assessment
#' @param wles data.frame; contains ID_t, WLEs and SEs as estimated by tam.wle()
#'   for the current assessment
#' @param linked_wles data.frame; contains the linked WLEs as returned by
#'   link_samples() / linking()
#' @param linked_location character; path to linking results as saved by
#'   linking(); alternative to linked_wles
#' @param facet character vector; contains the variable name indicating the
#'   test rotation
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param competence character; name of tested competence for labelling the
#'   data (capitalized!)
#' @param filename character; indicates final name of SUF according to study
#' @param path_results character; indicates where SUF shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#'
#' @export
create_suf <- function(resp, vars, items_suf, wle_name, xsi_fixed = NULL,
             rotated = FALSE, linked = FALSE, wles = NULL,
             linked_wles = NULL, linked_location = NULL,
             facet = NULL, select = NULL, valid = NULL, mvs = NULL,
             scoring = NULL, competence,
             filename = "suf", path_results = here::here('Results'),
             save = TRUE, return = FALSE) {

  # check whether WLEs have already been linked
  if (linked) {
    if (is.null(linked_wles) & is.null(linked_location)) {
      stop("Please provide linked WLEs for the SUF.")
    }
    if (is.null(linked_wles)) {
      linked_wles <- readRDS(linked_location)
      linked_wles <- linked_wles$wle_linked
    }
    linked_wles <- linked_wles[, c("ID_t", "wle", "error")]
    names(linked_wles) <- c("ID_t", paste0(wle_name, c("_sc1u", "_sc2u")))
  }

  if (is.null(wles)) {
    # estimate current (potentially rotated) WLEs and SEs
    irtmodel <- ifelse(any(vars[[scoring]][vars[[select]]] == 0.5),
               "PCM2", "1PL")
    if (rotated) {
      wles <- estimated_rotated_wles(resp, vars, select, valid, facet,
                       xsi_fixed, scoring, mvs, wle_name,
                       irtmodel)
    } else {
      wles <- irt_model(resp, vars, select, valid, mvs, irtmodel,
                scoring, verbose = FALSE, path = NULL,
                filename = NULL)$wle
      names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
    }
  } else {
    # wles <- wles[, c("pid", "theta", "error")]
    names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
  }

  # select items for SUF
  suf <- resp[, c("ID_t", vars$item[vars[[items_suf]]])]

  # merge data sets
  suf <- merge(suf, wles, by = "ID_t")
  if (linked) {
    suf <- merge(suf, linked_wles, by = "ID_t")
  }

  # add labels to response items, ID_t and WLEs and SEs
  suf <- set_labels(suf, vars, items_suf, competence, wle_name, linked)

  # convert still present NA to -55 (not determinable)
  suf[is.na(suf)] <- -55

  # (save SUF in rds, sav and dta format)
  if (save) {
    save_suf(suf, path_results, filename)
  }

  # (return suf)
  if (return) {
    return(suf)
  }
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
#'   returned by tam.mml()
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param mvs  named integer vector; contains user-defined missing values
#' @param wle_name character; name of the wle -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param irtmodel  string; "1PL" for Rasch, or "PCM2" for PCM analysis
#' @return a data.frame containing ID_t, wle and se of wle (named like indicated
#'   in wle_name)
#'
#' @export
estimated_rotated_wles <- function(resp, vars, select, valid = NULL, facet, xsi_fixed,
                   scoring, mvs, wle_name, irtmodel) {

  # Test data
  check_variables(resp, "resp", facet)
  ckeck_numerics(vars, "vars", scoring)

  if (is.null(facet)) {
    stop("Please provide the facet indicating the testlet rotation.")
  }
  if (is.null(xsi_fixed)) {
    stop("Please provide the item parameters to ensure the correct",
       " results in the WLE estimation.")
  }

  # Prepare data
  facet <- resp[resp[[valid]], facet, drop = FALSE]
  pid <- resp$ID_t[resp[[valid]]]
  check_pid(pid)
  resp_ <- prepare_resp(resp, vars, select, use_only_valid = TRUE,
                        valid = valid, convert = TRUE, mvs = mvs,
                        warn = warn)

  # Test resp
  check_numerics(resp, "resp", check_invalid = TRUE)

  # Conduct analyses
  frmA <- as.formula(paste0("~ item + ",
                ifelse(irtmodel == "PCM2", "item*step +", ""),
                names(facet)))

  if (irtmodel == "PCM2") {
    # get design matrix for model with 0.5 scoring
    B <- TAM::designMatrices(modeltype = "PCM", resp = resp_)$B
    B[vars$item[vars[[select]]], , 1] <-
      B[vars$item[vars[[select]]], , 1] * vars[[scoring]][vars[[select]]]
  } else {
    B <- NULL
  }

  mod <- TAM::tam.mml.mfr(
    resp = resp_, facets = facet, xsi.fixed = xsi_fixed,
    verbose = FALSE, pid = pid, formulaA = frmA, irtmodel = irtmodel,
    B = B
  )

  reached_maxiter(mod, "")

  wles <- TAM::tam.wle(mod, progress = FALSE)[, c("pid", "theta", "error")]
  names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
  return(wles)
}


#' Set labels of items in SUF
#'
#' @param suf data.frame; SUF to be (re)labelled
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param items_suf character; indicates the logical variable in vars which
#'   contains the item names designated for the SUF (original scored items
#'   before the first IRT analyses)
#' @param competence character; name of tested competence for labelling the
#'   data (capitalized!)
#' @param wle_name character; name of the wle -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param linked logical; whether the test is linked to a previous assessment
#' @return suf with labels
#' @importFrom stats setNames
#' @export
set_labels <- function(suf, vars, items_suf, competence, wle_name, linked) {
  attr(suf$ID_t, "label") <- "Unique person identifier"
  j <- 1
  for (i in vars$item[vars[[items_suf]]]) {
    # value labels
    # dichotomous items
    if (max(suf[[i]], na.rm = TRUE) == 1) {
      suf[[i]] <- haven::labelled_spss(suf[[i]],
                       label = paste0(competence, ": Item ", j),
                       labels = c("incorrect" = 0, "correct" = 1,
                            "omitted" = -97, "not valid" = -95,
                            "not reached" = -94, "test aborted" = -91,
                            "unspecific missing" = -90,
                            "not determinable" = -55,
                            "not administered" = -54,
                            "reset response" = -21))
      # polytomous items
    } else {
      maxK <- max(suf[[i]], na.rm = TRUE)
      lbl <- setNames(object = seq(0, maxK),
              paste0(seq(0, maxK), " correct of ", maxK))
      suf[[i]] <- haven::labelled_spss(suf[[i]],
                       label = paste0(competence, ": Item ", j),
                       labels = c(lbl,
                            "omitted" = -97, "not valid" = -95,
                            "not reached" = -94, "test aborted" = -91,
                            "unspecific missing" = -90,
                            "not determinable" = -55,
                            "not administered" = -54,
                            "reset response" = -21))
    }
    j <- j + 1
  }
  suf[[paste0(wle_name, "_sc1")]] <- haven::labelled_spss(
    suf[[paste0(wle_name, "_sc1")]],
    label = paste0(competence, ": cross-sectional WLE"),
    labels = c("omitted" = -97, "not valid" = -95,
           "not reached" = -94, "test aborted" = -91,
           "unspecific missing" = -90, "not administered" = -54,
           "reset response" = -21)
  )
  suf[[paste0(wle_name, "_sc2")]] <- haven::labelled_spss(
    suf[[paste0(wle_name, "_sc2")]],
    label = paste0(competence, ": standard error of cross-sectional WLE"),
    labels = c("omitted" = -97, "not valid" = -95,
           "not reached" = -94, "test aborted" = -91,
           "unspecific missing" = -90, "not administered" = -54,
           "reset response" = -21)
  )
  if (linked) {
    suf[[paste0(wle_name, "_sc1u")]] <- haven::labelled_spss(
      suf[[paste0(wle_name, "_sc1u")]],
      label = paste0(competence, ": longitudinal WLE"),
      labels = c("omitted" = -97, "not valid" = -95,
             "not reached" = -94, "test aborted" = -91,
             "unspecific missing" = -90, "not administered" = -54,
             "reset response" = -21)
    )
    suf[[paste0(wle_name, "_sc2u")]] <- haven::labelled_spss(
      suf[[paste0(wle_name, "_sc2u")]],
      label = paste0(competence, ": standard error of longitudinal WLE"),
      labels = c("omitted" = -97, "not valid" = -95,
             "not reached" = -94, "test aborted" = -91,
             "unspecific missing" = -90, "not administered" = -54,
             "reset response" = -21)
    )
  }
  return(suf)
}

#' Save SUF in three formats
#'
#' @param suf data.frame; SUF to be saved
#' @param filename character; indicates final name of SUF according to study
#' @param path_results character; indicates where SUF shall be saved
#' @noRd
save_suf <- function(suf, path_results, filename) {
  saveRDS(suf, file = paste0(path_results, "/", filename, ".rds"))
  haven::write_dta(suf, path = paste0(path_results, "/", filename, ".dta"))
  haven::write_sav(suf, path = paste0(path_results, "/", filename, ".sav"))
}


#' create sum scores
#'
#' Sum scores for PC items are only counted as 1 if all subitems are solved
#'   correctly
#'
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
#' @return integer vector of length nrow(resp) containing the row sums for the
#'   specified item set
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @export
sum_scores <- function(resp, vars, select, warn = TRUE) {
  # Test data
  check_logicals(vars, "vars", select, warn = warn)
  items <- vars$item[vars[[select]]]
  check_numerics(resp, "resp", items, check_invalid = TRUE)
  # count only correctly scored binary or FULLY correctly scored PC items
  resp[, items] <- lapply(items, function(x) {
                          ifelse(resp[, x] == vars$max[x],
                               1, NA)})
  rs <- data.frame(
    ID_t = resp$ID_t,
    sumscore = rowSums(resp[, items], na.rm = TRUE)
  )
  return(rs)
}
