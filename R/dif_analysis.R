#' DIF analyses - all in one function
#'
#' Testing for differential item functioning for binary and polytomous data.
#' Main effects and DIF effects models are estimated.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items string; defines name of logical variable in vars that indicates
#' which items to use for the analysis; if some of the \code{dif_vars}
#' come with a different set of analysis items, this argument becomes a
#' vector of \code{length(dif_vars)} containing the respective selection
#' variables in vars
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param dif_vars character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param scoring string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param verbose  logical; whether to print processing information to console

#' @param ... additional arguments to be passed to tam.mml
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export

dif_analysis <- function(resp, vars, items, dif_vars, valid = NULL, mvs = NULL,
                         scoring = "scoring", overwrite = FALSE, save = TRUE,
                         print = TRUE, return = FALSE, verbose = FALSE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         ...) {

  check_items(items, dif_vars)

  dif <- list()

  dif$models <- conduct_dif_analysis(
    items = items, dif_vars = dif_vars, resp = resp, vars = vars,
    scoring = scoring, valid = valid, mvs = mvs, verbose = verbose
  )

  dif$summaries <- summarize_dif_analysis(
    dif_models = dif$models, dif_vars = dif_vars,
    path_table = path_table, path_results = path_results,
    print = print, save = save, overwrite = overwrite
  )

  build_dif_tr_tables(dif_summaries = dif$summaries, save = save,
                      path_table = path_table, overwrite = overwrite)

  if (return) return (dif)
}

check_items <- function(items, dif_vars) {
  if (length(items) > 1 & length(items) != length(dif_vars)) {
    stop("Please check 'items' and 'dif_vars'. At least one of them does ",
         "not match the intended analysis.")
  }
}


conduct_dif_analysis <- function(items, dif_vars, resp, vars, scoring,
                                 valid, mvs, verbose) {

  dif_models <- list()

  if (length(items) == 1) {
    items <- rep(items, length(dif_vars))
  }

  for (i in seq_along(dif_vars)) {
    dif_models[[i]] <- dif_model(resp = resp, vars = vars, items = items[i],
                                 facets = dif_vars[i], scoring = scoring,
                                 valid = valid, verbose = verbose)
  }
  names(dif_models) <- dif_vars

  return(dif_models)
}


summarize_dif_analysis <- function(dif_models, dif_vars, path_table,
                                   path_results, print, save, overwrite) {

  dif_summaries <- list()

  for (i in seq_along(dif_vars)) {

    dif_summaries[[i]] <- dif_summary(dif_models[[i]], print = print,
                                      save = save, path_table = path_table,
                                      path_results = path_results,
                                      overwrite = overwrite)
  }
  names(dif_summaries) <- dif_vars

  return(dif_summaries)
}



#' DIF analyses
#'
#' Testing for differential item functioning for binary and polytomous data.
#' Main effects and DIF effects models are estimated.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items string; defines name of logical variable in vars that indicates
#' which items to use for the analysis; if some of the \code{dif_vars}
#' come with a different set of analysis items, this argument becomes a
#' vector of \code{length(dif_vars)} containing the respective selection
#' variables in vars
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param facets string; defines the name of the variable to be tested for DIF
#' (e.g., "gender")
#' @param verbose  logical; whether to print processing information to console
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @importFrom stats as.formula
#' @export

dif_model <- function(resp, vars, items, facets, scoring = "scoring",
                      valid = NULL, mvs = NULL, verbose = FALSE) {

  # Select only valid cases
  resp <- only_valid(resp, valid = valid)

  # Create ID and facets variable
  pid <- resp$ID_t
  check_pid(pid)
  facets <- resp[, facets, drop = FALSE]

  # Select only indicated items and convert mvs
  # add default MVs message here once instead of every time, convert_mv is
  # called
  resp <- prepare_resp(resp, vars = vars, items = items, convert = TRUE,
                       mvs = mvs)
  message(names(facets),
          ": User-defined missing values (-999 to -20) converted to NA.")

  # Prepare DIF analysis
  is_pcm <- any(apply(resp, 2, max, na.rm = TRUE) > 1)
  dif_var <- colnames(facets)
  tmp_formula <- paste("~ item +", ifelse(is_pcm, "item * step +", ""))
  formula_dmod <- as.formula(paste(tmp_formula, "item *", dif_var))
  formula_mmod <- as.formula(paste(tmp_formula, dif_var))
  rm(tmp_formula)
  mis <- is.na(facets[[dif_var]])

  if (any(mis)) {
    facets <- facets[!mis, ]
    resp <- resp[!mis, ]
    pid <- pid[!mis]
    warning("Missing values were found in the DIF variable. The corresponding",
            " cases have been excluded from the analysis.\n")
  }

  # DIF analysis
  if (is_pcm) {
    mmod <- pcm_dif(
      resp = resp, facets = facets, formulaA = formula_mmod, pid = pid,
      vars = vars, select = items, scoring = scoring, verbose = verbose
    )
    dmod <- pcm_dif(
      resp = resp, facets = facets, formulaA = formula_dmod, pid = pid,
      vars = vars, select = items, scoring = scoring, verbose = verbose
    )
  } else {
    Q <- as.matrix(vars$scoring[vars[[items]]])
    dmod <- TAM::tam.mml.mfr(resp,
                             irtmodel = "1PL", facets = facets, Q = Q, pid = pid,
                             formulaA = formula_dmod, verbose = verbose
    )
    mmod <- TAM::tam.mml.mfr(resp,
                             irtmodel = "1PL", facets = facets, Q = Q, pid = pid,
                             formulaA = formula_mmod, verbose = verbose
    )
  }

  list(mmod = mmod, dmod = dmod, dif_var = dif_var)
}

#' DIF analyses for PCM model
#'
#' Testing for differential item functioning for polytomous data.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param facets  data.frame of one column for variable to be tested for DIF;
#'   column is named after DIF variable (e.g., "gender"); must contain the same
#'   persons in the same order as resp
#' @param formulaA  an R formula for the DIF analysis
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param verbose  logical; whether to print processing information to console
#' @param pid  character vector; contains person identifiers
#'
#' @return a tam.mml model
#' @noRd

pcm_dif <- function(resp, facets, formulaA, vars, select, scoring = "scoring",
                    pid, verbose) {

  # get design matrix for model
  B <- TAM::designMatrices(modeltype = "PCM", resp = resp)$B

  # 0.5 scoring for PCM
  B[vars$items[vars[[select]]], , 1] <-
    B[vars$items[vars[[select]]], , 1] * vars[[scoring]][vars[[select]]]

  TAM::tam.mml.mfr(formulaA = formulaA, facets = facets, B = B, pid = pid,
                   irtmodel = "PCM2", resp = resp, verbose = verbose)

}


#' Summary for DIF analysis
#'
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param print logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @export

dif_summary <- function(diflist, print = TRUE, save = TRUE,
                        path_results = here::here('Results'),
                        path_table = here::here('Tables'),
                        overwrite = FALSE) {
  # information criteria for DIF and main model
  # main effects of main and DIF model + standardized
  # DIF per item + standard error + meht p-values

  group <- grep(diflist$dif_var, rownames(diflist$dmod$xsi), value = TRUE)
  group <- unique(sapply(group, function(x) {substr(x, nchar(x), nchar(x))}))
  group <- as.integer(group)
  res <- difsum(obj = diflist, facet = diflist$dif_var, group = group,
                group2 = NULL)
  res$compared_against <- group

  if (print) {
    print_dif_summary(diflist = diflist, res = res)
  }

  if (save) {
    save_dif_summary(diflist = diflist, res = res, overwrite = overwrite,
                     path_results = path_results, path_table = path_table)
  }

  return(res)
}



#' Summarizes DIF effects
#'
#' @param obj list; return object of dif_model()
#' @param facet character string; dif variable
#' @param group integer; group to compare against
#' @param group2 integer; in case of more than two groups
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @importFrom stats deviance
#' @noRd

difsum <- function(obj, facet, group = 1, group2 = NULL) {

  # all included items
  it <- colnames(obj$dmod$resp_orig)
  if (is.null(it)) {
    s <- grepl(paste0("^(.+)-", facet, group),
               rownames(obj$dmod$B))
    it <- gsub(paste0("-", facet, group), "",
               rownames(obj$dmod$B)[s])
  }

  # DIF effects for first group
  sel <- rownames(obj$dmod$xsi) %in%
    paste0(it, ":", facet, group)
  est <- obj$dmod$xsi[sel, ]
  est$item <- gsub(paste0(":", facet, group), "", rownames(est))

  # calculate DIF effect for last item (= constrained for identification)
  lst <- data.frame(item = it[!(it %in% est$item)],
                    xsi = sum(est$xsi),
                    se.xsi = sqrt(sum(est$se.xsi^2)))
  est <- rbind(est, lst)
  rownames(est) <- NULL

  # DIF effects for second group
  # (only relevant if more than 2 groups were used as DIF variable)
  if (!is.null(group2)) {
    sel <- rownames(obj$dmod$xsi) %in%
      paste0(it, ":", facet, group2)
    est2 <- obj$dmod$xsi[sel, ]
    est2$item <- gsub(paste0(":", facet, group2), "", rownames(est2))

    # calculate DIF effect for last item (= constrained for identification)
    lst <- data.frame(item = it[!(it %in% est2$item)],
                      xsi = sum(est2$xsi),
                      se.xsi = sqrt(sum(est2$se.xsi^2)))
    est2 <- rbind(est2, lst)
    rownames(est2) <- NULL
  }

  # difference in item parameters
  if (is.null(group2)) {
    est$xsi <- 2 * est$xsi
    est$se.xsi2 <- est$se.xsi
  } else {
    est$xsi <- est$xsi - est2$xsi
    est$se.xsi2 <- est2$se.xsi
  }

  # standardized difference
  est$std <- est$xsi / sqrt(obj$dmod$variance[1])

  # minimum effects hypothesis test
  est$p <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    round(meht((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2,
               df1 = 1, df2 = obj$dmod$nstud - 2, verbose = FALSE)$pmin, 3)
  })
  est$Fkrit <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    round(meht((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2,
               df1 = 1, df2 = obj$dmod$nstud - 2, verbose = FALSE)$Fmin, 3)
  })
  est$F <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    round((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2, 3)
  })

  # reorder
  out <- list(est = est[, c("item", "xsi", "std", "F", "Fkrit", "p")])

  # main effects
  est <- obj$dmod$xsi[rownames(obj$dmod$xsi) == paste0(facet, group), ]
  mne <- c(2 * est$xsi[1],
           2 * est$xsi[1] / sqrt(obj$dmod$variance[1]))
  est <- obj$mmod$xsi[rownames(obj$mmod$xsi) == paste0(facet, group), ]
  mne <- rbind(mne,
               c(2 * est$xsi[1],
                 2 * est$xsi[1] / sqrt(obj$mmod$variance[1])))
  colnames(mne) <- c("Unstandardized", "Standardized")
  rownames(mne) <- c("DIF model", "Main effects model")

  # main effects refer to item difficulties
  #  -> recode to person main effects
  out$mne <- -1 * mne

  # goodness-of-fit indices
  gof <- data.frame(
    `DIF variable` = facet,
    Model = c("Main effect", "DIF"),
    N = c(obj$mmod$nstud, obj$dmod$nstud),
    Deviance = c(deviance(obj$mmod), deviance(obj$dmod)),
    `Number of parameters` = c(obj$mmod$ic$Npars, obj$dmod$ic$Npars),
    AIC = c(AIC(obj$mmod), AIC(obj$dmod)),
    BIC = c(BIC(obj$mmod), BIC(obj$dmod)))
  out$gof <- gof

  out
}


#' Combine individual DIF results
#'
#' Combine the individual DIF analysis results to excel tables that can be
#' used for the technical reports
#'
#' @param dif_summaries named list of dif_summary() return objects; the list
#'   elements must be named after their DIF variable
#' @param save logical; whether results shall be saved to hard drive
#' @param path_table string; indicates the folder location where the summaries
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @export

build_dif_tr_tables <- function(dif_summaries, save = TRUE,
                                path_results = here::here('Results'),
                                path_table = here::here('Tables'),
                                overwrite = FALSE) {

  dif_vars <- names(dif_summaries)

  # information criteria table
  gof <- Reduce(rbind, lapply(dif_summaries, function(x) x$gof))

  # DIF effects table + main effects
  est <- lapply(dif_summaries, function(x) x$est)
  est <- lapply(dif_vars, function(x) {
    est[[x]][[x]] <- paste0(round(est[[x]]$xsi, 2), " (",
                            round(est[[x]]$std, 2), ")")
    est[[x]][, c("item", x)]
  })
  est <- Reduce(function(e1, e2) {dplyr::full_join(e1, e2, by = "item")}, est)

  mne <- lapply(dif_summaries, function(x) {
    m <- as.data.frame(x$mne)
    m$item <- c("Main effect (DIF model)", "Main effect (Main effect model)")
    m
  })
  mne <- lapply(dif_vars, function(x) {
    mne[[x]][[x]] <- paste0(round(mne[[x]]$Unstandardized, 2), " (",
                            round(mne[[x]]$Standardized, 2), ")")
    mne[[x]][, c("item", x)]
  })
  mne <- Reduce(function(m1, m2) {dplyr::full_join(m1, m2, by = "item")}, mne)

  est <- rbind(est, mne)

  if (save) {
    save_dif_tr_tables(gof = gof, est = est, overwrite = overwrite,
                       path_results = path_results, path_table = path_table)
  }
}


save_dif_tr_tables <- function(gof, est, path_results, path_table, overwrite) {

  dif_tr_tables <- list(gof = gof, estimates = est)
  save_results(dif_tr_tables, filename = "dif_tr_tables.Rdata", path = path_results)
  save_table(dif_tr_tables, filename = "dif_tr_tables.xlsx", path = path_table,
             overwrite = overwrite)
}



print_dif_summary <- function(diflist, res) {
  # information criteria table
  message("Information criteria regarding the ", diflist$dif_var,
          " DIF analysis:")
  print(res$gof)
  # main effects table
  message("\nMain effects of DIF and main effects model for ",
          diflist$dif_var, " (compared against: ", res$compared_against, "):")
  print(res$mne)
  # problematic dif values (significant p-value, larger than 0.5 logits)
  message("\nItems exhibiting problematic DIF for ", diflist$dif_var,
          " (|xsi| >= 0.5 and p < 0.05):")
  print(res$est[abs(res$est$xsi) >= 0.5 & res$est$p < 0.05, ])
}



save_dif_summary <- function(diflist, res, path_results, path_table,
                             overwrite) {

  # Save results
  dif_results <- list(diflist = diflist, summary = res)
  save_results(dif, filename = "dif_results.Rdata", path = path_results)

  # Save table
  dif_table <- list(gof = res$gof, estimates = res$est, main_effects = res$mne)
  save_table(dif_table, filename = "dif_results.xlsx", path = path_table,
             overwrite = overwrite)
}
