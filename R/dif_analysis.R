#' DIF analyses - all in one function
#'
#' Testing for differential item functioning for binary and polytomous data.
#' Main effects and DIF effects models are estimated.
#'
#' @param resp data.frame containing the responses. y in {0, 1} for binary data
#'   and y in {0, 1, ... k-1} for polytomous responses with k categories.
#'   Missing values (-97 to -21) are coded as NA internally. Also contains a
#'   column with the DIF variable.
#' @param vars data.frame with all variables as rows with at least following columns:
#'   items: contains all item names (both scored and unscored)
#'   further custom-named columns indicating the items to select and the item
#'   scoring via logicals
#' @param items character. contains name of variable (boolean) in vars that
#'   indicates which items to use for analysis. If some of the \code{dif_vars}
#'   come with a different set of analysis items, this argument becomes a
#'   vector of \code{length(dif_vars)} containing the respective selection
#'   variables in vars
#' @param dif_vars character vector. contains the variable names to be tested for DIF
#'   (e.g., "gender")
#' @param valid character string. defines name of boolean variable in dat,
#'   indicating (in)valid cases.
#' @param scoring character string; refers to the scoring variable in vars.
#'   Defaults to "scoring".
#' @param overwrite_table boolean; indicates whether to overwrite existing file when saving table.
#' @param print logical indicating whether summary is printed to the console
#' @param save logical indicating whether summary / data is stored on hard disk
#' @param verbose logical; should progress be printed to console?
#' @param return_results  boolean. indicates whether to return results.
#' @param path character vector; indicates the folder locations where the
#'   resulting summaries (first element of vector) and data (second element of
#'   vector) is stored on the hard drive. Please note that the
#'   path is relative to the current working path set by here::i_am()
#' @param ... additional arguments to be passed to tam.mml
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export

dif_analysis <- function(resp, vars, items, dif_vars, valid = NULL,
                         scoring = "scoring", overwrite_table = FALSE,
                         print = TRUE, verbose = FALSE, save = TRUE,
                         return_results = FALSE,
                         save = TRUE, path = c("Tables", "Results"),
                         ...) {

  path_table <- path[1]
  path_results <- path[2]

  res <- conduct_dif_analysis_summary(
    items, dif_vars, resp, vars, scoring, valid, verbose, path_table,
    path_results, print, save, overwrite_table
  )

  build_dif_tables(dif_summaries = res$dif_summaries, save = save,
                   path_table = path_table, overwrite = overwrite_table)

  if (return_results) {
    return(res)
  }
}


conduct_dif_analysis_summary <- function(items, dif_vars, resp, vars, scoring,
                                         valid, verbose, path_table,
                                         path_results, print, save,
                                         overwrite_table) {

  dif_models <- list()
  dif_summaries <- list()

  if (length(items) > 1 & length(items) != length(dif_vars)) {
    stop("Please check 'items' and 'dif_vars'. At least one of them does ",
         "not match the intended analysis.")
  }
  if (length(items) == 1) {
    items <- rep(items, length(dif_vars))
  }

  for (i in seq_along(dif_vars)) {

    dif_models[[i]] <- dif_model(resp = resp, vars = vars, items = items[i],
                                 facets = dif_vars[i], scoring = scoring,
                                 valid = valid, verbose = verbose)

    dif_summaries[[i]] <- dif_summary(dif_models[[i]], print = print,
                                      save = save, path_table = path_table,
                                      path_results = path_results,
                                      overwrite = overwrite_table)
  }
  names(dif_summaries) <- names(dif_models) <- dif_vars

  return(list(dif_summaries = dif_summaries, dif_models = dif_models))
}



#' DIF analyses
#'
#' Testing for differential item functioning for binary and polytomous data.
#' Main effects and DIF effects models are estimated.
#'
#' @param resp data.frame containing the responses. y in {0, 1} for binary data
#'   and y in {0, 1, ... k-1} for polytomous responses with k categories.
#'   Missing values (-97 to -21) are coded as NA internally. Also contains a
#'   column with the DIF variable.
#' @param vars data.frame with all variables as rows with at least following columns:
#'   items: contains all item names (both scored and unscored)
#'   further custom-named columns indicating the items to select and the item
#'   scoring via logicals
#' @param items character. contains name of variable (boolean) in vars that
#'   indicates which items to use for analysis.
#' @param scoring character string; refers to the scoring variable in vars.
#'   Defaults to "scoring".
#' @param facets character string signifying the variable to be tested for DIF
#'   (e.g., "gender")
#' @param scoring numeric vector; scoring factor to be applied to loading matrix;
#'   can be NULL for Rasch model
#' @param valid character string. defines name of boolean variable in data,
#'   indicating (in)valid cases.
#' @param verbose logical; should progress be printed to console?
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @importFrom stats as.formula
#' @export

dif_model <- function(resp, vars, items, facets, scoring = "scoring",
                      valid = NULL, verbose = FALSE) {

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
                       without_valid = TRUE)
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
#' @param resp data.frame containing the responses. y in {0, 1} for binary data
#'   and y in {0, 1, ... k-1} for polytomous responses with k categories.
#'   Missing values are coded as NA.
#' @param facets data.frame of one column for variable to be tested for DIF;
#'   column is named after DIF variable (e.g., "gender"); must contain the same
#'   persons in the same order as resp
#' @param formulaA an R formula for the DIF analysis
#' @param vars data.frame with all variables as rows with at least following columns:
#'   items: contains all item names (both scored and unscored)
#'   scoring: numeric; scoring of IRT items;
#'   ???: logical, can be named arbitrarily and needs to contain the variable
#'   names for the current estimation
#' @param select character. contains name of variable (boolean) in vars that
#'   indicates which items to use for analysis.
#' @param scoring character string; refers to the scoring variable in vars.
#'   Defaults to "scoring".
#' @param verbose logical; should progress be printed to console?
#' @param valid character string. defines name of boolean variable in dat,
#'   indicating (in)valid cases.
#' @param pid vector with person identifiers
#'
#' @return a tam.mml model
#' @noRd

pcm_dif <- function(resp, facets, formulaA, vars, select, scoring = "scoring", valid = NULL, pid,
                    verbose) {

  # Select only valid cases and convert mvs
  if (!is.null(valid) && valid %in% names(resp)) {
    resp <- convert_mv(resp = only_valid(resp, valid = valid), mvs = -999:-20)
  } else {
    message("The response matrix is used as is. Please check yourself whether ",
            "all cases are valid or provide a variable within the response ",
            "matrix for pcm_dif() to check.")
  }

  # Create ID and facets variable
  check_pid(pid)

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
#' @param diflist list with main and dif model as returned by dif_model()
#' @param print logical indicating whether summary is printed to the console
#' @param save_at character string; indicates the folder location where the
#'   summaries are stored on the hard drive. Please note that the
#'   path is relative to the current working path set by here::i_am(). Defaults
#'   to NULL (not stored)
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @export

dif_summary <- function(diflist, print = TRUE, save = TRUE,
                        path_table = "Tables", path_results = "Results",
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
    print_dif_summary(diflist, res)
  }

  if (save) {
    save_dif_summary(path_table, path_results, diflist, res, overwrite)
  }

  return(res)
}



#' Summarizes DIF effects
#'
#' @param obj results from DIF analyses for criterion as returned by dif_model
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
#' elements must be named after their DIF variable
#' @param save logical indicator whether the results are to be written to the
#'   hard disk
#' @param path_table character string; indicates the folder location where the
#'   summaries are stored on the hard drive. Please note that the
#'   path is relative to the current working path set by here::i_am()
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#'
#' @export
build_dif_tables <- function(dif_summaries, save = TRUE, path_table, #path_results,
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
    # save to hard drive
    check_folder(path_table)
    # check_folder(path_results)

    # save(gof, est,
    #      file = here::here(paste0(path_results, "/dif_all_summary.Rdata")))

    openxlsx::write.xlsx(
      gof,
      file = here::here(paste0(path_table, "/dif_all_goodness_of_fit.xlsx")),
      showNA = FALSE, overwrite = overwrite
    )

    openxlsx::write.xlsx(
      est,
      file = here::here(paste0(path_table, "/dif_all_estimates.xlsx")),
      showNA = FALSE, overwrite = overwrite
    )
  }
}



print_dif_summary <- function(diflist, res) {
  # information criteria table
  message("Information criteria regarding the ", diflist$dif_var,
          " DIF analysis:")
  print(res$gof)
  # main effects table
  message("\nMain effects of DIF and main effects model for ",
          diflist$dif_var, ":")
  print(res$mne)
  # problematic dif values (significant p-value, larger than 0.5 logits)
  message("\nItems exhibiting problematic DIF for ", diflist$dif_var,
          " (|xsi| >= 0.5 and p < 0.05):")
  print(res$est[abs(res$est$xsi) >= 0.5 & res$est$p < 0.05, ])
}



save_dif_summary <- function(path_table, path_results, diflist, res,
                             overwrite) {
  check_folder(path_table)
  check_folder(path_results)

  save(diflist, file = here::here(paste0(path_results, "/dif_",
                                         diflist$dif_var, ".Rdata")))
  save(res, file = here::here(paste0(path_table, "/dif_",
                                     diflist$dif_var, "_summary.Rdata")))
  openxlsx::write.xlsx(
    res$gof,
    file = here::here(paste0(path_table, "/dif_",
                             diflist$dif_var, "_goodness_of_fit.xlsx")),
    showNA = FALSE, overwrite = overwrite
  )
  openxlsx::write.xlsx(
    res$est,
    file = here::here(paste0(path_table, "/dif_",
                             diflist$dif_var, "_dif_estimates.xlsx")),
    showNA = FALSE, overwrite = overwrite
  )
  openxlsx::write.xlsx(
    res$mne,
    file = here::here(paste0(path_table, "/dif_",
                             diflist$dif_var, "_main_effects.xlsx")),
    showNA = FALSE, overwrite = overwrite
  )
}
