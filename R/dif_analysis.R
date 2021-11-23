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
#' @param items character vector; defines name(s) of logical variable(s) in vars
#' that indicates which items to use for analysis; if some of the \code{dif_vars}
#' come with a different set of analysis items, this argument becomes a
#' vector of \code{length(dif_vars)} containing the respective selection
#' variables in vars
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param mvs named integer vector; contains user-defined missing values
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
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export

dif_analysis <- function(resp, vars, items, dif_vars, valid = NULL, mvs = NULL,
                         scoring = "scoring", overwrite = FALSE, save = TRUE,
                         print = TRUE, return = FALSE, verbose = FALSE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables')) {

  check_items(items, dif_vars)

  dif <- list()

  dif$models <- conduct_dif_analysis(
    items = items, dif_vars = dif_vars, resp = resp, vars = vars,
    scoring = scoring, valid = valid, save = save, path = path_results,
    mvs = mvs, verbose = verbose
  )

  dif$summaries <- summarize_dif_analysis(
    dif_models = dif$models, dif_vars = dif_vars,
    path_table = path_table, path_results = path_results,
    print = print, save = save, overwrite = overwrite
  )

  dif$tr_tables <- build_dif_tr_tables(dif_summaries = dif$summaries, save = save,
                                       path = path_table, overwrite = overwrite)

  if (return) return(dif)
}

check_items <- function(items, dif_vars) {
  if (length(items) > 1 & length(items) != length(dif_vars)) {
    stop("Please check 'items' and 'dif_vars'. At least one of them does ",
         "not match the intended analysis.")
  }
}

#' Conduct DIF analyses
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
#' @param items character vector; defines name(s) of logical variable(s) in vars
#' that indicates which items to use for analysis; if some of the \code{dif_vars}
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
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param verbose  logical; whether to print processing information to console
#' @param mvs named integer vector; contains user-defined missing values
#'
#' @return a list of length(dif_vars) lists containing each:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export
conduct_dif_analysis <- function(resp, vars, items, dif_vars, valid, scoring,
                                 mvs = NULL, save = TRUE, verbose = FALSE,
                                 path = here::here(('Results'))) {

  dif_models <- list()

  if (length(items) == 1) {
    items <- rep(items, length(dif_vars))
  }

  for (i in seq_along(dif_vars)) {
    dif_models[[i]] <- dif_model(resp = resp, vars = vars, items = items[i],
                                 dif_var = dif_vars[i], scoring = scoring,
                                 valid = valid, verbose = verbose,
                                 mvs = mvs)
  }
  names(dif_models) <- dif_vars

  if (save) {
      save_results(dif_models, filename = "dif_models.Rdata", path = path)
  }

  return(dif_models)
}

#' summarize DIF analysis
#'
#' @param dif_models return object of conduct_dif_analysis()
#' @param dif_vars character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @returns a list of dif summaries for each input entry in dif_models
#' @export
summarize_dif_analysis <- function(dif_models, dif_vars,
                                   print = TRUE, save = TRUE, overwrite = FALSE,
                                   path_results = here::here('Results'),
                                   path_table = here::here('Tables')) {

  dif_summaries <- list()

  for (i in dif_vars) {

    dif_summaries[[i]] <- dif_summary(dif_models[[i]], print = print,
                                      save = save, path = path_table,
                                      overwrite = overwrite)
  }

  if (save) {
      save_results(dif_models, filename = "dif_summaries.Rdata", path = path_results)
  }

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
#' @param mvs named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param dif_var string; defines the name of the variable to be tested for DIF
#' (e.g., "gender")
#' @param verbose  logical; whether to print processing information to console
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @importFrom stats as.formula
#' @export
dif_model <- function(resp, vars, items, dif_var, scoring = 'scoring',
                      valid = NULL, mvs = NULL, verbose = FALSE) {

  # Select only valid cases
  resp <- only_valid(resp, valid = valid)

  # Create ID and facets variable
  pid <- resp$ID_t
  check_pid(pid)
  facets <- resp[, dif_var, drop = FALSE]

  # Select only indicated items, valid responders and convert mvs
  resp <- prepare_resp(resp, vars = vars, items = items, convert = TRUE,
                       mvs = mvs, warn = FALSE, use_only_valid = TRUE,
                       valid = valid)
  message(names(facets),
          ": User-defined missing values (-999 to -1) converted to NA.")

  # Prepare DIF analysis
  is_pcm <- any(apply(resp, 2, max, na.rm = TRUE) > 1)
  tmp_formula <- paste("~ item +", ifelse(is_pcm, "item * step +", ""))
  formula_dmod <- as.formula(paste(tmp_formula, "item *", dif_var))
  formula_mmod <- as.formula(paste(tmp_formula, dif_var))
  rm(tmp_formula)
  mis <- is.na(facets[[dif_var]])

  if (any(mis)) {
    facets <- facets[!mis, , drop = FALSE]
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
    Q <- as.matrix(vars[[scoring]][vars[[items]]])
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
#' @param path  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @export

dif_summary <- function(diflist, print = TRUE, save = TRUE,
                        path = here::here('Tables'),
                        overwrite = FALSE) {
  # information criteria for DIF and main model
  # main effects of main and DIF model + standardized
  # DIF per item + standard error + meht p-values

  dif_var <- diflist$dif_var
  group <- grep(diflist$dif_var, rownames(diflist$dmod$xsi), value = TRUE)
  group <- unique(sapply(group, function(x) {substr(x, nchar(x), nchar(x))}))
  group <- as.integer(group)
  res <- difsum(obj = diflist, dif_var = dif_var, group = group, group2 = NULL)
  res$compared_against <- group

  if (print) {
    print_dif_summary(diflist = diflist, res = res)
  }

  if (save) {
    save_table(res, filename = paste0("dif_results_", dif_var, ".xlsx"),
               path = path, overwrite = overwrite, show_rownames = FALSE)
  }

  return(res)
}



#' Summarizes DIF effects
#'
#' @param obj list; return object of dif_model()
#' @param dif_var character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param group integer; group to compare against
#' @param group2 integer; in case of more than two groups
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @importFrom stats deviance
#' @noRd

difsum <- function(obj, dif_var, group = 1, group2 = NULL) {

  # all included items
  it <- colnames(obj$dmod$resp_orig)
  if (is.null(it)) {
    s <- grepl(paste0("^(.+)-", dif_var, group),
               rownames(obj$dmod$B))
    it <- gsub(paste0("-", dif_var, group), "",
               rownames(obj$dmod$B)[s])
  }

  # DIF effects for first group
  sel <- rownames(obj$dmod$xsi) %in%
    paste0(it, ":", dif_var, group)
  est <- obj$dmod$xsi[sel, ]
  est$item <- gsub(paste0(":", dif_var, group), "", rownames(est))

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
      paste0(it, ":", dif_var, group2)
    est2 <- obj$dmod$xsi[sel, ]
    est2$item <- gsub(paste0(":", dif_var, group2), "", rownames(est2))

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
  mne <- data.frame(Model = c("DIF model", "Main effects model"),
                    Unstandardized = rep(NA, 2),
                    Standardized = rep(NA, 2))
  est <- obj$dmod$xsi[rownames(obj$dmod$xsi) == paste0(dif_var, group), ]
  mne[1, 2:3] <- c(2 * est$xsi[1],
                   2 * est$xsi[1] / sqrt(obj$dmod$variance[1]))
  est <- obj$mmod$xsi[rownames(obj$mmod$xsi) == paste0(dif_var, group), ]
  mne[2, 2:3] <- c(2 * est$xsi[1],
                   2 * est$xsi[1] / sqrt(obj$mmod$variance[1]))

  # main effects refer to item difficulties
  #  -> recode to person main effects
  out$mne <- mne
  out$mne[, 2:3] <- -1 * out$mne[, 2:3]

  # goodness-of-fit indices
  gof <- data.frame(
    `DIF variable` = dif_var,
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
#' @param path string; indicates the folder location where the summaries
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return table as shown in TR
#' @export

build_dif_tr_tables <- function(dif_summaries, save = TRUE,
                                path = here::here('Tables'),
                                overwrite = FALSE) {

  dif_vars <- names(dif_summaries)

  # information criteria table
  gof <- Reduce(rbind, lapply(dif_summaries, function(x) x$gof))
  gof[,-c(1:2)] <- round(gof[, -c(1:2)])

  # DIF effects table + main effects
  est <- lapply(dif_summaries, function(x) x$est)
  est <- lapply(dif_vars, function(x) {
    est[[x]][[x]] <- paste0(format(round(est[[x]]$xsi, 3), nsmall = 3), " (",
                            format(round(est[[x]]$std, 3), nsmall = 3), ")")
    est[[x]][, c("item", x)]
  })
  est <- Reduce(function(e1, e2) {dplyr::full_join(e1, e2, by = "item")}, est)

  mne <- lapply(dif_summaries, function(x) {
    m <- rename(x$mne, item = 'Model')
    m$item <- c("Main effect (DIF model)", "Main effect (Main effect model)")
    m
  })
  mne <- lapply(dif_vars, function(x) {
    mne[[x]][[x]] <- paste0(format(round(mne[[x]]$Unstandardized, 3), nsmall = 3), " (",
                            format(round(mne[[x]]$Standardized, 3), nsmall = 3), ")")
    mne[[x]][, c("item", x)]
  })
  mne <- Reduce(function(m1, m2) {dplyr::full_join(m1, m2, by = "item")}, mne)

  est <- rbind(est, mne)

  dif_tr_tables <- list(gof = gof, estimates = est)

  # format est

  if (save) {
      save_table(dif_tr_tables, filename = "dif_tr_tables.xlsx", path = path,
                 overwrite = overwrite, show_rownames = FALSE)
  }

  return(dif_tr_tables)
}


#' Print DIF summaries to console
#'
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param res list; return object of dif_summary()
#' @export
print_dif_summary <- function(diflist, res) {
  # information criteria table
  message("\n\nInformation criteria regarding the ", diflist$dif_var,
          " DIF analysis:\n")
  print(res$gof)
  # main effects table
  message("\nMain effects of DIF and main effects model for ",
          diflist$dif_var, " (compared against: ", res$compared_against, "):\n")
  print(res$mne)
  # problematic dif values (significant p-value, larger than 0.5 logits)
  message("\nItems exhibiting problematic DIF for ", diflist$dif_var,
          " (|xsi| >= 0.5 and p < 0.05):\n")
  print(res$est[abs(res$est$xsi) >= 0.5 & res$est$p < 0.05, ])
}
