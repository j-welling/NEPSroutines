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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select character vector; defines name(s) of logical variable(s) in vars
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
#' contains the scoring factor to be applied to loading matrix
#' @param include_mv numeric; identifies threshold for which group size missing
#' values should be included in analysis as an extra group (defaults to 200)
#' @param two_par logical; whether two parameter model (2PL or GPCM) shall be
#' used as base for DIF analyses (defaults to FALSE)
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param verbose  logical; whether to print processing information to console
#' @param prob_dif numeric scalar; indicates absolute threshold of problematic DIF (defaults to 0.5)
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export

dif_analysis <- function(resp, vars, select, dif_vars, valid = NULL, mvs = NULL,
                         scoring = NULL, overwrite = FALSE, two_par = FALSE,
                         save = TRUE, print = TRUE, return = FALSE,
                         include_mv = 200, control = NULL, pweights = NULL,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         verbose = FALSE, warn = TRUE, prob_dif = 0.5) {

  # Test data
  check_logicals(vars, "vars", select, warn = warn)
  check_logicals(resp, "resp", valid, warn = warn)
  check_variables(resp, "resp", dif_vars)


  if (!is.null(scoring)) check_numerics(vars, "vars", scoring,
                                        check_invalid = TRUE)

  if (warn) {
      if (is.null(mvs)) {
          warning("No user defined missing values provided. ",
                  "Default of '-999 to -1' is used.\n")
      }

      if (is.null(valid)) {
          warning("No variable with valid cases provided. ",
                  "All cases are used for analysis.\n")
      }
  }

  check_items(select, dif_vars)

  # Create list for results
  dif <- list()

  # Conduct dif analyses
  dif$models <- conduct_dif_analysis(
    select = select, dif_vars = dif_vars, resp = resp, vars = vars,
    scoring = scoring, include_mv = include_mv, valid = valid,
    path = path_results, mvs = mvs, verbose = verbose, warn = warn, save = save,
    control = control, pweights = pweights, test = FALSE, two_par = two_par
  )

  # Create summary
  dif$summaries <- summarize_dif_analysis(
    dif_models = dif$models, dif_vars = dif_vars, prob_dif = prob_dif,
    path_table = path_table, path_results = path_results,
    print = print, save = save, overwrite = overwrite
  )

  # Create table for TR
  dif$tr_tables <- build_dif_tr_tables(
    dif_summaries = dif$summaries,  save = save,
    path = path_table, overwrite = overwrite
  )

  # Return results
  if (return) return(dif)
}

#' Checks whether arguments select and dif_vars match
#' @param select function argument 'select'
#' @param dif_vars function argument 'dif_vars'
#'
#' @noRd
check_items <- function(select, dif_vars) {
  if (length(select) > 1 & length(select) != length(dif_vars)) {
    stop("Please check 'select' and 'dif_vars'. At least one of them does ",
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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select character vector; defines name(s) of logical variable(s) in vars
#' that indicates which items to use for analysis; if some of the \code{dif_vars}
#' come with a different set of analysis items, this argument becomes a
#' vector of \code{length(dif_vars)} containing the respective selection
#' variables in vars
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param dif_vars character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param scoring string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param include_mv numeric; identifies threshold for which group size missing
#' values should be included in analysis as an extra group (defaults to 200)
#' @param two_par logical; whether two parameter model (2PL or GPCM) shall be
#' used as base for DIF analyses (defaults to FALSE)
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param verbose  logical; whether to print processing information to console
#' @param mvs named integer vector; contains user-defined missing values
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return a list of length(dif_vars) lists containing each:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @export
conduct_dif_analysis <- function(resp, vars, select, dif_vars, valid = NULL,
                                 scoring = NULL, mvs = NULL, include_mv = 200,
                                 path = here::here('Results'), save = TRUE,
                                 verbose = FALSE, warn = TRUE, test = TRUE,
                                 two_par = F, control = NULL, pweights = NULL) {

  # Test data
  if (test) {
    check_logicals(vars, "vars", select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_variables(resp, "resp", dif_vars)

    if (!is.null(scoring)) check_numerics(vars, "vars", scoring,
                                          check_invalid = TRUE)

    if (warn) {
        if (is.null(mvs)) {
            warning("No user defined missing values provided. ",
                    "Default of '-999 to -1' is used.\n")
        }

        if (is.null(valid)) {
            warning("No variable with valid cases provided. ",
                    "All cases are used for analysis.\n")
        }
    }
  }

  # Create list for results
  dif_models <- list()

  # Set same items to all dif variables if items has length 1
  if (length(select) == 1) {
    select <- rep(select, length(dif_vars))
  }

  # Conduct dif analyses
  for (i in seq_along(dif_vars)) {
    dif_models[[i]] <- dif_model(resp = resp, vars = vars, select = select[i],
                                 valid = valid, dif_var = dif_vars[i],
                                 scoring = scoring, include_mv = include_mv,
                                 verbose = verbose, mvs = mvs, warn = warn,
                                 test = FALSE, two_par = two_par,
                                 control = control, pweights = pweights)
  }
  names(dif_models) <- dif_vars

  # Save results
  if (save) {
      are_poly <- sapply(select, function(x) is_poly(resp, vars, x))
      irt_type <- ifelse(sum(are_poly) == length(are_poly), 'poly',
                         ifelse(sum(are_poly) == 0, 'dich', 'mixed'))
      save_results(dif_models, path = path,
                   filename = paste0("dif_", irt_type, "_models.rds"))
  }

  # Return results
  return(dif_models)
}

#' summarize DIF analysis
#'
#' @param dif_models return object of conduct_dif_analysis()
#' @param dif_vars character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param prob_dif numeric scalar; indicates absolute threshold of problematic DIF (defaults to 0.5)
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @returns a list of dif summaries for each input entry in dif_models
#' @export
summarize_dif_analysis <- function(dif_models, dif_vars, prob_dif = 0.5,
                                   print = TRUE, save = TRUE, overwrite = FALSE,
                                   path_results = here::here('Results'),
                                   path_table = here::here('Tables')) {

  dif_summaries <- list()

  for (i in dif_vars) {

    dif_summaries[[i]] <- dif_summary(dif_models[[i]], prob_dif = prob_dif,
                                      print = print, overwrite = overwrite,
                                      save = save, path = path_table)
  }

  if (save) {

      are_poly <- sapply(dif_summaries, function(x) x$irt_type == 'poly')
      irt_type <- ifelse(sum(are_poly) == length(are_poly), 'poly',
                         ifelse(sum(are_poly) == 0, 'dich', 'mixed'))
      save_results(dif_summaries, path = path_results,
                   filename = paste0("dif_", irt_type, "_summaries.rds"))
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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select string; defines name of logical variable in vars that indicates
#' which items to use for the analysis; if some of the \code{dif_vars}
#' come with a different set of analysis items, this argument becomes a
#' vector of \code{length(dif_vars)} containing the respective selection
#' variables in vars
#' @param include_mv numeric; identifies threshold for which group size missing
#' values should be included in analysis as an extra group (defaults to 200)
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param two_par logical; whether two parameter model (2PL or GPCM) shall be
#' used as base for DIF analyses (defaults to FALSE)
#' @param mvs named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param dif_var string; defines the name of the variable to be tested for DIF
#' (e.g., "gender")
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model
#' @importFrom stats as.formula
#' @export
dif_model <- function(resp, vars, select, dif_var, scoring = NULL,
                      valid = NULL, include_mv = 200, two_par = FALSE,
                      mvs = NULL, verbose = FALSE, warn = TRUE, test = TRUE,
                      control = NULL, pweights = NULL) {

  # Test data
  if (test) {
    check_logicals(vars, "vars", select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_variables(resp, "resp", dif_var)

    if (!is.null(scoring)) check_numerics(vars, "vars", scoring,
                                          check_invalid = TRUE)

    if (warn) {
        if (is.null(mvs)) {
            warning("No user defined missing values provided. ",
                    "Default of '-999 to -1' is used.\n")
        }

        if (is.null(valid)) {
            warning("No variable with valid cases provided. ",
                    "All cases are used for analysis.\n")
        }
    }
  }

  # Select only valid cases
  resp <- only_valid(resp, valid = valid, warn = FALSE)

  # Create ID, facets and pweights variable
  pid <- resp$ID_t
  check_pid(pid)
  facets <- resp[, dif_var, drop = FALSE]
  lbls_facet <- attributes(resp[[dif_var]])$label
  pws <- create_ifelse(is.null(pweights), NULL, resp[[pweights]])

  # Prepare resp by converting missing values and selecting only necessary variables
  resp <- prepare_resp(resp, vars = vars, select = select,
                       convert = TRUE, mvs = mvs, warn = FALSE)

  # Test resp
  check_numerics(resp, "resp", check_invalid = TRUE)

  # Identify IRT type
  irt_type <- ifelse(is_poly(resp, vars, select), 'poly', 'dich')

  # Prepare DIF analysis
  tmp_formula <- paste("~ item +", ifelse(irt_type == 'poly', "item * step +", ""))
  formula_dmod <- as.formula(paste(tmp_formula, "item *", dif_var))
  formula_mmod <- as.formula(paste(tmp_formula, dif_var))
  rm(tmp_formula)

  # Prepare facets
  facets[[dif_var]] <- as.integer(facets[[dif_var]])
  invalid <- facets[[dif_var]] < 0

  if (sum(invalid, na.rm = TRUE) > 0) {
    facets[[dif_var]][ifelse(is.na(invalid), FALSE, invalid), ] <- NA
    warning(paste0(sum(invalid, na.rm = TRUE), " invalid values (< 0) were found",
    " in the DIF variable ", dif_var, ". The corresponding cases were replaced",
    " by NAs.\n"))
  }

  mis <- is.na(facets[[dif_var]])

  if (any(mis)) {

    if (sum(mis) < include_mv) {

      facets <- facets[!mis, , drop = FALSE]
      resp <- resp[!mis, ]
      pid <- pid[!mis]

      fcts <- create_facets_df(facets[[dif_var]], labels = lbls_facet)

      warning(paste0(sum(mis), " missing values were found in the DIF variable ",
                     dif_var, ". The corresponding cases have been excluded from the analysis.\n"))

    } else {

      vals <- unique(facets[[dif_var]])
      max_val <- max(vals, na.rm = TRUE)
      min_val <- min(vals, na.rm = TRUE)
      facets[mis, ] <- max_val + 1

      fcts <- create_facets_df(facets[[dif_var]], labels = lbls_facet,
                               missings = TRUE)

      # DIF analysis does not work with more than two groups when one group == 0
      if (min_val == 0) {
        facets <- facets + 1
        fcts$number <- as.integer(fcts$number)
      }

      warning(paste0(sum(mis), " missing values were found in the DIF variable ",
      dif_var, ". The corresponding cases have been included in the analysis as",
      " an extra group.\n"))

    }
  } else {
    fcts <- create_facets_df(facets[[dif_var]], labels = lbls_facet)
  }

  # DIF analysis


  if (irt_type == 'poly') {

    if(is.null(scoring)) warning("No scoring variable provided. All items are scored with 1.")

    mmod <- pcm_dif(
      resp = resp, facets = facets, formulaA = formula_mmod, pid = pid,
      vars = vars, select = select, scoring = scoring, verbose = verbose,
      two_par = two_par, control = control, pweights = pws
    )

    dmod <- pcm_dif(
      resp = resp, facets = facets, formulaA = formula_dmod, pid = pid,
      vars = vars, select = select, scoring = scoring, verbose = verbose,
      two_par = two_par, control = control, pweights = pws
    )

  } else {

    # Check whether resp contains only dichotomous items
    check_dich(resp, "resp")

    Q <- create_q(vars, select = select, scoring = scoring, poly = FALSE)
    irtmodel <- ifelse(two_par, '2PL', '1PL')

    dmod <- TAM::tam.mml.mfr(resp, irtmodel = irtmodel, facets = facets,
                             Q = Q, pid = pid, formulaA = formula_dmod,
                             control = control, pweights = pws,
                             verbose = verbose
    )

    mmod <- TAM::tam.mml.mfr(resp, irtmodel = irtmodel, facets = facets,
                             Q = Q, pid = pid, formulaA = formula_mmod,
                             control = control, pweights = pws,
                             verbose = verbose
    )

  }

  # Warn if maximum number of iterations were reached
  reached_maxiter(mmod, paste0("'", dif_var, "' without DIF"))
  reached_maxiter(dmod, paste0("'", dif_var, "' with DIF"))

  list(mmod = mmod, dmod = dmod, facets = fcts,
       dif_var = dif_var, irt_type = irt_type)
}

#' Create data.frame for facets with counts
#'
#' @param facet factor or numeric vector; defines groups of facet
#' @param missings logical; whether table shall include missings
#'
#' @noRd
create_facets_df <- function(facet, missings = FALSE, labels = NULL) {

  df <- data.frame(table(facet))
  names(df) <- c("number", "counts")
  row.names(df) <- create_ifelse(!missings,
                                 paste0("Group ", sort(unique(facet))),
                                 c(paste0("Group ", sort(unique(facet))[-length(unique(facet))]), "missings"))
  if(!is.null(labels)) {
      df$label <- create_ifelse(!missings, names(labels), c(names(labels), 'missings'))
  }

  df
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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param facets  data.frame of one column for variable to be tested for DIF;
#'   column is named after DIF variable (e.g., "gender"); must contain the same
#'   persons in the same order as resp
#' @param formulaA  an R formula for the DIF analysis
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param two_par logical; whether two parameter model (2PL or GPCM) shall be
#' used as base for DIF analyses (defaults to FALSE)
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param verbose  logical; whether to print processing information to console
#' @param pid  character vector; contains person identifiers
#'
#' @return a tam.mml model
#' @noRd

pcm_dif <- function(resp, facets, formulaA, vars, select, pid, verbose,
                    two_par = F, scoring = NULL, control = NULL, pweights = NULL) {

  # get design matrix for model
  B <- TAM::designMatrices(modeltype = 'PCM', resp = resp)$B

  pcm_scoring <- create_ifelse(is.null(scoring),
                               rep(1, length(vars[[select]])),
                               vars[[scoring]][vars[[select]]])

  # 0.5 scoring for PCM
  B[vars$item[vars[[select]]], , 1] <- B[vars$item[vars[[select]]], , 1] * pcm_scoring

  # set irtmodel
  irtmodel <- ifelse(two_par, 'GPCM', 'PCM2')

  TAM::tam.mml.mfr(formulaA = formulaA, facets = facets, B = B, pid = pid,
                   irtmodel = irtmodel, resp = resp, verbose = verbose,
                   control = control, pweights = pweights)

}


#' Summary for DIF analysis
#'
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param prob_dif numeric scalar; indicates absolute threshold of problematic DIF (defaults to 0.5)
#' @param print logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @export

dif_summary <- function(diflist, print = TRUE, save = TRUE,
                        path = here::here('Tables'), prob_dif = 0.5,
                        overwrite = FALSE) {
    # information criteria for DIF and main model
    # main effects of main and DIF model + standardized
    # DIF per item + standard error + meht p-values

    dif_var <- diflist$dif_var
    irt_type <- diflist$irt_type

    ### TG: start

    # group <- grep(diflist$dif_var, rownames(diflist$dmod$xsi), value = TRUE)
    # group <- unique(sapply(group, function(x) {substr(x, nchar(x), nchar(x))}))
    # group <- as.integer(group)
    # res <- difsum(obj = diflist, dif_var = dif_var, group = group, group2 = NULL)
    # res$compared_against <- group

    groups <- diflist$mmod$xsi.facets$parameter[diflist$mmod$xsi.facets$facet == dif_var]
    groups <- gsub(diflist$dif_var, "", groups)
    res <- difsum(obj = diflist, dif_var = dif_var, groups = groups)

    ### TG: end

    if (print) {
        print_dif_summary(resp = resp, diflist = diflist, res = res,
                          prob_dif = prob_dif)
    }

    if (save) {
        name <- paste0("dif_", irt_type, "_", dif_var, ".xlsx")
        res_ <- res
        names(res_$est) <- paste0("Estimates ", names(res_$est))
        names(res_$mne) <- paste0("Main effect ", names(res_$mne))
        res_ <- c(res_, res_$est, res_$mne)
        res_$est <- res_$mne <- NULL
        names(res_) <- gsub(":", "", names(res_))
        save_table(res_, filename = name,
                   path = path, overwrite = overwrite, show_rownames = FALSE)
    }

    return(res)
}


#' Summarizes DIF effects
#'
#' @param obj list; return object of dif_model()
#' @param dif_var character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
# #' @param group integer; group to compare against
# #' @param group2 integer; in case of more than two groups
#' @param groups numeric vector; contains group identificators (e.g., 1, 2)
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis
#' @importFrom stats deviance
#' @noRd

### TG: start

#difsum <- function(obj, dif_var, group = 1, group2 = NULL) {
difsum <- function(obj, dif_var, groups = 1) {

    ### TG: end

    # all included items
    it <- colnames(obj$dmod$resp_orig)
    if (is.null(it)) {
        s <- grepl(paste0("^(.+)-", dif_var, group),
                   rownames(obj$dmod$B))
        it <- gsub(paste0("-", dif_var, group), "",
                   rownames(obj$dmod$B)[s])
    }

    ### TG: start

    # Estimated DIF effects in each group
    est <- list()
    for (g in groups) {
        sel <- rownames(obj$dmod$xsi) %in% paste0(it, ":", dif_var, g)
        est[[g]] <- obj$dmod$xsi[sel, ]
        est[[g]]$item <- gsub(paste0(":", dif_var, g), "", rownames(est[[g]]))

        # calculate DIF effect for last item (= constrained for identification)
        lst <- data.frame(item = it[!(it %in% est[[g]]$item)],
                          xsi = sum(est[[g]]$xsi),
                          se.xsi = sqrt(sum(est[[g]]$se.xsi^2)))
        est[[g]] <- rbind(est[[g]], lst)
        rownames(est[[g]]) <- NULL
    }

    # DIF effects in reference group
    f <- sapply(est, \(x) { all(x$xsi == 0) })
    est[f][[1]]$xsi <- rowSums(sapply(est[!f], \(x) x$xsi)) * -1

    # DIF effects for first group
    # sel <- rownames(obj$dmod$xsi) %in%
    #   paste0(it, ":", dif_var, group)
    # est <- obj$dmod$xsi[sel, ]
    # est$item <- gsub(paste0(":", dif_var, group), "", rownames(est))
    #
    # # calculate DIF effect for last item (= constrained for identification)
    # lst <- data.frame(item = it[!(it %in% est$item)],
    #                   xsi = sum(est$xsi),
    #                   se.xsi = sqrt(sum(est$se.xsi^2)))
    # est <- rbind(est, lst)
    # rownames(est) <- NULL

    # DIF effects for second group
    # (only relevant if more than 2 groups were used as DIF variable)
    # if (!is.null(group2)) {
    #   sel <- rownames(obj$dmod$xsi) %in%
    #     paste0(it, ":", dif_var, group2)
    #   est2 <- obj$dmod$xsi[sel, ]
    #   est2$item <- gsub(paste0(":", dif_var, group2), "", rownames(est2))
    #
    #   # calculate DIF effect for last item (= constrained for identification)
    #   lst <- data.frame(item = it[!(it %in% est2$item)],
    #                     xsi = sum(est2$xsi),
    #                     se.xsi = sqrt(sum(est2$se.xsi^2)))
    #   est2 <- rbind(est2, lst)
    #   rownames(est2) <- NULL
    # }

    ### TG: end

    ### TG: start
    gp <- merge(groups, groups)
    mest <- list()
    for (g in seq_len(nrow(gp))) {

        grps <- sort(unlist(gp[g, ]))
        lbl <- paste(grps, collapse = "-")
        if (lbl %in% names(mest) | grps[[1]] == grps[[2]]) next

        # Differences in item parameters
        mest[[lbl]] <- est[[grps[1]]]
        mest[[lbl]]$xsi <-  est[[grps[1]]]$xsi - est[[grps[2]]]$xsi  #2 * mest[[gp[g, 1]]]$xsi
        mest[[lbl]]$se.xsi <- sqrt(est[[grps[1]]]$se.xsi^2 + est[[grps[2]]]$se.xsi^2)

        # Standardized difference
        mest[[lbl]]$std <- mest[[lbl]]$xsi / sqrt(obj$dmod$variance[1])

        # minimum effects hypothesis test
        fit_meht <- apply(mest[[lbl]][, c("xsi", "se.xsi")], 1, function(x) {
            fit <- meht((x["xsi"] / x["se.xsi"])^2,
                        df1 = 1, df2 = obj$dmod$nstud - 2, verbose = FALSE)
            c(Fkrit = fit$Fmin,
              p = fit$pmin,
              df1 = fit$df1,
              df2 = fit$df2,
              Femp = (x["xsi"] / x["se.xsi"])^2)
        })

        mest[[lbl]]$p <- fit_meht["p.xsi", ]
        mest[[lbl]]$Fkrit <- round(fit_meht["Fkrit", ], 3)
        mest[[lbl]]$df1 <- fit_meht["df1", ]
        mest[[lbl]]$df2 <- fit_meht["df2", ]
        mest[[lbl]]$Femp <- round(fit_meht["Femp.xsi", ], 3)

        # reorder
        mest[[lbl]] <- mest[[lbl]][, c("item", "xsi", "se.xsi", "std",
                                       "Femp", "Fkrit", "df1", "df2", "p")]
    }

    # difference in item parameters

    # if (is.null(group2)) {
    #   est$xsi <- 2 * est$xsi
    #   est$se.xsi2 <- est$se.xsi
    # } else {
    #   est$xsi <- est$xsi - est2$xsi
    #   est$se.xsi2 <- est2$se.xsi
    # }

    # standardized difference

    # est$std <- est$xsi / sqrt(obj$dmod$variance[1])
    #
    # # minimum effects hypothesis test

    # est$p <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    #   round(meht((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2,
    #              df1 = 1, df2 = obj$dmod$nstud - 2, verbose = FALSE)$pmin, 3)
    # })
    # est$Fkrit <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    #   round(meht((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2,
    #              df1 = 1, df2 = obj$dmod$nstud - 2, verbose = FALSE)$Fmin, 3)
    # })
    # est$F <- apply(est[, c("xsi", "se.xsi", "se.xsi2")], 1, function(x) {
    #   round((x["xsi"] / sqrt(x["se.xsi"]^2 + x["se.xsi2"]^2))^2, 3)
    # })

    ### TG: end

    ### TG: start

    # reorder

    # out <- list(est = est[, c("item", "xsi", "std", "F", "Fkrit", "p")])
    out <- list(est = mest)

    # main effects
    mne <- list()
    mns_mmod <- obj$mmod$xsi.facets[obj$mmod$xsi.facets$facet == dif_var, ]
    mns_dmod <- obj$dmod$xsi.facets[obj$dmod$xsi.facets$facet == dif_var, ]
    for (i in seq_len(nrow(gp))) {

        grps <- sort(unlist(gp[i, ]))
        lbl <- paste(grps, collapse = "-")
        if (lbl %in% names(mne) | grps[[1]] == grps[[2]]) next

        # Differences in main effects
        mne[[lbl]] <- data.frame(Model = c("DIF model", "Main effects model"),
                                 Unstandardized = rep(NA, 2),
                                 Standardized = rep(NA, 2))
        mn1 <- mns_dmod$xsi[mns_dmod$parameter == paste0(dif_var, grps[1])]
        mn2 <- mns_dmod$xsi[mns_dmod$parameter == paste0(dif_var, grps[2])]
        mne[[lbl]][1, 2:3] <- c(mn1 - mn2,
                                (mn1 - mn2) / sqrt(obj$dmod$variance[1]))
        mn1 <- mns_mmod$xsi[mns_mmod$parameter == paste0(dif_var, grps[1])]
        mn2 <- mns_mmod$xsi[mns_mmod$parameter == paste0(dif_var, grps[2])]
        mne[[lbl]][2, 2:3] <- c(mn1 - mn2,
                                (mn1 - mn2) / sqrt(obj$mmod$variance[1]))

        # main effects refer to item difficulties
        #  -> recode to person main effects
        mne[[lbl]][, 2:3] <- -1 * mne[[lbl]][, 2:3]
    }

    out$mne <- mne

    # main effects

    # mne <- data.frame(Model = c("DIF model", "Main effects model"),
    #                   Unstandardized = rep(NA, 2),
    #                   Standardized = rep(NA, 2))
    # est <- obj$dmod$xsi[rownames(obj$dmod$xsi) == paste0(dif_var, group), ]
    # mne[1, 2:3] <- c(2 * est$xsi[1],
    #                  2 * est$xsi[1] / sqrt(obj$dmod$variance[1]))
    # est <- obj$mmod$xsi[rownames(obj$mmod$xsi) == paste0(dif_var, group), ]
    # mne[2, 2:3] <- c(2 * est$xsi[1],
    #                  2 * est$xsi[1] / sqrt(obj$mmod$variance[1]))
    #
    # # main effects refer to item difficulties
    # #  -> recode to person main effects
    # out$mne <- mne
    # out$mne[, 2:3] <- -1 * out$mne[, 2:3]

    ### TG: end

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


    # facets
    out$facets <- obj$facets

    # irt_type
    out$irt_type <- obj$irt_type

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

build_dif_tr_tables <- function(dif_summaries, save = TRUE, overwrite = FALSE,
                                path = here::here('Tables')) {

    dif_vars <- names(dif_summaries)
    are_poly <- sapply(dif_summaries, function(x) x$irt_type == 'poly')
    irt_type <- ifelse(sum(are_poly) == length(are_poly), 'poly',
                       ifelse(sum(are_poly) == 0, 'dich', 'mixed'))

    # information criteria table
    gof <- Reduce(rbind, lapply(dif_summaries, function(x) x$gof))
    gof[,-c(1:2)] <- round(gof[, -c(1:2)])

    ### TG: start

    # DIF effects table + main effects

    # est <- lapply(dif_summaries, function(x) x$est)
    # est <- lapply(dif_vars, function(x) {
    #   est[[x]][[x]] <- paste0(format(round(est[[x]]$xsi, 3), nsmall = 3), " (",
    #                           format(round(est[[x]]$std, 3), nsmall = 3), ")")
    #   est[[x]][, c("item", x)]
    # })
    # est <- Reduce(function(e1, e2) {dplyr::full_join(e1, e2, by = "item")}, est)
    est <- lapply(dif_vars, function(x) {

        # DIF effects
        r <- sapply(dif_summaries[[x]]$est, \(y) {
            paste0(format(round(y$xsi, 3), nsmall = 3), " (",
                   format(round(y$std, 3), nsmall = 3), ")")

        })
        colnames(r) <- paste(x, colnames(r))

        # Main effects
        m <- sapply(dif_summaries[[x]]$mne, \(y) {
            paste0(format(round(y$Unstandardized, 3), nsmall = 3), " (",
                   format(round(y$Standardized, 3), nsmall = 3), ")")
        })
        colnames(m) <- paste(x, colnames(m))

        # Combine
        rm <- rbind(r, m)
        rm <- cbind(item = c(dif_summaries[[x]]$est[[1]]$item,
                             "Main effect (DIF model)",
                             "Main effect (Main effect model)"), rm)
        dplyr::as_tibble(rm)
    })

    est <- Reduce(function(e1, e2) {dplyr::full_join(e1, e2, by = "item")}, est)

    # mne <- lapply(dif_summaries, function(x) {
    #   m <- dplyr::rename(x$mne, item = 'Model')
    #   m$item <- c("Main effect (DIF model)", "Main effect (Main effect model)")
    #   m
    # })
    # mne <- lapply(dif_vars, function(x) {
    #   mne[[x]][[x]] <- paste0(format(round(mne[[x]]$Unstandardized, 3), nsmall = 3), " (",
    #                           format(round(mne[[x]]$Standardized, 3), nsmall = 3), ")")
    #   mne[[x]][, c("item", x)]
    # })
    # mne <- Reduce(function(m1, m2) {dplyr::full_join(m1, m2, by = "item")}, mne)
    #
    # est <- rbind(est, mne)

    ### TG: end

    dif_tr_tables <- list(gof = gof, estimates = est)

    # format est

    if (save) {
        save_table(dif_tr_tables,
                   filename = paste0("dif_", irt_type, "_TR_tables.xlsx"),
                   path = path, overwrite = overwrite, show_rownames = FALSE)
    }

    return(dif_tr_tables)
}


#' Print DIF summaries to console
#'
#' @param resp  data.frame; includes all DIF variables
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param res list; return object of dif_summary()
#' @param prob_dif numeric scalar; indicates absolute threshold of problematic DIF (defaults to 0.5)
#'
#' @export
print_dif_summary <- function(resp, diflist, res, prob_dif = 0.5) {

    dif_var <- diflist$dif_var

    message("\nRESULTS FOR THE DIF VARIABLE '", dif_var, "':")

    # facets and group counts
    message("\nFacets and group counts:\n")
    print(res$facets)

    # information criteria table
    message("\nInformation criteria:\n")
    print(res$gof)

    # main effects table

    message("\nMain effects of DIF model and main effects model:\n")

    for (i  in names(res$mne)) {
        cat(paste0("Comparison ", i, ":\n"))
        print(res$mne[[i]])
    }

    # problematic dif values (significant p-value, larger than 0.5 logits)
    message("\nItems exhibiting problematic DIF (|xsi| >= ", prob_dif, "):\n")

    for (i  in names(res$est)) {
        cat(paste0("Comparison ", i, ":\n"))
        f <- abs(res$est[[i]]$xsi) >= prob_dif
        if (any(f)) {
            sig <- res$est[[i]]$p < .05
            res$est[[i]]$p <- paste0(round(res$est[[i]]$p, 3), ifelse(sig, "*", ""))
            print(res$est[[i]][f, ])
            if (any(sig)) cat("\n*: p < .05")
        } else {
            cat("No items found.")
        }
        cat("\n\n")
    }
}
