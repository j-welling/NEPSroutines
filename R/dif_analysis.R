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
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param verbose  logical; whether to print processing information to console
#' @param dif_threshold numeric scalar; indicates absolute threshold of
#' problematic DIF (defaults to 0.5)
#' @param digits  integer; number of decimals for rounding
#'
#' @return (if return = TRUE) a list of:
#'   models: list with DIF model results for all variables defined in 'dif_vars'
#'   summaries: list with a summary of DIF results for all variables defined in 'dif_vars'
#'   tr_tables: table for TR with summary of results for all variables defined in 'dif_vars'
#' @export

dif_analysis <- function(resp,
                         vars,
                         select,
                         dif_vars,
                         valid = NULL,
                         mvs = NULL,
                         scoring = NULL,
                         overwrite = FALSE,
                         save = TRUE,
                         print = TRUE,
                         return = FALSE,
                         include_mv = 200,
                         control = NULL,
                         pweights = NULL,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         name_group = NULL,
                         verbose = FALSE,
                         warn = TRUE,
                         dif_threshold = 0.5,
                         digits = 3
                         ) {

    # Test data
    scaling:::test_dif_data(
      resp = resp,
      vars = vars,
      valid = valid,
      dif_vars = dif_vars,
      scoring = scoring,
      pweights = pweights,
      mvs = mvs,
      warn = warn
    )

    scaling:::check_select(select, dif_vars)

    # Create list for results
    dif <- list()

    # Conduct dif analyses
    dif$models <- scaling:::conduct_dif_analysis(
        select = select,
        dif_vars = dif_vars,
        resp = resp,
        vars = vars,
        scoring = scoring,
        include_mv = include_mv,
        valid = valid,
        path = path_results,
        mvs = mvs,
        verbose = verbose,
        warn = warn,
        save = save,
        name_group = name_group,
        control = control,
        pweights = pweights,
        test = FALSE
    )

    # Create summary
    dif$summaries <- scaling:::summarize_dif_analysis(
        dif_models = dif$models,
        dif_vars = dif_vars,
        dif_threshold = dif_threshold,
        path_table = path_table,
        path_results = path_results,
        print = print,
        save = save,
        name_group = name_group,
        overwrite = overwrite,
        digits = digits
    )

    # Create table for TR
    dif$tr_tables <- scaling:::build_dif_tr_tables(
        dif_summaries = dif$summaries,
        save = save,
        name_group = name_group,
        path = path_table,
        overwrite = overwrite
    )

    # Return results
    if (return) return(dif)
}

#' Checks whether arguments select and dif_vars match
#' @param select function argument 'select'
#' @param dif_vars function argument 'dif_vars'
#'
#' @noRd
check_select <- function(select, dif_vars) {
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
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
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
                                 name_group = NULL, control = NULL, pweights = NULL,
                                 verbose = FALSE, warn = TRUE, test = TRUE) {

    # Test data
    if (test) {
        scaling:::test_dif_data(
          resp = resp,
          vars = vars,
          valid = valid,
          dif_vars = dif_vars,
          scoring = scoring,
          pweights = pweights,
          mvs = mvs,
          warn = warn
        )
    }

    # Create list for results
    dif_models <- list()

    # Set same items to all dif variables if items has length 1
    if (length(select) == 1) {
        select <- rep(select, length(dif_vars))
    }

    # Conduct dif analyses
    for (i in seq_along(dif_vars)) {
        dif_models[[i]] <- scaling:::dif_model(
            resp = resp,
            vars = vars,
            select = select[i],
            valid = valid,
            dif_var = dif_vars[i],
            scoring = scoring,
            include_mv = include_mv,
            verbose = verbose,
            mvs = mvs,
            warn = warn,
            control = control,
            pweights = pweights,
            save = FALSE,
            test = FALSE
        )
    }
    names(dif_models) <- dif_vars

    # Save results
    if (save) {
        are_poly <- sapply(select, function(x) is_poly(resp, vars, x))
        irt_type <- ifelse(
            sum(are_poly) == length(are_poly),
            'poly',
            ifelse(sum(are_poly) == 0, 'dich', 'mixed')
        )
        name <- scaling:::create_name(
            paste0("dif_", irt_type, "_models"), name_group, ".rds"
        )
        scaling:::save_results(dif_models, path = path, filename = name)
    }

    # Return results
    return(dif_models)
}

#' summarize DIF analysis
#'
#' @param dif_models return object of conduct_dif_analysis()
#' @param dif_vars character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param dif_threshold numeric scalar; indicates absolute threshold of
#' problematic DIF (defaults to 0.5)
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param digits  integer; number of decimals for rounding
#'
#' @returns a list of dif summaries for each input entry in dif_models.
#' @export
summarize_dif_analysis <- function(dif_models, dif_vars, dif_threshold = 0.5,
                                   print = TRUE, save = TRUE, overwrite = FALSE,
                                   path_results = here::here('Results'),
                                   path_table = here::here('Tables'),
                                   name_group = NULL, digits = 3) {

    dif_summaries <- list()

    for (i in dif_vars) {

        dif_summaries[[i]] <- scaling:::dif_summary(
            dif_models[[i]],
            dif_threshold = dif_threshold,
            print = print,
            overwrite = overwrite,
            save = save,
            name_group = name_group,
            path = path_table,
            digits = digits
        )
    }

    # Save results
    if (save) {
        are_poly <- sapply(dif_summaries, function(x) x$irt_type == 'poly')
        irt_type <- ifelse(
            sum(are_poly) == length(are_poly),
            'poly',
            ifelse(sum(are_poly) == 0, 'dich', 'mixed')
        )
        name <- scaling:::create_name(
            paste0("dif_", irt_type, "_summaries"), name_group, ".rds"
        )
        scaling:::save_results(dif_summaries, path = path_results, filename = name)
    }

    # Return results
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
#' @param mvs named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param dif_var string; defines the name of the variable to be tested for DIF
#' (e.g., "gender")
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return a list of:
#'   mmod: main effects model
#'   dmod: DIF effects model.
#' @importFrom stats as.formula
#' @export
dif_model <- function(resp, vars, select, dif_var, scoring = NULL,
                      valid = NULL, include_mv = 200,
                      mvs = NULL, verbose = FALSE, warn = TRUE, test = TRUE,
                      control = NULL, pweights = NULL, save = TRUE,
                      path = here::here("Results"), name_group = NULL) {

    # Test data
    if (test) {
        scaling:::test_dif_data(
          resp = resp,
          vars = vars,
          valid = valid,
          dif_vars = dif_var,
          scoring = scoring,
          pweights = pweights,
          mvs = mvs,
          warn = warn)
    }

    scaling:::check_items(vars$item[vars[[select]]])
    scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
    scaling:::check_items(vars$item[vars[[select]]])

    # Select only valid cases
    resp <- scaling:::only_valid(resp, valid = valid, warn = FALSE)

    # Create ID, facets and pweights variable
    pid <- resp$ID_t
    scaling:::check_pid(pid)
    facets <- resp[, dif_var, drop = FALSE]
    lbls_facet <- attributes(resp[[dif_var]])$label
    pws <- scaling:::create_ifelse(is.null(pweights), NULL, resp[[pweights]])

    # Prepare resp by converting missing values and selecting only necessary variables
    resp <- scaling:::prepare_resp(resp, vars = vars, select = select,
                                   convert = TRUE, mvs = mvs, warn = FALSE)

    # Test resp
    scaling:::check_numerics(resp, "resp", check_invalid = TRUE)

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
        if (warn) {
            warning(paste0(
                sum(invalid, na.rm = TRUE),
                " invalid values (< 0) were found",
                " in the DIF variable ",
                dif_var,
                ". The corresponding cases were replaced",
                " by NAs.\n")
            )
        }
    }

    mis <- is.na(facets[[dif_var]])

    if (any(mis)) {

        if (sum(mis) < include_mv) {

            facets <- facets[!mis, , drop = FALSE]
            resp <- resp[!mis, ]
            pid <- pid[!mis]

            fcts <- scaling:::create_facets_df(facets[[dif_var]], labels = lbls_facet)

            if (warn) {
                warning(paste0(sum(mis), " missing values were found in the DIF variable ",
                               dif_var, ". The corresponding cases have been excluded from the analysis.\n"))
            }

        } else {

            vals <- unique(facets[[dif_var]])
            max_val <- max(vals, na.rm = TRUE)
            min_val <- min(vals, na.rm = TRUE)
            facets[mis, ] <- max_val + 1

            fcts <- scaling:::create_facets_df(
                facets[[dif_var]],
                labels = lbls_facet,
                missings = TRUE
            )

            # DIF analysis does not work with more than two groups when one group == 0
            if (min_val == 0) {
                facets <- facets + 1
                fcts$number <- as.integer(fcts$number)
            }

            if (warn) {
                warning(paste0(sum(mis), " missing values were found in the DIF variable ",
                               dif_var, ". The corresponding cases have been included in the analysis as",
                               " an extra group.\n"))
            }

        }
    } else {
        fcts <- scaling:::create_facets_df(facets[[dif_var]], labels = lbls_facet)
    }

    # DIF analysis


    if (irt_type == 'poly') {

        if(is.null(scoring) & warn)
            warning("No scoring variable provided. All items are scored with 1.")

        mmod <- scaling:::pcm_dif(
            resp = resp,
            facets = facets,
            formulaA = formula_mmod,
            pid = pid,
            vars = vars,
            select = select,
            scoring = scoring,
            verbose = verbose,
            control = control,
            pweights = pws
        )

        dmod <- scaling:::pcm_dif(
            resp = resp,
            facets = facets,
            formulaA = formula_dmod,
            pid = pid,
            vars = vars,
            select = select,
            scoring = scoring,
            verbose = verbose,
            control = control,
            pweights = pws
        )

    } else {

        # Check whether resp contains only dichotomous items
        scaling:::check_dich(resp, "resp")

        Q <- scaling:::create_q(vars, select = select, scoring = scoring, poly = FALSE)
        irtmodel <- '1PL'

        dmod <- TAM::tam.mml.mfr(
          resp,
          irtmodel = irtmodel,
          facets = facets,
          Q = Q,
          pid = pid,
          formulaA = formula_dmod,
          control = control,
          pweights = pws,
          verbose = verbose
        )

        mmod <- TAM::tam.mml.mfr(
          resp,
          irtmodel = irtmodel,
          facets = facets,
          Q = Q,
          pid = pid,
          formulaA = formula_mmod,
          control = control,
          pweights = pws,
          verbose = verbose
        )

    }

    # Warn if maximum number of iterations were reached
    scaling:::reached_maxiter(mmod, paste0("'", dif_var, "' without DIF"))
    scaling:::reached_maxiter(dmod, paste0("'", dif_var, "' with DIF"))

    # Create list with results
    results <- list(
        mmod = mmod,
        dmod = dmod,
        facets = fcts,
        dif_var = dif_var,
        irt_type = irt_type
    )

    # Save results
    if (save) {
        name <- scaling:::create_name(
            paste0("dif_", irt_type, "_model"), name_group, ".rds"
        )
        scaling:::save_results(results, path = path, filename = name)
    }

    # Return results
    return(results)
}

#' Create data.frame for facets with counts
#'
#' @param facet factor or numeric vector; defines groups of facet
#' @param missings logical; whether table shall include missings
#' @param labels character vector; contains names of facets
#'
#' @return table with frequency of facets.
#' @noRd
create_facets_df <- function(facet, missings = FALSE, labels = NULL) {

    df <- data.frame(table(facet))
    names(df) <- c("number", "counts")
    row.names(df) <- scaling:::create_ifelse(
      !missings,
      paste0("Group ", sort(unique(facet))),
      c(paste0("Group ", sort(unique(facet))[-length(unique(facet))]), "missings")
    )
    if(!is.null(labels)) {
        df$label <- scaling:::create_ifelse(
          !missings,
          names(labels),
          c(names(labels), 'missings'))
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
#' @param control list; function argument as passed to TAM-functions
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param verbose  logical; whether to print processing information to console
#' @param pid  character vector; contains person identifiers
#'
#' @return a tam.mml model.
#' @noRd

pcm_dif <- function(resp, facets, formulaA, vars, select, pid,
                    verbose, scoring = NULL, control = NULL, pweights = NULL) {

    # get design matrix for model
    B <- TAM::designMatrices(modeltype = 'PCM', resp = resp)$B

    pcm_scoring <- scaling:::create_ifelse(is.null(scoring),
                                           rep(1, length(vars[[select]])),
                                           vars[[scoring]][vars[[select]]])

    # 0.5 scoring for PCM
    B[vars$item[vars[[select]]], , 1] <- B[vars$item[vars[[select]]], , 1] * pcm_scoring

    # set irtmodel
    irtmodel <- 'GPCM'

    TAM::tam.mml.mfr(formulaA = formulaA, facets = facets, B = B, pid = pid,
                     irtmodel = irtmodel, resp = resp, verbose = verbose,
                     control = control, pweights = pweights)

}


#' Summary for DIF analysis
#'
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param dif_threshold numeric scalar; indicates absolute threshold of
#' problematic DIF (defaults to 0.5)
#' @param print logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where tables shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis.
#' @export

dif_summary <- function(diflist, print = TRUE, save = TRUE,
                        path = here::here('Tables'), dif_threshold = 0.5,
                        overwrite = FALSE, name_group = NUL, digits = 3L) {
    # information criteria for DIF and main model
    # main effects of main and DIF model + standardized
    # DIF per item + standard error + meht p-values

    dif_var <- diflist$dif_var
    irt_type <- diflist$irt_type

    groups <- diflist$mmod$xsi.facets$parameter[diflist$mmod$xsi.facets$facet == dif_var]
    groups <- gsub(diflist$dif_var, "", groups)
    res <- difsum(obj = diflist, dif_var = dif_var, groups = groups, digits = digits)

    # Print results
    if (print) {
        scaling:::print_dif_summary(
            resp = resp,
            diflist = diflist,
            res = res,
            dif_threshold = dif_threshold
        )
    }

    # Save results
    if (save) {
        res_ <- res
        names(res_$est) <- paste0("Estimates ", names(res_$est))
        names(res_$mne) <- paste0("Main effect ", names(res_$mne))
        res_ <- c(res_, res_$est, res_$mne)
        res_$est <- res_$mne <- NULL
        names(res_) <- gsub(":", "", names(res_))

        name <- scaling:::create_name(
            paste0("dif_", irt_type, "_", dif_var), name_group, ".xlsx"
        )
        scaling:::save_table(
            res_,
            filename = name,
            path = path,
            overwrite = overwrite,
            show_rownames = FALSE
        )
    }

    # Return results
    return(res)
}


#' Summarizes DIF effects
#'
#' @param obj list; return object of dif_model()
#' @param dif_var character vector; contains the variable names to be tested
#'   for DIF (e.g., "gender")
#' @param groups numeric vector; contains group identificators (e.g., 1, 2)
#' @param digits  integer; number of decimals for rounding
#'
#' @return list of information criteria, dif estimates and main effects in
#'   data frames for dif analysis.
#' @importFrom stats deviance
#' @noRd

difsum <- function(obj, dif_var, groups = 1, digits = 3) {

    # all included items
    it <- colnames(obj$dmod$resp_orig)
    if (is.null(it)) {
        s <- grepl(paste0("^(.+)-", dif_var, group),
                   rownames(obj$dmod$B))
        it <- gsub(paste0("-", dif_var, group), "",
                   rownames(obj$dmod$B)[s])
    }

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

    gp <- merge(groups, groups)
    mest <- list()
    for (g in seq_len(nrow(gp))) {

        grps <- sort(unlist(gp[g, ]))
        lbl <- paste(grps, collapse = "-")
        if (lbl %in% names(mest) | grps[[1]] == grps[[2]]) next

        # Differences in item parameters
        mest[[lbl]] <- est[[grps[1]]]
        mest[[lbl]]$xsi <-  round(est[[grps[1]]]$xsi - est[[grps[2]]]$xsi, digits)
        mest[[lbl]]$se.xsi <- round(scaling:::create_ifelse(
          any(grps == max(groups)),
          sqrt(est[[grps[!grps == max(groups)]]]$se.xsi^2 * 2),
          sqrt(est[[grps[1]]]$se.xsi^2 + est[[grps[2]]]$se.xsi^2)
        ), digits)

        # Standardized difference
        mest[[lbl]]$std <- round(mest[[lbl]]$xsi / sqrt(obj$dmod$variance[1]), digits)

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

        mest[[lbl]]$p <- round(fit_meht["p.xsi", ], 3)
        mest[[lbl]]$Femp <- round(fit_meht["Femp.xsi", ], digits)
        mest[[lbl]]$Fkrit <- round(fit_meht["Fkrit", ], digits)
        mest[[lbl]]$df1 <- fit_meht["df1", ]
        mest[[lbl]]$df2 <- fit_meht["df2", ]

        # reorder
        mest[[lbl]] <- mest[[lbl]][, c("item", "xsi", "se.xsi", "std",
                                       "Femp", "Fkrit", "df1", "df2", "p")]
    }


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
        mne[[lbl]] <- data.frame(
          Model = c("DIF model", "Main effects model"),
          Unstandardized = rep(NA, 2),
          Standardized = rep(NA, 2)
        )
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
        mne[[lbl]][, 2:3] <- round(-1 * mne[[lbl]][, 2:3], digits)
    }

    out$mne <- mne

    # goodness-of-fit indices
    gof <- data.frame(
        `DIF variable` = dif_var,
        Model = c("Main effect", "DIF"),
        N = c(obj$mmod$nstud, obj$dmod$nstud),
        Deviance = round(c(deviance(obj$mmod), deviance(obj$dmod))),
        `Number of parameters` = c(obj$mmod$ic$Npars, obj$dmod$ic$Npars),
        AIC = round(c(AIC(obj$mmod), AIC(obj$dmod))),
        BIC = round(c(BIC(obj$mmod), BIC(obj$dmod)))
    )
    out$gof <- gof

    # facets
    out$facets <- obj$facets

    # irt_type
    out$irt_type <- obj$irt_type

    # Return results
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
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @return table with results for TR.
#' @export

build_dif_tr_tables <- function(dif_summaries, save = TRUE, overwrite = FALSE,
                                path = here::here('Tables'), name_group = NULL) {

    dif_vars <- names(dif_summaries)
    are_poly <- sapply(dif_summaries, function(x) x$irt_type == 'poly')
    irt_type <- ifelse(sum(are_poly) == length(are_poly), 'poly',
                       ifelse(sum(are_poly) == 0, 'dich', 'mixed'))

    # information criteria table
    gof <- Reduce(rbind, lapply(dif_summaries, function(x) x$gof))
    gof[,-c(1:2)] <- round(gof[, -c(1:2)])

    # DIF effects table + main effects
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

    # Create TR table
    dif_tr_tables <- list(gof = gof, estimates = est)

    # Save results
    if (save) {
        name <- scaling:::create_name(
            paste0("dif_", irt_type, "_TR"), name_group, ".xlsx"
        )
        scaling:::save_table(
            dif_tr_tables,
            filename = name,
            path = path,
            overwrite = overwrite,
            show_rownames = FALSE
        )
    }

    # Return results
    return(dif_tr_tables)
}


#' Print DIF summaries to console
#'
#' @param resp  data.frame; includes all DIF variables
#' @param diflist list; return object of dif_model(); with main and dif model
#' @param res list; return object of dif_summary()
#' @param dif_threshold numeric scalar; indicates absolute threshold of
#' problematic DIF (defaults to 0.5)
#'
#' @export
print_dif_summary <- function(resp, diflist, res, dif_threshold = 0.5) {

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
    message("\nItems exhibiting problematic DIF (|xsi| >= ", dif_threshold, "):\n")

    for (i  in names(res$est)) {
        cat(paste0("Comparison ", i, ":\n"))
        f <- abs(res$est[[i]]$xsi) >= dif_threshold
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

#' Test DIF data
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param mvs named integer vector; contains user-defined missing values
#' @param dif_vars character vector; contains the variable name(s) to be tested
#'   for DIF (e.g., "gender")
#' @param scoring string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @noRd
test_dif_data <- function(resp,
                          vars,
                          dif_vars,
                          valid = NULL,
                          mvs = NULL,
                          scoring = NULL,
                          pweights = NULL,
                          warn = TRUE) {

    scaling:::check_logicals(resp, "resp", valid, warn = warn)
    scaling:::check_variables(resp, "resp", dif_vars)

    if (!is.null(scoring))
        scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)

    if (!is.null(pweights))
        scaling:::check_numerics(resp, "resp", pweights, check_invalid = TRUE)

    if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)
}
