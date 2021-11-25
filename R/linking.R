#' Mean/mean linking of current and previous study
#'
#' @param

linking <- function(resp_previous, resp_current, resp_link_sample = NULL,
                    vars, valid, select_previous, select_current,
                    select_link_sample = NULL, scoring = "scoring", overwrite,
                    mvs = NULL, maxiter, snodes, verbose, anchors = NULL,
                    longitudinal = TRUE, path_table, path_results, pid = "ID_t",
                    wid = "wle", dim = "link", print = TRUE, save = TRUE,
					return = FALSE) {

    # check measurement invariance over time
    link_dif <- check_dif_anchor(
        resp_previous, resp_current, resp_link_sample = resp_link_sample,
        anchors = anchors, mvs = mvs, return = TRUE, valid, scoring = scoring,
        vars, items = select_link_sample, select_previous, select_current)

    # check unidimensionality over time
    link_dim <- check_dif_dimensionality(
        resp_previous, resp_current, resp_link_sample, vars,
        select_current, select_previous, select_link_sample,
        scoring, dim = dim, mvs, irtmodel = "PCM2", maxiter, snodes,
        verbose = FALSE, valid)

    # conduct linking
    link_results <- link_samples(
        resp_previous, resp_current, resp_link_sample = resp_link_sample, vars,
        select_current, select_previous, select_link_sample, valid, scoring,
        anchors = anchors, longitudinal = longitudinal, pid = pid, wid = wid,
        mvs = mvs)

    # print results
    if (print) {
        print_link_results(link_dif, link_dim, link_results)
    }

    # save results
    if (save) {
        save_table(link_dif$link_dif_table, filename = "link_tr_tables.xlsx",
                   path = path_table, overwrite = overwrite,
                   show_rownames = FALSE)
        save_results(link_dif, filename = "link_dif_results.rdata",
                     path = path_results)
        save_results(link_dim, filename = "link_dim_results.rdata",
                     path = path_results)
        save_results(link_results, filename = "link_analysis_results.rdata",
                     path = path_results)
    }

    # return results
    if (return) {
        return(list(link_dif = link_dif, link_dim = link_dim,
                    link_results = link_results))
    }

}


#' Link item parameters with known link constant
#'
#' @param xsi data.frame; xsi parameters as returned by TAM::tam.mml()
#' @param const numeric; link constant
#' @param return_steps logical; should steps be returned (not re-estimated in
#'   subsequent analysis with TAM::tam.mml()). Defaults to FALSE.
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select string; defines name of logical variable in vars that
#'   indicates which items to use for the analysis
#' @param scoring string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to the link constant (e.g., only
#'   half the link constant is needed if a PC item is scored 0.5)
#'
#' @returns matrix in xsi.fixed format needed for TAM::tam.mml()
#' @export
link_item_parameters <- function(xsi, const, return_steps = FALSE, vars,
                                 select, scoring = "scoring") {
    # xsi parameters with parameter index
    xsi.fixed <- cbind(seq_along(xsi$xsi), xsi$xsi)
    rownames(xsi.fixed) <- rownames(xsi)

    # identify step parameters
    f <- !grepl("_step[1-9]$", rownames(xsi.fixed))

    # remove step parameters
    if (!return_steps) {
        xsi.fixed <- xsi.fixed[f, ]
    }

    # add linking constant to all parameters (except steps)
    # TAM uses the parameterization a*theta - const
    # to account for the 0.5 scoring we only have to add half the
    # linking constant to PCMs
    xsi.fixed[f, 2] <- xsi.fixed[f, 2] +
        const * vars[[scoring]][[vars[[select]]]]

    return(xsi.fixed)
}



#' Link WLEs using a known linking constant while correcting for selection
#'
#' @param wle_previous      data.frame with WLEs at first measurement wave
#' @param wle_current      data.frame with WLEs at second measurement wave
#' @param const         linking constant
#' @param pid       character; variable name used as person identifier
#' @param wid       character; variable name used as WLE identifier; optional
#'   as vector containing different WLE identifier names
#' @param ids vector of person identifiers for longitudinal sample;
#'   if null, all cases with the same pid in wle_previous and wle_current
#'   will be used
#' @param use_longitudinal_subsample boolean indicating whether linking should
#'   be based on the entire samples given in wle_previous and wle_current or
#'   only on the longitudinal sample with common pids; defaults to TRUE
#'
#' @returns   linked WLEs at second measurement wave
#' @export
link_wles <- function(wle_previous, wle_current, const,
                      pid = "ID_t", wid = "wle",
                      ids = NULL, use_longitudinal_subsample = TRUE) {

    # Set variable name for person identifier
    wle_previous <- check_var_in_df(df = wle_previous, var = pid,
                                    new_var = "ID_t",
                                    name = get_object_name(wle_previous))
    wle_current <- check_var_in_df(df = wle_current, var = pid,
                                   new_var = "ID_t",
                                   name = get_object_name(wle_current))

    # Set variable name for WLEs
    if (length(wid) == 1) {
        wid <- rep(wid, 2)
    }
    wle_previous <- check_var_in_df(df = wle_previous, var = wid[1],
                                    new_var = "wle",
                                    name = get_object_name(wle_previous))
    wle_current <- check_var_in_df(df = wle_current, var = wid[2],
                                   new_var = "wle",
                                   name = get_object_name(wle_current))

    # Remove missing values
    wle_previous <- subset(wle_previous, !is.na(wle))
    wle_current <- subset(wle_current, !is.na(wle))

    # Select persons for link
    if (is.null(ids)) {
        ids <- get_final_ids(id_previous = wle_previous$ID_t,
                             id_current = wle_current$ID_t,
                             longitudinal_subsample = use_longitudinal_subsample)
    }

    # Link WLEs at second measurement occasion
    correction <- mean(wle_current$wle[wle_current$ID_t %in% ids]) -
        mean(wle_previous$wle[wle_previous$ID_t %in% ids]) -
        const
    wle_current$wle <- wle_current$wle - correction

    # Return results
    return(wle_current)
}


check_var_in_df <- function(df, var, new_var, name) {
    # Set variable name for person identifier
    if (!(var %in% colnames(df))) {
        stop(paste0("Variable ", var, " not found in ", name, "."))
    }
    names(df)[which(names(df)) == var] <- new_var

    return(df)
}


#' Link two samples using mean-mean linking
#'
#' @param resp_previous data.frame with responses of first measurement wave,
#'    a person identifier and a variable indicating valid cases; may also
#'    include a WLE as given in wid
#' @param resp_current data.frame with responses of second measurement wave,
#'    a person identifier and a variable indicating valid cases
#' @param resp_link_sample data.frame with responses of link sample,
#'    a person identifier and a variable indicating valid cases; variables of
#'    the link sample have to be adapted to the item names and the item scoring
#'    in the previous and current measurement
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select_current character; contains name of logical variable in vars
#'   identifying the item set for the current measurement time point
#' @param select_previous character; contains name of logical variable in vars
#'   identifying the item set for the previous measurement time point
#' @param select_link_sample character; contains name of logical variable in vars
#'   identifying the item set for the link sample
#' @param valid string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param scoring string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to the link constant (e.g., only
#'   half the link constant is needed if a PC item is scored 0.5)
#' @param anchors   character vector with link items
#'            if NULL, all common items are used
#' @param longitudinal do within cohort linking (TRUE) or between cohort
#'               linking (FALSE)
#' @param pid       variable name used as person identifier
#' @param wid       variable name used as WLE identifier in first measurement wave
#' @param mvs named integer vector; contains user-defined missing values
#'
#' @return    list: wle = linked WLEs for second measurement with
#'                        standard errors
#'                  const   = linking constant
#'                  const.err = linking error
#'                  anchors = used anchor items
#'                  k = number of anchor items
#'                  xsi = linked item parameters
#' @export
link_samples <- function(resp_previous, resp_current, resp_link_sample = NULL,
                         vars, select_current, select_previous,
                         select_link_sample, valid, scoring,
                         anchors = NULL, longitudinal = TRUE,
                         pid = "ID_t", wid = "wle", mvs = NULL) {

    # Set variable name for person identifier
    resp_previous <- check_var_in_df(df = resp_previous, var = pid,
                                     new_var = "ID_t",
                                     name = get_object_name(resp_previous))
    resp_current <- check_var_in_df(df = resp_current, var = pid,
                                    new_var = "ID_t",
                                    name = get_object_name(resp_current))
    if (!is.null(resp_link_sample)) {
        resp_link_sample <- check_var_in_df(
            df = resp_link_sample, var = pid, new_var = "ID_t",
            name = get_object_name(resp_link_sample)
        )
    }

    # Extract WLEs for first measurement wave
    wle_previous <- NULL
    if (wid %in% colnames(resp_previous)) {
        wle_previous <- resp_previous[, c("ID_t", wid)]
        resp_previous[[wid]] <- NULL
    }

    # Select longitudinal sample
    ids <- get_final_ids(id_previous = resp_previous$ID_t,
                         id_current = resp_current$ID_t,
                         longitudinal_subsample = longitudinal)
    resp_previous_ <- resp_previous[resp_previous$ID_t %in% ids, ]
    resp_current_ <- resp_current[resp_current$ID_t %in% ids, ]

    # select irtmodel
    is_pcm_previous <- any(apply(resp_previous_, 2, max, na.rm = TRUE) > 1)
    is_pcm_current <- any(apply(resp_current_, 2, max, na.rm = TRUE) > 1)

    # Estimate item parameters for first measurement wave
    xsi_previous <- irt_model(resp = resp_previous_, vars = vars,
                              items = select_previous,
                              valid = valid, mvs = mvs,
                              irtmodel = ifelse(is_pcm_previous, "PCM2", "1PL"),
                              scoring = NULL, verbose = FALSE,
                              path = NULL, filename = NULL)$mod$xsi

    # Estimate item parameters for second measurement wave
    xsi_current <- irt_model(resp = resp_current_, vars = vars,
                             items = select_current,
                             valid = valid, mvs = mvs,
                             irtmodel = ifelse(is_pcm_current, "PCM2", "1PL"),
                             scoring = NULL, verbose = FALSE,
                             path = NULL, filename = NULL)$mod$xsi

    # Estimate item parameters for link sample
    xsi_link_sample <- NULL
    if (!is.null(resp_link_sample)) {
        xsi_link_sample <- irt_model(
            resp = resp_link_sample, vars = vars, items = select_link_sample,
            valid = valid, mvs = mvs,
            irtmodel = ifelse(is_pcm_previous | is_pcm_current, "PCM2", "1PL"),
            scoring = NULL, verbose = FALSE,
            path = NULL, filename = NULL
        )$mod$xsi
    }

    res <- calculate_link_parameters(
        is_anchor_items = is.null(resp_link_sample),
        select_previous = select_previous, select_current = select_current,
        anchors = anchors, xsi_previous = xsi_previous,
        xsi_current = xsi_current, select_link_sample = select_link_sample,
        xsi_link_sample = xsi_link_sample, vars = vars, scoring = scoring)
    const <- res$const
    const.err <- res$const.err
    anchors_previous <- res$anchors_previous
    anchors_current <- res$anchors_current
    xsi_current.linked <- res$xsi_current.linked

    # Estimate WLEs for first measurement wave
    if (is.null(wle_previous)) {
        wle_previous <- irt_model(
            resp_previous_, vars = vars, items = select_previous, valid = valid,
            mvs = mvs,
            irtmodel = ifelse(any(apply(resp_previous_, 2, max, na.rm = TRUE) > 1),
                              "PCM2", "1PL"),
            scoring = scoring, verbose = FALSE, path = NULL, filename = NULL)
        wle_previous <- wle_previous$wle
        names(wle_previous)[which(names(wle_previous)) == pid] <- "ID_t"
        names(wle_previous)[which(names(wle_previous)) == wid] <- "wle"
    }

    # Link WLEs
    xsi_current.fixed <- cbind(seq_along(xsi_current$xsi), xsi_current$xsi)
    wle_current <- irt_model(
        resp_current, vars = vars, items = select_current, valid = valid,
        mvs = mvs,
        irtmodel = ifelse(any(apply(resp_previous_, 2, max, na.rm = TRUE) > 1),
                          "PCM2", "1PL"),
        scoring = scoring, verbose = FALSE, path = NULL, filename = NULL,
        xsi.fixed = xsi_current.fixed)$wle
    names(wle_current)[which(names(wle_current)) == pid] <- "ID_t"
    wle_current <- link_wles(wle_previous = wle_previous,
                             wle_current = wle_current, const = const,
                             pid = "ID_t", wid = c(wid, "wle"), ids = ids,
                             use_longitudinal_subsample = longitudinal)
    names(wle_previous)[which(names(wle_previous)) == pid] <- "ID_t"
    names(wle_current)[which(names(wle_current)) == pid] <- "ID_t"

    # Return results
    list(wle_previous = wle_previous, wle_current = wle_current,
         const = const, const.err = const.err,
         xsi_previous = xsi_previous, xsi_current = xsi_current,
         xsi_link_sample = xsi_link_sample,
         xsi_current.linked = xsi_current.linked,
         k = length(c(anchors_previous, anchors_current)),
         anchors = c(anchors_previous, anchors_current),
         longitudinal = longitudinal)
}


get_final_ids <- function(id_previous, id_current, longitudinal_subsample = TRUE) {
    if (longitudinal_subsample) {
        ids <- intersect(id_previous, id_current)
    } else {
        ids <- unique(c(id_previous, id_current))
    }
    return(ids)
}



#' calculate link constant and link error for anchor group or items design
#'
#' @param is_anchor_items logical; whether anchor items (TRUE) or anchor group
#'   (FALSE) design is used
#' @param anchors character vector of selected anchor items
#' @param xsi_previous matrix; item difficulty parameters of previous
#'   measurement
#' @param xsi_current matrix; item difficulty parameters of current measurement
#' @param xsi_link_sample matrix; item difficulty parameters of link sample
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select_current character; contains name of logical variable in vars
#'   identifying the item set for the current measurement time point
#' @param select_previous character; contains name of logical variable in vars
#'   identifying the item set for the previous measurement time point
#' @param select_link_sample string; defines name of logical variable in vars
#'   that indicates the items for the link sample analysis
#' @param scoring string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to the link constant (e.g., only
#'   half the link constant is needed if a PC item is scored 0.5)
#'
#' @returns list(const, const.err, anchors_previous, anchors_current,
#'   xsi_current.linked)
#' @export
calculate_link_parameters <- function(is_anchor_items = TRUE, select_previous,
                                      select_current, anchors = NULL,
                                      xsi_previous, xsi_current,
                                      select_link_sample = NULL,
                                      xsi_link_sample = NULL, vars,
                                      scoring = "scoring") {
    # Calculate linking constant for anchor items design
    if (is_anchor_items) {

        # Identify anchor items
        anchors_previous <- intersect(vars$items[[select_previous]],
                                      vars$items[[select_current]])
        anchors_current <- NULL
        if (!is.null(anchors)) {  # only selected anchors
            anchors_previous <- anchors_previous[anchors_previous %in% anchors]
        }

        # Linking constant
        const <- mean(xsi_previous[anchors_previous, "xsi"]) -
            mean(xsi_current[anchors_previous, "xsi"])

        # Linking error
        xsi_current.linked <- link_item_parameters(xsi = xsi_current,
                                                   const = const, vars = vars,
                                                   select = select_current,
                                                   scoring = scoring)
        const.err <- sd(xsi_previous[anchors_previous, "xsi"] -
                            xsi_current.linked[anchors_previous, 2]) /
            sqrt(length(anchors_previous))

        # Calculate linking constant for anchor group design
    } else {

        # Identify anchor items
        anchors_previous <-  # first measurement
            intersect(vars$items[[select_previous]],
                      vars$items[[select_link_sample]])
        anchors_current <-  # second measurement
            intersect(vars$items[[select_current]],
                      vars$items[[select_link_sample]])
        if (!is.null(anchors)) {               # only selected anchors
            anchors_previous <- anchors_previous[anchors_previous %in% anchors]
            anchors_current <- anchors_current[anchors_current %in% anchors]
        }

        # Linking constants
        const1 <- mean(xsi_previous[anchors_previous, "xsi"]) -
            mean(xsi_link_sample[anchors_previous, "xsi"])
        const2 <- mean(xsi_current[anchors_current, "xsi"]) -
            mean(xsi_link_sample[anchors_current, "xsi"])
        const <- const1 - const2

        # Linking error
        xsi_link_sample.linked <- link_item_parameters(
            xsi = xsi_link_sample, const = const1, vars = vars,
            select = select_link_sample, scoring = scoring)
        xsi_current.linked <- link_item_parameters(
            xsi = xsi_current, const = const2, vars = vars,
            select = select_current, scoring = scoring)
        const.err1 <- sd(xsi_previous[anchors_previous, "xsi"] -
                             xsi_link_sample.linked[anchors_previous, 2]) /
            sqrt(length(anchors_previous))
        const.err2 <- sd(xsi_current.linked[anchors_current, 2] -
                             xsi_link_sample[anchors_current, "xsi"]) /
            sqrt(length(anchors_current))
        const.err <- (const.err1^2 + const.err2^2)^0.5

    }

    return(list(const = const, const.err = const.err,
                anchors_previous = anchors_previous,
                anchors_current = anchors_current,
                xsi_current.linked = xsi_current.linked))
}



#' Test measurement invariance / DIF over time
#'
#' @param resp_previous     data.frame with responses of first measurement wave;
#'   contains ID_t; anchor items are named to fit the current analysis
#' @param resp_current     data.frame with responses of second measurement wave;
#'   contains ID_t
#' @param resp_link_sample data.frame with responses of link sample;
#'   contains ID_t; required if anchor group design is used
#' @param anchors   data.frame with two columns including the link items
#'            in the two waves; if NULL, all common items are used
#' @param mvs named integer vector; contains user-defined missing values
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param items character; contains name of logical variable in vars
#'   identifying the item set for the both time points
#' @param select_current character; contains name of logical variable in vars
#'   identifying the item set for the current measurement time point; needed to
#'   build tables
#' @param select_previous character; contains name of logical variable in vars
#'   identifying the item set for the previous measurement time point; needed to
#'   build tables
#' @return    list with DIF effects:
#'            - xsi: item parameters for anchor items
#'            - dif: difference in item parameters for anchor items
#'            - n: sample sizes for both samples
#'            - Fkrit: critical F statistic for minimum effects test
#'            - df1, df2: degrees of freedom for Fkrit
#'            - eta2: proportion of explained variance (= effect size for Fkrit)
#'            - xsi: standardized mean differences (= effect size for Fkrit)
#' @export
check_dif_anchor <- function(resp_previous, resp_current,
                             resp_link_sample = NULL,
                             anchors = NULL, mvs = NULL, return = TRUE, valid,
                             scoring = "scoring", vars,
                             items = NULL,
                             select_previous, select_current) {

    is_anchor_group <- !is.null(resp_link_sample)

    # identify longitudinal subsample
    ids <- get_final_ids(id_previous = resp_previous$ID_t,
                         id_current = resp_current$ID_t)
    resp_previous_ <- resp_previous[resp_previous$ID_t %in% ids, ]
    resp_current_ <- resp_current[resp_current$ID_t %in% ids, ]

    # Item names
    items_previous <- vars$items[[select_previous]]
    items_current <- vars$items[[select_current]]

    dif <- list()

    if (is_anchor_group) {
        resp <- dplyr::bind_cols(resp_previous_, resp_current_)
        resp$main_sample <- 1
        resp_link_sample$main_sample <- 0
        resp <- dplyr::bind_rows(resp, resp_link_sample)

        dif$dif_model <- dif_model(resp = resp, vars = vars, items = items,
                                   dif_var = "main_sample", scoring = scoring,
                                   valid = valid, verbose = FALSE,
                                   mvs = mvs)

        dif$summary <- dif_summary(dif$dif_model, print = print, save = FALSE,
                                   overwrite = FALSE)

        dif$link_dif_summary <- summarize_link_dif(
            dif_summary = dif$summary, items_previous, items_current,
            save = FALSE, path = NULL, overwrite = FALSE
        )

    } else {
        # select irtmodel
        is_pcm_previous <- any(apply(resp_previous_, 2, max, na.rm = TRUE) > 1)
        is_pcm_current <- any(apply(resp_current_, 2, max, na.rm = TRUE) > 1)

        # Estimate item parameters for first measurement wave
        dif$mod_previous <- irt_model(
            resp = resp_previous_, vars = vars, items = select_previous,
            valid = valid, mvs = mvs,
            irtmodel = ifelse(is_pcm_previous, "PCM2", "1PL"),
            scoring = NULL, verbose = FALSE, path = NULL, filename = NULL
        )$mod

        # Estimate item parameters for second measurement wave
        dif$mod_current <- irt_model(
            resp = resp_current_, vars = vars, items = select_current,
            valid = valid, mvs = mvs,
            irtmodel = ifelse(is_pcm_current, "PCM2", "1PL"),
            scoring = NULL, verbose = FALSE, path = NULL, filename = NULL
        )$mod

        dif$link_dif_summary <- summarize_link_dif(
            mod_current = dif$mod_current, mod_previous = dif$mod_previous,
            items_previous = items_previous, items_current = items_current,
            anchors = anchors)
    }

    if (return) return(dif)
}


summarize_link_dif <- function(dif_summary = NULL, mod_current = NULL,
                               mod_previous = NULL, items_previous,
                               items_current, anchors = NULL,
                               save, path, overwrite) {

    is_anchor_items <- is.null(dif_summary)

    if (is_anchor_items) {
        # Identify anchor items
        if (is.null(anchors)) {
            anchors <- intersect(items_previous, items_current)
            anchors <- cbind(anchors, anchors)
        }

        # Estimate DIF
        est <- data.frame(item_previous = anchors[, 1],
                          item_current = anchors[, 2],
                          xsi_previous = mod_previous$xsi[anchors[, 1], "xsi"],
                          xsi_current = mod_current$xsi[anchors[, 2], "xsi"],
                          se.xsi_previous = mod_previous$xsi[anchors[, 1], "se.xsi"],
                          se.xsi_current = mod_current$xsi[anchors[, 2], "se.xsi"])
        est$xsi_previous <- est$xsi_previous - mean(est$xsi_previous)
        est$xsi_current <- est$xsi_current - mean(est$xsi_current)
        est$xsi <- est$xsi_previous - est$xsi_current
        est$se.xsi <- (est$se.xsi_previous^2 + est$se.xsi_current^2)^0.5

        # Minimum effects test for DIF
        est$F <- (est$xsi / est$se.xsi)^2
        mineff <- meht(stat = est$F, df1 = 1,
                       df2 = mod_previous$nstud + mod_current$nstud - 2,
                       verbose = FALSE)
        est$p <- mineff$pmin
        est$item_previous <- paste0(est$item_previous,
                                    ifelse(est$xsi < 0.5 &
                                               est$p > 0.05, "+", ""))
        est$item_current <- paste0(est$item_current,
                                   ifelse(est$xsi < 0.5 &
                                              est$p > 0.05, "+", ""))
        est$xsi <- paste0(est$xsi, ifelse(est$p < 0.001, "***",
                                          ifelse(est$p < 0.01, "**",
                                                 ifelse(est$p < 0.05, "*", ""))))
        est <- est[, c("item_previous", "item_current", "xsi", "se.xsi", "F")]

        Fkrit <- mineff$Fmin

    } else {

        est <- dif_summary$est
        est_previous <- est[est$item %in% items_previous, ]
        est_previous$item <- paste0(est_previous$item,
                                    ifelse(est_previous$xsi < 0.5 &
                                               est_previous$p > 0.05, "+", ""))
        est_previous$xsi <- paste0(est_previous$xsi,
                                   ifelse(est_previous$p < 0.001, "***",
                                          ifelse(est_previous$p < 0.01, "**",
                                                 ifelse(est_previous$p < 0.05, "*", ""))))
        names(est_previous)[names(est_previous) == "item"] <- "Item 1"
        names(est_previous)[names(est_previous) == "xsi"] <- "Xsi 1"

        est_current <- est[est$item %in% items_current, ]
        est_current$item <- paste0(est_current$item,
                                   ifelse(est_current$xsi < 0.5 &
                                              est_current$p > 0.05, "+", ""))
        est_current$xsi <- paste0(est_current$xsi,
                                  ifelse(est_current$p < 0.001, "***",
                                         ifelse(est_current$p < 0.01, "**",
                                                ifelse(est_current$p < 0.05, "*", ""))))
        names(est_current)[names(est_current) == "item"] <- "Item 2"
        names(est_current)[names(est_current) == "xsi"] <- "Xsi 2"

        Fkrit <- c(est_previous$Fkrit[1], est_current$Fkrit[1])
        est <- dplyr::bind_cols(est_previous[, c("Item 1", "Xsi 1", "std", "F")],
                                est_current[, c("Item 2", "Xsi 2", "std", "F")])
    }

    res <- list(link_dif_table = est,
                Fkrit = Fkrit)
    return(res)
}


#' Test unidimensionality over time
#'
#' Applies only to link sample!
#'
#' @param vars data.frame; contains an item identifier for all
#'   items in the link sample and a dimension identifier for the time points
check_dif_dimensionality <- function(resp_previous, resp_current,
                                     resp_link_sample, vars,
                                     select_current, select_previous,
                                     select_link_sample,
                                     scoring, dim = "link",
                                     mvs, irtmodel = "PCM2", maxiter,
                                     snodes, verbose = FALSE, valid) {
    if (is.null(select_link_sample)) {
        vars$common <- vars[[select_current]] * vars[[select_previous]]
        dimensionality <- conduct_dim_analysis(
            resp = dplyr::bind_rows(resp_previous, resp_current), vars,
            items = "common", scoring, dim = "common", valid, irtmodel,
            maxiter, snodes, verbose, mvs
        )
    } else {
        dimensionality <- conduct_dim_analysis(
            resp = resp_link_sample, vars, items = select_link_sample, scoring,
            dim, valid, irtmodel, maxiter, snodes, verbose, mvs)
    }
    dimsum <- dim_summary(dimensionality)

    return(list(dimensionality = dimensionality, dim_sum = dimsum))
}


print_link_results <- function(link_dif, link_dim = NULL, link_results) {
    is_anchor_groups <- !is.null(link_dim)

    N <- paste0("N long. subsample: ",
                length(get_final_ids(link_results$wle_previous$ID_t,
                                     link_results$wle_current$ID_t)), "\n")
    Fkrit <- paste0("The critical F value for the DIF between time points is: ",
                    link_dif$link_dif_summary$Fkrit, "\n")

    dif_table <- paste0("\n\n",
                        "Items eligible for link (marked with +): ",
                        "|xsi| < 0.5 & p > 0.05; *: p < 0.05; **: p < 0.01; ",
                        "***: p < 0.001\n\n")

    const <- paste0("Link constant: ", link_results$const, "\n",
                    "Link error: ", link_results$const.err, "\n")

    if (is_anchor_groups) {
        N <- paste0(N,
                    "N link sample: ", link_dim$dimensionality$uni$nstud, "\n")

        xsi1 <- link_dif$link_dif_summary$link_dif_table[["Xsi 1"]]
        xsi2 <- link_dif$link_dif_summary$link_dif_table[["Xsi 2"]]
        usable <- paste0("No. of usable items for link at time 1: ",
                         sum(!grepl("*", xsi1)), " ",
                         round(sum(!grepl("*", xsi1)) / length(xsi1), 3) * 100,
                         "%\n",
                         "No. of usable items for link at time 2: ",
                         sum(!grepl("*", xsi2)), " ",
                         round(sum(!grepl("*", xsi2)) / length(xsi2), 3) * 100,
                         "%\n")
        xsi1 <- gsub("*", "", xsi1)
        xsi1 <- as.numeric(xsi1)
        xsi2 <- gsub("*", "", xsi2)
        xsi2 <- as.numeric(xsi2)
        dif_table <- paste0(
            dif_table,
            "Min. estimate (previous time point): \n", min(xsi1, na.rm = T), "\n",
            "Min. estimate (current time point): \n", min(xsi2, na.rm = T), "\n",
            "Max. estimate (previous time point): \n", max(xsi1, na.rm = T), "\n",
            "Min. estimate (current time point): \n", max(xsi2, na.rm = T), "\n")
    } else {
        xsi <- link_dif$link_dif_summary$link_dif_table[["xsi"]]
        usable <- paste0("No. of usable items for link: ", sum(!grepl("*", xsi)),
                         " ",
                         round(sum(!grepl("*", xsi)) / length(xsi), 3) * 100,
                         "%\n")
        xsi <- gsub("*", "", xsi)
        xsi <- as.numeric(xsi)
        dif_table <- paste0(
            dif_table,
            "Min. estimate: \n", min(xsi, na.rm = T), "\n",
            "Min. estimate: \n", max(xsi, na.rm = T), "\n")
        #
    }
    # print N longitudinal subsample, N link study
    cat(N)
    # print link_dif$link_dif_table and link_dif$Fkrit
    # --> criteria: less than 0.5 logit difference, non-significant (5% level)
    # --> "+" usable, "*/**/***" usual meaning
    # print min/max of absolute logit differences for tp1 and tp2
    cat("Difference estimates and significance status of the ")
    cat("DIF estimates: \n")
    print(link_dif$link_dif_summary$link_dif_table)
    cat(dif_table)
    cat(Fkrit)
    # print absolute number and percentage of usable items for tp1 and tp2
    cat(usable)
    # print AIC/BIC of link_dim
    lapply(link_dim$dim_sum, print)
    # print link constant and link error (link_results)
    cat(const)

}
