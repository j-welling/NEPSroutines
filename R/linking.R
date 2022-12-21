#' Mean/mean linking of current and previous study
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param pweights_curr string; defines name of numerical variable in resp_curr
#' that contains the person weights for the current measurement wave passed to
#' TAM-functions
#' @param pweights_prev string; defines name of numerical variable in resp_prev
#' that contains the person weights for the previous measurement wave passed to
#' TAM-functions
#' @param pweights_link string; defines name of numerical variable in resp_link
#' that contains the person weights for the link study passed to TAM-functions;
#' optional variable for anchor group design
#' @param mvs  named integer vector; contains user-defined missing values
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param longitudinal  logical; do within cohort linking (TRUE) or between
#'   cohort linking (FALSE)
#' @param path_results  string; defines path to folder where results shall be
#'   saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param overwrite  logical; whether to overwrite existing file when saving
#'   table
#' @param verbose  logical; verbose as passed to the TAM function
#' @param dif_threshold  numeric; threshold under which DIF in common link items
#'   is accepted; defaults to 0.5
#' @param wid  string; variable name used as WLE identifier in first measurement wave
#' @param snodes  snodes as passed to the TAM function for the dimensionality
#'   analyses
#' @param maxiter maximum number of iterations as passed to the TAM function
#'   for the dimensionality analyses
#' @param digits  numeric; number of decimal places for rounding
#' @param control_tam list; function argument as passed to TAM-functions
#' @param do_dif logical; do DIF analyses
#' @param do_dim logical; do dimensionality analyses
#' @param warn  logical; whether to print warnings
#'
#' @export
linking <- function(resp_curr,
                    resp_prev,
                    resp_link = NULL,
                    vars_curr,
                    vars_prev,
                    vars_link = NULL,
                    select_curr,
                    select_prev,
                    select_link = NULL,
                    valid_curr = NULL,
                    valid_prev = NULL,
                    valid_link = NULL,
                    scoring_curr = NULL,
                    scoring_prev = NULL,
                    scoring_link = NULL,
                    pweights_curr = NULL,
                    pweights_prev = NULL,
                    pweights_link = NULL,
                    mvs = NULL,
                    anchors = NULL,
                    longitudinal = TRUE,
                    path_table = here::here("Tables"),
                    path_results = here::here("Results"),
                    print = TRUE, save = TRUE, return = FALSE,
                    overwrite = FALSE, verbose = TRUE,
                    wid = NULL, dif_threshold = 0.5,
                    snodes = 5000, maxiter = 10000,
                    digits = 3, control_tam = NULL,
                    do_dif = TRUE, do_dim = TRUE, warn = TRUE) {

    # Test data
    test_linking_data(
        resp_curr = resp_curr,
        resp_prev = resp_prev,
        resp_link = resp_link,
        vars_curr = vars_curr,
        vars_prev = vars_prev,
        vars_link = vars_link,
        select_curr = select_curr,
        select_prev = select_prev,
        select_link = select_link,
        valid_curr = valid_curr,
        valid_prev = valid_prev,
        valid_link = valid_link,
        scoring_curr = scoring_curr,
        scoring_prev = scoring_prev,
        scoring_link = scoring_link,
        pweights_curr = pweights_curr,
        pweights_prev = pweights_prev,
        pweights_link = pweights_link,
        anchors = anchors,
        mvs = mvs,
        warn = warn
    )

    # Check measurement invariance over time
    if (do_dif) {
        link_dif <-
            check_dif_anchor(
                resp_curr = resp_curr,
                resp_prev = resp_prev,
                resp_link = resp_link,
                vars_curr = vars_curr,
                vars_prev = vars_prev,
                vars_link = vars_link,
                select_curr = select_curr,
                select_prev = select_prev,
                select_link = select_link,
                valid_curr = valid_curr,
                valid_prev = valid_prev,
                valid_link = valid_link,
                scoring_curr = scoring_curr,
                scoring_prev = scoring_prev,
                scoring_link = scoring_link,
                pweights_curr = pweights_curr,
                pweights_prev = pweights_prev,
                pweights_link = pweights_link,
                anchors = anchors,
                mvs = mvs,
                return = TRUE,
                dif_threshold = dif_threshold,
                control_tam = control_tam,
                warn = FALSE,
                test = FALSE
            )
    } else {
        link_dif <- NULL
    }

    # Check unidimensionality for anchor items
    if (do_dim) {
        link_dim <-
            check_link_dimensionality(
                resp_curr = resp_curr,
                resp_prev = resp_prev,
                resp_link = resp_link,
                vars_curr = vars_curr,
                vars_prev = vars_prev,
                vars_link = vars_link,
                select_curr = select_curr,
                select_prev = select_prev,
                select_link = select_link,
                valid_curr = valid_curr,
                valid_prev = valid_prev,
                valid_link = valid_link,
                scoring_curr = scoring_curr,
                scoring_prev = scoring_prev,
                scoring_link = scoring_link,
                mvs = mvs,
                anchors = anchors,
                verbose = FALSE,
                snodes = snodes,
                maxiter = maxiter,
                warn = FALSE,
                test = FALSE
            )
    } else {
        link_dim <- NULL
    }

    # Conduct linking
    link_results <-
        link_samples(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            pweights_curr = pweights_curr,
            pweights_prev = pweights_prev,
            pweights_link = pweights_link,
            anchors = anchors,
            longitudinal = longitudinal,
            mvs = mvs,
            wid = wid,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )

    # Print results
    if (print & do_dif & do_dim) {
        print_link_results(
            link_dif = link_dif,
            link_dim = link_dim,
            link_results = link_results,
            dif_threshold = dif_threshold,
            digits = digits
        )
    }

    # Save results
    if (save) {
        if (do_dif) {
            if (is.null(resp_link)) {
                varcor <- link_dim$dim_sum$`Cor-Var anchoritems`
            } else {
                varcor <- link_dim$dim_sum$`Cor-Var wave`
            }
            scaling:::save_table(
                list(DIF = link_dif$link_dif_summary$link_dif_table,
                     'Dimensionality (Cor-Var)' = varcor,
                     'Dimensionality (GoF)' = link_dim$dim_sum$`Goodness Of fit`),
                filename = "link_tr_tables.xlsx",
                path = path_table,
                overwrite = overwrite,
                show_rownames = FALSE
            )
            scaling:::save_results(
                link_dif,
                filename = "link_dif_results.rds",
                path = path_results
            )
        }
        if (do_dim) {
            scaling:::save_results(
                link_dim,
                filename = "link_dim_results.rds",
                path = path_results
            )
        }
        scaling:::save_results(
            link_results,
            filename = "link_analysis_results.rds",
            path = path_results
        )
    }

    # Return results
    if (return) {
        return(list(link_dif = link_dif, link_dim = link_dim,
                    link_results = link_results))
    }

}


################################################################################
########################### Prepare longitudinal data ##########################
################################################################################

#' Prepare response data for linking
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param mvs named integer vector; contains user-defined missing values
#' @param wid  variable name used as WLE identifier in first measurement wave
#' @param use_longitudinal_subsample logical; whether an intra-cohort link (TRUE)
#'   using only persons taking part in both time points or
#'   an inter-cohort link (FALSE) is conducted
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @returns    list with response data
#' @export
prepare_longitudinal_resp <- function(resp_curr,
                                      resp_prev,
                                      resp_link = NULL,
                                      vars_curr,
                                      vars_prev,
                                      vars_link = NULL,
                                      select_curr,
                                      select_prev,
                                      select_link = NULL,
                                      valid_curr = NULL,
                                      valid_prev = NULL,
                                      valid_link = NULL,
                                      anchors = NULL,
                                      mvs = NULL,
                                      wid = "wle",
                                      use_longitudinal_subsample = TRUE,
                                      warn = TRUE,
                                      test = TRUE) {

    # Test data
    if (test) {
        test_linking_data(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            anchors = anchors,
            mvs = mvs,
            warn = warn
        )
    }

    # Identify linking design
    is_anchor_group <- !is.null(resp_link)

    # Check if ID_ts are available
    scaling:::check_variables(resp_curr, "resp_curr", "ID_t")
    scaling:::check_variables(resp_prev, "resp_prev", "ID_t")
    if (is_anchor_group)
        scaling:::check_variables(resp_link, "resp_link", "ID_t")

    # Prepare response data
    resp_curr <-
        scaling:::prepare_resp(
            resp = resp_curr,
            valid = valid_curr,
            use_only_valid = TRUE,
            convert = TRUE,
            mvs = mvs,
            warn = FALSE
        )
    resp_prev <-
        scaling:::prepare_resp(
            resp = resp_prev,
            valid = valid_prev,
            use_only_valid = TRUE,
            convert = TRUE,
            mvs = mvs,
            warn = FALSE
        )
    if (is_anchor_group) {
        resp_link <-
            scaling:::prepare_resp(
                resp = resp_link,
                valid = valid_link,
                use_only_valid = TRUE,
                convert = TRUE,
                mvs = mvs,
                warn = FALSE
            )
    }

    # Identify longitudinal subsample
    ids <-
        get_final_ids(
            id_prev = resp_prev$ID_t,
            id_curr = resp_curr$ID_t,
            longitudinal_subsample = use_longitudinal_subsample
        )

    # Select longitudinal sample
    resp_prev <- resp_prev[resp_prev$ID_t %in% ids, ]
    resp_curr <- resp_curr[resp_curr$ID_t %in% ids, ]

    # Determine WLE in previous assessment
    if (!is.null(wid) && wid %in% colnames(resp_prev)) {
        wle_prev <- resp_prev[, c("ID_t", wid)]
        resp_prev[[wid]] <- NULL
    } else {
        wle_prev <- NULL
    }

    # Item names
    items_curr <- vars_curr$item[vars_curr[[select_curr]]]
    scaling:::check_numerics(resp_curr, "resp_curr", items_curr, check_invalid = TRUE)

    items_prev <- vars_prev$item[vars_prev[[select_prev]]]
    scaling:::check_numerics(resp_prev, "resp_prev", items_prev, check_invalid = TRUE)

    if (is_anchor_group) {
        items_link <- vars_link$item[vars_link[[select_link]]]
        scaling:::check_numerics(resp_link, "resp_link", items_link, check_invalid = TRUE)
    } else {
        items_link <- NULL
    }

    # Identify IRT model
    is_pcm_curr <-
        scaling:::is_poly(
            resp_curr[, items_curr],
            vars = vars_curr,
            select = select_curr
        )
    is_pcm_prev <-
        scaling:::is_poly(
            resp_prev[, items_prev],
            vars = vars_prev,
            select = select_prev
        )
    if (is_anchor_group) {
        is_pcm_link <-
            scaling:::is_poly(
                resp_link[, items_link],
                vars = vars_link,
                select = select_link
            )
    } else {
        is_pcm_link <- NULL
    }

    # Identify anchor items
    if (is.null(anchors)) {
        if (is_anchor_group) {
            anchors <- intersect(c(items_prev, items_curr), items_link)
            anchors <- cbind(anchors, anchors)
            colnames(anchors) <- c("Main", "Link")
        } else {
            anchors <- intersect(items_prev, items_curr)
            anchors <- cbind(anchors, anchors)
            colnames(anchors) <- c("Previous", "Current")
        }
    }
    if (any(dim(anchors) < 2))
        stop("No anchor items found!")

    # Return results
    out <- list(
        resp_curr = resp_curr,
        resp_prev = resp_prev,
        resp_link = resp_link,
        items_curr = items_curr,
        items_prev = items_prev,
        items_link = items_link,
        is_pcm_curr = is_pcm_curr,
        is_pcm_prev = is_pcm_prev,
        is_pcm_link = is_pcm_link,
        is_anchor_group = is_anchor_group,
        anchors = anchors,
        wle_prev = wle_prev
    )
    return(out)

}



#' Get IDs for linking
#'
#' @param id_prev integer vector; person identifiers of previous assessment
#' @param id_curr integer vector; person identifiers of current assessment
#' @param longitudinal_subsample logical; whether an intra-cohort link (TRUE)
#'   using only persons taking part in both time points or
#'   an inter-cohort link (FALSE) is conducted
#' @noRd
get_final_ids <- function(id_prev, id_curr,
                          longitudinal_subsample = TRUE) {
    if (longitudinal_subsample) {
        ids <- intersect(id_prev, id_curr)
    } else {
        ids <- unique(c(id_prev, id_curr))
    }
    return(ids)
}


################################################################################
##################################### DIF ######################################
################################################################################

#' Test measurement invariance / DIF over time
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param pweights_curr string; defines name of numerical variable in resp_curr
#' that contains the person weights for the current measurement wave passed to
#' TAM-functions
#' @param pweights_prev string; defines name of numerical variable in resp_prev
#' that contains the person weights for the previous measurement wave passed to
#' TAM-functions
#' @param pweights_link string; defines name of numerical variable in resp_link
#' that contains the person weights for the link study passed to TAM-functions;
#' optional variable for anchor group design
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param mvs named integer vector; contains user-defined missing values
#' @param control_tam list; control argument as passed to tam.mml-functions
#' @param return  logical; whether results shall be returned
#' @param dif_threshold numeric; threshold under which DIF in common link items
#'   is accepted; defaults to 0.5
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @returns    list with DIF model(s), DIF summary and anchor items
#' @export
check_dif_anchor <- function(resp_curr,
                             resp_prev,
                             resp_link = NULL,
                             vars_curr,
                             vars_prev,
                             vars_link = NULL,
                             select_curr,
                             select_prev,
                             select_link = NULL,
                             valid_curr = NULL,
                             valid_prev = NULL,
                             valid_link = NULL,
                             scoring_curr = NULL,
                             scoring_prev = NULL,
                             scoring_link = NULL,
                             pweights_curr = NULL,
                             pweights_prev = NULL,
                             pweights_link = NULL,
                             anchors = NULL,
                             mvs = NULL,
                             control_tam = NULL,
                             return = TRUE,
                             dif_threshold = 0.5,
                             warn = TRUE,
                             test = TRUE) {

    # Test data
    if (test) {
        test_linking_data(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            pweights_curr = pweights_curr,
            pweights_prev = pweights_prev,
            pweights_link = pweights_link,
            anchors = anchors,
            mvs = mvs,
            warn = warn
        )
    }

    # Preprocess data
    pre_dat <-
        prepare_longitudinal_resp(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            anchors = anchors,
            mvs = mvs,
            warn = FALSE,
            test = FALSE
        )

    dif <- list()

    # Estimate item parameters for first measurement wave
    dif$mod_prev <-
        scaling:::irt_model(
            resp = pre_dat$resp_prev,
            vars = vars_prev,
            select = select_prev,
            valid = valid_prev,
            scoring = scoring_prev,
            mvs = mvs,
            irtmodel = ifelse(pre_dat$is_pcm_prev, "PCM2", "1PL"),
            verbose = FALSE,
            path = NULL,
            filename = NULL,
            pweights = pweights_prev,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )$mod

    # Estimate item parameters for second measurement wave
    dif$mod_curr <-
        scaling:::irt_model(
            resp = pre_dat$resp_curr,
            vars = vars_curr,
            select = select_curr,
            valid = valid_curr,
            scoring = scoring_curr,
            mvs = mvs,
            irtmodel = ifelse(pre_dat$is_pcm_curr, "PCM2", "1PL"),
            verbose = FALSE,
            path = NULL,
            filename = NULL,
            pweights = pweights_curr,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )$mod

    # Estimate item parameters for link study
    if (pre_dat$is_anchor_group) {
        dif$mod_link <-
            scaling:::irt_model(
                resp = pre_dat$resp_link,
                vars = vars_link,
                select = select_link,
                valid = valid_link,
                scoring = scoring_link,
                mvs = mvs,
                irtmodel = ifelse(pre_dat$is_pcm_link, "PCM2", "1PL"),
                verbose = FALSE,
                path = NULL,
                filename = NULL,
                pweights = pweights_link,
                control_tam = control_tam,
                warn = FALSE,
                test = FALSE
            )$mod
    }

    # Estimate DIF effects
    dif$link_dif_summary <-
        summarize_link_dif(
            mod_curr = dif$mod_curr,
            mod_prev = dif$mod_prev,
            mod_link = dif$mod_link,
            items_prev = pre_dat$items_prev,
            items_curr = pre_dat$items_curr,
            items_link = pre_dat$items_link,
            anchors = pre_dat$anchors,
            dif_threshold = dif_threshold
        )
    f <- grepl("^item_.+$", names(dif$link_dif_summary$link_dif_table))
    dif$anchors <- dif$link_dif_summary$link_dif_table[, f]

    if (return) return(dif)

}



#' Summarize link DIF results
#'
#' @param mod_curr return object of irt_model() for current assessment
#' @param mod_prev return object of irt_model() for previous assessment
#' @param mod_link return object of irt_model() for link study
#' @param items_curr character vector; item names of current assessment
#' @param items_prev character vector; item names of previous assessment
#' @param items_link character vector; item names of link study
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param dif_threshold numeric; threshold under which DIF in common link items
#'   is accepted; defaults to 0.5
#'
#' @returns list with DIF results
#' @export
summarize_link_dif <- function(mod_curr,
                               mod_prev,
                               mod_link = NULL,
                               items_curr,
                               items_prev,
                               items_link = NULL,
                               anchors = NULL,
                               dif_threshold = 0.5) {

    # Identify linking design
    is_anchor_items <- is.null(mod_link)

    # Anchor items design
    if (is_anchor_items) {

        # Estimate DIF
        est <- data.frame(item_prev = anchors[, 1],
                          item_curr = anchors[, 2],
                          xsi_prev = mod_prev$xsi[anchors[, 1], "xsi"],
                          xsi_curr = mod_curr$xsi[anchors[, 2], "xsi"],
                          se.xsi_prev = mod_prev$xsi[anchors[, 1], "se.xsi"],
                          se.xsi_curr = mod_curr$xsi[anchors[, 2], "se.xsi"])
        est$xsi_prev <- est$xsi_prev - mean(est$xsi_prev)
        est$xsi_curr <- est$xsi_curr - mean(est$xsi_curr)
        est$xsi <- est$xsi_prev - est$xsi_curr
        est$se.xsi <- (est$se.xsi_prev^2 + est$se.xsi_curr^2)^0.5
        est$df <- mod_prev$nstud + mod_curr$nstud - 2

        # Anchor group design
    } else {

        # Estimate DIF
        anchors_prev <- anchors[anchors[, 1] %in% items_prev, ]
        anchors_curr <- anchors[anchors[, 1] %in% items_curr, ]
        est <- data.frame(wave = c(rep("previous", nrow(anchors_prev)),
                                   rep("current", nrow(anchors_curr))),
                          item_main = anchors[, 1],
                          item_link = anchors[, 2],
                          xsi_main = c(mod_prev$xsi[anchors_prev[, 1], "xsi"],
                                       mod_curr$xsi[anchors_curr[, 1], "xsi"]),
                          xsi_link = mod_link$xsi[c(anchors_prev[, 2],
                                                    anchors_curr[, 2]), "xsi"],
                          se.xsi_main = c(mod_prev$xsi[anchors_prev[, 1], "se.xsi"],
                                          mod_curr$xsi[anchors_curr[, 1], "se.xsi"]),
                          se.xsi_link = mod_link$xsi[c(anchors_prev[, 2],
                                                       anchors_curr[, 2]), "se.xsi"])
        est$xsi_main <- est$xsi_main - mean(est$xsi_main)
        est$xsi_link <- est$xsi_link - mean(est$xsi_link)
        est$xsi <- est$xsi_main - est$xsi_link
        est$se.xsi <- (est$se.xsi_main^2 + est$se.xsi_link^2)^0.5
        est$df <- c(rep(mod_prev$nstud, nrow(anchors_prev)),
                    rep(mod_curr$nstud, nrow(anchors_curr))) +
            mod_link$nstud - 2
    }

    # Minimum effects test for DIF
    est$F <- (est$xsi / est$se.xsi)^2
    mineff <-
        meht(
            stat = est$F,
            df1 = 1,
            df2 = est$df,
            verbose = FALSE
        )
    est$p <- mineff$pmin
    est$dif_flag <-
        ifelse(
            abs(est$xsi) > dif_threshold & est$p < 0.05,
            "*",
            ifelse(abs(est$xsi) > dif_threshold | est$p < 0.05, "+", "")
        )
    est$se.xsi <- round(est$se.xsi, 3)
    est <- est[, grepl("(^wave$)|(^item_.+$)|(^xsi$)|(^se.xsi$)|(^F$)|(^p$)|(^dif_flag$)",
                       names(est))]

    res <- list(link_dif_table = est, Fkrit = mineff$Fmin, dfkrit = mineff$df2)
    return(res)
}


################################################################################
##################################### DIM ######################################
################################################################################

#' Test unidimensionality over time
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param mvs named integer vector; contains user-defined missing values
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param maxiter  max iterations as passed to the TAM function
#' @param snodes  snodes as passed to the TAM function
#' @param verbose   verbose as passed to the TAM function
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @returns list with dimensionality results
#' @export
check_link_dimensionality <- function(resp_curr,
                                      resp_prev,
                                      resp_link = NULL,
                                      vars_curr,
                                      vars_prev,
                                      vars_link = NULL,
                                      select_curr,
                                      select_prev,
                                      select_link = NULL,
                                      valid_curr = NULL,
                                      valid_prev = NULL,
                                      valid_link = NULL,
                                      scoring_curr = NULL,
                                      scoring_prev = NULL,
                                      scoring_link = NULL,
                                      mvs = NULL,
                                      anchors = NULL,
                                      verbose = FALSE,
                                      maxiter = 10000,
                                      snodes = 5000,
                                      warn = TRUE,
                                      test = TRUE) {

    # Test data
    if (test) {
        test_linking_data(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            anchors = anchors,
            mvs = mvs,
            warn = warn
        )
    }

    # Preprocess data
    pre_dat <-
        prepare_longitudinal_resp(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            anchors = anchors,
            mvs = mvs,
            warn = FALSE,
            test = FALSE
        )

    # Anchor item design
    if (pre_dat$is_anchor_group) {

        vars_link$wave <- ifelse(vars_link$item %in% pre_dat$items_prev, 1,
                                 ifelse(vars_link$item %in% pre_dat$items_curr, 2,
                                        NA))
        dimensionality <-
            scaling:::conduct_dim_analysis(
                resp = pre_dat$resp_link,
                vars = vars_link,
                select = select_link,
                scoring = scoring_link,
                dim = "wave",
                irtmodel = ifelse(pre_dat$is_pcm_link, "PCM2", "1PL"),
                maxiter = maxiter,
                snodes = snodes,
                verbose = verbose,
                mvs = mvs,
                warn = FALSE,
                test = FALSE
            )

        dimsum <- dim_summary(dimensionality)

        # Anchor group design
    } else {

        dimensionality <- list()
        dimsum <- list()

        # Dimensionality of previous assessment
        vars_prev$anchoritems <- ifelse(vars_prev$item %in% pre_dat$anchors[, 1], 2,
                                        ifelse(vars_prev$item %in% pre_dat$items_prev, 1,
                                               NA))

        dimensionality$previous <-
            scaling:::conduct_dim_analysis(
                resp = pre_dat$resp_prev,
                vars = vars_prev,
                select = select_prev,
                scoring = scoring_prev,
                dim = "anchoritems",
                irtmodel = ifelse(pre_dat$is_pcm_prev, "PCM2", "1PL"),
                maxiter = maxiter,
                snodes = snodes,
                verbose = verbose,
                mvs = mvs,
                warn = FALSE,
                test = FALSE
            )
        dimsum$previous <- dim_summary(dimensionality$previous)

        # Dimensionality of current assessment
        vars_curr$anchoritems <- ifelse(vars_curr$item %in% pre_dat$anchors[, 2], 2,
                                        ifelse(vars_curr$item %in% pre_dat$items_curr, 1,
                                               NA))
        dimensionality$current <-
            scaling:::conduct_dim_analysis(
                resp = pre_dat$resp_curr,
                vars = vars_curr,
                select = select_curr,
                scoring = scoring_curr,
                dim = "anchoritems",
                irtmodel = ifelse(pre_dat$is_pcm_curr, "PCM2", "1PL"),
                maxiter = maxiter,
                snodes = snodes,
                verbose = verbose,
                mvs = mvs,
                warn = FALSE,
                test = FALSE
            )
        dimsum$current <- dim_summary(dimensionality$current)
    }

    return(list(dimensionality = dimensionality, dim_sum = dimsum))

}


################################################################################
#################################### Linking ###################################
################################################################################

#' Link two samples using mean-mean linking
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param pweights_curr string; defines name of numerical variable in resp_curr
#' that contains the person weights for the current measurement wave passed to
#' TAM-functions
#' @param pweights_prev string; defines name of numerical variable in resp_prev
#' that contains the person weights for the previous measurement wave passed to
#' TAM-functions
#' @param pweights_link string; defines name of numerical variable in resp_link
#' that contains the person weights for the link study passed to TAM-functions;
#' optional variable for anchor group design
#' @param wid variable name used as WLE identifier in first measurement wave
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param longitudinal do within cohort linking (TRUE) or between cohort linking
#' (FALSE)
#' @param mvs named integer vector; contains user-defined missing values
#' @param control_tam list; control argument as passed to tam.mml-functions
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return    list: wle = linked WLEs for second measurement with
#'                        standard errors
#'                  const   = linking constant
#'                  const.err = linking error
#'                  anchors = used anchor items
#'                  k = number of anchor items
#'                  xsi = linked item parameters
#' @export
link_samples <- function(resp_curr,
                         resp_prev,
                         resp_link = NULL,
                         vars_curr,
                         vars_prev,
                         vars_link = NULL,
                         select_curr,
                         select_prev,
                         select_link = NULL,
                         valid_curr = NULL,
                         valid_prev = NULL,
                         valid_link = NULL,
                         scoring_curr = NULL,
                         scoring_prev = NULL,
                         scoring_link = NULL,
                         pweights_curr = NULL,
                         pweights_prev = NULL,
                         pweights_link = NULL,
                         wid = NULL,
                         anchors = NULL,
                         longitudinal = TRUE,
                         mvs = NULL,
                         control_tam = NULL,
                         warn = TRUE,
                         test =TRUE) {

    # Test data
    if (test) {
        test_linking_data(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            pweights_curr = pweights_curr,
            pweights_prev = pweights_prev,
            pweights_link = pweights_link,
            anchors = anchors,
            mvs = mvs,
            warn = warn
        )
    }

    # Preprocess data
    pre_dat <-
        prepare_longitudinal_resp(
            resp_curr = resp_curr,
            resp_prev = resp_prev,
            resp_link = resp_link,
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            valid_curr = valid_curr,
            valid_prev = valid_prev,
            valid_link = valid_link,
            anchors = anchors,
            mvs = mvs,
            wid = wid,
            warn = FALSE,
            test = FALSE
        )

    # Estimate item parameters for first measurement wave
    xsi_prev <-
        scaling:::irt_model(
            resp = pre_dat$resp_prev,
            vars = vars_prev,
            select = select_prev,
            valid = valid_prev,
            scoring = scoring_prev,
            mvs = mvs,
            irtmodel = ifelse(pre_dat$is_pcm_prev, "PCM2", "1PL"),
            verbose = FALSE,
            path = NULL,
            filename = NULL,
            pweights = pweights_prev,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )$mod$xsi

    # Estimate item parameters for second measurement wave
    xsi_curr <-
        scaling:::irt_model(
            resp = pre_dat$resp_curr,
            vars = vars_curr,
            select = select_curr,
            valid = valid_curr,
            scoring = scoring_curr,
            mvs = mvs,
            irtmodel = ifelse(pre_dat$is_pcm_curr, "PCM2", "1PL"),
            verbose = FALSE,
            path = NULL,
            filename = NULL,
            pweights = pweights_curr,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )$mod$xsi

    # Estimate item parameters for link study
    if (pre_dat$is_anchor_group) {
        xsi_link <-
            scaling:::irt_model(
                resp = pre_dat$resp_link,
                vars = vars_link,
                select = select_link,
                valid = valid_link,
                scoring = scoring_link,
                mvs = mvs,
                irtmodel = ifelse(pre_dat$is_pcm_link, "PCM2", "1PL"),
                verbose = FALSE,
                path = NULL,
                filename = NULL,
                pweights = pweights_link,
                control_tam = control_tam,
                warn = FALSE,
                test = FALSE
            )$mod$xsi
    } else {
        xsi_link <- NULL
    }

    # Calculate linking constant
    res <-
        calculate_link_parameters(
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            xsi_prev = xsi_prev,
            xsi_curr = xsi_curr,
            xsi_link = xsi_link,
            anchors = pre_dat$anchors,
            warn = FALSE,
            test = FALSE
        )

    # WLEs for first measurement wave
    if (is.null(pre_dat$wle_prev)) {
        wle_prev <-
            scaling:::irt_model(
                resp = resp_prev,
                vars = vars_prev,
                select = select_prev,
                valid = valid_prev,
                scoring = scoring_prev,
                mvs = mvs,
                irtmodel = ifelse(pre_dat$is_pcm_prev, "PCM2", "1PL"),
                verbose = FALSE,
                path = NULL,
                filename = NULL,
                pweights = pweights_prev,
                control_tam = control_tam,
                warn = FALSE,
                test = FALSE
            )$wle
        wle_prev <- as.data.frame(wle_prev)
        names(wle_prev)[which(names(wle_prev) == "pid")] <- "ID_t"
        names(wle_prev)[which(names(wle_prev) == "theta")] <- "wle"
        wid <- "wle"
    } else {
        wle_prev <- pre_dat$wle_prev
    }

    # WLEs for second measurement wave
    wle_curr <-
        scaling:::irt_model(
            resp = resp_curr,
            vars = vars_curr,
            select = select_curr,
            valid = valid_curr,
            scoring = scoring_curr,
            mvs = mvs,
            irtmodel = ifelse(pre_dat$is_pcm_curr, "PCM2", "1PL"),
            verbose = FALSE,
            path = NULL,
            filename = NULL,
            pweights = pweights_curr,
            control_tam = control_tam,
            warn = FALSE,
            test = FALSE
        )$wle
    wle_curr <- as.data.frame(wle_curr)
    names(wle_curr)[which(names(wle_curr) == "pid")] <- "ID_t"
    names(wle_curr)[which(names(wle_curr) == "theta")] <- "wle"

    # Link WLEs
    wle_linked <-
        link_wles(
            wle_prev = wle_prev,
            wle_curr = wle_curr,
            const = res$const,
            wid = c(wid, "wle"),
            use_longitudinal_subsample = longitudinal
        )

    # Return results
    out <-
        list(
            wle_curr = wle_curr,
            wle_prev = wle_prev,
            wle_linked = wle_linked,
            const = res$const,
            const.err = res$const.err,
            xsi_prev = xsi_prev,
            xsi_curr = xsi_curr,
            xsi_link = xsi_link,
            xsi_curr.linked = res$xsi_curr.linked,
            k = nrow(pre_dat$anchors),
            anchors = pre_dat$anchors,
            longitudinal = longitudinal
        )
    return(out)

}


#' calculate link constant and link error for anchor group or items design
#'
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param xsi_curr matrix; item difficulty parameters of current measurement
#' @param xsi_prev matrix; item difficulty parameters of previous measurement
#' @param xsi_link matrix; item difficulty parameters of link sample
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @returns list(const, const.err, anchors_prev, anchors_curr, xsi_curr.linked)
#' @export
calculate_link_parameters <- function(vars_curr,
                                      vars_prev,
                                      vars_link = NULL,
                                      select_curr,
                                      select_prev,
                                      select_link = NULL,
                                      scoring_curr = NULL,
                                      scoring_prev = NULL,
                                      scoring_link = NULL,
                                      xsi_curr,
                                      xsi_prev,
                                      xsi_link = NULL,
                                      anchors = NULL,
                                      warn = TRUE,
                                      test = TRUE) {

    # Test data
    if (test) {
        test_linking_data(
            vars_curr = vars_curr,
            vars_prev = vars_prev,
            vars_link = vars_link,
            select_curr = select_curr,
            select_prev = select_prev,
            select_link = select_link,
            scoring_curr = scoring_curr,
            scoring_prev = scoring_prev,
            scoring_link = scoring_link,
            anchors = anchors,
            warn = warn
        )
    }

    # Identify linking design
    is_anchor_group <- !is.null(vars_link)

    # Item names
    items_curr <- vars_curr$item[vars_curr[[select_curr]]]
    items_prev <- vars_prev$item[vars_prev[[select_prev]]]
    items_link <- create_ifelse(is_anchor_group,
                                vars_link$item[vars_link[[select_link]]],
                                NULL)

    # Anchor group design
    if (is_anchor_group) {

        # Identify anchor items
        anchors_prev <- anchors[anchors[, 1] %in% items_prev, 1]
        anchors_curr <- anchors[anchors[, 1] %in% items_curr, 1]
        anchors_prev_link <- anchors[anchors[, 1] %in% items_prev, 2]
        anchors_curr_link <- anchors[anchors[, 1] %in% items_curr, 2]

        # Linking constants
        const1 <- mean(xsi_prev[anchors_prev, "xsi"]) -
            mean(xsi_link[anchors_prev_link, "xsi"])
        const2 <- mean(xsi_curr[anchors_curr, "xsi"]) -
            mean(xsi_link[anchors_curr_link, "xsi"])
        const <- const1 - const2

        # Linking error
        xsi_link.linked <-
            link_item_parameters(
                xsi = xsi_link,
                const = const1,
                vars = vars_link,
                select = select_link,
                scoring = scoring_link,
                warn = FALSE,
                test = FALSE
            )
        xsi_curr.linked <-
            link_item_parameters(
                xsi = xsi_curr,
                const = const2,
                vars = vars_curr,
                select = select_curr,
                scoring = scoring_curr,
                warn = FALSE,
                test = FALSE
            )
        const.err1 <- sd(xsi_prev[anchors_prev, "xsi"] -
                             xsi_link.linked[anchors_prev_link, 2]) /
            sqrt(length(anchors_prev))
        const.err2 <- sd(xsi_curr.linked[anchors_curr_link, 2] -
                             xsi_link[anchors_curr, "xsi"]) /
            sqrt(length(anchors_curr))
        const.err <- (const.err1^2 + const.err2^2)^0.5

        # Anchor items design
    } else {

        # Linking constant
        const <- mean(xsi_prev[anchors[, 1], "xsi"]) -
            mean(xsi_curr[anchors[, 2], "xsi"])

        # Linking error
        xsi_curr.linked <-
            link_item_parameters(
                xsi = xsi_curr,
                const = const,
                vars = vars_curr,
                select = select_curr,
                scoring = scoring_curr,
                warn = FALSE,
                test = FALSE
            )

        if (any(!anchors[, 1] %in% row.names(xsi_prev)) |
            any(!anchors[, 2] %in% row.names(xsi_curr.linked))) {
            stop("")
        }
        const.err <- sd(xsi_prev[anchors[, 1], "xsi"] -
                            xsi_curr.linked[anchors[, 2], 2]) / sqrt(nrow(anchors))

    }

    return(list(const = const, const.err = const.err,
                xsi_curr.linked = xsi_curr.linked))
}



#' Link item parameters with known link constant
#'
#' @param xsi data.frame; xsi parameters as returned by TAM::tam.mml()
#' @param const numeric; link constant
#' @param return_steps logical; should steps be returned (not re-estimated in
#'   subsequent analysis with TAM::tam.mml()). Defaults to FALSE.
#' @param vars data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select string; defines name of logical variable in vars that
#'   indicates which items to use for the analysis
#' @param scoring string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to the link constant (e.g., only
#'   half the link constant is needed if a PC item is scored 0.5)
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @returns matrix in xsi.fixed format needed for TAM::tam.mml()
#' @export
link_item_parameters <- function(xsi, const, vars, select,
                                 scoring = NULL,
                                 return_steps = FALSE,
                                 warn = TRUE, test = TRUE) {

    # Test data
    if (test) {
        scaling:::check_logicals(vars, "vars", select, warn = warn)
    }

    # Check scoring vector
    if (!is.null(scoring)) {
      scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)
    } else {
      vars$scoring <- 1
      scoring <- "scoring"
      warning("No variable name for scoring factor for polytomous analysis ",
              "provided. Scoring is set to 1 for all items.")
    }

    # xsi parameters with parameter index
    xsi.fixed <- cbind(seq_along(xsi$xsi), xsi$xsi)
    rownames(xsi.fixed) <- rownames(xsi)

    # remove step parameters
    if (!return_steps) {
        # identify step parameters
        f <- !grepl("_step[1-9]$", rownames(xsi.fixed))
        xsi.fixed <- xsi.fixed[f, ]
    }
    f <- !grepl("_step[1-9]$", rownames(xsi.fixed))

    # add linking constant to all parameters (except steps)
    # TAM uses the parameterization a*theta - const
    # to account for the 0.5 scoring we only have to add half the
    # linking constant to PCMs

    xsi.fixed[f, 2] <- xsi.fixed[f, 2] + const * vars[[scoring]][vars[[select]]]

    return(xsi.fixed)
}



#' Link WLEs using a known linking constant while correcting for selection
#'
#' @param wle_prev data.frame with WLEs at first measurement wave
#' @param wle_curr data.frame with WLEs at second measurement wave
#' @param const linking constant
#' @param wid character; variable name used as WLE identifier
#' @param use_longitudinal_subsample boolean indicating whether linking should
#'   be based on the entire samples given in wle_prev and wle_curr or
#'   only on the longitudinal sample with common pids; defaults to TRUE
#'
#' @returns   linked WLEs at second measurement wave
#' @export
link_wles <- function(wle_prev,
                      wle_curr,
                      const,
                      wid = "wle",
                      use_longitudinal_subsample = TRUE) {

    # Set variable name for WLEs
    if (length(wid) == 1) {
        wid <- rep(wid, 2)
    }
    wle_prev <- check_var_in_df(df = wle_prev, var = wid[1],
                                new_var = "wle",
                                name = get_object_name(wle_prev))
    wle_curr <- check_var_in_df(df = wle_curr, var = wid[2],
                                new_var = "wle",
                                name = get_object_name(wle_curr))

    # Remove missing values
    wle_prev <- wle_prev[!is.na(wle_prev$wle), ]
    wle_curr <- wle_curr[!is.na(wle_curr$wle), ]

    # Select persons for link
    ids <- get_final_ids(id_prev = wle_prev$ID_t,
                         id_curr = wle_curr$ID_t,
                         longitudinal_subsample = use_longitudinal_subsample)

    # Link WLEs at second measurement occasion
    correction <-
        mean(wle_curr$wle[wle_curr$ID_t %in% ids]) -
        mean(wle_prev$wle[wle_prev$ID_t %in% ids]) -
        const
    wle_curr$wle <- wle_curr$wle - correction

    # Return results
    wle_curr <- as.data.frame(wle_curr)
    return(wle_curr)
}

check_var_in_df <- function(df, var, new_var, name) {
    # Set variable name for person identifier
    check_variables(df = df, name_df = name, variables = var)
    names(df)[which(names(df) == var)] <- new_var

    return(df)
}



#' Print linking results for TR
#'
#' @param link_dif return object of check_dif_anchor()
#' @param link_dim return object of check_link_dimensionality()
#' @param link_results return object of link_samples()
#' @param dif_threshold numeric; threshold under which DIF in common link items
#'   is accepted; defaults to 0.5
#' @param digits  integer; number of decimals for rounding
#'
#' @returns summary of results in console
#' @export
print_link_results <- function(link_dif,
                               link_dim,
                               link_results,
                               dif_threshold,
                               digits = 3) {

    N <- paste0("N long. subsample: ",
                length(get_final_ids(link_results$wle_prev$ID_t,
                                     link_results$wle_curr$ID_t)), "\n")
    if (!is.null(link_dif$mod_link)) {
        N <- paste0(N, "N link sample: ",
                    link_dim$dimensionality$uni$nstud, "\n")
    }

    # Print N longitudinal subsample, N link study
    cat(paste0(N, "\n"))

    # Print DIF table
    cat("DIF estimates for anchor items:\n")
    print(link_dif$link_dif_summary$link_dif_table)
    cat(paste0("\n",
               "Items with potential DIF:\n",
               "|xsi| < ", dif_threshold, " or p < 0.05 (marked with +) \n",
               "|xsi| < ", dif_threshold, " and p < 0.05 (marked with *) \n\n"))

    # Print critical F for DIF
    cat("The critical values for the DIF between time points are:\n")
    Fs <- unique(link_dif$link_dif_summary$Fkrit)
    dfs <- sapply(Fs, \(x) {
        link_dif$link_dif_summary$dfkrit[link_dif$link_dif_summary$Fkrit == x][1]
    })
    cat(paste0("F(1, ", dfs, ") = ", round(Fs, digits = digits), "\n"))
    cat("\n")

    # Print range of DIF
    xsi <- round(abs(link_dif$link_dif_summary$link_dif_table$xsi), digits = digits)
    cat(paste0("Min. / max. abs. estimate: ",
               paste(range(xsi, na.rm = TRUE), collapse = ", "), "\n\n"))

    # Print link constant and link error (link_results)
    const <- paste0("Linking constant: ", round(link_results$const, digits = digits), "\n",
                    "Linking error: ", round(link_results$const.err, digits = digits), "\n")
    cat(paste0(const, "\n"))

    # Print GoF for dimensionality analyses
    if (!is.null(link_dif$mod_link)) {
        cat("\nDimensionality analysis results for link study:")
        cat("\nFactor correlations (off-diagonal) with variances (diagonal):\n")
        print(round(link_dim$dim_sum$`Cor-Var wave`, digits = digits))
        cat("\nGoodness-of-fit indices for unidimensional (uni) and two-dimensional (wave) model:\n")
        print(link_dim$dim_sum$`Goodness Of fit`)
    } else {
        cat("\nDimensionality analysis results for previous test:\n")
        cat("\nFactor correlations (off-diagonal) with variances (diagonal):\n")
        print(round(link_dim$dim_sum$previous$`Cor-Var anchoritems`, digits = digits))
        cat("\nGoodness-of-fit indices for unidimensional and two-dimensional model:\n")
        print(link_dim$dim_sum$previous$`Goodness Of fit`)

        cat("\nDimensionality analysis results for current test:\n")
        cat("\nFactor correlations (off-diagonal) with variances (diagonal):\n")
        print(round(link_dim$dim_sum$current$`Cor-Var anchoritems`, digits = digits))
        cat("\nGoodness-of-fit indices for unidimensional and two-dimensional model:\n")
        print(link_dim$dim_sum$current$`Goodness Of fit`)
    }
}



#' Test linking data
#'
#' @param resp_curr data.frame with responses of current measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include pweights
#' @param resp_prev data.frame with responses of previous measurement wave,
#' a person identifier 'ID_t' and a variable indicating valid cases; may also
#' include a WLE as given in wid and pweights
#' @param resp_link data.frame with responses of link study, a person identifier
#' 'ID_t' and a variable indicating valid cases; may also include pweights;
#' required if anchor group design is used
#' @param vars_curr data.frame; contains information about items of current
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_prev data.frame; contains information about items of previous
#' measurement wave with items as rows; includes variable 'item' containing item
#' names and may include variable with item scoring
#' @param vars_link data.frame; contains information about items of link study
#' with items as rows; includes variable 'item' containing item names and may
#' include variable with item scoring; required if anchor group design is used
#' @param select_curr character; contains name of logical variable in vars_curr
#' identifying the item set for the current measurement wave
#' @param select_prev character; contains name of logical variable in vars_prev
#' identifying the item set for the previous measurement wave
#' @param select_link character; contains name of logical variable in vars_link
#' identifying the item set for the link study; required if anchor group design
#' is used
#' @param valid_curr string; defines name of logical variable in resp_curr that
#' indicates (in)valid cases for the current measurement wave
#' @param valid_prev string; defines name of logical variable in resp_prev that
#' indicates (in)valid cases for the previous measurement wave
#' @param valid_link string; defines name of logical variable in resp_link that
#' indicates (in)valid cases for the link study; optional variable for anchor
#' group design
#' @param scoring_curr string; defines name of numerical variable in vars_curr
#' that contains the scoring factor to be applied to the loading matrix for the
#' current measurement point; can be NULL for the Rasch model
#' @param scoring_prev string; defines name of numerical variable in vars_prev
#' that contains the scoring factor to be applied to the loading matrix for the
#' previous measurement point; can be NULL for the Rasch model
#' @param scoring_link string; defines name of numerical variable in vars_link
#' that contains the scoring factor to be applied to the loading matrix for the
#' link study; can be NULL for the Rasch model; optional variable for anchor
#' group design
#' @param pweights_curr string; defines name of numerical variable in resp_curr
#' that contains the person weights for the current measurement wave passed to
#' TAM-functions
#' @param pweights_prev string; defines name of numerical variable in resp_prev
#' that contains the person weights for the previous measurement wave passed to
#' TAM-functions
#' @param pweights_link string; defines name of numerical variable in resp_link
#' that contains the person weights for the link study passed to TAM-functions;
#' optional variable for anchor group design
#' @param anchors  character; data.frame with two columns including the link
#'   items; for anchor item designs the first column refers to the previous
#'   measurement time point and the second column to the current measurement
#'   time point; for anchor group designs the first column refers to the main
#'   sample (for both time points) and the second column refers to the link
#'   sample; if NULL, all common items are used
#' @param mvs  named integer vector; contains user-defined missing values
#' @param warn  logical; whether to print warnings
#'
#' @noRd
test_linking_data <- function(vars_curr,
                              vars_prev,
                              vars_link = NULL,
                              select_curr,
                              select_prev,
                              select_link = NULL,
                              scoring_curr = NULL,
                              scoring_prev = NULL,
                              scoring_link = NULL,
                              resp_curr = NULL,
                              resp_prev = NULL,
                              resp_link = NULL,
                              valid_curr = NULL,
                              valid_prev = NULL,
                              valid_link = NULL,
                              pweights_curr = NULL,
                              pweights_prev = NULL,
                              pweights_link = NULL,
                              anchors = NULL,
                              mvs = NULL,
                              warn = warn
) {

    # Test select and item names
    scaling:::check_logicals(vars_curr, "vars_curr", select_curr, warn = warn)
    scaling:::check_items(vars_curr$item[vars_curr[[select_curr]]])
    scaling:::check_logicals(vars_prev, "vars_prev", select_prev, warn = warn)
    scaling:::check_items(vars_prev$item[vars_prev[[select_prev]]])
    if (!is.null(select_link) & !is.null(vars_link))
        scaling:::check_logicals(vars_link, "vars_link", select_link, warn = warn)
    scaling:::check_items(vars_link$item[vars_link[[select_link]]])

    # Test items
    if (!is.null(resp_curr))
      scaling:::check_numerics(resp_curr, "resp_curr", vars_curr$item[vars_curr[[select_curr]]])
    if (!is.null(resp_prev))
      scaling:::check_numerics(resp_prev, "resp_prev", vars_prev$item[vars_prev[[select_prev]]])
    if (!is.null(resp_link) & !is.null(vars_link) & !is.null(select_link))
      scaling:::check_numerics(resp_link, "resp_link", vars_link$item[vars_link[[select_link]]])

    # Test valid
    if (!is.null(valid_curr) & !is.null(resp_curr))
        scaling:::check_logicals(resp_curr, "resp_curr", valid_curr, warn = warn)
    if (!is.null(valid_prev) & !is.null(resp_prev))
        scaling:::check_logicals(resp_prev, "resp_prev", valid_prev, warn = warn)
    if (!is.null(valid_link) & !is.null(resp_link))
        scaling:::check_logicals(resp_link, "resp_link", valid_link, warn = warn)

    # Test scoring
    if (!is.null(scoring_curr))
        scaling:::check_numerics(vars_curr, "vars_curr", scoring_curr, check_invalid = TRUE)
    if (!is.null(scoring_prev))
        scaling:::check_numerics(vars_prev, "vars_prev", scoring_prev, check_invalid = TRUE)
    if (!is.null(scoring_link) & !is.null(vars_link))
        scaling:::check_numerics(vars_link, "vars_link", scoring_link, check_invalid = TRUE)

    # Test pweights
    if (!is.null(pweights_curr) & !is.null(resp_curr))
        scaling:::check_numerics(resp_curr, "resp_curr", pweights_curr, check_invalid = TRUE)
    if (!is.null(pweights_prev) & !is.null(resp_prev))
        scaling:::check_numerics(resp_prev, "resp_prev", pweights_prev, check_invalid = TRUE)
    if (!is.null(pweights_link) & !is.null(resp_link))
        scaling:::check_numerics(resp_link, "resp_link", pweights_link, check_invalid = TRUE)

    # Test anchors
    if (is.null(vars_link) & !is.null(anchors)) {
        if (any(!anchors[, 1] %in% vars_prev$item[vars_prev[[select_prev]]]) |
            any(!anchors[, 2] %in% vars_curr$item[vars_curr[[select_curr]]])) {
            stop("Dataframe 'anchors' includes items that are not defined as ",
                 "linking items in the 'select' arguments. Please check again.")
        }
    } else if (!is.null(anchors)) {
        if (any(!anchors[, 1] %in% c(vars_prev$item[vars_prev[[select_prev]]],
                                     vars_curr$item[vars_curr[[select_curr]]])) |
            any(!anchors[, 2] %in% vars_link$item[vars_link[[select_link]]])) {
            stop("Dataframe 'anchors' includes items that are not defined as ",
                 "linking items in the 'select' arguments. Please check again.")
        }
    }

    # Test mvs
    if (warn) scaling:::is_null_mvs_valid(mvs = mvs)

}
