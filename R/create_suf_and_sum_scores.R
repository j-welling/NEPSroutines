
#' Create SUF
#'
#' @param resp
#' @param vars
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
#' @param wles data.frame; contains WLEs as estimated by tam.wle() for the
#'   current assessment
#' @param linked_wles data.frame; contains the linked WLEs as returned by
#'   link_samples() / linking()
#' @param linked_location character; path to linking results as saved by
#'   linking(); alternative to linked_wles
#' @param facet
#' @param items
#' @param valid
#' @param mvs
#' @param scoring
#' @param competence character; name of tested competence for labelling the
#'   data (capitalized!)
#' @param filename character; indicates final name of SUF according to study
#' @param path_results character; indicates where SUF shall be saved
#' @param save
#' @param return
#' @export
create_suf <- function(resp, vars, items_suf, wle_name, xsi_fixed = NULL,
                       rotated = FALSE, linked = FALSE, wles = NULL,
                       linked_wles = NULL, linked_location = NULL,
                       facet = NULL, items = NULL, valid = NULL, mvs = NULL,
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
        linked_wles <- linked_wles[, c("pid", "theta", "error")]
        names(linked_wles) <- c("ID_t", paste0(wle_name, c("_sc1u", "_sc2u")))
    }

    if (is.null(wles)) {
        # estimate current (potentially rotated) WLEs and SEs
        if (rotated) {
            wles <- estimated_rotated_wles(resp, vars, items, valid, facet,
                                           xsi_fixed, scoring, mvs, wle_name)
        } else {
            irtmodel <- ifelse(any(vars[[scoring]][vars[[items]]] == 0.5),
                               "PCM2", "1PL")
            wles <- irt_model(resp, vars, items, valid, mvs, irtmodel,
                              scoring, verbose = FALSE, path = NULL,
                              filename = NULL)$wle
            names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
        }
    } else {
        wles <- wles[, c("pid", "theta", "error")]
        names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
    }

    # select items for SUF
    suf <- resp[, c("ID_t", vars$items[vars[[items_suf]]])]

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
estimated_rotated_wles <- function(resp, vars, items, valid, facet, xsi_fixed,
                                   scoring, mvs, wle_name) {
    if (is.null(facet)) {
        stop("Please provide the facet indicating the testlet rotation.")
    }
    if (is.null(xsi_fixed)) {
        stop("Please provide the item parameters to ensure the correct",
             " results in the WLE estimation.")
    }
    facet <- resp[resp[[valid]], facet, drop = FALSE]
    pid <- resp$ID_t[resp[[valid]]]
    resp_ <- prepare_resp(resp, vars, items, use_only_valid = TRUE,
                          valid, convert = TRUE, mvs)
    mod <- TAM::tam.mml.mfr(
        resp = resp_, facets = facet, xsi.fixed = xsi_fixed,
        verbose = FALSE, pid = pid,
        Q = as.matrix(vars[[scoring]][vars[[items]]])
    )
    wles <- TAM::tam.wle(mod)[, c("pid", "theta", "error")]
    names(wles) <- c("ID_t", paste0(wle_name, c("_sc1", "_sc2")))
    return(wles)
}


set_labels <- function(suf, vars, items_suf, competence, wle_name, linked) {
    attr(suf$ID_t, "label") <- "Unique person identifier"
    j <- 1
    for (i in vars[[items_suf]]) {
        # variable label
        attr(suf[[i]], "label") <- paste0(competence, " competence: Item ", j)
        j <- j + 1

        # value labels
        # dichotomous items
        if (max(suf[[i]], na.rm = TRUE) == 1) {
            suf[[i]] <- haven::labelled(suf[[i]],
                                         c("incorrect" = 0, "correct" = 1,
                                           "omitted" = -97, "not valid" = -95,
                                           "not reached" = -94, "test aborted" = -91,
                                           "unspecific missing" = -90,
                                           "not determinable" = -55,
                                           "not administered" = -54,
                                           "Angabe zurückgesetzt" = -21))
            # polytomous items
        } else {
            maxK <- max(suf[[i]], na.rm = TRUE)
            lbl <- setNames(object = seq(0, maxK),
                            paste0(seq(0, maxK), " correct of ", maxK))
            suf[[i]] <- haven::labelled(suf[[i]],
                                         c(lbl,
                                           "omitted" = -97, "not valid" = -95,
                                           "not reached" = -94, "test aborted" = -91,
                                           "unspecific missing" = -90,
                                           "not determinable" = -55,
                                           "not administered" = -54,
                                           "Angabe zurueckgesetzt" = -21))
        }
    }
    suf[[paste0(wle_name, "_sc1")]] <- haven::labelled_spss(
        suf[[paste0(wle_name, "_sc1")]],
        label = paste(competence, "competence: cross-sectional WLE"),
        labels = c("omitted" = -97, "not valid" = -95,
                   "not reached" = -94, "test aborted" = -91,
                   "unspecific missing" = -90, "not administered" = -54,
                   "Angabe zurückgesetzt" = -21)
    )
    suf[[paste0(wle_name, "_sc2")]] <- haven::labelled_spss(
        suf[[paste0(wle_name, "_sc2")]],
        label = paste(competence, "competence: standard error of cross-sectional WLE"),
        labels = c("omitted" = -97, "not valid" = -95,
                   "not reached" = -94, "test aborted" = -91,
                   "unspecific missing" = -90, "not administered" = -54,
                   "Angabe zurückgesetzt" = -21)
    )
    if (linked) {
        suf[[paste0(wle_name, "_sc1u")]] <- haven::labelled_spss(
            suf[[paste0(wle_name, "_sc1u")]],
            label = paste(competence, "competence: longitudinal WLE"),
            labels = c("omitted" = -97, "not valid" = -95,
                       "not reached" = -94, "test aborted" = -91,
                       "unspecific missing" = -90, "not administered" = -54,
                       "Angabe zurückgesetzt" = -21)
        )
        suf[[paste0(wle_name, "_sc2u")]] <- haven::labelled_spss(
            suf[[paste0(wle_name, "_sc2u")]],
            label = paste(competence, "competence: standard error of longitudinal WLE"),
            labels = c("omitted" = -97, "not valid" = -95,
                       "not reached" = -94, "test aborted" = -91,
                       "unspecific missing" = -90, "not administered" = -54,
                       "Angabe zurückgesetzt" = -21)
        )
    }
    return(suf)
}


save_suf <- function(suf, path_results, filename) {
    saveRDS(suf, file = paste0(path_results, filename, ".rds"))
    haven::write_dta(suf, path = paste0(path_results, filename, ".dta"))
    haven::write_sav(suf, path = paste0(path_results, filename, ".sav"))
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
#'   includes variable 'items' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @return integer vector of length nrow(resp) containing the row sums for the
#'   specified item set
#' @export
sum_scores <- function(resp, vars, items, poly_items) {
    # count only correctly scored binary or FULLY correctly scored PC items
    resp[, vars$items[vars[[items]]]] <- lapply(vars$items[vars[[items]]],
                                            function(x) {
                                                ifelse(resp[, x] == vars$max[x],
                                                       1, NA)
                                            })
    rs <- data.frame(
        ID_t = resp$ID_t,
        sumscore = rowSums(resp[, vars$items[vars[[items]]]], na.rm = TRUE)
    )
    return(rs)
}
