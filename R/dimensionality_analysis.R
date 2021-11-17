#' Dimensionality analysis
#'
#' @param resp      data.frame. contains:
#'                    (1) the responses. y in {0, 1} for binary data and y in
#'                      {0, 1, ... k-1} for polytomous responses with k categories
#'                    (2) ID_t: column indicating ID of participants
#' @param vars      data.frame with all variables as rows with at least the
#'                  following columns:
#'                  items: contains name for each item (scored and unscored)
#'                  and a column identifying item subsets for the analyses and
#'                  one for the item scoring
#' @param items     character. contains name of variable (boolean) in vars that
#'                    indicates which items to use for analysis.
#' @param scoring   character. Contains the name of the variable in vars that
#'                    indicates the variable used for scoring. Defaults to
#'                    "scoring"
#' @param dim       character vector. contains names of all dimension variables
#'                    that shall be analyzed (NOTE: the dimensions must be
#'                    coded as integers from 1 to the number of dimensions!)
#' @param valid     character string. defines name of boolean variable in dat,
#'                    indicating (in)valid cases.
#' @param irtmodel  the choice of irtmodel as passed to the TAM function
#' @param maxiter   max iterations as passed to the TAM function
#' @param snodes    snodes as passed to the TAM function
#' @param verbose   verbose as passed to the TAM function
#' @param return_results  boolean. indicates whether to return results.
#' @param save logical indicating whether summary / data is stored on hard disk
#' @param path character vector; indicates the folder locations where the
#'   resulting summaries (first element of vector) and data (second element of
#'   vector) is stored on the hard drive. Please note that the
#'   path is relative to the current working path set by here::i_am()
#' @param overwrite boolean; indicates whether to overwrite existing file when
#'   saving table.
#' @param print locigal indicating whether the summary is printed to console
#'
#' @return          list of results for each dimensional analysis
#'
#' @export
#' @importFrom stats AIC BIC logLik

dimension_analysis <- function(resp, vars, items, scoring = "scoring",
                               dim = NULL, valid = NULL, irtmodel = "PCM2",
                               maxiter = 10, snodes = 5, verbose = FALSE,
                               return = FALSE, save = TRUE,
                               path_results = here::here('Results'),
                               path_table = here::here('Tables'),
                               overwrite = TRUE, print = TRUE) {

    dimensionality <- list()

    dimensionality$analysis <- conduct_dimensionality_analysis(
        resp = resp, vars = vars, items = items, scoring = scoring, dim = dim,
        valid = valid, irtmodel = irtmodel, maxiter = maxiter, snodes = snodes,
        verbose = verbose
    )
    dimensionality$summary <- dimension_summary(dimensionality$analysis)

    if (save) {
      save_results(dimensionality,
                   filename = "dimensionality.Rdata", path = path_results)
      save_table(dimensionality$summary, overwrite = overwrite,
                 filename = "dimensionality.xlsx", path = path_table)
    }

    if (print) print_dim_summary(dimensionality$summary)

    if (return) return(dimensionality)
}

conduct_dimensionality_analysis <- function(resp, vars, items, scoring, dim,
                                            valid, irtmodel, maxiter, snodes,
                                            verbose) {
    # Select only valid cases
    resp <- only_valid(resp, valid = valid)

    # Create ID and facets variable
    pid <- resp$ID_t
    check_pid(pid)

    # Select only indicated items and convert mvs
    resp <- prepare_resp(resp, vars = vars, items = items, convert = TRUE,
                         without_valid = TRUE)

    # Create object for results
    dimensionality <- list()

    ## 1D reference model

    # Create Q matrix
    Q <- as.matrix(vars[[scoring]][vars[[items]]])

    # Compute reference model
    dimensionality$uni <- TAM::tam.mml(
        resp = resp, pid = pid, Q = Q, irtmodel = irtmodel, verbose = verbose,
        control = list(maxiter = maxiter, snodes = snodes)
    )
    message("Finished unidimensional reference model.")

    if (!is.null(dim)) {

        for (d in dim) {

            # Create Q matrix
            v <- as.double(vars[vars[[items]], d])
            dimensions <- sort(unique(v))
            Q <- matrix(0, nrow = length(v), ncol = length(dimensions))
            for (i in dimensions) {
                Q[v == i, i] <- 1
            }
            Q <- Q * vars[[scoring]][vars[[items]]]

            # Compute dimensional model
            dimensionality[[d]] <- TAM::tam.mml(
                resp = resp, pid = pid, Q = Q, irtmodel = irtmodel,
                control = list(maxiter = maxiter, snodes = snodes),
                verbose = verbose
            )
            message("Finished ", d," model.")
        }
    }

    return(dimensionality)
}


#' Summary of dimensionality analysis (saves summary in excel sheet)
#'
#' @param dimensionality list with results of the dimension_analysis function
#'
#' @return data.frame of summary results
#'
#' @importFrom stats logLik AIC BIC cov2cor
#' @export
#'

dimension_summary <- function(dimensionality) {

    dim <- names(dimensionality)
    dimsum <- list()
    gof <- data.frame(Stat = c("loglik", "AIC", "BIC"))

    for (d in dim) {
        gof[[d]] <- c(logLik(dimensionality[[d]]),
                      AIC(dimensionality[[d]]),
                      BIC(dimensionality[[d]]))
        tmp <- dimensionality[[d]]$variance
        dimsum[[paste("Cor-Var", d)]] <- cov2cor(tmp)
        diag(dimsum[[paste("Cor-Var", d)]]) <- diag(tmp)
    }
    dimsum[["Goodness Of fit"]] <- gof

    return(dimsum)
}

print_dim_summary <- function(dimsum) {
    for (nms in names(dimsum)) {
      print(nms)
      print(dimsum[[nms]])
    }
}
