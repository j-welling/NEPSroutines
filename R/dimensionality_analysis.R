#' Dimensionality analysis
#'
#' @param resp      data.frame. contains:
#'                    (1) the responses. y in {0, 1} for binary data and y in
#'                      {0, 1, ... k-1} for polytomous responses with k categories
#'                    (2) ID_t: column indicating ID of participants
#' @param vars      data.frame with all variables as rows with at least the following columns:
#'                  items: contains name for each item (scored and unscored)
#'                  poly: identifies all polytomous (?) items (logical)
#'                  scored: identifies all scored items (logical)
#' @param items     character. contains name of variable (boolean) in vars that
#'                    indicates which items to use for analysis.
#' @param dim       character vector. contains names of all dimension variables that shall be analyzed
#' @param min.val   minimum number of valid values; if negative,
#'                  set to the number of variables
#' @param irtmodel  the choice of irtmodel as passed to the TAM function
#' @param maxiter   max iterations as passed to the TAM function
#' @param snodes    snodes as passed to the TAM function
#' @param verbose   verbose as passed to the TAM function
#'
#' @return          list of results for each dimensional analysis
#'
#' @export
#' @importFrom stats AIC BIC logLik

dimension_analysis <- function(resp, vars, items, dim = NULL, min.val = 3,
                               irtmodel = "PCM2", maxiter = 10, snodes = 5,
                               verbose = FALSE) {

    # Prepare data
    resp <- min_val(resp, min.val = min.val)
    pid <- resp$ID_t
    resp <- convert_mv(resp)[ , vars$items[vars[[items]]]]
    sel <- as.integer(apply(resp, 2, max, na.rm = TRUE) > 1) # identify polytomous items

    # Create object for results
    dimensionality <- list()

    ## 1D reference model

    # Create Q matrix
    Q <- matrix( 1 , nrow = ncol(resp), ncol = 1)
    Q[sel, 1] <- 0.5    # score of 0.5 for polyomous items

    # Compute dimensional model
    dimensionality$uni <- TAM::tam.mml(
        resp = resp, pid = pid, Q = Q, irtmodel = irtmodel,
        control = list(maxiter = maxiter, snodes = snodes), verbose = verbose
    )

    if (!is.null(dim)) {

        for (d in dim) {

            # Create Q matrix
            v <- as.double(vars[vars[[items]], d])
            dimensions <- unique(v)
            Q <- matrix(0, nrow = length(v), ncol = length(dimensions))
            for (i in dimensions) {
                Q[v == i, i] <- 1
            }
            Q[sel,] <- Q[sel, ] * 0.5

            # Compute dimensional model
            dimensionality[[d]] <- TAM::tam.mml(
                resp = resp, pid = pid, Q = Q, irtmodel = irtmodel,
                control = list(maxiter = maxiter, snodes = snodes), verbose = verbose
            )
        }
    }

    return(dimensionality)
}


#' Summary of dimensionality analysis (saves summary in excel sheet)
#'
#' @param dimensionality list with results of the dimension_analysis function
#' @param print logical indicating whether summary is printed to the console;
#'   prints the information criteria for all multidimensional models
#' @param save_at character string; indicates the folder location where the
#'   summaries are stored on the hard drive (first, the dimensionality object is
#'   saved unchanged as Rdata; second, the information criteria are saved as a
#'   data.frame as Rdata; third, the information criteria are saved in an excel
#'   file). Please note that the path is relative to the current working path
#'   set by here::i_am(). Defaults to NULL (not stored)
#'
#' @return data.frame of summary results
#'
#' @importFrom stats logLik AIC BIC
#' @export
#'

dimension_summary <- function(dimensionality, print = TRUE, save_at = NULL) {

    dim <- names(dimensionality)
    gof <- data.frame(Stat = c("loglik", "AIC", "BIC"))

    for (d in dim) {
        gof[[d]] <- c(logLik(dimensionality[[d]]),
                        AIC(dimensionality[[d]]),
                        BIC(dimensionality[[d]]))
    }

    if (print) {
        message("Information criteria for dimensionality analyses \n")
        print(gof)
    }


    if (!is.null(save_at)) {

        if (!file.exists(save_at)) {
            stop("The location ", save_at, " does not exist. Please provide a ",
                 "valid folder path to save the dimensionality analyses.")
        }

        save(dimensionality,
             file = here::here(paste0(save_at, "/dimensionality.Rdata")))
        save(gof,
             file = here::here(paste0(save_at, "/dimensionality_fit.Rdata")))
        openxlsx::write.xlsx(
            gof,
            file = here::here(paste0(save_at, "/dimensionality_fit.xlsx")),
            showNA = FALSE
        )
    }


    return(gof)
}
