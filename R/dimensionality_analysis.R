#' Dimensionality analysis
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param dim  character vector. contains names of all dimension variables
#' that shall be analyzed (NOTE: the dimensions must be coded as integers from 1
#' to the number of dimensions!)
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param mvs named integer vector; contains user-defined missing values
#' @param irtmodel  string; "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#' "GPCM" for GPCM analysis
#' @param maxiter  max iterations as passed to the TAM function
#' @param snodes  snodes as passed to the TAM function
#' @param return  logical; whether results shall be returned
#' @param save  logical; whether results shall be saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param verbose   verbose as passed to the TAM function
#'
#' @return          list of results for each dimensional analysis
#'
#' @export
#' @importFrom stats AIC BIC logLik

dim_analysis <- function(resp, vars, items, scoring = "scoring",
                         dim = NULL, valid = NULL, mvs = NULL,
                         irtmodel = "PCM2", maxiter = 10, snodes = 5,
                         return = FALSE, save = TRUE, print = TRUE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         overwrite = TRUE, verbose = FALSE) {

  dimensionality <- list()

  dimensionality$analysis <- conduct_dim_analysis(
    resp = resp, vars = vars, items = items, scoring = scoring, dim = dim,
    valid = valid, irtmodel = irtmodel, maxiter = maxiter, snodes = snodes,
    mvs = mvs, verbose = verbose
  )
  dimensionality$summary <- dim_summary(dimensionality$analysis)

  if (save) {
    save_results(dimensionality,
                 filename = "dimensionality.rds", path = path_results)
    save_table(dimensionality$summary, overwrite = overwrite,
               filename = "dimensionality.xlsx", path = path_table)
  }

  if (print) print_dim_summary(dimensionality$summary)

  if (return) return(dimensionality)
}

#' Conduct dimensionality analysis
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'items' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param scoring  string; defines name of numerical variable in vars that
#' contains the scoring factor to be applied to loading matrix; defaults to
#' "scoring"
#' @param dim  character vector. contains names of all dimension variables
#' in vars that shall be analyzed (NOTE: the dimensions must be coded as integers
#' from 1 to the number of dimensions!)
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param irtmodel  string; "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#' "GPCM" for GPCM analysis
#' @param maxiter  max iterations as passed to the TAM function
#' @param snodes  snodes as passed to the TAM function
#' @param verbose   verbose as passed to the TAM function
#' @param mvs named integer vector; contains user-defined missing values
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @return          list of results for each dimensional analysis
#' @export
conduct_dim_analysis <- function(resp, vars, items, dim, scoring = 'scoring',
                                 valid = NULL, irtmodel, maxiter, snodes,
                                 mvs = NULL, verbose = FALSE, warn = TRUE) {
  # Test data
  check_logicals(vars, "vars", c(dim, scoring), warn = warn)

  # Select only valid cases
  resp <- only_valid(resp, valid = valid, warn = warn)

  # Create ID and facets variable
  pid <- resp$ID_t
  check_pid(pid)

  # Select only indicated items and convert mvs
  resp <- prepare_resp(resp, vars = vars, items = items, convert = TRUE,
                       mvs = mvs, warn = warn)

  check_numerics(resp, "resp")

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
#' @param dimensionality list with results of the dim_analysis function
#'
#' @return data.frame of summary results
#'
#' @importFrom stats logLik AIC BIC cov2cor
#' @export
#'
dim_summary <- function(dimensionality) {

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

#' Print summary of dimensionality analysis
#'
#' @param dimsum return object of dim_summary()
#' @export
print_dim_summary <- function(dimsum) {
  for (nms in names(dimsum)) {
    print(nms)
    print(dimsum[[nms]])
  }
}
