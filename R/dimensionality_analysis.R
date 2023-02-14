#' Dimensionality analysis
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
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
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param verbose   verbose as passed to the TAM function
#'
#' @return   (if return = TRUE) list of results:
#'  analysis: list with results for each dimensionality model
#'  summary: list with summary of results (variance-covariance matrices & model fit)
#'
#' @export
#' @importFrom stats AIC BIC logLik

dim_analysis <- function(resp, vars, select, scoring = "scoring",
                         dim = NULL, valid = NULL, mvs = NULL,
                         irtmodel = "PCM2", maxiter = 10000, snodes = 5000,
                         return = FALSE, save = TRUE, print = TRUE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         name_group = NULL, overwrite = TRUE,
                         verbose = FALSE) {

  dimensionality <- list()

  dimensionality$analysis <- scaling:::conduct_dim_analysis(
    resp = resp, vars = vars, select = select, scoring = scoring, dim = dim,
    valid = valid, irtmodel = irtmodel, maxiter = maxiter, snodes = snodes,
    mvs = mvs, verbose = verbose, save = FALSE, test = TRUE
  )

  dimensionality$summary <- scaling:::dim_summary(
      dimensionality$analysis,
      save = FALSE
  )

  # Save results
  if (save) {
      name <- scaling:::create_name("dimensionality", name_group)
      scaling:::save_results(
          dimensionality,
          filename = paste0(name, ".rds"),
          path = path_results
      )
      scaling:::save_table(
          dimensionality$summary,
          overwrite = overwrite,
          filename = paste0(name, ".xlsx"),
          path = path_table
      )
  }

  # Print results
  if (print) scaling:::print_dim_summary(dimensionality$summary)

  # Return results
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
#' includes variable 'item' containing item names; additionally includes all
#' variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
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
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return   list with results for each dimensionality model.
#' @export
conduct_dim_analysis <- function(resp, vars, select, dim, scoring = 'scoring',
                                 valid = NULL, irtmodel, maxiter = 10000,
                                 snodes = 5000, mvs = NULL, verbose = FALSE,
                                 save = TRUE, name_group = NULL,
                                 path = here::here('Results'),
                                 warn = TRUE, test = TRUE) {
  # Test data
  if (test) {
      scaling::check_logicals(vars, "vars", select, warn = warn)
      scaling::check_logicals(resp, "resp", valid, warn = warn)
      scaling::check_variables(vars, "vars", c(dim, scoring))
      scaling::check_items(vars$item[vars[[select]]])
      scaling::check_numerics(resp, "resp", vars$item[vars[[select]]])
  }

  if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)

  # Select only valid cases
  resp <- scaling:::only_valid(resp, valid = valid, warn = FALSE)

  # Create ID and facets variable
  pid <- resp$ID_t
  scaling::check_pid(pid)

  # Select only indicated items and convert mvs
  resp <- scaling::prepare_resp(
      resp,
      vars = vars,
      select = select,
      convert = TRUE,
      mvs = mvs,
      warn = FALSE
  )

  scaling::check_numerics(resp, "resp", check_invalid = TRUE)

  # Create object for results
  dimensionality <- list()

  ## 1D reference model

  # Create Q matrix
  Q <- as.matrix(vars[[scoring]][vars[[select]]])

  # Compute reference model
  dimensionality$uni <- TAM::tam.mml(
    resp = resp, pid = pid, Q = Q, irtmodel = irtmodel, verbose = verbose,
    control = list(maxiter = maxiter, snodes = snodes)
  )

  scaling:::reached_maxiter(dimensionality$uni, "'unidimensional'")

  message("Finished unidimensional reference model.")

  if (!is.null(dim)) {

    for (d in dim) {

      # Create Q matrix
      v <- as.double(vars[vars[[select]], d])
      dimensions <- sort(unique(v))
      Q <- matrix(0, nrow = length(v), ncol = length(dimensions))
      for (i in dimensions) {
        Q[v == i, i] <- 1
      }
      Q <- Q * vars[[scoring]][vars[[select]]]

      # Compute dimensional model
      dimensionality[[d]] <- TAM::tam.mml(
        resp = resp,
        pid = pid,
        Q = Q,
        irtmodel = irtmodel,
        control = list(maxiter = maxiter, snodes = snodes),
        verbose = verbose
      )

      scaling:::reached_maxiter(dimensionality[[d]], paste0("'", d, "-dimensional'"))

      message("Finished ", d," model.")
    }
  }

  # Save results
  if (save) {
      name <- scaling:::create_name("dimensionality", name_group, ".rds")
      scaling:::save_results(dimensionality, filename = name, path = path)
  }

  # Return results
  return(dimensionality)
}


#' Summary of dimensionality analysis (saves summary in excel sheet)
#'
#' @param dimensionality list with results of the conduct_dim_analysis function
#' (or results$analysis from the dim_analysis function)
#' @param save  logical; whether table shall be saved to hard drive
#' @param path  string; defines path to folder where table shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @return list with summary of results:
#'  Cor-Var X: variance-covariance matrix for each dimensionality model (= X)
#'  Goodness of fit: table with number of parameters, Loglikelihood, AIC & BIC for each model
#'
#' @importFrom stats logLik AIC BIC cov2cor
#' @export
#'
dim_summary <- function(dimensionality, save = FALSE, name_group = NULL,
                        overwrite = FALSE, path = here::here("Tables")) {

  dim <- names(dimensionality)
  dimsum <- list()
  gof <- data.frame(Stat = c("Npars", "loglik", "AIC", "BIC"))

  for (d in dim) {
    gof[[d]] <- c(
        dimensionality[[d]]$ic$Npars,
        logLik(dimensionality[[d]]),
        AIC(dimensionality[[d]]),
        BIC(dimensionality[[d]])
    )
    tmp <- dimensionality[[d]]$variance
    dimsum[[paste("Cor-Var", d)]] <- cov2cor(tmp)
    diag(dimsum[[paste("Cor-Var", d)]]) <- diag(tmp)
  }
  dimsum[["Goodness of fit"]] <- gof

  # Save results
  if (save) {
      name <- scaling:::create_name("dimensionality", name_group, ".xlsx")
      scaling:::save_table(dimsum, overwrite = overwrite, filename = name, path = path)
  }

  # Return results
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
