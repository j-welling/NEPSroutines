#' IRT analyses for several groups
#'
#' @param groups named character vector; contains name of item selection
#'   variable of each group (e.g. easy = 'easy_final')
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param xsi.fixed matrix; contains fixed (linked) item parameters; optional
#' @param control_tam list; control argument as passed to tam.mml-functions
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param plots  logical; whether plots shall be created and saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE to
#' avoid problems with the data structure)
#'
#' @return list with a list for each group with all results
#' (as return object of irt_analysis())
#' @export

grouped_irt_analysis <- function(groups, resp, vars, valid = NULL, mvs = NULL,
                                 scoring = NULL, plots = FALSE, save = TRUE,
                                 print = TRUE, return = FALSE, overwrite = FALSE,
                                 path_plots = here::here("Plots"),
                                 path_table = here::here("Tables"),
                                 path_results = here::here("Results"),
                                 digits = 2, verbose = FALSE, warn = TRUE,
                                 xsi.fixed = NULL, pweights = NULL,
                                 control_tam = NULL, control_wle = NULL) {

  # Test data
  check_logicals(vars, "vars", groups, warn = warn)
  check_logicals(resp, "resp", c(valid, names(groups)), warn = warn)
  if (!is.null(scoring))
    check_numerics(vars, "vars", scoring, check_invalid = TRUE)
  if (!is.null(pweights))
    check_numerics(resp, "resp", pweights, check_invalid = TRUE)
  if (warn) is_null_mvs_valid(mvs = mvs, valid = valid)

  # Create list for results
  irt_groups <- list()
  i <- 1

  # Conduct irt_analysis for each group
  for (g in names(groups)) {

    select <- groups[[g]]
    message(toupper(paste0("\n\n\n(", i, ") irt analysis (",
                           ifelse(is_poly(resp, vars, select), 'poly', 'dich'),
                           ") for group '", g, "':\n")))

    irt_groups[[g]] <- irt_analysis(resp = resp[resp[[g]], ], vars = vars,
                                    select = select, valid = valid,
                                    scoring = scoring, print = print,
                                    plots = plots, save = save, return = TRUE,
                                    path_results = path_results,
                                    path_table = path_table,
                                    path_plots = path_plots,
                                    overwrite = overwrite, digits = digits,
                                    name_group = g, warn = FALSE, test = FALSE,
                                    control_tam = control_tam, control_wle = control_wle,
                                    pweights = pweights, xsi.fixed = xsi.fixed)

    i <- i + 1
  }

  if (return) return(irt_groups)
}


#' IRT analyses - all in one function
#'
#' Perform 1PL and 2PL analyses for binary or PCM and GPCM analyses for
#' polytomous data, generate summary and plots.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param xsi.fixed matrix; contains fixed (linked) item parameters; optional
#' @param control_tam list; control argument as passed to tam.mml-functions
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param plots  logical; whether plots shall be created and saved to hard drive
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results  string; defines path to folder where results shall be saved
#' @param path_table  string; defines path to folder where tables shall be saved
#' @param path_plots  string; defines path to folder where plots shall be saved
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE to
#' avoid problems with the data structure)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return   list with all results
#' @export

irt_analysis <- function(resp, vars, select, valid = NULL, mvs = NULL,
                         scoring = NULL, plots = FALSE, save = TRUE,
                         print = TRUE, return = FALSE,
                         path_plots = here::here("Plots"),
                         path_table = here::here("Tables"),
                         path_results = here::here("Results"),
                         overwrite = FALSE, digits = 2, name_group = NULL,
                         verbose = FALSE, warn = TRUE, test = TRUE,
                         xsi.fixed = NULL, pweights = NULL,
                         control_tam = NULL, control_wle = NULL) {

  # Test data
  if (test) {
    check_logicals(vars, "vars", select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    if (!is.null(scoring))
      check_numerics(vars, "vars", scoring, check_invalid = TRUE)
    if (!is.null(pweights))
      check_numerics(resp, "resp", pweights, check_invalid = TRUE)
    if (warn) is_null_mvs_valid(mvs = mvs, valid = valid)
  }

  check_items(vars$item[vars[[select]]])

  # Identify IRT type
  irt_type <- ifelse(is_poly(resp, vars, select), 'poly', 'dich')

  # Create list with results
  irt <- list()

  # Conduct IRT analyses
  if (irt_type == 'dich') {

    irt$model.1pl <- irt_model(resp = resp, vars = vars, select = select,
                               valid = valid, mvs = mvs, irtmodel = '1PL',
                               verbose = verbose, warn = FALSE, test = FALSE,
                               control_tam = control_tam, control_wle = control_wle,
                               pweights = pweights, xsi.fixed = xsi.fixed)
    irt$model.2pl <- irt_model(resp = resp, vars = vars, select = select,
                               valid = valid, mvs = mvs, irtmodel = '2PL',
                               verbose = verbose, warn = FALSE, test = FALSE,
                               control_tam = control_tam, control_wle = control_wle,
                               pweights = pweights, xsi.fixed = xsi.fixed)

  } else if (irt_type == 'poly') {

    irt$model.pcm <- irt_model(resp = resp, vars = vars, select = select, mvs = mvs,
                               valid = valid, irtmodel = 'PCM2', scoring = scoring,
                               verbose = verbose, warn = FALSE, test = FALSE,
                               control_tam = control_tam, control_wle = control_wle,
                               pweights = pweights, xsi.fixed = xsi.fixed)
    irt$model.gpcm <- irt_model(resp = resp, vars = vars, select = select, mvs = mvs,
                                valid = valid, irtmodel = 'GPCM', scoring = scoring,
                                verbose = verbose, warn = FALSE, test = FALSE,
                                control_tam = control_tam, control_wle = control_wle,
                                pweights = pweights, xsi.fixed = xsi.fixed)

  }

  # Create plots
  if (plots) {

    for (i in 1:2) {

      # ICC plots
      icc_plots(model = irt[[i]], path = path_plots, name_group = name_group)

      # Wright map
      wright_map(model = irt[[i]], path = path_plots, name_group = name_group)

    }
  }

  # Create tables
  if (return | print | save) {
    # IRT summary
    irt$summary <- irt_summary(resp = resp, vars = vars, results = irt[[1]],
                               disc = irt[[2]]$mod$item[, "B.Cat1.Dim1"],
                               valid = valid, mvs = mvs, digits = digits)

    # Model fit
    irt$model_fit <- irt_model_fit(model_1p = irt[[1]], model_2p = irt[[2]])

    # Steps analysis
    if (irt_type == 'poly') {
      irt$steps <- steps_analysis(pcm_model = irt$model.pcm, digits = digits)
    }
  }

  # Print results
  if (print)  {
    message("\nIRT summary table\n")
    print(irt$summary)
    message("\nModel fit table\n")
    print(irt$model_fit)
    if (irt_type == 'poly') {
      message("\nSteps analysis table\n")
      print(irt$steps)
    }
    message("\nSUMMARY FOR TR\n")
    print_irt_summary(model = irt[[1]],
                      irt_sum = irt$summary,
                      steps_sum = irt$steps)
  }

  # Save results
  if (save) {
    name <- create_ifelse(is.null(name_group),
                          paste0("irt_", irt_type),
                          paste0("irt_", irt_type, "_", name_group))
    irt_summary <- irt[-c(1:2)]

    save_results(irt, filename = paste0(name, ".rds"), path = path_results)
    save_table(irt_summary, filename = paste0(name, ".xlsx"), path = path_table,
               overwrite = overwrite)
  }

  # Return results
  if (return)  return(irt)
}

#' IRT analyses
#'
#' Perform Rasch or 2PL analyses for binary or PCM or GPCM analyses for
#' polytomous data.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  string; defines name of logical variable in vars that indicates
#'   which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param irtmodel  string; "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#'   "GPCM" for GPCM analysis
#' @param scoring  string; defines name of numerical variable in vars that
#'   contains the scoring factor to be applied to loading matrix; can be NULL for
#'   Rasch model
#' @param xsi.fixed matrix; contains fixed (linked) item parameters; optional
#' @param control_tam list; control argument as passed to tam.mml-functions
#' @param control_wle list; can contain Msteps and/or convM as to pass to tam.wle()
#'    as elements of the list
#' @param pweights character; defines name of numerical variable in resp that
#' contains person weights as passed to TAM-function
#' @param path  string; defines path to folder where results shall be saved
#' @param filename  string; defines name of file that shall be saved
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return (if return_results = TRUE) a list of:
#'   mod: tam.mml; estimated item response model
#'   fit: data.frame; item fit (WMNSQ) of the model
#'   pars: data.frame; item difficulties and scoring parameters with respective standard errors
#'   mfit: data.frame; (adjusted) Yen's Q3 statistics of the model
#'   wle: data.frame; WLEs for the model
#'   wle_rel: numeric; WLE reliability of the model
#'   info_crit: data.frame with information criteria of the model
#' @export

irt_model <- function(resp, vars, select, valid = NULL, mvs = NULL, irtmodel,
                      scoring = NULL, verbose = FALSE,
                      path = here::here("Results"), filename = NULL,
                      xsi.fixed = NULL, pweights = NULL,
                      control_tam = NULL, control_wle = NULL,
                      warn = TRUE, test = TRUE) {

  # Test data
  if (test) {
    check_logicals(vars, "vars", select, warn = warn)
    check_logicals(resp, "resp", valid, warn = warn)
    check_items(vars$item[vars[[select]]])
    if (!is.null(scoring))
      check_numerics(vars, "vars", scoring, check_invalid = TRUE)
    if (!is.null(pweights))
      check_numerics(resp, "resp", pweights, check_invalid = TRUE)
    if (warn) is_null_mvs_valid(mvs = mvs, valid = valid)
  }

  # Check if input is correct
  if (!irtmodel %in% c("1PL", "2PL", "GPCM", "PCM2")) {
    stop("Invalid irtmodel. Please provide one of the following: '1PL',
         '2PL', 'PCM2', 'GPCM'.")
  }

  # Select only valid cases
  resp <- only_valid(resp, valid = valid, warn = FALSE)

  # Create ID variable
  pid <- resp$ID_t
  check_pid(pid)
  pws <- create_ifelse(is.null(pweights), NULL, resp[[pweights]])

  # Prepare data
  resp <- prepare_resp(resp, vars = vars, select = select, convert = TRUE,
                       mvs = mvs, warn = FALSE)

  # Test data
  check_numerics(resp, "resp", check_invalid = TRUE) # this check is very important as otherwise R will be aborted!!
  if (irtmodel %in% c("1PL", "2PL")) check_dich(resp, "resp")

  # Create scoring matrix if not provided in function arguments
  Q <- create_q(vars, select = select, scoring = scoring,
                poly = ifelse(irtmodel %in% c("PCM2", "GPCM"), TRUE, FALSE))

  # IRT model
  if (irtmodel %in% c("1PL", "PCM2")) {

    mod <- TAM::tam.mml(
      resp = resp, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose, xsi.fixed = xsi.fixed,
      control = control_tam, pweights = pws
    )

  } else if (irtmodel %in% c("2PL", "GPCM")) {

    mod <- TAM::tam.mml.2pl(
      resp = resp, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose, xsi.fixed = xsi.fixed,
      control = control_tam,
      pweights = pws
    )
  }


  # Warn if maximum number of iterations were reached
  reached_maxiter(mod, paste0("'", irtmodel, "'"))

  # WMNSQ
  fit <- TAM::msq.itemfit(mod)$itemfit[, c("item", "Infit",
                                           "Infit_t", "Infit_p")]

  # Item parameters and standard errors
  x <- capture.output(pars <- TAM::tam.se(mod))
  pars <- dplyr::left_join(pars$xsi, pars$B, by = "item")
  names(pars) <- c("Item", "xsi", "se(xsi)", "alpha", "se(alpha)")

  # Model fit
  mfit <- TAM::tam.modelfit(mod, progress = verbose)

  # WLEs
  if (is.null(control_wle)) control_wle <- list()
  if (is.null(control_wle$convM)) control_wle$convM <- .0001
  if (is.null(control_wle$Msteps)) control_wle$Msteps <- 50

  wle <- TAM::tam.wle(mod, convM = control_wle$convM,
                      Msteps = control_wle$Msteps,
                      progress = verbose)
  wle_rel <- wle$WLE.rel[1]
  wle <- wle[, c("pid", "theta", "error")]

  # information criteria
  info_crit <- mod$ic

  # List with results
  results <- list(
    mod = mod, fit = fit, pars = pars, mfit = mfit,
    wle = wle, wle_rel = wle_rel, info_crit = info_crit, irtmodel = irtmodel
  )

  # Save results
  save_results(results, filename = filename, path = path)

  # Return results
  return(results)
}


#' ICC plots
#'
#' Create ICC plots for IRT models.
#'
#' @param model  list; return object of irt_model()
#' @param path  string; defines path to folder where plots shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @export

icc_plots <- function(model, path = here::here("Plots"), name_group = NULL) {

  # Identify kind of irt model
  irtmodel <- model$irtmodel

  # Add group name to path
  path <- create_ifelse(is.null(name_group),
                        paste0(path, "/ICCs/ICCs_for_", irtmodel),
                        paste0(path, "/ICCs/", name_group, "/ICCs_for_", irtmodel))

    # create directory for plots
  check_folder(path = here::here(path))

  # ICC plots
  for (i in 1:model$mod$nitems) {
    tiff(paste0(here::here(paste0(path, "/item_")),
                model$mod$item[i, 1],
                ".tiff"),
        width = 800, height = 800, bg = "white",
        res = 300, compression = "lzw", pointsize = 6)
    plot(model$mod, export = FALSE, type = "expected", items = i,
         wle = model$wle$theta)
    dev.off()
  }
}



#' Wright maps
#'
#' Create Wright maps for IRT models.
#'
#' @param model  list; return object of irt_model()
#' @param path  string; defines path to folder where plots shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @importFrom grDevices dev.off png tiff
#' @importFrom graphics mtext text
#' @export

wright_map <- function(model, path = here::here("Plots"), name_group = NULL) {

  # Identify kind of irt model
  irtmodel <- model$irtmodel

  # Add group name to path
  path <- create_ifelse(is.null(name_group),
                        paste0(path, "/Wright_Maps"),
                        paste0(path, "/Wright_Maps/", name_group))

  # Create directory for plots
  check_folder(path = here::here(path))

  # Create Wright Map
  png(here::here(paste0(path, "/Wright_map_for_", irtmodel, ".png")),
      width = 800, height = 1300, bg = "white",
      res = 300, pointsize = 10)
  TAM::IRT.WrightMap(TAM::IRT.threshold(model$mod),
                     main.title = "Wright map",
                     label.items =  paste0("I",c(1:length(model$mod$xsi))),
                     item.side = "itemClassic",
                     return.thresholds = FALSE, dim.names = "",
                     show.axis.logits = "R",
                     axis.items = "",
                     axis.persons = "")
  text(0, 0, "")
  mtext("Item difficulties", 3, line = 0.5, cex = .65)
  mtext("Respondents", 3, line = 0.5, cex = .65, at = 0.04)
  mtext("Logits", 3, line = 0.5, cex = .65, at = 1)
  dev.off()
}


#' Summary of IRT analysis
#'
#' Create table with results of IRT analysis.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#'   includes all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param valid  string; defines name of logical variable in resp that indicates
#'   (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param results  list; return object of irt_model(); one parameter model
#' @param disc  list; return object of irt_model(); two parameter model
#' @param path  string; defines path to folder where table shall be saved
#' @param filename  string; defines name of table that shall be saved
#' @param digits  integer; number of decimals for rounding
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @return a data.frame containing the item name, N, percentage correct,
#'   item difficulty, SE, WMNSQ, t, rit, item discrimination, Q3.
#' @importFrom rlang .data
#' @export

irt_summary <- function(resp, vars, valid = NULL, mvs = NULL,
                        results, disc = NULL,
                        path = here::here("Tables"), filename = NULL,
                        digits = 2, overwrite = FALSE, warn = TRUE) {

  # prepare data
  check_items(rownames(results$mod$xsi))
  vars$irt_item <- vars$item %in% rownames(results$mod$xsi)
  vars_ <- vars[vars$irt_item, ]
  resp <- prepare_resp(resp, vars = vars, select = 'irt_item', valid = valid,
                       use_only_valid = TRUE, convert = TRUE, mvs = mvs,
                       warn = warn)


  # item parameters
  pars <- results$mod$xsi[, c("xsi", "se.xsi")]
  pars$item <- rownames(results$mod$xsi)
  pars <- pars[vars_$item, ]

  # percentage correct
  pars$pc <- round(ifelse(vars_$dich, colMeans(resp[, vars_$item], na.rm = TRUE) * 100, NA), 0)

  # number of valid responses
  pars$N <- colSums(!is.na(resp))

  # items fit
  pars$WMNSQ   <- results$fit$Infit[results$fit$item %in% vars_$item]
  pars$WMNSQ_t <- results$fit$Infit_t[results$fit$item %in% vars_$item]

  # corrected item-total discrimination
  rit <- c()
  for (i in pars$item) {
    rest <- pars$item[!(pars$item %in% i)]
    score <- NA
    score <- rowSums(resp[, rest], na.rm = TRUE)
    rit <- c(rit, cor(resp[, i], score, use = "complete.obs"))
  }
  pars$rit <- rit

  # 2PL discrimination
  if (!(results$irtmodel %in% c("1PL", "PCM2"))) {
      pars$disc <- results$mod$item[, "B.Cat1.Dim1"]
  } else if (!is.null(disc)) {
      pars$disc <- disc
  }

  # Yen Q3: average absolute residual correlation for items (adjusted)
  pars$Q3 <- colMeans(abs(results$mfit$aQ3.matr), na.rm = TRUE)

  # numbering
  pars$num <- seq(1, nrow(pars))

  # reorder columns
  pars <- pars[ , c("num", "item", "N", "pc", "xsi", "se.xsi",
                    "WMNSQ", "WMNSQ_t", "rit", "disc", "Q3")]
  colnames(pars) <- c("Number", "Item", "N", "% correct",
                      "xsi", "SE", "WMNSQ", "t", "rit", "Discr.", "aQ3")
  pars[, -c(1:4)] <- round(pars[, -c(1:4)], digits)

  # Format table
  pars_formatted <- pars
  pars_formatted[, -c(1:4)] <- format(pars_formatted[, -c(1:4)], nsmall = digits)

  # Save table
  save_table(pars_formatted, filename = filename, path = path,
             overwrite = overwrite, show_rownames = FALSE)

  # Return table
  return(pars)
}



#' IRT model fit
#'
#' Create table with model fit for 1 parameter and 2 parameter IRT model.
#'
#' @param model_1p  list; return object of irt_model(); 1PL / PCM analysis
#' @param model_2p  list; return object of irt_model(); 2PL / GPCM analysis
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param path  string; defines path to folder where table shall be saved
#' @param filename  string; defines name of table that shall be saved
#'
#' @return data.frame with AIC, BIC and number of parameters for both models
#' @export

irt_model_fit <- function(model_1p, model_2p, overwrite = FALSE,
                          path = here::here("Tables"), filename = NULL) {

  mfit <- data.frame(N = rep(NA_integer_, 2),
                     Npars = rep(NA_integer_, 2),
                     Deviance = rep(NA_integer_, 2),
                     AIC = rep(NA_integer_, 2),
                     BIC = rep(NA_integer_, 2))

  irt_type <- ifelse(model_1p$irtmodel == '1PL', 'dich', 'poly')

  if(irt_type == 'dich') {
    row.names(mfit) <- c("1PL model", "2PL model")
  } else if (irt_type == 'poly') {
    row.names(mfit) <- c("PCM model", "GPCM model")
  } else {
    stop("No valid irt_type provided. Possible are 'dich' for dichotomous ",
         "analysis or 'poly' for polytomous analysis.")
  }

  mfit$N[1] <- model_1p$info_crit$n
  mfit$Npars[1] <- model_1p$info_crit$Npars
  mfit$Deviance[1] <- model_1p$info_crit$deviance
  mfit$AIC[1] <- model_1p$info_crit$AIC
  mfit$BIC[1] <- model_1p$info_crit$BIC

  mfit$N[2] <- model_2p$info_crit$n
  mfit$Npars[2] <- model_2p$info_crit$Npars
  mfit$Deviance[2] <- model_2p$info_crit$deviance
  mfit$AIC[2] <- model_2p$info_crit$AIC
  mfit$BIC[2] <- model_2p$info_crit$BIC

  # Save table
  save_table(mfit, filename = filename, path = path, overwrite = overwrite)

  # Return table
  return(mfit)
}


#' Step analysis
#'
#' Create table with pcm_model of step analysis.
#'
#' @param pcm_model  list; return object of irt_model(); PCM analysis
#' @param path  string; defines path to folder where table shall be saved
#' @param filename  string; defines name of table that shall be saved
#' @param digits  integer; number of decimals for rounding
#' @param overwrite  logical; whether to overwrite existing file when saving table
#'
#' @return a data.frame containing the step parameters and SEs for each step
#' @export

steps_analysis <- function(pcm_model, digits = 2, overwrite = FALSE,
                           path = here::here("Tables"), filename = NULL) {

  # step parameters
  step <- round(pcm_model$mod$xsi[, c("xsi", "se.xsi")], digits)
  step$item <- sub("_step[0-9]$", "", rownames(pcm_model$mod$xsi))
  step$step <- as.numeric(gsub(".+_step", "", rownames(pcm_model$mod$xsi)))
  step <- step[!is.na(step$step), ]

  # create matrix for pcm_model
  steps <- matrix(NA, ncol = max(step$step) + 1, nrow = length(unique(step$item)))
  rownames(steps) <- unique(step$item)
  colnames(steps) <- paste0("step", seq_len(ncol(steps)))
  pars <- steps

  # include step parameters and SEs
  for (i in seq_len(nrow(step))) {
    steps[step$item[i], step$step[i]] <- paste0(format(step$xsi[i], nsmall = digits), " (",
                                                format(step$se.xsi[i]), nsmall = digits, ")")
    pars[step$item[i], step$step[i]] <- step$xsi[i]
  }

  # sum 0 constraint for last step
  for (i in seq_len(nrow(steps))) {
    steps[i, seq_len(ncol(steps))[is.na(pars[i, ])][1]] <- format(1 * sum(pars[i, ],
                                                                          na.rm = TRUE),
                                                                  nsmall = digits)
  }

  # convert to data.frame
  steps <- data.frame(steps)

  # Save table as Excel sheet
  save_table(steps, filename = filename, path = path, overwrite = overwrite)

  # Return table
  return(steps)
}


#' Print IRT results
#'
#' Print and highlight IRT (and steps) analysis results.
#'
#' @param model  list; return object of irt_model()
#' @param irt_sum  data.frame; return object of irt_summary()
#' @param steps_sum  data.frame; return object of steps_analysis()
#'
#' @export

print_irt_summary <- function(model, irt_sum, steps_sum = NULL) {

  # Percentage correct
  pc_min <- min(irt_sum[['% correct']], na.rm = TRUE)
  pc_min_item <- irt_sum$Item[irt_sum[['% correct']] %in% pc_min]
  pc_max <- max(irt_sum[['% correct']], na.rm = TRUE)
  pc_max_item <- irt_sum$Item[irt_sum[['% correct']] %in% pc_max]
  pc_mean <- round(mean(irt_sum[['% correct']], na.rm = TRUE))
  pc_median <- round(median(irt_sum[['% correct']], na.rm = TRUE))
  message("Percentage correct:\n",
          "The percentage of correct responses within dichotomous items varied between ",
          pc_min, "% (", ifelse(length(pc_min_item) > 1, "items ", "item "),
          paste(pc_min_item, collapse = ", "), ") and ",
          pc_max, "% (", ifelse(length(pc_max_item) > 1, "items ", "item "),
          paste(pc_max_item, collapse = ", "),
          ") with an average of ", pc_mean, "% correct responses, and a median of ",
          pc_median, "% correct responses.\n")

  # Item difficulties
  xsi_min <- min(irt_sum$xsi, na.rm = TRUE)
  xsi_min_item <- irt_sum$Item[irt_sum$xsi %in% xsi_min]
  xsi_max <- max(irt_sum$xsi, na.rm = TRUE)
  xsi_max_item <- irt_sum$Item[irt_sum$xsi %in% xsi_max]
  xsi_mean <- round(mean(irt_sum$xsi, na.rm = TRUE), 2)
  xsi_median <- round(median(irt_sum$xsi, na.rm = TRUE), 2)
  xsi_sd <- round(sd(irt_sum$xsi, na.rm = TRUE), 2)
  message("Item difficulties:\n",
          "The estimated item difficulties (or location parameters for polytomous variables) varied between ",
          xsi_min, " (", ifelse(length(xsi_min_item) > 1, "items ", "item "),
          paste(xsi_min_item, collapse = ", "), ") and ",
          xsi_max, " (", ifelse(length(xsi_max_item) > 1, "items ", "item "),
          paste(xsi_max_item, collapse = ", "),
          ") with an average of ", xsi_mean, " (", xsi_sd,
          "), and a median of ", xsi_median, ".\n")

  # SE of item difficulties
  se_max <- max(irt_sum$SE, na.rm = TRUE)
  se_max_item <- irt_sum$Item[irt_sum$SE %in% se_max]
  message("SE of item difficulties:\n",
          "The maximum of SEs is ", se_max,
          " (", paste(se_max_item, collapse = ", "), ".\n")

  # WMNSQ
  wmnsq_min <- min(irt_sum$WMNSQ, na.rm = TRUE)
  wmnsq_min_item <- irt_sum$Item[irt_sum$WMNSQ %in% wmnsq_min]
  wmnsq_max <- max(irt_sum$WMNSQ, na.rm = TRUE)
  wmnsq_max_item <- irt_sum$Item[irt_sum$WMNSQ %in% wmnsq_max]
  wmnsq_mean <- round(mean(irt_sum$WMNSQ, na.rm = TRUE), 2)
  wmnsq_median <- round(median(irt_sum$WMNSQ, na.rm = TRUE), 2)
  wmnsq_sd <- round(sd(irt_sum$WMNSQ, na.rm = TRUE), 2)
  message("WMNSQ:\n",
          "The values of the WMNSQ were ... close to 1 with the lowest value being ",
          wmnsq_min, " (", ifelse(length(wmnsq_min_item) > 1, "items ", "item "),
          paste(wmnsq_min_item, collapse = ", "),
          ") and the highest being ",
          wmnsq_max, " (", ifelse(length(wmnsq_max_item) > 1, "items ", "item "),
          paste(wmnsq_max_item, collapse = ", "),
          ") with an average of ", wmnsq_mean, " (", wmnsq_sd,
          "), and a median of ", wmnsq_median, ".")

  wmnsq_misfit <- irt_sum$Item[irt_sum$WMNSQ > 1.15]
  message(ifelse(length(wmnsq_misfit) == 0, "No item",
                 paste0(ifelse(length(wmnsq_misfit) > 1, "Items ", "Item "),
                        paste(wmnsq_misfit, collapse = ", "))),
            " exhibited a WMNSQ of at least 1.15.")

  # WMNSQ t-value
  t_min <- min(irt_sum$t, na.rm = TRUE)
  t_min_item <- irt_sum$Item[irt_sum$t %in% t_min]
  t_max <- max(irt_sum$t, na.rm = TRUE)
  t_max_item <- irt_sum$Item[irt_sum$t %in% t_max]
  t_mean <- round(mean(irt_sum$t, na.rm = TRUE), 2)
  t_median <- round(median(irt_sum$t, na.rm = TRUE), 2)
  message("\nWMNSQ t-value:\n",
          "The WMNSQ t-values varied between ",
          t_min, " (", ifelse(length(t_min_item) > 1, "items ", "item "),
          paste(t_min_item, collapse = ", "), ") and ",
          t_max, " (", ifelse(length(t_max_item) > 1, "items ", "item "),
          paste(t_max_item, collapse = ", "), ") with an average of ",
          t_mean, ", and a median of ", t_median, ".")

  t_misfit <- irt_sum$Item[abs(irt_sum$t) > 8]

  message(ifelse(length(t_misfit) == 0, "No item",
          paste0(ifelse(length(t_misfit) > 1, "Items ", "Item "),
          paste(t_misfit, collapse = ", "))),
          " exhibited an absolute t-value of at least 8.")

  # Correlation of item scores with total correct score
  rit_min <- min(irt_sum$rit, na.rm = TRUE)
  rit_min_item <- irt_sum$Item[irt_sum$rit %in% rit_min]
  rit_max <- max(irt_sum$rit, na.rm = TRUE)
  rit_max_item <- irt_sum$Item[irt_sum$rit %in% rit_max]
  rit_mean <- round(mean(irt_sum$rit, na.rm = TRUE), 2)
  rit_median <- round(median(irt_sum$rit, na.rm = TRUE), 2)
  message("\nCorrelation of item scores with total correct score:\n",
          "The correlations between the item scores and the total correct scores varied between ",
          rit_min, " (", ifelse(length(rit_min_item) > 1, "items ", "item "),
          paste(rit_min_item, collapse = ", "), ") and ",
          rit_max, " (", ifelse(length(rit_max_item) > 1, "items ", "item "),
          paste(rit_max_item, collapse = ", "), ") with an average correlation of ",
          rit_mean, ", and a median correlation of ", rit_median, ".\n")

  # Model variance
  message("Model variance:\n",
          "The variance of the model was estimated to be ", round(model$mod$variance[1], 3),".\n")

  # Test reliability
  eap_rel <- round(model$mod$EAP.rel[1], 3)
  wle_rel <- round(model$wle_rel[1], 3)
  message("Test reliability:\n",
          "The reliabilities of the test (EAP/PV reliability = ", eap_rel,
          ", WLE reliability = ", wle_rel, ") were ... .\n")

  # Item discrimination
  disc_min <- min(irt_sum$Discr., na.rm = TRUE)
  disc_min_item <- irt_sum$Item[irt_sum$Discr. %in% disc_min]
  disc_max <- max(irt_sum$Discr., na.rm = TRUE)
  disc_max_item <- irt_sum$Item[irt_sum$Discr. %in% disc_max]
  disc_mean <- round(mean(irt_sum$Discr., na.rm = TRUE), 2)
  disc_median <- round(median(irt_sum$Discr., na.rm = TRUE), 2)
  message("Item discrimination:\n",
          "The estimated discrimination parameters varied between ",
          disc_min, " (", ifelse(length(disc_min_item) > 1, "items ", "item "),
          paste(disc_min_item, collapse = ", "), ") and ",
          disc_max, " (", ifelse(length(disc_max_item) > 1, "items ", "item "),
          paste(disc_max_item, collapse = ", "), ") with an average discrimination of ",
          disc_mean, ", and a median discrimination of ", disc_median, ".\n")

  # Thresholds
  thresh_mean <- round(mean(c(TAM::IRT.threshold(model$mod)), na.rm = TRUE), 3)
  thresh_median <- round(median(c(TAM::IRT.threshold(model$mod)), na.rm = TRUE), 3)
  message("Thresholds:\n",
          "The mean threshold is ", thresh_mean,
          " and the median threshold is ", thresh_median, ".\n")
}
