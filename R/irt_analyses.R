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
#' @return (if return = TRUE) list with a list for each group with all results
#' (as return object of irt_analysis()) as list elements.
#' @export

grouped_irt_analysis <- function(groups, resp, vars, valid = NULL, mvs = NULL,
                                 scoring = NULL, plots = FALSE, save = TRUE,
                                 print = TRUE, return = FALSE, overwrite = FALSE,
                                 path_plots = here::here("Plots"),
                                 path_table = here::here("Tables"),
                                 path_results = here::here("Results"),
                                 digits = 3, verbose = FALSE, warn = TRUE,
                                 xsi.fixed = NULL, pweights = NULL,
                                 control_tam = NULL, control_wle = NULL) {

    # Test data
    scaling:::check_logicals(vars, "vars", c(groups, "dich"), warn = warn)
    scaling:::check_logicals(resp, "resp", c(valid, names(groups)), warn = warn)
    if (!is.null(scoring))
        scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)
    if (!is.null(pweights))
        scaling:::check_numerics(resp, "resp", pweights, check_invalid = TRUE)
    if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)

    # Create list for results
    irt_groups <- list()
    i <- 1

    # Conduct irt_analysis for each group
    for (g in names(groups)) {

        select <- groups[[g]]
        scaling:::check_items(vars$item[vars[[select]]])
        scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])

        message(toupper(paste0("\n\n\n(", i, ") irt analysis (",
                               ifelse(is_poly(resp, vars, select), 'poly', 'dich'),
                               ") for group '", g, "':\n")))

        irt_groups[[g]] <- scaling:::irt_analysis(
          resp = resp[resp[[g]], ],
          vars = vars,
          select = select,
          valid = valid,
          scoring = scoring,
          print = print,
          plots = plots,
          save = save,
          return = TRUE,
          path_results = path_results,
          path_table = path_table,
          path_plots = path_plots,
          overwrite = overwrite,
          digits = digits,
          name_group = g,
          warn = FALSE,
          test = FALSE,
          control_tam = control_tam,
          control_wle = control_wle,
          pweights = pweights,
          xsi.fixed = xsi.fixed
        )

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
#' @return   (if return = TRUE) list with all results:
#'             model.1pl/model.pcm: results of irt analysis with one parameter
#'             model.2pl/model.gpcm: results of irt analysis with two parameters
#'           (depending on function arguments / type of analysis) +
#'             summary: table with results for TR
#'             model_fit: table with goodness of fit indicators for all mdoels
#'             steps: table with results of steps analysis (only for polytomous analysis)
#' @export

irt_analysis <- function(resp, vars, select, valid = NULL, mvs = NULL,
                         scoring = NULL, plots = FALSE, save = TRUE,
                         print = TRUE, return = FALSE,
                         path_plots = here::here("Plots"),
                         path_table = here::here("Tables"),
                         path_results = here::here("Results"),
                         overwrite = FALSE, digits = 3, name_group = NULL,
                         verbose = FALSE, warn = TRUE, test = TRUE,
                         xsi.fixed = NULL, pweights = NULL,
                         control_tam = NULL, control_wle = NULL) {

    # Test data
    if (test) {
        scaling:::check_logicals(vars, "vars", c(select, "dich"), warn = warn)
        scaling:::check_logicals(resp, "resp", valid, warn = warn)
        scaling:::check_items(vars$item[vars[[select]]])
        scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
        if (!is.null(scoring))
            scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)
        if (!is.null(pweights))
            scaling:::check_numerics(resp, "resp", pweights, check_invalid = TRUE)
        if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)
    }

    scaling:::check_items(vars$item[vars[[select]]])

    # Identify IRT type
    irt_type <- ifelse(is_poly(resp, vars, select), 'poly', 'dich')

    # Create list with results
    irt <- list()

    # Conduct IRT analyses
    if (irt_type == 'dich') {

        irt$model.1pl <- scaling:::irt_model(
            resp = resp,
            vars = vars,
            select = select,
            valid = valid,
            mvs = mvs,
            irtmodel = '1PL',
            control_tam = control_tam,
            control_wle = control_wle,
            pweights = pweights,
            xsi.fixed = xsi.fixed,
            verbose = verbose,
            save = FALSE,
            warn = FALSE,
            test = FALSE
        )

        irt$model.2pl <- scaling:::irt_model(
            resp = resp,
            vars = vars,
            select = select,
            valid = valid,
            mvs = mvs,
            irtmodel = '2PL',
            control_tam = control_tam,
            control_wle = control_wle,
            pweights = pweights,
            xsi.fixed = xsi.fixed,
            verbose = verbose,
            save = FALSE,
            warn = FALSE,
            test = FALSE
        )

    } else if (irt_type == 'poly') {

      irt$model.pcm <- scaling:::irt_model(
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          mvs = mvs,
          irtmodel = 'PCM2',
          scoring = scoring,
          control_tam = control_tam,
          control_wle = control_wle,
          pweights = pweights,
          xsi.fixed = xsi.fixed,
          verbose = verbose,
          save = FALSE,
          warn = FALSE,
          test = FALSE
      )

      irt$model.gpcm <- scaling:::irt_model(
          resp = resp,
          vars = vars,
          select = select,
          valid = valid,
          mvs = mvs,
          irtmodel = 'GPCM',
          scoring = scoring,
          control_tam = control_tam,
          control_wle = control_wle,
          pweights = pweights,
          xsi.fixed = xsi.fixed,
          verbose = verbose,
          save = FALSE,
          warn = FALSE,
          test = FALSE
      )

    }

  # Create plots
  if (plots) {

    for (i in 1:2) {

      # ICC plots
      scaling:::icc_plots(model = irt[[i]], path = path_plots, name_group = name_group)

      # Wright map
      scaling:::wright_map(model = irt[[i]], path = path_plots, name_group = name_group)

    }
  }

  # Create tables
  if (return | print | save) {
    # IRT summary
    irt$summary <- scaling:::irt_summary(
      resp = resp,
      vars = vars,
      results = irt[[1]],
      disc = irt[[2]],
      valid = valid,
      mvs = mvs,
      digits = digits,
      save = FALSE,
      test = FALSE
    )

    # Model fit
    irt$model_fit <- scaling:::irt_model_fit(
      model_1p = irt[[1]],
      model_2p = irt[[2]],
      save = FALSE
    )

    # Steps analysis
    if (irt_type == 'poly') {
      irt$steps <- scaling:::steps_analysis(
        pcm_model = irt$model.pcm,
        digits = digits,
        save = FALSE
      )
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
    scaling:::print_irt_summary(
      model = irt[[1]],
      irt_sum = irt$summary,
      steps_sum = irt$steps
    )
  }

  # Save results
  if (save) {
      name <- scaling:::create_name(paste0("irt_", irt_type), name_group)
      irt_summary <- irt[-c(1:2)]

      scaling:::save_results(irt, filename = paste0(name, ".rds"), path = path_results)
      scaling:::save_table(
        irt_summary,
        filename = paste0(name, ".xlsx"),
        path = path_table,
        overwrite = overwrite
      )
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
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where results shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param verbose  logical; whether to print processing information to console
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return list with results:
#'   mod: tam.mml; estimated item response model
#'   fit: data.frame; item fit (WMNSQ) of the model
#'   pars: data.frame; item difficulties and scoring parameters with respective standard errors
#'   mfit: data.frame; (adjusted) Yen's Q3 statistics of the model
#'   wle: data.frame; WLEs for the model
#'   wle_rel: numeric; WLE reliability of the model
#'   info_crit: data.frame with information criteria of the model.
#' @export

irt_model <- function(resp, vars, select, valid = NULL, mvs = NULL, irtmodel,
                      scoring = NULL, verbose = FALSE, save = TRUE,
                      path = here::here("Results"), name_group = NULL,
                      xsi.fixed = NULL, pweights = NULL,
                      control_tam = NULL, control_wle = NULL,
                      warn = TRUE, test = TRUE) {

  # Test data
  if (test) {
      scaling:::check_logicals(vars, "vars", select, warn = warn)
      scaling:::check_logicals(resp, "resp", valid, warn = warn)
      scaling:::check_items(vars$item[vars[[select]]])
      scaling:::check_numerics(resp, "resp", vars$item[vars[[select]]])
      if (!is.null(scoring))
          scaling:::check_numerics(vars, "vars", scoring, check_invalid = TRUE)
      if (!is.null(pweights))
          scaling:::check_numerics(resp, "resp", pweights, check_invalid = TRUE)
      if (warn) scaling:::is_null_mvs_valid(mvs = mvs, valid = valid)
  }

  # Check if input is correct
  if (!irtmodel %in% c("1PL", "2PL", "GPCM", "PCM2")) {
    stop("Invalid irtmodel. Please provide one of the following: '1PL',
         '2PL', 'PCM2', 'GPCM'.")
  }

  # Select only valid cases
  resp <- scaling:::only_valid(resp, valid = valid, warn = FALSE)

  # Create ID variable
  pid <- resp$ID_t
  scaling:::check_pid(pid)
  pws <- scaling:::create_ifelse(is.null(pweights), NULL, resp[[pweights]])

  # Prepare data
  resp <- scaling:::prepare_resp(
    resp,
    vars = vars,
    select = select,
    convert = TRUE,
    mvs = mvs,
    warn = FALSE
  )

  # Test data
  scaling:::check_numerics(resp, "resp", check_invalid = TRUE) # this check is very important as otherwise R might be aborted!!
  if (irtmodel %in% c("1PL", "2PL")) scaling:::check_dich(resp, "resp")

  # Create scoring matrix if not provided in function arguments
  Q <- scaling:::create_q(
    vars,
    select = select,
    scoring = scoring,
    poly = ifelse(irtmodel %in% c("PCM2", "GPCM"), TRUE, FALSE)
  )

  # IRT model
  if (irtmodel %in% c("1PL", "PCM2")) {

    mod <- TAM::tam.mml(
      resp = resp,
      irtmodel = irtmodel,
      Q = Q,
      pid = pid,
      verbose = verbose,
      xsi.fixed = xsi.fixed,
      control = control_tam,
      pweights = pws
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
  scaling:::reached_maxiter(mod, paste0("'", irtmodel, "'"))

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
  if (is.null(control_wle$Msteps)) control_wle$Msteps <- 1000

  wle <- TAM::tam.wle(
    mod,
    convM = control_wle$convM,
    Msteps = control_wle$Msteps,
    progress = verbose
  )
  wle_rel <- wle$WLE.rel[1]
  wle <- wle[, c("pid", "theta", "error")]

  # information criteria
  info_crit <- mod$ic

  # List with results
  results <- list(
    mod = mod,
    fit = fit,
    pars = pars,
    mfit = mfit,
    wle = wle,
    wle_rel = wle_rel,
    info_crit = info_crit,
    irtmodel = irtmodel
  )

  # Save results
  if (save) {
      name <- scaling:::create_name(irtmodel, name_group, ".rds")
      scaling:::save_results(results, filename = name, path = path)
  }

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
  path_ <- scaling:::create_ifelse(
      is.null(name_group),
      paste0(path, "/ICCs/ICCs_for_", irtmodel),
      paste0(path, "/ICCs/", name_group, "/ICCs_for_", irtmodel)
  )

  # create directory for plots
  scaling:::check_folder(path = here::here(path_))

  # ICC plots
  for (i in 1:model$mod$nitems) {
    tiff(paste0(here::here(paste0(path_, "/item_")),
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
  path_ <- scaling:::create_name(paste0(path, "/Wright_Maps"), name_group, sep = "/")

  # Create directory for plots
  scaling:::check_folder(path = here::here(path_))

  # Create Wright Map
  png(here::here(paste0(path_, "/Wright_map_for_", irtmodel, ".png")),
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
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where table shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param digits  integer; number of decimals for rounding
#' @param overwrite  logical; whether to overwrite existing file when saving table
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return a data.frame (for TR) containing the item name, N, percentage correct,
#'   item difficulty, SE, WMNSQ, t, rit, item discrimination, Q3.
#' @importFrom rlang .data
#' @export

irt_summary <- function(resp, vars, valid = NULL, mvs = NULL,
                        results, disc = NULL, save = TRUE,
                        path = here::here("Tables"), name_group = NULL,
                        digits = 3, overwrite = FALSE, warn = TRUE, test = TRUE) {

  # prepare data
  if (test) scaling:::check_logicals(vars, "vars", "dich", warn = warn)
  scaling:::check_items(rownames(results$mod$xsi))
  vars$irt_item <- vars$item %in% rownames(results$mod$xsi)
  vars <- vars[vars$irt_item, ]
  resp <- scaling:::prepare_resp(
    resp,
    vars = vars,
    select = 'irt_item',
    valid = valid,
    use_only_valid = TRUE,
    convert = TRUE,
    mvs = mvs,
    warn = warn
  )

  # create dataframe
  rows <- nrow(results$mod$xsi)
  pars <- data.frame(
      Item = rep(NA, rows),
      N = rep(NA, rows),
      correct = rep(NA, rows),
      xsi = rep(NA, rows),
      SE = rep(NA, rows),
      WMNSQ = rep(NA, rows),
      t = rep(NA, rows),
      rit = rep(NA, rows),
      aQ3 = rep(NA, rows)
  )

  # item parameters
  pars$Item <- rownames(results$mod$xsi)
  pars$xsi <- results$mod$xsi$xsi
  pars$SE <- results$mod$xsi$se.xsi

  # proceed only with selected variables
  pars <- pars[pars$Item %in% vars$item, ]

  # percentage correct
  pars$correct <- round(ifelse(vars$dich, colMeans(resp[, vars$item], na.rm = TRUE) * 100, NA), 2)

  # number of valid responses
  pars$N <- colSums(!is.na(resp))

  # items fit
  pars$WMNSQ   <- results$fit$Infit[results$fit$item %in% vars$item]
  pars$t <- results$fit$Infit_t[results$fit$item %in% vars$item]

  # corrected item-total discrimination
  rit <- c()
  for (i in pars$Item) {
    rest <- pars$Item[!(pars$Item %in% i)]
    score <- NA
    score <- rowSums(resp[, rest], na.rm = TRUE)
    rit <- c(rit, cor(resp[, i], score, use = "complete.obs"))
  }
  pars$rit <- rit

  # Yen Q3: average absolute residual correlation for items (adjusted)
  pars$aQ3 <- colMeans(abs(results$mfit$aQ3.matr), na.rm = TRUE)

  # 2PL discrimination
  if (!(results$irtmodel %in% c("1PL", "PCM2"))) {
      pars$Discr. <- results$mod$item[, "B.Cat1.Dim1"]
  } else if (!is.null(disc)) {
      pars$Discr. <- disc$mod$item[, "B.Cat1.Dim1"]
  }

  # Format table
  pars[, -c(1:3)] <- round(pars[ , -c(1:3)], digits)
  pars_formatted <- pars
  pars_formatted[, -c(1:3)] <- format(pars_formatted[, -c(1:3)], nsmall = digits)

  # Save table
  if (save) {
    name <- scaling:::create_name(results$irtmodel, name_group, ".xlsx")
    scaling:::save_table(
      pars_formatted,
      filename = name,
      path = path,
      overwrite = overwrite,
      show_rownames = FALSE
    )
  }

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
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where table shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @return data.frame with N, Deviance, AIC, BIC and number of parameters for
#' both models.
#' @export

irt_model_fit <- function(model_1p, model_2p, overwrite = FALSE, save = TRUE,
                          path = here::here("Tables"), name_group = NULL) {

  mfit <- data.frame(
      N = rep(NA_integer_, 2),
      Npars = rep(NA_integer_, 2),
      Deviance = rep(NA_integer_, 2),
      AIC = rep(NA_integer_, 2),
      BIC = rep(NA_integer_, 2)
  )

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
  if (save) {
    name <- scaling:::create_name(paste0("model.fit_", irt_type), name_group, ".xlsx")
    scaling:::save_table(
      mfit,
      filename = name,
      path = path,
      overwrite = overwrite
    )
  }

  # Return table
  return(mfit)
}


#' Step analysis
#'
#' Create table with pcm_model of step analysis.
#'
#' @param pcm_model  list; return object of irt_model(); PCM analysis
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; defines path to folder where table shall be saved
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param digits  integer; number of decimals for rounding
#' @param overwrite  logical; whether to overwrite existing file when saving table
#'
#' @return a data.frame containing the step parameters and SEs for each step.
#' @export

steps_analysis <- function(pcm_model, digits = 3, save = TRUE, overwrite = FALSE,
                           path = here::here("Tables"), name_group = NULL) {

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
    steps[step$item[i], step$step[i]] <- paste0(
      format(step$xsi[i], nsmall = digits), " (",
      format(step$se.xsi[i]), nsmall = digits, ")"
    )
    pars[step$item[i], step$step[i]] <- step$xsi[i]
  }

  # sum 0 constraint for last step
  for (i in seq_len(nrow(steps))) {
    steps[i, seq_len(ncol(steps))[is.na(pars[i, ])][1]] <-
      format(1 * sum(pars[i, ], na.rm = TRUE), nsmall = digits)
  }

  # convert to data.frame
  steps <- data.frame(steps)

  # Save table as Excel sheet
  if (save) {
    name <- scaling:::create_name("steps_analysis", name_group, ".xlsx")
    scaling:::save_table(
      steps,
      filename = name,
      path = path,
      overwrite = overwrite
    )
  }

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
  pc_min <- min(irt_sum[['correct']], na.rm = TRUE)
  pc_min_item <- irt_sum$Item[irt_sum[['correct']] %in% pc_min]
  pc_max <- max(irt_sum[['correct']], na.rm = TRUE)
  pc_max_item <- irt_sum$Item[irt_sum[['correct']] %in% pc_max]
  pc_mean <- round(mean(irt_sum[['correct']], na.rm = TRUE))
  pc_median <- round(median(irt_sum[['correct']], na.rm = TRUE))
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
          ") with an average of ", xsi_mean, " (SD = ", xsi_sd,
          "), and a median of ", xsi_median, ".\n")

  # SE of item difficulties
  se_max <- max(irt_sum$SE, na.rm = TRUE)
  se_max_item <- irt_sum$Item[irt_sum$SE %in% se_max]
  message("SE of item difficulties:\n",
          "The maximum of SEs is ", se_max,
          " (", paste(se_max_item, collapse = ", "), ").\n")

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
          ") with an average of ", wmnsq_mean, " (SD = ", wmnsq_sd,
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
          "EAP/PV reliability = ", eap_rel, "\n",
          "WLE reliability = ", wle_rel, "\n")

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
