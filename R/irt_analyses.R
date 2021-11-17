#' IRT analyses - all in one function
#'
#' Perform 1PL and 2PL analyses for binary or PCM and GPCM analyses for
#' polytomous data, generate summary and plots.
#'
#' @param resp      data.frame. contains:
#'                    (1) the responses. y in {0, 1} for binary data and y in
#'                      {0, 1, ... k-1} for polytomous responses with k categories
#'                    (2) ID_t: column indicating ID of participants
#' @param vars      data.frame. contains all competence items as rows,
#'                  and at least the following variables:
#'                    character vector named "items"; contains the names of the items.
#'                    boolean vector; indicates which items to use for analysis.
#' @param items     character. contains name of variable (boolean) in vars that
#'                    indicates which items to use for analysis.
#' @param valid     character string. defines name of boolean variable in resp,
#'                  indicating (in)valid cases.
#' @param irt_type  character string; either "dich" for dichotomous analysis
#'   or "poly" for polytomous analysis.
#' @param scoring   numeric vector; scoring factor to be applied to loading matrix;
#'                    can be NULL for Rasch model; if NULL for PCM model,
#'                    scoring matrix is reconstructed from item names.
#' @param plots  boolean; indicates whether to create plots.
#' @param path_plots character. contains name of path for plots.
#' @param path_tables character. contains name of path for tables.
#' @param path_results character. contains name of path for data.
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#' @param digits    number of decimals for rounding
#' @param print  boolean; indicates whether to print results  to console or html.
#' @param return  boolean. indicates whether to return results.
#' @param name_group
#'
#' @return
#' @export

irt_analysis <- function(resp, vars, items, valid = NULL, irt_type, scoring = NULL,
                         plots = FALSE, save = TRUE, print = TRUE, return = FALSE,
                         path_plots = here::here("Plots"),
                         path_tables = here::here("Tables"),
                         path_results = here::here("Results"),
                         overwrite = FALSE, digits = 2, name_group = NULL) {

  irt <- list()

  if (irt_type == 'dich') {

    irt$model.1pl <- irt_model(resp = resp, vars = vars, items = items,
                               valid = valid, mvs = mvs, irtmodel = '1PL')
    irt$model.2pl <- irt_model(resp = resp, vars = vars, items = items,
                               valid = valid, mvs = mvs, irtmodel = '2PL')

    irtmodel = c("1PL", "2PL")

  } else if (irt_type == 'poly') {

    irt$model.pcm <- irt_model(resp = resp, vars = vars, items = items, mvs = mvs,
                               valid = valid, irtmodel = 'PCM2', scoring = scoring)
    irt$model.gpcm <- irt_model(resp = resp, vars = vars, items = items, mvs = mvs,
                                valid = valid, irtmodel = 'GPCM', scoring = scoring)

    irtmodel = c("PCM2", "GPCM")

  } else {

    stop("No valid irt_type provided. Possible are 'dich' for dichotomous ",
         "analysis or 'poly' for polytomous analysis.")

  }

  if (plots) {

    for (i in seq_along(irtmodel)) {

      # ICC plots
      icc_plots(results = irt[[i]], name = irtmodel[i], path = path_plots)

      # Wright map
      wright_map(results = irt[[i]], name = irtmodel[i], path = path_plots)

    }
  }


  if (return | print | save) {
    # IRT summary
    irt$summary <- irt_summary(resp = resp, vars = vars,
                               results = irt[[1]], disc = irt[[2]],
                               valid = valid, mvs = mvs, digits = digits)

    # Model fit
    irt$model_fit <- irt_model_fit(model_dich = irt[[1]],
                                   model_poly = irt[[2]],
                                   irt_type = irt_type)

    # Steps analysis
    if (irt_type = 'poly') {
      irt$steps <- steps_analysis(results = irt$model.pcm, digits = digits)
    }
  }

  # Print irt
  if (print)  {
    print(irt$summary)
    print(irt$model_fit)
    if (irt_type == 'poly') print(irt$steps)
    print_irt_summary(model = irt[[1]],
                      irt_sum = irt$summary,
                      steps_sum = irt$steps)
  }

  # Save irt
  if (save) {
    if (is.null(name_group)) {
      name <- paste0("irt_", irt_type)
    } else {
      name <- paste0("irt_", irt_type, "_", name_group)
    }
    irt_summary <- irt[-c(1:2)]

    save_results(irt, filename = paste0(name, ".Rdata"), path = path_results)
    save_table(irt_summary, filename = paste0(name, ".xlsx"), path = path_table)
  }

  # Return irt
  if (return)  return(irt)
}

#' IRT analyses
#'
#' Perform Rasch or 2PL analyses for binary or PCM or GPCM analyses for
#' polytomous data.
#'
#' @param resp      data.frame. contains:
#'                    (1) the responses. y in {0, 1} for binary data and y in
#'                      {0, 1, ... k-1} for polytomous responses with k categories
#'                    (2) ID_t: column indicating ID of participants
#' @param vars      data.frame. contains all competence items as rows,
#'                  and at least the following variables:
#'                    character vector named "items"; contains the names of the items.
#'                    boolean vector; indicates which items to use for analysis.
#' @param items     character. contains name of variable (boolean) in vars that
#'                    indicates which items to use for analysis.
#' @param valid     character string. defines name of boolean variable in resp,
#'                  indicating (in)valid cases.
#' @param irtmodel  character. "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#'                    "GPCM" for GPCM analyses.
#' @param scoring   numeric vector; scoring factor to be applied to loading matrix;
#'                    can be NULL for Rasch model; if NULL for PCM model,
#'                    scoring matrix is reconstructed from item names.
#' @param path      folder path for data
#' @param filename  string with name of file that shall be saved (including file type).
#' @param verbose   logical. If verbose == TRUE information about the estimation
#'                    progress is printed to the console.
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

irt_model <- function(resp, vars, items, valid = NULL, mvs = NULL, irtmodel, scoring = NULL,
                         path = here::here("Results"), filename = NULL,
                         verbose = FALSE) {

  # Check if input is correct
  if (!irtmodel %in% c("1PL", "2PL", "GPCM", "PCM2")) {
    stop("Invalid irtmodel. Please provide one of the following: '1PL',
         '2PL', 'PCM2', 'GPCM'.")
  }

  # Select only valid cases
  resp <- only_valid(resp, valid = valid)

  # Create ID variable
  pid <- resp$ID_t
  check_pid(pid)

  # Prepare data
  resp <- prepare_resp(resp, vars = vars, items = items, convert = TRUE, mvs = mvs)

  # Create scoring matrix if not provided in function arguments
  if (!is.null(scoring)) {
      Q = as.matrix(vars[[scoring]][vars[[items]]])
  } else if (irtmodel %in% c("GPCM", "PCM2")) {
      stop("Please provide variable name for scoring factor for polytomous analysis.")
  } else {
      Q <- NULL
  }

  # IRT model
  if (irtmodel %in% c("1PL", "PCM2")) {
    mod <- TAM::tam.mml(
      resp = resp, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose
    )
  } else {
    mod <- TAM::tam.mml.2pl(
      resp = resp, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose
    )
  }

  # WMNSQ
  fit <- TAM::msq.itemfit(mod)$itemfit[, c("item", "Infit",
                                           "Infit_t", "Infit_p")]

  # Item parameters and standard errors
  pars <- TAM::tam.se(mod)
  pars <- dplyr::left_join(pars$xsi, pars$B, by = "item")
  names(pars) <- c("Item", "xsi", "se(xsi)", "alpha", "se(alpha)")

  # Model fit
  mfit <- TAM::tam.modelfit(mod, progress = verbose)

  # WLEs
  wle <- TAM::tam.wle(mod, progress = verbose)
  wle_rel <- wle$WLE.rel[1]
  wle <- wle[, c("pid", "theta", "error")]

  # information criteria
  info_crit <- mod$ic

  # List with results
  results <- list(
    mod = mod, fit = fit, pars = pars, mfit = mfit,
    wle = wle, wle_rel = wle_rel, info_crit = info_crit
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
#' @param results list. Contains results from IRT analysis as returned by function
#'                'irt_model'.
#' @param name    character. Part of the filename for the directory of the plots,
#'                following "ICCs_for_" (e.g., the kind of IRT analysis: "1PL").
#' @param path    character. contains name of path for plots.
#'
#' @export

icc_plots <- function(results, name, path = here::here("Plots")) {

  # create directory for plots
  check_folder(path = here::here(paste0(path, "/ICCs_for_", name)))

  # ICC plots
  for (i in 1:results$mod$nitems) {
    tiff(paste0(here::here(paste0(path, "/ICCs_for_", name, "/plots")),
                results$mod$item[i, 1],
                ".tiff"),
        width = 800, height = 800, bg = "white",
        res = 300, compression = "lzw", pointsize = 6)
    plot(results$mod, export = FALSE, type = "expected", items = i,
         wle = results$wle$theta)
    dev.off()
  }
}



#' Wright maps
#'
#' Create Wright maps for IRT models.
#'
#' @param results list. Contains results from IRT analysis as returned by function
#'                'irt_model'.
#' @param name    character. Part of the filename for the directory of the plots,
#'                following "ICCs_for_" (e.g., the kind of IRT analysis: "1PL").
#' @param path    character. contains name of path for plots.
#'
#' @importFrom grDevices dev.off png tiff
#' @importFrom graphics mtext text
#' @export

wright_map <- function(results, name, path = here::here("Plots")) {

  # Create directory for plots
  check_folder(path = here::here(path, "/Wright_Maps"))

  # Create Wright Map
  png(here::here(paste0(path, "/Wright_Maps/Wright_map_for_", name, ".png")),
      width = 800, height = 1300, bg = "white",
      res = 300, pointsize = 10)
  TAM::IRT.WrightMap(TAM::IRT.threshold(results$mod),
                main.title = "Wright map with all items",
                label.items =  paste0("I",c(1:length(results$mod$xsi))),
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
#' @param resp     data.frame. contains:
#'                   (1) the responses. y in {0, 1} for binary data and y in
#'                   {0, 1, ... k-1} for polytomous responses with k categories
#'                   (2) ID_t: column indicating ID of participants
#' @param vars     data.frame. contains information about all competence items
#'                 and includes the following columns:
#'                   items: character indicating names of items.
#'                   final: logical indicating whether item is a final item
#' @param results  list. contains results from IRT analysis with one parameter
#'                 (e.g., Rasch analysis)
#' @param disc     list. contains results from IRT analysis with two parameters
#'                 (e.g., 2PL analysis), to include item discrimination
#' @param path     character. defines name of path for table.
#' @param filename character. defines name for excel document. if NULL (default),
#'                 the table will not be saved.
#' @param valid     character string. defines name of boolean variable in resp,
#'                  indicating (in)valid cases.
#' @param digits integer; how many digits after rounding
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#'
#' @return a data.frame containing the item name, N, percentage correct,
#'   item difficulty, SE, WMNSQ, t, rit, item discrimination, Q3.
#' @importFrom rlang .data
#' @export

irt_summary <- function(resp, vars, results, disc, mvs = NULL, valid = NULL,
                        path = here::here("Tables"), filename = NULL,
                        digits = 2, overwrite = FALSE) {

  # prepare data
  vars$irt_items <- vars$items %in% rownames(results$mod$xsi)
  vars_ <- dplyr::rename(vars[vars$irt_items, ], item = 'items')
  resp <- prepare_resp(resp, vars = vars, items = 'irt_items', valid = valid,
                       use_only_valid = TRUE, convert = TRUE, mvs = mvs)


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
  if (!is.null(disc)) {
    pars$disc <- disc$mod$item[, "B.Cat1.Dim1"]
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
#' @param model_dich  list; results of dichotomous irt analysis, as returned by function irt_model()
#' @param model_poly  list; results of polytomous irt analysis, as returned by function irt_model()
#' @param irt_type  character string; either "dich" for dichotomous analysis or "poly" for polytomous analysis.
#'
#' @return data.frame with AIC, BIC and number of parameters for both models will be returned.
#' @export

irt_model_fit <- function(model_dich, model_poly, irt_type, overwrite = FALSE,
                          path = here::here("Tables"), filename = NULL) {

  mfit <- data.frame(AIC = rep(NA_integer_, 2),
                     BIC = rep(NA_integer_, 2),
                     Npars = rep(NA_integer_, 2))

  if(irt_type == 'dich') {
    row.names(mfit) <- c("1PL model", "2PL model")
  } else if (irt_type == 'poly') {
    row.names(mfit) <- c("PCM model", "GPCM model")
  } else {
    stop("No valid irt_type provided. Possible are 'dich' for dichotomous ",
         "analysis or 'poly' for polytomous analysis.")
  }

  mfit$AIC[1] <- model_dich$info_crit$AIC
  mfit$BIC[1] <- model_dich$info_crit$BIC
  mfit$Npars[1] <- model_dich$info_crit$Npars

  mfit$AIC[2] <- model_poly$info_crit$AIC
  mfit$BIC[2] <- model_poly$info_crit$BIC
  mfit$Npars[2] <- model_poly$info_crit$Npars

  # Save table
  save_table(mfit, filename = filename, path = path, overwrite = overwrite)

  # Return table
  return(mfit)
}


#' Step analysis
#'
#' Create table with results of step analysis.
#'
#' @param results  list. contains results from IRT analysis with one parameter
#'                 (PCM analysis).
#' @param path     character. defines name of path for table.
#' @param filename character. defines name for excel document. if NULL (default),
#'                 the table will not be saved.
#' @param digits integer; how many digits after rounding
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#'
#' @return a data.frame containing the step parameters and SEs for each step
#'
#' @export

steps_analysis <- function(results, path = here::here("Tables"), filename = NULL,
                           digits = 2, overwrite = FALSE) {

  # step parameters
  step <- round(results$mod$xsi[, c("xsi", "se.xsi")], digits)
  step$item <- sub("_step[0-9]$", "", rownames(results$mod$xsi))
  step$step <- as.numeric(gsub(".+_c_step", "", rownames(results$mod$xsi)))
  step <- step[!is.na(step$step), ]

  # create matrix for results
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
  save_table(steps, filename = filename, path = path, overwrite = overwrite,
             show_rownames = TRUE)

  # Return table
  return(steps)
}


#' Print IRT results
#'
#' Print and highlight IRT (and steps) analysis results.
#'
#' @param model      list; results of irt analysis, as returned by function irt_model()
#' @param irt_sum    data.frame; results of irt analysis, as returned by function irt_summary()
#' @param steps_sum  data.frame; results of steps analysis, as returned by function steps_analysis()
#'
#' @export

print_irt_summary <- function(model, irt_sum, steps_sum = NULL) {

  # Percentage correct
  print("Percentage correct", quote = FALSE)
  pc_min <- min(irt_sum[['% correct']], na.rm = TRUE)
  pc_max <- max(irt_sum[['% correct']], na.rm = TRUE)
  pc_mean <- round(mean(irt_sum[['% correct']], na.rm = TRUE), 2)
  message("The percentage of correct responses within dichotomous items varied between ",
          pc_min, " % (item ", irt_sum$Item[irt_sum[['% correct']] %in% pc_min], ") and ",
          pc_max, " % (item ", irt_sum$Item[irt_sum[['% correct']] %in% pc_max], ") with an average of ",
          pc_mean, " % correct responses.")

  # Item difficulties
  print("Item difficulties", quote = FALSE)
  xsi_min <- min(irt_sum$xsi, na.rm = TRUE)
  xsi_max <- max(irt_sum$xsi, na.rm = TRUE)
  xsi_mean <- round(mean(irt_sum$xsi, na.rm = TRUE), 2)
  message("The estimated item difficulties (or location parameters for polytomous variables) varied between ",
          xsi_min, " (item ", irt_sum$Item[irt_sum$xsi %in% xsi_min], ") and ",
          xsi_max, " (item ", irt_sum$Item[irt_sum$xsi %in% xsi_max], ") with an average of ",
          xsi_mean, ".")

  # SE of item difficulties
  print("SE of item difficulties", quote = FALSE)
  message("The maximum of SEs is ", max(irt_sum$SE, na.rm = TRUE),".")

  # WMNSQ
  print("WMNSQ", quote = FALSE)
  wmnsq_min <- min(irt_sum$WMNSQ, na.rm = TRUE)
  wmnsq_max <- max(irt_sum$WMNSQ, na.rm = TRUE)
  wmnsq_mean <- round(mean(irt_sum$wmnsq, na.rm = TRUE), 2)
  message("The values of the WMNSQ were ... close to 1 with the lowest value being ",
          wmnsq_min, " (item ", irt_sum$Item[irt_sum$WMNSQ %in% wmnsq_min], ") and the highest being ",
          wmnsq_max, " (item ", irt_sum$Item[irt_sum$WMNSQ %in% wmnsq_max], ") with an average of ",
          wmnsq_mean, ".")

  wmnsq_misfit <- irt_sum$Item[irt_sum$WMNSQ > 1.15]
  if (length(wmnsq_misfit == 1)) {
    message("Item ", wmnsq_misfit, " exhibited a WMNSQ of at least 1.15.")
  } else if (length(wmnsq_misfit > 1)) {
    message("Items ", wmnsq_misfit, " exhibited a WMNSQ of at least 1.15.")
  }

  # WMNSQ t-value
  print("WMNSQ t-value", quote = FALSE)
  t_min <- min(irt_sum$t, na.rm = TRUE)
  t_max <- max(irt_sum$t, na.rm = TRUE)
  t_mean <- round(mean(irt_sum$t, na.rm = TRUE), 2)
  message("The WMNSQ t-values varied between ",
          t_min, " (item ", irt_sum$Item[irt_sum$t %in% t_min], ") and ",
          t_max, " (item ", irt_sum$Item[irt_sum$t %in% t_max], ") with an average of ",
          t_mean, ".")

  t_misfit <- irt_sum$Item[abs(irt_sum$t) > 8]
  if (length(t_misfit == 1)) {
    message("Item ", t_misfit, " exhibited an absolute t-value of at least 8.")
  } else if (length(t_misfit > 1)) {
    message("Items ", t_misfit, " exhibited an absolute t-value of at least 8.")
  }

  # Correlation of item scores with total correct score
  print("Correlation of item scores with total correct score", quote = FALSE)
  rit_min <- min(irt_sum$rit, na.rm = TRUE)
  rit_max <- max(irt_sum$rit, na.rm = TRUE)
  rit_mean <- round(mean(irt_sum$rit, na.rm = TRUE), 2)
  message("The correlations between the item scores and the total correct scores varied between ",
          rit_min, " (item ", irt_sum$Item[irt_sum$rit %in% rit_min], ") and ",
          rit_max, " (item ", irt_sum$Item[irt_sum$rit %in% rit_max], ") with an average correlation of ",
          rit_mean, ".")

  # Model variance
  print("Model variance", quote = FALSE)
  message("The variance of the model was estimated to be ", round(model$mod$variance[1], 3),".")

  # Test reliability
  print("Test reliability", quote = FALSE)
  eap_rel <- round(model$mod$EAP.rel[1], 3)
  wle_rel <- round(model$wle_rel[1], 3)
  message("The reliabilities of the test (EAP/PV reliability = ", eap_rel,
          ", WLE reliability = ", wle_rel, ") were ... .")

  # Item discrimination
  print("Item discrimination", quote = FALSE)
  disc_min <- min(irt_sum$Discr., na.rm = TRUE)
  disc_max <- max(irt_sum$Discr., na.rm = TRUE)
  disc_mean <- round(mean(irt_sum$Discr., na.rm = TRUE), 2)
  message("The estimated discrimination parameters varied between ",
          disc_min, " (item ", irt_sum$Item[irt_sum$Discr. %in% disc_min], ") and ",
          disc_max, " (item ", irt_sum$Item[irt_sum$Discr. %in% disc_max], ") with an average discrimination of ",
          disc_mean, ".")
}
