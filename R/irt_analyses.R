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
#' @param Q         matrix with one column and ncol(resp) rows. Binary items are
#'                    scored with 1, polytomous items are scored with 0.5.
#'                    Divergent scoring decisions can be incorporated by this matrix
#'                    (e.g., item 7 is scored with 0.75 by assigning 0.75 to the seventh row).
#' @param digits    number of decimals for rounding
#' @param irt_type  character string; either "dich" for dichotomous analysis
#'   or "poly" for polytomous analysis.
#' @param icc_plots boolean; indicates whether to create and save ICC plots.
#' @param wright_map boolean; indicates whether to create and save Wright Map.
#' @param path_plots character. contains name of path for plots.
#' @param name_table string with name of table file that shall be saved (including file type);
#'   if left empty, table will not be saved..
#' @param path_table character. contains name of path for table.
#' @param name_data string with name of data file that shall be saved (including file type);
#' if left empty, data will not be saved..
#' @param path_data character. contains name of path for data.
#' @param return_results  boolean. indicates whether to return results.
#'
#' @export

irt_all <- function(resp, vars, items, irt_type, Q = NULL,
                    icc_plots = TRUE, wright_map = TRUE, path_plots = here::here("Plots"),
                    name_table = NULL, path_table = here::here("Tables"), digits = 2,
                    return_results = TRUE, name_data = NULL, path_data = here::here("Data")) {

  results <- list()

  if (irt_type == 'dich') {

    results$model.1pl <- irt_analysis(resp = resp, vars = vars, items = items, irtmodel = '1PL')
    results$model.2pl <- irt_analysis(resp = resp, vars = vars, items = items, irtmodel = '2PL')

    irtmodel = c("1PL", "2PL")

  } else if (irt_type == 'poly') {

    results$model.pcm <- irt_analysis(resp = resp, vars = vars, items = items, irtmodel = 'PCM2')
    results$model.gpcm <- irt_analysis(resp = resp, vars = vars, items = items, irtmodel = 'GPCM')

    irtmodel = c("PCM2", "GPCM")

  } else {

    stop("No valid irt_type provided. Possible are 'dich' for dichotomous ",
         "analysis or 'poly' for polytomous analysis.")

  }

  for (i in seq_along(irtmodel)) {

    # ICC plots
    if (icc_plots) {
      icc_plots(results = results[[i]], name = irtmodel[i], path = path_plots)
    }

    # Wright map
    if (wright_map) {
      wright_map(results = results[[i]], name = irtmodel[i], path = path_plots)
    }

  }

  # IRT summary
  if (return_results | !is.null(name_table) | !is.null(name_data)) {
    results$summary <- irt_summary(resp = resp, vars = vars,
                                   results = results[[1]], disc = results[[2]],
                                   filename = name_table, path = path_table,
                                   return_table = FALSE)
  }

  # Save results
  if (!is.null(name_data)) {

    # Create directory for data
    if (!file.exists(path_data)) {
      dir.create(path_data, recursive = TRUE)
    }

    save(results, file = here::here(paste0(path_data, "/", name_data)))
  }

  # Return results
  if (return_results)  return(results)
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
#' @param irtmodel  character. "1PL" for Rasch, "2PL" for 2PL, "PCM2" for PCM and
#'                    "GPCM" for GPCM analyses.
#' @param Q         matrix with one column and ncol(resp) rows. Binary items are
#'                    scored with 1, polytomous items are scored with 0.5.
#'                    Divergent scoring decisions can be incorporated by this matrix
#'                    (e.g., item 7 is scored with 0.75 by assigning 0.75 to the seventh row).
#' @param path      folder path for data
#' @param filename  string with name of file that shall be saved (including file type).
#' @param verbose   logical. If verbose == TRUE information about the estimation
#'                    progress is printed to the console
#' @param return_results  boolean. indicates whether to return results.
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

irt_analysis <- function(resp, vars, items, irtmodel, Q = NULL,
                         path = here::here("Data"), filename = NULL,
                         verbose = FALSE, return_results = TRUE) {

  # Check if input is correct
  if (!irtmodel %in% c("1PL", "2PL", "GPCM", "PCM2")) {
    stop("Invalid irtmodel. Please provide one of the following: '1PL',
         '2PL', 'PCM2', 'GPCM'.")
  }

  # Prepare data
  pid <- resp$ID_t

  if (length(pid) != length(unique(pid))) {
    stop("There are duplicates in the person identifiers.")
  }

  resp_ <- resp[ , vars$items[vars[[items]]]] %>% min_val() %>% convert_mv

  if (irtmodel %in% c("GPCM", "PCM2") && is.null(Q)) {
    Q <- matrix(1, ncol = 1, nrow = ncol(resp_))
    Q[grepl("s_c", names(resp_)), ] <- 0.5
    warning(
      "Scoring matrix Q was not supplied to polytomous analysis. ",
      "It is reconstructed from the item names in resp_."
    )
  }


  # IRT model
  if (irtmodel %in% c("1PL", "PCM2")) {
    mod <- TAM::tam.mml(
      resp = resp_, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose
    )
  } else {
    mod <- TAM::tam.mml.2pl(
      resp = resp_, irtmodel = irtmodel, Q = Q, pid = pid,
      verbose = verbose
    )
  }

  # WMNSQ
  fit <- TAM::tam.fit(mod)$itemfit[, c(
    "parameter", "Infit", "Infit_t",
    "Infit_p", "Infit_pholm"
  )]

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
  if (!is.null(filename)) {

    # Create directory for data
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    save(results, file = here::here(paste0(path, "/", filename)))
  }

  # Return results
  if (return_results)  return(results)
}


#' ICC plots
#'
#' Create ICC plots for IRT models.
#'
#' @param results list. Contains results from IRT analysis as returned by function
#'                'irt_analysis'.
#' @param name    character. Part of the filename for the directory of the plots,
#'                following "ICCs_for_" (e.g., the kind of IRT analysis: "1PL").
#' @param path    character. contains name of path for plots.
#'
#' @export

icc_plots <- function(results, name, path = here::here("Plots")) {

  # create directory for plots
  if ( !file.exists(here::here(paste0(path, "/ICCs_for_", name)))) {
    dir.create(here::here(paste0(path, "/ICCs_for_", name)), recursive = TRUE)}

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
#'                'irt_analysis'.
#' @param name    character. Part of the filename for the directory of the plots,
#'                following "ICCs_for_" (e.g., the kind of IRT analysis: "1PL").
#' @param path    character. contains name of path for plots.
#'
#' @importFrom grDevices dev.off png tiff
#' @importFrom graphics mtext text
#' @export

wright_map <- function(results, name, path = here::here("Plots")) {

  if ( !file.exists(here::here(path, "/Wright_Maps"))) {
    dir.create(here::here(path, "/Wright_Maps"), recursive = TRUE)}

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
#'                   position: contains position per item. if different for
#'                   different groups, several variables necessary.
#' @param results  list. contains results from IRT analysis with one parameter
#'                 (e.g., Rasch analysis)
#' @param disc     list. contains results from IRT analysis with two parameters
#'                 (e.g., 2PL analysis), to include item discrimination
#' @param path     character. defines name of path for table.
#' @param filename character. defines name for excel document. if NULL (default),
#'                 the table will not be saved.
#' @param digits integer; how many digits after rounding
#' @param return_table logical; whether resulting table should be returned
#'
#' @return a data.frame containing the item name, position, N, percentage correct,
#'   item difficulty, SE, WMNSQ, t, rit, item discrimination, Q3.
#' @importFrom rlang .data
#' @export

irt_summary <- function(resp, vars, results, disc = NULL,
                        path = here::here("Tables"), filename = NULL,
                        digits = 2, return_table = TRUE) {

  # prepare data
  vars_ <- dplyr::select(vars[vars$items %in% rownames(results$mod$xsi), ],
                         .data$items, .data$position)
  resp_ <- resp[ , vars_$item] %>% min_val() %>% convert_mv

  # item parameters
  pars <- results$mod$xsi[, c("xsi", "se.xsi")]
  pars$item <- rownames(results$mod$xsi)
  pars <- pars[vars_$item, ]

  # item position
  pars <- merge(pars, vars_[ , c("items", "position")],
                by.x = "item", by.y = "items")

  # percentage correct
  pars$pc <- colMeans(resp_, na.rm = TRUE) * 100

  # number of valid responses
  pars$N <- colSums(!is.na(resp_))

  # items fit
  pars$WMNSQ   <- results$fit$Infit[results$fit$parameter %in% vars_$item]
  pars$WMNSQ_t <- results$fit$Infit_t[results$fit$parameter %in% vars_$item]

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
  if (!is.null(disc)) {
    pars <- pars[ , c("num", "item", "position", "N", "pc", "xsi", "se.xsi",
                     "WMNSQ", "WMNSQ_t", "rit", "disc", "Q3")]
    colnames(pars) <- c("Number", "Item", "Pos","N", "% correct",
                        "xsi", "SE", "WMNSQ", "t", "rit", "Discr.", "aQ3")
  } else {
    pars <- pars[ , c("num", "item", "position", "N", "pc", "xsi", "se.xsi",
                     "WMNSQ", "WMNSQ_t", "rit", "Q3")]
    colnames(pars) <- c("Number", "Item", "Pos","N", "% correct",
                        "xsi", "SE", "WMNSQ", "t", "rit", "aQ3")
  }
  pars[, -c(1:4)] <- format(round(pars[, -c(1:4)], digits), nsmall = digits)

  # Save table as Excel sheet
  if (!is.null(filename)) {

    # Create directory for table
    if (!file.exists(path)) {
      dir.create(path, recursive = TRUE)
    }

    # Create table
    openxlsx::write.xlsx(pars,
                         file = paste0(path, "/", filename),
                         showNA = FALSE, rowNames = FALSE, overwrite = TRUE)

  }

  if (return_table)  return(pars)
}
