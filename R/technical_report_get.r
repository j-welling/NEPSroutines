#'
#' Extractor functions for NEPS Survey Papers
#'


#' Determine the number of items with a given item property
#'
#' @param obj A data frame with item characteristics.
#' @param prop A variable name in `vars` identifying the item property such
#' as response format, text function, or cognitive function (default: `type`).
#' @param val The values of `prop` to count.
#' @param item A logical to return the item names (`TRUE`) or the numeric
#' values (`FALSE`).
#' @returns The number of items or the item names.
#' @inheritParams irt_analysis
#' @export
#' @examples
#' data("ex2")
#'
#' # Number of CMC items
#' GetProp(ex2$vars, select = "mixed", prop = "type", val = "CMC")
#'
#' # Names of CMC items
#' GetProp(ex2$vars, select = "mixed", prop = "type", val = "CMC",
#'                  item = TRUE)
GetProp <- function(obj, select, prop = "type", val = c("CMC", "MA"),
                    item = FALSE) {

  out <- obj$item[obj[[select]] & obj[[prop]] %in% val]
  if (item) {
    return(out)
  } else {
    return (length(out))
  }

}


#' Calculate statistics for missing values by item
#'
#' @param obj A list with data frames with sheets from "mv_item.xlsx"
#' or a single data frame with the sheet "summary" from "mv_item.xlsx" created
#' by [mv_item()].
#' @param type The type of missing values identifying the row in `obj` which is
#' typcially one of OM, NR, NV, ND, or ALL.
#' @param stat The calculated statistic identifying the column in `obj` which is
#' typically one of Mean, SD, Median, Min, or Max.
#' @param digits A number for rounding.
#' @returns A number with the calculated statistic.
#' @export
#' @examples
#' data("ex1")
#'
#' # Missing analyses by item
#' tmpdir <- tempdir()
#' mv_item(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  pos = "pos",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of missing analyses
#' mvi <- Import(tmpdir, "/mv_item.xlsx")
#'
#' # Largest percentage of invalid responses
#' GetMvi(mvi, "NV", "Max")
#'
#' # Median percentage of missing values with two decimals
#' GetMvi(mvi, "ALL", "Median", digits = 2)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/mv_item.xlsx"))
#' file.remove(paste0(tmpdir, "/mv_item.rds"))
GetMvi <- function(obj, type, stat, digits = 0) {

  if (is.list(obj) & "summary" %in% names(obj))
    obj <- obj[["summary"]]
  tab <- as.data.frame(obj)
  type <- as.character(type[1])
  stat <- as.character(stat[1])
  digits <- as.integer(digits[1])
  return(rnd(tab[tab[, 1] == type, stat], digits = digits))

}

#' @rdname GetMvi
GetMVI <- GetMvi


#' Calculate percentage of missing values by person
#'
#' @param obj A list with data frames with sheets from "mv_person.xlsx"
#' created by [mv_person()].
#' @param type The type of missing value which is typically one of
#' OM, NR, NV, ND, or ALL.
#' @param value An operator (=, <, >) with a number of missing values; e.g.,
#' * =3 returns percentage for 3 missing values
#' * <3 returns percentage for less than 3 missing values
#' * >3 returns percentage for more than 3 missing values
#' @param digits A number for rounding.
#' @returns The calculated percentage.
#' @export
#' @examples
#' data("ex1")
#'
#' # Missing analyses by person
#' tmpdir <- tempdir()
#' mv_person(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of missing analyses
#' mvp <- Import(tmpdir, "/mv_person.xlsx")
#'
#' # Percentage of respondents with 0 invalid responses
#' GetMvp(mvp, "NV", "=0")
#'
#' # Percentage of respondents with more than 5 missing responses with one decimal
#' GetMvp(mvp, "ALL", ">5", digits = 1)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/mv_person.xlsx"))
#' file.remove(paste0(tmpdir, "/mv_person.rds"))
GetMvp <- function(obj, type, value, digits = 0) {

  type <- as.character(type[1])
  value <- as.character(value[1])
  operator <- "="
  if (substr(value, 1, 1) %in% c("=", "<", ">")) {
    operator = substr(value, 1, 1)
    value <- as.numeric(substring(value, 2))
  }
  if (operator == "=") operator <- paste0(operator, "=")
  n <- as.numeric(obj[[type]]$`Number.of.missing.responses`)
  out <- sum(obj[[type]]$Percentage[methods::getFunction(operator)(n, value)])
  return(rnd(out, digits = digits))

}

#' @rdname GetMvp
GetMVP <- GetMvp


#' Summarize item parameters
#'
#' @param obj A data frame with sheets from "irt_dich.xlsx" or "irt_poly.xlsx"
#' created by [irt_analysis()].
#' @param type A column name in `obj` identifying an item statistic.
#' @param stat A function used to summarize `type` or a string specifying
#' an operator (=, <, >) with a number; e.g.,
#' * =3 returns number of type equal to 3
#' * <3 returns number of type less than 3
#' * >3 returns number of type greater than 3
#'  Multiple conditions can be combined using & and |.
#' @param item A logical indicating whether to return the results from stat
#' (`FALSE`) or the item name corresponding to the value of `stat` (`TRUE`).
#' @param excl A string specifying an operator (=, <, >) with a number
#' indicating values to exclude; e.g.,
#' * =3 excludes number of type equal to 3
#' * <3 excludes number of type less than 3
#' * >3 excludes number of type greater than 3
#' Multiple conditions can be combined using & and |.
#' @param digits A number for rounding.
#' @returns A number with the calculated statistic or character vector with
#' item names.
#' @export
#' @examples
#' data("ex1")
#'
#' # IRT analyses
#' tmpdir <- tempdir()
#' irt_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of IRT analyses
#' pars <- Import(tmpdir, "/irt_dich.xlsx")
#'
#' # Smallest percentage of correct responses
#' GetPars(pars, "correct", min)
#'
#' # Most difficult item
#' GetPars(pars, "xsi", max, item = TRUE)
#'
#' # Items with a difficulty smaller than -2
#' GetPars(pars, "xsi", "<-2", item = TRUE)
#'
#' # Largest WMNSQ between 1 and 1.2
#' GetPars(pars, "WMNSQ", max, excl = "<1|>1.2")

#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/irt_dich.xlsx"))
#' file.remove(paste0(tmpdir, "/irt_dich.rds"))
GetPars <- function(obj, type, stat = median, item = FALSE,
                    excl = NULL, digits = 2) {

  # normalize arguments
  type <- as.character(type[1])
  item <- as.logical(item[1])
  digits <- as.integer(digits[1])

  # select data
  tab <- as.data.frame(obj[["summary"]])
  if (!is.null(excl)) {

    # identify multiple conditions
    logicals <- base::strsplit(gsub("[^&|]", "", excl), "")[[1]]
    excl <- base::strsplit(base::trimws(excl), "[&|]")[[1]]

    # identify operator and value for each condition
    operators <- values <- c()
    for (i in excl) {
      if (!(substr(base::trimws(i), 1, 1) %in% c("=", "<", ">")))
        stop("Unknown stat function.")
      operators[i] = substr(i, 1, 1)
      if (operators[i] == "=") operators[i] <- paste0(operators[i], "=")
      values[i] <- as.numeric(base::trimws(substring(i, 2)))
    }

    # combine conditions to select data
    for (i in seq_along(operators)) {
      if (i == 1) {
        boolvec <- methods::getFunction(operators[i])(tab[, type], values[i])
      } else {
        expr <-
          call(
            logicals[i - 1],
            boolvec,
            methods::getFunction(operators[i])(tab[, type], values[i])
          )
        boolvec <- eval(expr)
      }
    }
    tab <- tab[!boolvec, ]
    rm(logicals, operators, values, boolvec)

  }

  # summary function available
  if (is.function(stat)) {

    if (!item) {
      return(rnd(stat(tab[, type], na.rm = TRUE), digits = digits))
    } else {
      i <- tab[tab[, type] == stat(tab[, type], na.rm = TRUE), "Item"]
      return(paste0(i, collapse = ", "))
    }

    # no summary function available
  } else {

    # identify multiple conditions
    logicals <- base::strsplit(gsub("[^&|]", "", stat), "")[[1]]
    stat <- base::strsplit(base::trimws(stat), "[&|]")[[1]]

    # identify operator and value for each condition
    operators <- values <- c()
    for (i in stat) {
      if (!(substr(base::trimws(i), 1, 1) %in% c("=", "<", ">")))
        stop("Unknown stat function.")
      operators[i] = substr(i, 1, 1)
      if (operators[i] == "=") operators[i] <- paste0(operators[i], "=")
      values[i] <- as.numeric(base::trimws(substring(i, 2)))
    }

    # combine conditions in new summary function
    stat <- \(x, na.rm, item = FALSE) {
      for (i in seq_along(operators)) {
        if (i == 1) {
          boolvec <- methods::getFunction(operators[i])(x, values[i])
        } else {
          expr <-
            call(
              logicals[i - 1],
              boolvec,
              methods::getFunction(operators[i])(x, values[i])
            )
          boolvec<- eval(expr)
        }
      }
      if (item) {
        return(boolvec)
      } else {
        return(sum(boolvec, na.rm = na.rm))
      }
    }

    # because frequencies are returned, there are no digits
    digits <- 0

    # Apply summary function
    if (!item) {
      return(rnd(stat(tab[, type], na.rm = TRUE), digits = digits))
    } else {
      i <- tab[stat(tab[, type], na.rm = TRUE, item = TRUE), "Item"]
      return(paste0(i, collapse = ", "))
    }

  }

}


#' Summarize category thresholds
#'
#' @param obj A [TAM::tam.mml()] object with a fitted IRT model from
#' "irt_dich.rds" or "irt_poly.rds" generated by[irt_analysis()].
#' @param stat A function used to summarize the category thresholds.
#' @param item A logical indicating whether to return the results from `stat`
#' (`FALSE`) or the item name corresponding to the value of `stat` (`TRUE`).
#' @param digits A number for rounding.
#' @returns The calculated statistic or a vector of item names.
#' @export
#' @examples
#' data("ex2")
#'
#' # IRT analyses
#' tmpdir <- tempdir()
#' irt_analysis(
#'  resp = ex2$resp,
#'  vars = ex2$vars,
#'  select = "mixed",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of IRT analyses
#' irt <- readRDS(paste0(tmpdir, "/irt_poly.rds"))
#'
#' # Largest category threshold
#' GetCat(irt, max)
#'
#' # Item with the largest category threshold
#' GetCat(irt, max, item = TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/irt_poly.xlsx"))
#' file.remove(paste0(tmpdir, "/irt_poly.rds"))
GetCat <- function(obj, stat = median, item = FALSE, digits = 2) {

  # Normalize arguments
  item <- as.logical(item[1])
  digits <- as.integer(digits[1])

  # Category thresholds
  mod <-  obj$model.pcm$mod
  if(is.null(mod)) mod <- obj$model.1pl$mod
  cat <- TAM::IRT.threshold(mod)

  if (!item) {
    return(rnd(stat(na.omit(c(cat))), digits = digits))
  } else {
    i <- rownames(cat)[rowSums(cat == stat(na.omit(c(cat))), na.rm = TRUE) >= 1]
    return(paste0(i, collapse = ", "))
  }

}


#' Get variance of latent proficiency distribution
#'
#' @param obj A [TAM::tam.mml()] object with a fitted IRT model in
#' "irt_dich.rds" or "irt_poly.rds" generated by [irt_analysis()].
#' @param digits A number for rounding.
#' @returns The population variance.
#' @export
#' @examples
#' data("ex1")
#'
#' # IRT analyses
#' tmpdir <- tempdir()
#' irt_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of IRT analyses
#' irt <- readRDS(paste0(tmpdir, "/irt_dich.rds"))
#'
#' # Population variance
#' GetVar(irt)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/irt_dich.xlsx"))
#' file.remove(paste0(tmpdir, "/irt_dich.rds"))
GetVar <- function(obj, digits = 2) {

  digits <- as.integer(digits[1])
  mod <-  obj$model.pcm$mod
  if(is.null(mod)) mod <- obj$model.1pl$mod
  return(rnd(mod$variance[1], digits = digits))

}


#' Get reliability
#'
#' @param obj A [TAM__taml.mml()] object with a fitted IRT model from
#' "irt_dich.rds" or "irt_poly.rds" generated by [irt_analysis()].
#' @param WLE A logical to return the WLE reliability (`TRUE`) instead of the
#' EAP reliability (`FALSE`).
#' @param digits A number for rounding.
#' @returns The reliability.
#' @export
#' @examples
#' data("ex1")
#'
#' # IRT analyses
#' tmpdir <- tempdir()
#' irt_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of IRT analyses
#' irt <- readRDS(paste0(tmpdir, "/irt_dich.rds"))
#'
#' # EAP reliability
#' GetRel(irt)
#'
#' # WLE reliability
#' GetRel(irt, TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/irt_dich.xlsx"))
#' file.remove(paste0(tmpdir, "/irt_dich.rds"))
GetRel <- function(obj, WLE = FALSE, digits = 2) {

  digits <- as.integer(digits[1])
  WLE <- as.logical(WLE[1])
  mod <-  obj$model.pcm
  if(is.null(mod)) mod <- obj$model.1pl
  if (WLE) {
    return(rnd(mod$wle_rel, digits = digits))
  } else {
    return(rnd(mod$mod$EAP.rel, digits = digits))
  }

}


#' Summarize distractor analyses
#'
#' @param obj A data frame with sheets from "distractors_summary.xlsx"
#' created by [dis_analysis()].
#' @param stat A function used to summarize the distractor correlations.
#' @param correct  A logical indicating whether to return the results for
#' distractors (`FALSE`) or correct response options (`TRUE`).
#' @param item A logical indicating whether to return the results from `stat`
#' (`FALSE`) or the item name corresponding to the value of `stat` (`TRUE`).
#' @param digits A number for rounding.
#' @returns The calculated statistic or a vector of item names.
#' @export
#' @examples
#' data("ex2")
#'
#' # Distractor analyses
#' tmpdir <- tempdir()
#' dis_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select_raw = "raw",
#'  correct = "correct",
#'  valid = "valid",
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE
#' )
#'
#' # Import results of distractor analyses
#' dist <- Import(tmpdir, "distractors_summary.xlsx")
#'
#' # Median total-rest correlations for the distractors
#' GetDist(dist, median)
#'
#' # Smallest total-rest correlations for the correct responses
#' GetDist(dist, min, correct = TRUE)
#'
#' # Item with the largest total-rest correlations for the distractors
#' GetDist(dist, max, item = TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/distractors_summary.xlsx"))
#' file.remove(paste0(tmpdir, "/distractors_items.xlsx"))
#' file.remove(paste0(tmpdir, "/distractors.rds"))
GetDist <- function(obj, stat = median, correct = FALSE, item = FALSE,
                    digits = 2) {

  item <- as.logical(item[1])
  correct <- as.logical(correct[1])
  digits <- as.integer(digits[1])
  if (correct) {
    tab <- as.data.frame(obj$correct)
  } else {
    tab <- as.data.frame(obj$distractor)
  }
  if (!item) {
    return(rnd(stat(tab$corr, na.rm = TRUE), digits = digits))
  } else {
    i <- tab[tab$corr == stat(tab$corr, na.rm = TRUE), 1]
    i <- paste0(substring(i, 1, nchar(i) - 1), "_c")
    return(paste0(i, collapse = ", "))
  }

}


#' Get model fit
#'
#' @param obj A data frame with sheets from "irt_dich.xlsx" or "irt_poly.xlsx"
#' created by [irt_analysis()].
#' @param type A column name in `obj` identifying the model fit statistic.
#' @param GPCM A logical to return the GPCM fit (`TRUE`) instead of the PCM fit
#' (`FALSE`).
#' @param digits A number for rounding.
#' @returns The model fit statistic.
#' @export
#' @examples
#' data("ex1")
#'
#' # IRT analyses
#' tmpdir <- tempdir()
#' irt_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  plots = FALSE,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  warn = FALSE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of IRT analyses
#' pars <- Import(tmpdir, "irt_dich.xlsx")
#'
#' # BIC for the Rasch model
#' GetFit(pars, "BIC")
#'
#' # Number of parameters in the 2PL
#' GetFit(pars, "Npars", GPCM = TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/irt_dich.xlsx"))
#' file.remove(paste0(tmpdir, "/ir.rds"))
GetFit <- function(obj, type, GPCM = FALSE, digits = 2) {

  type <- as.character(type[1])
  GPCM <- as.logical(GPCM[1])
  digits <- as.integer(digits[1])
  tab <- as.data.frame(obj[["model_fit"]])
  row <- ifelse(GPCM, 2, 1)
  if (type %in% c("N", "Npars", "Deviance", "AIC", "BIC"))
    digits <- 0
  return(rnd(tab[row, type], digits = digits))

}


#' Get model fit for dimensionality analyses
#'
#' @param obj A data frame with sheets from "dimensionality.xlsx"
#' created by [dim_analysis()].
#' @param model A column name identifying the model in `obj` which is typically
#' one of uni or dim.
#' @param type A row name identifying the model fit statistic in `obj` which
#' is typically one of Npars, loglik, AIC, or BIC.
#' @returns The model fit statistic.
#' @export
#' @examples
#' \dontrun{
#' data("ex2")
#'
#' # Dimensionality analyses
#' tmpdir <- tempdir()
#' ex2$vars$content <- as.numeric(ex2$vars$content)
#' dim_analysis(
#'  resp = ex2$resp,
#'  vars = ex2$vars,
#'  select = "mixed",
#'  valid = "valid",
#'  dim = "content",
#'  snodes = 5,# only for speed; model will not converge!
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of dimensionality analyses
#' dim <- Import(tmpdir, "/dimensionality.xlsx")
#'
#' # AIC for multidimensonal model for variable content
#' GetDimFit(dim, "content", "AIC")
#'
#' # BIC for unidimensional model
#' GetDimFit(dim, "uni", "BIC")
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/dimensionality.xlsx"))
#' file.remove(paste0(tmpdir, "/dimensionality.rds"))
#' }
GetDimFit <- function(obj, model, type) {

  model <- as.character(model[1])
  type <- as.character(type[1])
  tab <- as.data.frame(obj[["Goodness of fit"]])
  return(rnd(tab[tab$Stat == type, model], digits = 0))

}


#' Summarize the latent correlations in multi-dimensional models
#'
#' @param obj A data frame with sheets from "dimensionality.xlsx"
#' created by [dim_analysis()].
#' @param model A model name which is typically one of uni or dim.
#' @param stat A function used to summarize the correlations.
#' @param var A logical to summarize the variances (`TRUE`) instead of the
#' correlations (`FALSE`).
#' @param digits A number for rounding.
#' @returns A numeric or character vector
#' @export
#' @examples
#' \dontrun{
#' data("ex2")
#'
#' # Dimensionality analyses
#' tmpdir <- tempdir()
#' ex2$vars$content <- as.numeric(ex2$vars$content)
#' dim_analysis(
#'  resp = ex2$resp,
#'  vars = ex2$vars,
#'  select = "mixed",
#'  valid = "valid",
#'  dim = "content",
#'  snodes = 5,# only for speed; model will not converge!
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  verbose = FALSE,
#' )
#'
#' # Import results of dimensionality analyses
#' dim <- Import(tmpdir, "/dimensionality.xlsx")
#'
#' # Median correlation between factors for variable content
#' #   Note: not meaningful here because the model did not converge
#' GetDim(dim, "content", median)
#'
#' # Largest variance of the dimensions for variable content
#' #   Note: not meaningful here because the model did not converge
#' GetDim(dim, "content", max, var = TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/dimensionality.xlsx"))
#' file.remove(paste0(tmpdir, "/dimensionality.rds"))
#' }
GetDim <- function(obj, model = "dim", stat = median, var = FALSE,
                   digits = 2) {

  tab <- obj[[paste0("Cor-Var ", model)]]
  tab <- as.matrix(tab[, -1])

  if (var) {
    values <- diag(tab)
    drop0leading <- FALSE
  } else {
    values <- tab[lower.tri(tab)]
    drop0leading <- TRUE
  }
  return(rnd(stat(values, na.rm = TRUE), digits = digits, d0 = drop0leading))

}


#' Get values from DIF analyses
#'
#' @param obj A data frame with sheets from "dif_dich_XXX.xlsx" or
#' "dif_poly_XXX.xlsx" created by [dif_analysis()].
#' @param n A group name to return the sample size of this group
#' (corresponds to the values in column "number" of sheet "facet").
#' @param main Return standardized main effects (`std`) or unstandardized
#' main effects (`ustd`). If the DIF variable has more than two groups
#' and `group` is missing, results for the first group comparison are returned.
#' @param group If the DIF variable has more than two groups, the name of
#' the group comparison.
#' @param dif A function used to summarize the DIF effects or a string
#' specifying an operator (=, <, >) with a number; e.g.,
#' * =1 returns number of DIF effects equal to 1
#' * <1 returns number of DIF effects less than 1
#' * >1 returns number of DIF effects greater than 1
#' Multiple conditions can be combined using & and |.
#' If the DIF variable has more than two group and `group` is missing, results
#' across all groups are returned.
#' @param item A logical to return the item names (`TRUE`) instead of the
#' calculated statistic for the DIF effects (`FALSE`).
#' @param model The name of the model for which the main effects should be
#' returned, one of `dif` or `main`.
#' @param signed A logical to use the unsigned DIF effects (`FALSE`) instead of
#' the signed DIF effects (`TRUE`).
#' @param digits A number for rounding.
#' @returns The calculated DIF statistics or a vector of item names.
#' @export
#' @examples
#' data("ex1")
#'
#' # DIF analyses
#' tmpdir <- tempdir()
#' dif_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  dif_vars = c("sex", "mig"),
#'  include_mv = 100,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  verbose = FALSE,
#'  warn = FALSE
#' )
#'
#' # Import results of DIF analyses
#' dif <- Import(tmpdir, regexp = "^dif_dich_([^_]+\\.xlsx)")
#'
#' # Sample size for men (coded as 0)
#' GetDif(dif$sex, n = 0)
#'
#' # Standardized main effect in the DIF model for sex
#' GetDif(dif$sex, main = "std", model = "dif")
#'
#' # Unstandardized main effect in the main effect model for mig
#' # for the comparison of migrants (coded 1) and the missing group (coded 3)
#' GetDif(dif$mig, main = "ustd", group = "1-3", model = "main")
#'
#' # Median of the unsigned (absolute) DIF effects for sex
#' GetDif(dif$sex, dif = median)
#'
#' # Largest signed DIF effects for mig for the comparision of migrants
#' # (coded 1) and non-migrants (coded 2)
#' GetDif(dif$mig, dif = max, signed = TRUE, group = "1-2")
#'
#' # Number of unsigned (absolute) DIF effects for mig greater than .4
#' # across all groups
#' GetDif(dif$mig, dif = ">.4")
#'
#' # Items with signed DIF effects for mig greater than .4
#' # across all groups
#' GetDif(dif$mig, dif = ">.4", signed = TRUE, item = TRUE)
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/dif_dich_sex.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_mig.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_TR.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_models.rds"))
#' file.remove(paste0(tmpdir, "/dif_dich_summaries.rds"))
GetDif <- function(obj, n = NULL, main = NULL, dif = NULL,
                   item = FALSE, group = NULL, model = "dif",
                   signed = FALSE, digits = 2) {

  # sample size
  if (!is.null(n)) {

    return(rnd(obj$facets$counts[obj$facets$number == n], digits = 0))

    # main effects for dif model
  } else if(!is.null(main)) {

    if (!(main %in% c("std", "ustd")))
      stop("Allowed values for argument main are 'std' and 'ustd'.")

    if (is.null(group)) {
      sheet <- names(obj)[grepl("^Main effect", names(obj))][1]
    } else {
      sheet <- paste0("Main effect ", group[1])
    }
    model <- ifelse(model == "dif", 1, 2)
    if (main == "std") {
      return(rnd(obj[[sheet]]$Standardized[model], digits = digits))
    } else {
      return(rnd(obj[[sheet]]$Unstandardized[model], digits = digits))
    }

    # DIF effects with summary function
  } else if (!is.null(dif) & is.function(dif)) {

    if (is.null(group)) {
      sheets <- names(obj)[grepl("^Estimates", names(obj))]
    } else {
      l <- grepl(paste0("^Estimates (", paste0(group, collapse = "|"), ")"),
                 names(obj))
      sheets <- names(obj)[l]
    }
    tab <- c()
    for (i in sheets) {
      tab <- rbind(tab, obj[[i]][, c("item", "xsi")])
    }
    if (!signed) tab$xsi <- abs(tab$xsi)
    if (!item) {
      return(rnd(dif(tab$xsi, na.rm = TRUE), digits = digits))
    } else {
      i <- tab[tab$xsi == dif(tab$xsi, na.rm = TRUE), "item"]
      return(paste0(unique(i), collapse = ", "))
    }

    # DIF effects without summary function
  } else if (!is.null(dif)) {

    # identify multiple conditions
    logicals <- strsplit(gsub("[^&|]", "", dif), "")[[1]]
    stat <- strsplit(trimws(dif), "[&|]")[[1]]

    # identify operator and value for each condition
    operators <- values <- c()
    for (i in stat) {
      if (!(substr(trimws(i), 1, 1) %in% c("=", "<", ">")))
        stop("Unknown stat function.")
      operators[i] = substr(i, 1, 1)
      if (operators[i] == "=") operators[i] <- paste0(operators[i], "=")
      values[i] <- as.numeric(trimws(substring(i, 2)))
    }

    # combine conditions in new summary function
    stat <- \(x, na.rm, item = FALSE) {
      for (i in seq_along(operators)) {
        if (i == 1) {
          boolvec <- methods::getFunction(operators[i])(x, values[i])
        } else {
          expr <-
            call(
              logicals[i - 1],
              boolvec,
              methods::getFunction(operators[i])(x, values[i])
            )
          boolvec<- eval(expr)
        }
      }
      if (item) {
        return(boolvec)
      } else {
        return(sum(boolvec, na.rm = na.rm))
      }
    }

    # because frequencies are returned, there are no digits
    digits <- 0

    # Apply summary function
    if (is.null(group)) {
      sheets <- names(obj)[grepl("^Estimates", names(obj))]
    } else {
      l <- grepl(paste0("^Estimates (", paste0(group, collapse = "|"), ")"),
                 names(obj))
      sheets <- names(obj)[l]
    }
    tab <- c()
    for (i in sheets) {
        tab <- rbind(tab, obj[[i]][, c("item", "xsi")])
    }
    if (!signed) tab$xsi <- abs(tab$xsi)
    if (!item) {
      return(rnd(stat(tab$xsi, na.rm = TRUE), digits))
    } else {
      i <- tab[stat(tab$xsi, na.rm = TRUE, item = TRUE), "item"]
      return(paste0(unique(i), collapse = ", "))
    }

  }

}

#' @rdname GetDif
GetDIF <- GetDif


#' Get model fit for DIF analyses
#'
#' @param obj A data frame with sheets from "dif_dich_TR.xlsx" or
#' "dif_poly_TR.xlsx" created by [dif_analysis()].
#' @param difvar The DIF variable.
#' @param type The model fit statistic corresponding to the column name in `obj`
#' @param model The name of the model for which the main effects should be
#' returned, one of `dif` or `main`.
#' @returns The model fit statistic.
#' @export
#' @examples
#' data("ex1")
#'
#' # DIF analyses
#' tmpdir <- tempdir()
#' dif_analysis(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  dif_vars = c("sex", "mig"),
#'  include_mv = 100,
#'  print = FALSE,
#'  save = TRUE,
#'  return = FALSE,
#'  path_results = tmpdir,
#'  path_table = tmpdir,
#'  overwrite = TRUE,
#'  verbose = FALSE,
#'  warn = FALSE
#' )
#'
#' # Import results of DIF analyses
#' dif <- Import(tmpdir, "dif_dich_TR.xlsx")
#'
#' # AIC for the variable sex in the DIF model
#' GetDifFit(dif, "sex", "AIC")
#'
#' # BIC for the variable mig in the main effect model
#' GetDifFit(dif, "mig", "BIC", model = "main")
#'
#' # Clean up generated files
#' file.remove(paste0(tmpdir, "/dif_dich_sex.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_mig.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_TR.xlsx"))
#' file.remove(paste0(tmpdir, "/dif_dich_models.rds"))
#' file.remove(paste0(tmpdir, "/dif_dich_summaries.rds"))
GetDifFit <- function(obj, difvar, type, model = "dif") {

  difvar <- as.character(difvar[1])
  type <- as.character(type[1])
  model <- ifelse(model[1] == "dif", "DIF", "Main effect")
  tab <- as.data.frame(obj$gof)
  out <- tab[tab$`DIF.variable` == difvar & tab$Model == model, type]
  return(rnd(out, digits = 0))

}

#' @rdname GetDifFit
GetDIFFit <- GetDifFit

