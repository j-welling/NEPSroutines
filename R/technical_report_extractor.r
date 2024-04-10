#'
#' Extractor functions for NEPS Survey Papers
#'


#' Import an excel file with all sheets
#'
#' @param path A path to the folder with the result tables created by the
#' `scaling` package.
#' @param filename An optional name of an excel file to import or a vector of
#' multiple file names; if not set `regexp` should be used.
#' @param sheet An optional name of an excel sheet; if set, only the specified
#' excel sheet will be imported.
#' @param regexp An optional regular expression to identify file names in the
#' folder given by `path`; if not set, `filename` should be used.
#' @param rename An optional named vector with new names for the imported files;
#' the names of the vector need to identify the original file names.
#' @returns A list of data frames if a multiple files were imported or a single
#' data frame if a single file was imported.
#' @examples
#' \dontrun{
#' # Import all sheets from file 'mv_item.xlsx' located in the folder 'tables'
#' mvi <- Import(path = "tables", filename = "mv_item.xlsx")
#'
#' # Import sheet 'summary' from file 'mv_item.xlsx' located in the folder 'tables'
#' mvisum <- Import(path = "tables", filename = "mv_item.xlsx",
#'                  sheet = "summary")
#'
#' # Import all sheets from files with names that begin with "dif_poly_" and
#' # end with "_isRegular" located in the folder 'tables'; the file named
#' # "dif_poly_TR_isRegular" is renamed to "TR" in the returned data.frame.
#' mvi <- Import(path = "tables",
#'               regexp = "dif_poly_(.+)_isRegular"),
#'               rename = c("dif_poly_TR_isRegular" = "TR"))
#' }
#' @export
Import <- function(path, filename = NULL, sheet = NULL, regexp = NULL,
                   rename = NULL) {

  # Normalize arguments
  path <- path[1]
  sheet <- sheet[1]
  regexp <- regexp[1]
  if (all(is.null(filename)) & is.null(regexp))
    stop("Please provide argument 'filename' or 'regexp'.")
  if (any(!is.null(filename)) & !is.null(regexp))
    warning("The argument 'filename' was ignored because 'regexp' was set.")

  # Normalize path
  if (!(substr(path, base::nchar(path), base::nchar(path)) %in% c("/", "\\")))
    path <- paste0(path, "/")

  # Determine file names
  if (!is.null(regexp)) {
    filename <- base::list.files(path = path, pattern = regexp)
    filename <- filename[!(grepl("^[~\\.]", filename))] # remove temp files
  }

  # Load all files
  out <- list()
  for (f in filename) {
    if (is.null(sheet)) {
      sheets <- openxlsx::getSheetNames(paste0(path, f))
      tbl <-
        sapply(
          sheets,
          \(sn) { openxlsx::read.xlsx(xlsxFile = paste0(path, f), sheet = sn) }
        )
    } else {
      tbl <-
        openxlsx::read.xlsx(
          xlsxFile = paste0(path, f),
          colNames = TRUE,
          rowNames = FALSE,
          sheet = sheet
        )
    }
    if (length(tbl) == 1) tbl <- tbl[[1]]
    out[[sub("([^.]+)\\.[[:alnum:]]+$", "\\1", base::basename(f))]] <- tbl
  }

  # Rename list elements
  if (!is.null(rename)) {
    lbl <- names(out)
    for (i in seq_along(rename)) {
      lbl[names(rename)[i] == lbl] <- rename[i]
    }
    names(out) <- lbl
  }

  if (length(out) == 1) out <- out[[1]]
  return(out)

}


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
#' by [scaling::mv_item()].
#' @param type The type of missing values identifying the row in `obj` which is
#' typcially one of OM, NR, NV, ND, or ALL.
#' @param stat The calculated statistic identifying the column in `obj` which is
#' typically one of Mean, SD, Median, Min, or Max.
#' @param digits A number for rounding.
#' @returns A number with the calculated statistic.
#' @export
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
#' created by [scaling::mv_person()].
#' @param type The type of missing value which is typically one of
#' OM, NR, NV, ND, or ALL.
#' @param value An operator (=, <, >) with a number of missing values; e.g.,
#' * =3 returns percentage for 3 missing values
#' * <3 returns percentage for less than 3 missing values
#' * >3 returns percentage for more than 3 missing values
#' @param digits A number for rounding.
#' @returns The calculated percentage.
#' @export
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
#' created by [scaling::irt_analysis()].
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
#' "irt_dich.rds" or "irt_poly.rds" generated by[scaling::irt_analysis()].
#' @param stat A function used to summarize the category thresholds.
#' @param item A logical indicating whether to return the results from `stat`
#' (`FALSE`) or the item name corresponding to the value of `stat` (`TRUE`).
#' @param digits A number for rounding.
#' @returns The calculated statistic or a vector of item names.
#' @export
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
#' "irt_dich.rds" or "irt_poly.rds" generated by [scaling::irt_analysis()].
#' @param digits A number for rounding.
#' @returns The population variance.
#' @export
GetVar <- function(obj, digits = 2) {

  digits <- as.integer(digits[1])
  mod <-  obj$model.pcm$mod
  if(is.null(mod)) mod <- obj$model.1pl$mod
  return(rnd(mod$variance[1], digits = digits))

}


#' Get reliability
#'
#' @param obj A [TAM__taml.mml()] object with a fitted IRT model from
#' "irt_dich.rds" or "irt_poly.rds" generated by [scaling::irt_analysis()].
#' @param WLE A logical to return the WLE reliability (`TRUE`) instead of the
#' EAP reliability (`FALSE`).
#' @param digits A number for rounding.
#' @returns The reliability.
#' @export
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
#' created by [scaling::dis_analysis()].
#' @param stat A function used to summarize the distractor correlations.
#' @param correct  A logical indicating whether to return the results for
#' distractors (`FALSE`) or correct response options (`TRUE`).
#' @param item A logical indicating whether to return the results from `stat`
#' (`FALSE`) or the item name corresponding to the value of `stat` (`TRUE`).
#' @param digits A number for rounding.
#' @returns The calculated statistic or a vector of item names.
#' @export
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
#' created by [scaling::irt_analysis()].
#' @param type A column name in `obj` identifying the model fit statistic.
#' @param GPCM A logical to return the GPCM fit (`TRUE`) instead of the PCM fit
#' (`FALSE`).
#' @param digits A number for rounding.
#' @returns The model fit statistic.
#' @export
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
#' created by [scaling::dim_analysis()].
#' @param model A column name identifying the model in `obj` which is typically
#' one of uni or dim.
#' @param type A row name identifying the model fit statistic in `obj` which
#' is typically one of Npars, loglik, AIC, or BIC.
#' @returns The model fit statistic.
#' @export
GetDimFit <- function(obj, model, type) {

  model <- as.character(model[1])
  type <- as.character(type[1])
  tab <- as.data.frame(obj[["Goodness of fit"]])
  return(rnd(tab[tab$Stat == type, model], digits = 0))

}


#' Summarize the latent correlations in multi-dimensional models
#'
#' @param obj A data frame with sheets from "dimensionality.xlsx"
#' created by [scaling::dim_analysis()].
#' @param model A model name which is typically one of uni or dim.
#' @param stat A function used to summarize the correlations.
#' @param var A logical to summarize the variances (`TRUE`) instead of the
#' correlations (`FALSE`).
#' @param digits A number for rounding.
#' @returns A numeric or character vector
#' @export
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
#' "dif_poly_XXX.xlsx" created by [scaling::dif_analysis()].
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
#' @param obj A data frame with sheets from "dif_dich.xlsx" or "dif_poly.xlsx"
#' created by [scaling::dif_analysis()].
#' @param type The model fit statistic corresponding to the column name in `obj`
#' @param dif A logical to return the fit of the DIF model (`TRUE`) instead of
#' the main effect model (`FALSE`).
#' @returns The model fit statistic.
#' @export
GetDifFit <- function(obj, type, dif = TRUE) {

  type <- as.character(type[1])
  dif <- as.logical(dif[1])
  tab <- as.data.frame(obj$gof)
  return(rnd(tab[ifelse(dif, 2, 1), type], digits = 0))

}

#' @rdname GetDifFit
GetDIFFit <- GetDifFit

