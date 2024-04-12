#'
#' Generate tables in NEPS Survey Papers
#'


#' Create table
#'
#' @param obj A data frame with the table content.
#' @param footnote A table note.
#' @param autofit A logical to automatically determine columns widths (`TRUE`);
#' will be set to `FALSE`, if `width` is given.
#' @param merge A logical to merge header cells with the same content.
#' @param lbl A vector of labels for the header; the length of the vector
#' must correspond to the number of columns in `obj`.
#' @param hline A (named) vector of line numbers in the body for which a bottom
#' border is added; names of each element can be used to set the border color.
#' @param hline_head A (named) vector of line numbers in the header for which a
#' bottom border is added; names of each element can be used to set the border
#' color.
#' @param size The general font size.
#' @param size_head The font size of the header.
#' @param size_foot The font size of the footer.
#' @param width The widths of the columns; if a single value is provided, it
#' will be applied to all columns; otherwise the length must correspond to the
#' number of columns in `obj`.
#' @param digits A number for rounding; if a single number is provided, it will
#' be applied to all columns; otherwise the length must correspond to the
#' number of columns in `obj`.
#' @param align The alignment for each column in the body; if a single value is
#' provide, it will be applied to all columns; otherwise the length must
#' correspond to the number of columns in `obj`.
#' @param align_head the alignment for each column in the header; if a single
#' value is provided, it will be applied to all columns; otherwise the length
#' must correspond to the number of columns in `obj`.
#' @returns A flextable.
#' @export
Tbl <- function(obj, footnote = NULL, autofit = TRUE, merge = TRUE, lbl = NULL,
                hline = NULL, hline_head = NULL, size = 12, size_head = NULL,
                size_foot = 10, width = NULL, digits = 2, align = "center",
                align_head = "center") {

  if(!requireNamespace("flextable", quietly = TRUE)) {
    stop("Please install flextable!")
  }
  if(!requireNamespace("officer", quietly = TRUE)) {
    stop("Please install officer!")
  }

  # Create flextable
  ft <- flextable::flextable(obj)

  # Set table properties
  ft <- flextable::fontsize(ft, size = size, part = "all")
  ft <- flextable::set_table_properties(ft, align = "left")
  if (length(digits) == 1) digits <- rep(digits, ncol(obj))
  for (j in seq_along(digits)) {
    ft <- flextable::colformat_double(ft, j = j, digits = digits[j])
  }

  # Set column widths
  if (!any(is.null(width))) {
    if(length(width) == 1) width <- rep(width, ncol(obj))
    for (j in seq(1, ncol(obj)))
      ft <- flextable::width(ft, j = j, width = width[j])
  } else if (autofit) {
    ft <- flextable::autofit(ft)
  }

  # Header
  if (!is.null(lbl))
    ft <- flextable::set_header_labels(ft, values = lbl)
  ft <- flextable::bold(ft, i = 1, part = "header")
  if (!is.null(size_head))
    ft <- flextable::fontsize(ft, size = size_head, part = "header")
  if (!any(is.null(hline_head))) {
    for (i in seq_along(hline_head)) {
      col <- ifelse(!is.null(names(hline_head)[i]),
                    names(hline_head)[i], "black")
      ft <- flextable::hline(
        ft, i = hline_head, part = "header",
        border = officer::fp_border(color = col, width = 0.5)
      )
    }
  }
  if (length(align_head) == 1) align_head <- rep(align_head, ncol(obj))
  for (j in seq(1, ncol(obj)))
    ft <- flextable::align(ft, j = j, align = align_head[j], part = "header")
  if (merge)
    ft <- flextable::merge_h(ft, i = 1, part = "header")

  # Body
  if (!any(is.null(hline))) {
    for (i in seq_along(hline)) {
      col <- ifelse(!is.null(names(hline)[i]), names(hline)[i], "black")
      ft <- flextable::hline(
        ft, i = hline[i], part = "body",
        border = officer::fp_border(color = col, width = 0.5)
      )
    }
  }
  if (length(align) == 1) align <- rep(align, ncol(obj))
  for (j in seq(1, ncol(obj)))
    ft <- flextable::align(ft, j = j, align = align[j], part = "body")

  # Footer
  if (!is.null(footnote)) {
    if (is.null(size_foot)) size_foot <- size
    ft <- flextable::fontsize(ft, size = size_foot, part = "footer")
    if (!is.list(footnote)) footnote <- list(footnote)
    for (i in seq_along(footnote)) {
      if (!("chunk" %in% class(footnote[[i]]))) {
        footnote[[i]] <-
          flextable::as_chunk(
            footnote[[i]],
            props = flextable::fp_text_default(font.size = size_foot)
          )
      } else {
        if (is.na(footnote[[i]]$font.size))
          footnote[[i]]$font.size <- size_foot
      }
    }
    footnote <- append(
      list(flextable::as_chunk(
        "Note. ",
        props = flextable::fp_text_default(italic = TRUE, font.size = size_foot)
      )),
      footnote
    )
    ft <- flextable::add_footer_lines(
      ft,
      values = flextable::as_paragraph(list_values = footnote)
    )
    ft <- flextable::align(ft, align = "left", part = "footer")
  }

  return(ft)

}


#' Create a table with item properties
#'
#' @param select A named vector of variable names in `vars` identifying
#' different groups of items for which the item property should be counted. The
#' name of the vector gives the column headings of the table.
#' @param prop A vector with the variable name in `vars` identifying the item
#' property such as response format, text function, or cognitive function.
#' @param propname A name for the item property to be used in the table heading.
#' @param footnote A table note.
#' @param na.rm A logical to remove empty properties.
#' @param formats Long names of item properties to be displayed in the table.
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams collapse_response_categories
#' @export
TblItemProps <- function(vars, select, prop, propname = "", footnote = NULL,
                         na.rm = TRUE, size = 12, width = NULL,
                         formats = c(MC  = "Simple multiple-choice items",
                                     CMC = "Complex multiple-choice items",
                                     SR  = "Short constructed responses",
                                     MA  = "Matching items",
                                     TET = "Text-enrichment tasks",
                                     HL  = "Highlighting tasks",
                                     finding = "Finding information in the text",
                                     conclusion = "Drawing text-related conclusions",
                                     reflecting = "Reflecting and assessing",
                                     information = "Information text",
                                     instruction = "Instruction text",
                                     advertising = "Advertising text",
                                     commenting = "Commenting or argumenting text",
                                     literary = "Literary text",
                                     change = "Change and relationship",
                                     data = "Data and chance",
                                     units = "Units and measuring",
                                     space = "Space and shape")) {

    # Create frequency table
    freq_groups <- NULL
    if (is.null(names(select)))
      names(select) <- rep("Number of items", length(select))
    for (g in names(select)) {
      tab <- table(vars[[prop]][vars[[select[[g]]]]])
      tab <- data.frame(r = names(tab), f = c(tab))
      if (is.null(freq_groups)) {
        freq_groups <- tab
      } else {
        freq_groups <- merge(freq_groups, tab, by = "r", all = TRUE)
      }
    }
    freq_groups[is.na(freq_groups)] <- 0
    if (na.rm) freq_groups <- freq_groups[freq_groups$f > 0, ]

    # Sort table according to ordering in formats
    keyDF <- data.frame(r = names(formats), w = 1:length(formats))
    freq_groups <- merge(freq_groups, keyDF, by = "r", all.x = TRUE)
    freq_groups <- freq_groups[order(freq_groups$w), 1:(length(select) + 1)]

    # Rename variables and formats
    freq_groups$r <- formats[freq_groups$r]
    colnames(freq_groups) <- c(propname, names(select))

    # Add total number of items
    sums <- colSums(freq_groups[, -1, drop = FALSE], na.rm = TRUE)
    freq_groups <- rbind(freq_groups, c("Total number of items", sums))

    # Create flextable
    ft <- Tbl(freq_groups, align = c("left", "center"),
              hline = nrow(freq_groups) - 1,
              size = size, width = width)
    return (ft)

  }



#' Create a table with facets for each item
#'
#' @param select A named vector of variable names in `vars` identifying
#' different groups of items for which the item property should be counted. The
#' name of the vector gives the column headings of the table.
#' @param facets Variable names in `vars` identifying the item facets.
#' @param position The variable name in `vars` giving the item position.
#' @param footnote A table note.
#' @param lbl Long names of item facets to be displayed in the table.
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams collapse_response_categories
#' @export
TblItemFacets <- function(vars, select, facets, position = NULL,
                          footnote = NULL, size = 12, width = 1.9,
                          lbl = c(finding = "Finding information in the text",
                                  conclusion = "Drawing text-related conclusions",
                                  reflecting = "Reflecting and assessing",
                                  information = "Information text",
                                  instruction = "Instruction text",
                                  advertising = "Advertising text",
                                  commenting = "Commenting or argumenting text",
                                  literary = "Literary text",
                                  change = "Change and relationship",
                                  data = "Data and chance",
                                  units = "Units and measuring",
                                  space = "Space and shape")) {

  # Select variables
  cols <- c("item", facets)
  if (!is.null(position)) cols <- c(position, cols)
  tab <- vars[vars[[select]], cols]
  if (is.null(position)) tab <- cbind(position = seq(1, nrow(tab)), tab)
  tab <- tab[order(tab[, 1]), ]

  # Rename variables and formats
  for (i in seq_along(facets)) {
    l <- lbl[as.character(tab[[facets[i]]])]
    l[is.na(l)] <- as.character(tab[[facets[i]]])[is.na(l)]
    tab[[facets[i]]] <- l
  }
  if (is.null(names(facets))) names(facets) <- facets
  col_names <- c("Pos.", "Item", names(facets))
  if (is.null(position)) col_names <- "No."
  colnames(tab) <- col_names

  # Create flextable
  if (length(width) == 1) width <- c(0.3, rep(width, ncol(tab) - 1))
  ft <- Tbl(tab, footnote = footnote, width = width, size = size)
  return (ft)

}



#' Create a table for missing responses by item
#'
#' @param obj A list with data frames with sheets from "mv_item.xlsx"
#' or a data frame with sheet "summary" from "mv_item.xlsx" created by
#' [scaling::mv_item()].
#' @param select An optional name of a specific group to select.
#' @param footnote An optional table note.
#' @param sort A column name to indicate by which column to sort.
#' @param excl A vector of column names to exclude.
#' @returns A flextable.
#' @inheritParams Tbl
#' @export
TblMvi <- function(obj, select = NULL, footnote = NULL, sort = "position",
                   size = 12, width = NULL,
                   excl = c("N_administered", "ND", "ALL")) {

  # Result table
  if (is.list(obj) & "list" %in% names(obj))
    obj <- obj[["list"]]
  tab <- obj

  # Select results for group
  if (!is.null(select)) {
    tab <- tab[, grepl(paste0("item|_", select), colnames(tab))]
    colnames(tab) <- sub(paste0("_", select), "", colnames(tab))
  }

  # Exclude columns
  if (!is.null(excl)) {
    regexp <- paste0("^$|", paste(excl, collapse = "|"))
    tab <- tab[, !grepl(regexp, colnames(tab))]
  }

  # Remove empty rows
  tab <- tab[!(rowSums(!is.na(tab)) == 1), ]

  # Sorting
  if (sort %in% names(tab)) {
    tab <- tab[order(as.numeric(tab[, sort])), ]
  }

  # Add item number
  tab <- cbind("Nr." = seq(1, nrow(tab)), tab)

  # Rename variables
  lbl <- colnames(tab)
  lbl[lbl == "item"] <- "Item"
  lbl[lbl == "position"] <- "Pos."
  lbl[lbl == "N_valid"] <- "N"
  colnames(tab) <- lbl

  # Create footnote
  note <- list()
  if ("Pos." %in% colnames(tab))
    note <- append(note, "Pos. = Item position within the test. ")
  if ("N" %in% colnames(tab)) {
    note <- append(note,
                   list(flextable::as_chunk(
                     "N", props = flextable::fp_text_default(italic = TRUE, font.size = 10)
                   )))
    note <- append(note, " = Number of valid responses. ")
  }
  if ("NR" %in% colnames(tab))
    note <- append(note,
                   "NR = Percentage of respondents that did not reach an item. "
    )
  if ("OM" %in% colnames(tab))
    note <- append(note,
                   "OM = Percentage of omitted responses. "
    )
  if ("NV" %in% colnames(tab))
    note <- append(note,
                   "NV = Percentage of invalid responses. "
    )
  if ("ND" %in% colnames(tab))
    note <- append(note,
                   "ND = Percentage of not determinable responses. "
    )
  if ("ALL" %in% colnames(tab))
    note <- append(note,
                   "ALL = Percentage of all types of missing responses. "
    )
  if (!is.null(footnote))
  note <- append(note, footnote)

  # Create flextable
  ft <- Tbl(tab, footnote = note, size = size, width = width)
  if ("Pos." %in% colnames(tab))
    ft <- flextable::colformat_double(ft, j = "Pos.", digits = 0)
  if ("N" %in% colnames(tab)) {
    ft <- flextable::italic(ft, j = "N", italic = TRUE, part = "header")
    ft <- flextable::colformat_double(ft, j = "N", digits = 0)
  }
  return(ft)

}

#' @rdname TblMvi
TblMVI <- TblMvi


#' Create table with item parameters
#'
#' @param obj A list with data frames with sheets from "irt_poly.xlsx"
#' or a data frame with sheet "summary" from "irt_poly.xlsx" created by
#' [scaling::irt_analysis()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
TblPars <- function(obj, footnote = NULL, excl = c("N_administered"),
                    size = 10, width = 0.5) {

  # Result table
  if (is.list(obj) & "summary" %in% names(obj))
    obj <- obj[["summary"]]
  tab <- obj

  # Exclude columns
  if (!is.null(excl)) {
    tab <- tab[, !grepl(paste(excl, collapse = "|"), colnames(tab))]
  }

  # Rename variables
  lbl <- colnames(tab)
  lbl[1] <- "Nr."
  lbl[lbl == "correct"] <- "Percentage\n correct"
  lbl[lbl == "N_valid"] <- "N"
  lbl[lbl == "xsi"] <- "Difficulty"
  colnames(tab) <- lbl

  # Create footnote
  note <- list()
  if ("N" %in% colnames(tab)) {
    note <- list(flextable::as_i("N"))
    note <- append(note, " = Number of observed responses. ")
  }
  if ("Difficulty" %in% colnames(tab))
    note <- append(note, "Difficulty =  Item difficulty / location. ")
  if ("SE" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("SE")))
    note <- append(note,
                   " =  Standard error of item difficulty / location. "
    )
  }
  if ("WMNSQ" %in% colnames(tab))
    note <- append(note, "WMNSQ = Weighted mean square error. ")
  if ("t" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("t")))
    note <- append(note, " = ")
    note <- append(note, list(flextable::as_i("t")))
    note <- append(note, "-value for WMNSQ. ")
  }
  if ("Discr." %in% colnames(tab))
    note <- append(note,
                   "Discr. = Discrimination parameter of a generalized partial credit model. "
    )
  if ("rit" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("r")))
    note <- append(note, list(flextable::as_sub(flextable::as_i("it"))))
    note <- append(note,
                   " = Corrected item-total correlation. "
    )
  }
  if ("aQ3" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("Q")))
    note <- append(note, list(flextable::as_sub("3")))
    note <- append(note,
                   " = Average absolute residual correlation for item (Yen, 1983). "
    )
  }
  if ("Percentage correct" %in% colnames(tab))
    note <- append(note,
                   paste0("Percent correct scores are not informative for polytomous ",
                          "item scores and, therefore, are not reported. ")
    )
  if (!is.null(footnote))
    note <- append(note, footnote)

  # Create flextable
  ft <- Tbl(tab, size = size, width = width, footnote = note)
  if ("Nr." %in% colnames(tab)) {
    ft <- flextable::colformat_double(ft, j = 1, digits = 0)
    ft <- flextable::width(ft, j = "Nr.", width = .3)
  }
  if ("Item" %in% colnames(tab))
    ft <- flextable::width(ft, j = "Item", width = max(nchar(tab$Item)) * .075)
  if ("N" %in% colnames(tab)) {
    ft <- flextable::italic(ft, j = "N", italic = TRUE, part = "header")
    ft <- flextable::colformat_double(ft, j = "N", digits = 0)
  }
  if ("Percentage\n correct" %in% colnames(tab))
    ft <- flextable::width(ft, j = "Percentage\n correct", width = .8)
  if ("Difficulty" %in% colnames(tab))
    ft <- flextable::width(ft, j = "Difficulty", width = .6)
  if ("SE" %in% colnames(tab))
    ft <- flextable::italic(ft, j = "SE", italic = TRUE, part = "header")
  if ("t" %in% colnames(tab))
    ft <- flextable::italic(ft, j = "t", italic = TRUE, part = "header")
  if ("rit" %in% colnames(tab))
    ft <- flextable::compose(ft, i = 1, j = "rit", part = "header",
                             value = flextable::as_paragraph(
                               flextable::as_i("r"),
                               flextable::as_sub(flextable::as_i("it"))
                             )
    )
  if ("aQ3" %in% colnames(tab))
    ft <- flextable::compose(ft,i = 1, j = "aQ3", part = "header",
                             value = flextable::as_paragraph(
                               flextable::as_i("Q"),
                               flextable::as_sub("3")
                             )
    )
  return(ft)

}


#' Create table with step parameters
#'
#' @param obj A list with data frames with sheets from "irt_poly.xlsx"
#' or sheet "steps" from "irt_poly.xlsx" created by [scaling::irt_analysis()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
TblSteps <- function(obj, footnote = NULL, size = 10, width = 1) {

  # Result table
  if (is.list(obj) & "steps" %in% names(obj))
    obj <- obj[["steps"]]
  tab <- obj

  # Rename variables
  colnames(tab) <- c("Item", paste0("Step ", seq(1, ncol(tab) - 1)))

  # Footnote
  note <- paste0("The last step parameter for each item is not ",
                 "estimated and has, thus, no standard error ",
                 "because it is a constrained parameter for ",
                 "model identification. ")
  if (!is.null(footnote))
    note <- list(note, footnote)

  # Create flextable
  if (length(width) == 1) {
    width <- rep(width, ncol(tab))
    width[1] <- max(nchar(tab$Item)) * .085
  }
  ft <- Tbl(tab, footnote = note, size = size, width = width)
  return(ft)

}


#' Create correlation table for multi-dimensional models
#'
#' @param obj A data frame with sheets from "dimensionality.xlsx"
#' created by [scaling::dim_analysis()].
#' @param model The model name, typically `uni` for the unidimensional
#' reference model and the name of the facet variable.
#' @param rownames A vector of labels for the rows.
#' @param colnames A vector of labels for the columns.
#' @param width The column widths; if a single value is given, it refers to the
#' first column; otherwise the number of values must correspond to the number of
#' columns in `obj`.
#' @return A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
TblDim <- function(obj, model, rownames = NULL, colnames = NULL,
                   footnote = NULL, size = 12, width = 3) {

  # Get table
  obj <- obj[[paste0("Cor-Var ", model)]]
  obj <- as.data.frame(obj[, -1])

  # Format table
  tab <- matrix("", nrow = nrow(obj), ncol = ncol(obj))
  for (i in seq(1, nrow(obj))) {
    for (j in seq(1, i)) {
      tab[i, j] <- rnd(obj[i, j], digits = 2, d0 = TRUE)
    }
  }
  tab <- as.data.frame(tab)

  # Rename variables
  if (!is.null(rownames) & length(rownames) != nrow(tab)) {
    warning("Number of rowname elements does not match table dimensions.")
    rownames <- NULL
  }
  if (is.null(rownames)) {
    rownames <- paste0("Dim ", seq(1, nrow(tab)))
  } else {
    rownames <- paste0(paste0("Dim ", seq(1, nrow(tab))), ": ", rownames)
  }
  if (!is.null(colnames) & length(colnames) != ncol(tab)) {
    warning("Number of colname elements does not match table dimensions.")
    colnames <- NULL
  }
  if (is.null(colnames)) colnames <- paste0("Dim ", seq(1, ncol(tab)))
  tab <- cbind(rownames, tab)
  colnames(tab) <- c("Dimension", colnames)

  # Footnote
  note <- paste0("Variances of the dimensions are given in the ",
                 "diagonal; correlations are given in the ",
                 "off-diagonal. ")
  if (!is.null(footnote))
    note <- list(note, footnote)

  # Create flextable
  if (length(width) == 1) width <- c(width, rep(0.6, ncol(tab) - 1))
  ft <- Tbl(tab, footnote = note, size = size, width = width)
  ft <- flextable::bold(ft, j = 1, part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  return(ft)

}


#' Create table for DIF analyses
#'
#' @param obj A list with data frames with sheets from "dif_poly_TR.xlsx"
#' or a data frame with sheet "estimates" from "dif_poly_TR.xlsx" created by
#' [scaling::dif_analysis()].
#' @param colnames1 A vector of names for the DIF variables included in the
#' first line of the table heading.
#' @param colnames2 A vector of groups for the DIF variables included in the
#' second line for the table heading.
#' @param width The widths of the columns; if a single values is given, it
#' corresponds to the first column; otherwise the number of values must
#' correspond to the number of columns in `obj`.
#' @return A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
TblDif <- function(obj, footnote = NULL, excl = NULL,
                   colnames1 = NULL, colnames2 = NULL,
                   width = 1.4, size = 10) {

  # Result table
  if (is.list(obj) & "estimates" %in% names(obj))
    obj <- obj[["estimates"]]
  tab <- obj

  # Exclude columns
  if (!is.null(excl)) {
    tab <- tab[, !grepl(paste(excl, collapse = "|"), colnames(tab))]
  }

  # Rename variables
  lbl <- trimws(gsub("[\\.0-9\\-]", "", colnames(tab)))
  vals <- rep("", length(lbl))
  vals[lbl == "sex"] <- "men vs. women"
  vals[lbl == "mig"] <- "without vs. with"
  vals[lbl == "books"] <- "<100 vs. >100"
  vals[lbl %in% c("rotation", "position")] <- "first vs. second"
  for (i in seq_along(colnames2)) {
    vals[colnames(tab) == names(colnames2)[i]] <- colnames2[i]
    }
  lbl[1] <- "Item"
  lbl[lbl == "sex"] <- "Sex"
  lbl[lbl == "mig"] <- "Migration"
  lbl[lbl == "books"] <- "Books"
  lbl[lbl %in% c("rotation", "position")] <- "Test position"
  lbl[lbl %in% c("sc", "cohort")] <- "Starting cohort"
  for (i in seq_along(colnames1)) {
    lbl[colnames(tab) == names(colnames1)[i]] <- colnames1[i]
  }

  # Line breaks for main effects
  tab[nrow(tab), 1] <- gsub(" \\(", "\n (", tab[nrow(tab), 1])
  tab[nrow(tab) - 1, 1] <- gsub(" \\(", "\n (", tab[nrow(tab) - 1, 1])

  # Create footnote
  note <- list()
  note <- append(note,
                 paste0("Raw differences between item difficulties (in logits) ",
                        "with standardized differences (Cohen's ")
  )
  note <- append(note, list(flextable::as_i("d")))
  note <- append(note, ") in parentheses. ")
  if (!is.null(footnote))
    note <- append(note, list(footnote))

  # Create flextable
  if (length(width) == 1) width <- c(width, rep(1, ncol(tab) - 1))
  ft <- Tbl(tab, footnote = note, size = size, width = width,
            hline = nrow(tab) - 2, lbl = lbl)
  ft <- flextable::add_header_row(ft, top = FALSE, values = vals)
  ft <- flextable::style(ft, i = 2, part = "header",
                         pr_t = officer::fp_text(bold = FALSE))
  ft <- flextable::hline(
    ft, i = 1, part = "header",
    border = officer::fp_border(color = "white", width = 0.5)
  )
  return(ft)

}

#' @rdname TblDif
TblDIF <- TblDif


#' Create table with fit statistics for DIF analyses
#'
#' @param label A vector of names for the DIF variables.
#' @return A flextable.
#' @inheritParams TblDif
#' @export
TblDifFit <-function(obj, footnote = NULL, excl = NULL, label = NULL,
                     size = 12, width = 0.9) {

  # Result table
  if (is.list(obj) & "gof" %in% names(obj))
    obj <- obj[["gof"]]
  tab <- obj

  # Exclude rows
  if (!is.null(excl)) {
    tab <- tab[!grepl(paste(excl, collapse = "|"), tab$'DIF.variable'), ]
  }

  # Rename variables
  lbl <- trimws(tab$'DIF.variable')
  lbl[lbl == "sex"] <- "Sex"
  lbl[lbl == "mig"] <- "Migration"
  lbl[lbl == "books"] <- "Books"
  lbl[lbl %in% c("rotation", "position")] <- "Test position"
  lbl[lbl %in% c("sc", "cohort")] <- "Starting cohort"
  for (i in seq_along(label)) {
    lbl[trimws(tab$'DIF.variable') == names(label)[i]] <- label[i]
  }
  tab$'DIF.variable' <- lbl
  colnames(tab) <- c("DIF variable", "Model", "N", "Deviance",
                     "Number of parameters", "AIC", "BIC")

  # Create footnote
  note <-
    paste0("The best-fitting model according to each information ",
           "criterion is underlined. ")
  if (!is.null(footnote))
    note <- append(note, list(footnote))

  # Create flextable
  ft <- Tbl(tab, footnote = note, size = size, width = width,
            digits = 0)
  ft <- flextable::merge_v(ft, j = 1, part = "body")
  ft <- flextable::compose(
    ft, i = 1, j = "N", part = "header",
    value = flextable::as_paragraph(flextable::as_i("N"))
  )

  # Underline smaller information criteria
  for (i in seq(1, nrow(tab), 2)) {
    row_aic <- ifelse(tab$AIC[i] > tab$AIC[i + 1], i + 1, i)
    row_bic <- ifelse(tab$BIC[i] > tab$BIC[i + 1], i + 1, i)
    ft <- flextable::style(
      ft, i = row_aic, j = "AIC",
      pr_t = officer::fp_text(underlined = TRUE, font.size = size)
    )
    ft <- flextable::style(
      ft, i = row_bic, j = "BIC",
      pr_t = officer::fp_text(underlined = TRUE, font.size = size)
    )
  }

  return(ft)

}

#' @rdname TblDifFit
TblDIFFit <- TblDifFit


#' Create a table with the analysis code
#'
#' @param collapsed A data frame with the collapsed item categories generated
#' by [scaling::collapse_response_categories()].
#' @param tbl A logical to return the table (`TRUE`) instead of the code (`FALSE`).
#' @param special A logical for special schools (`TRUE`) or regular schools
#' (`FALSE`).
#' @returns A flextable.
#' @inheritParams collapse_response_categories
#' @export
TblCode <- function(vars, select, collapsed = NULL, tbl = TRUE,
                    special = FALSE) {

  txt = c(
    '# load packages',
    'library(rio)      # to import SPSS files',
    'library(doBy)  # to recode variables',
    'library(TAM)   # for IRT analyses',
    '',
    '# load competence data',
    paste0('dat <- rio::import("xTarget',
           ifelse(special, 'SpecialNeeds', ''),
           'Competencies.sav") '),
    '',
    '# items of the competence test',
    'items <- c('
  )
  items <- vars$item[vars[[select]]]
  for (i in seq(1, length(items), 3)) {
    j <- i + 2
    while (j > length(items)) j <- j - 1
    sep <- ifelse(j < length(items), ",", "")
    txt <-
      c(
        txt,
        paste0(
          paste0('  "', items[seq(i, j)], '"', collapse = ", "),
          sep
        )
      )
  }
  txt <- c(
    txt,
    ')',
    ''
  )
  poly <- vars$item[vars$poly & vars[[select]]]
  if (length(poly) >= 1) {
    txt <- c(
      txt,
      '# polytomous items',
      'poly <- c('
    )
    for (i in seq(1, length(poly), 3)) {
      j <- i + 2
      while (j > length(poly)) j <- j - 1
      sep <- ifelse(j < length(poly), ",", "")
      txt <-
        c(
          txt,
          paste0(
          paste0('  "', poly[seq(i, j)], '"', collapse = ", "),
            sep
          )
        )
    }
    txt <- c(
      txt,
      ')',
      ''
    )
  }
  if (!is.null(collapsed)) {
      txt <- c(
      txt,
      '# collapse response categories'
    )
    for (i in seq(1, nrow(collapsed))) {
      txt <-
        c(
          txt,
          paste0('dat$', collapsed$original_item[i], ' <- '),
          '  doBy::recodeVar(',
          paste0('    dat$', collapsed$original_item[i], ', ',
                 'c(', gsub("=\\d", "", collapsed$scoring[i], perl=T), '), ',
                 'c(', gsub("\\d=", "", collapsed$scoring[i], perl=T), ')'),
          '  )'
        )
    }
    txt <- c(
      txt,
      ''
    )
  }
  if (length(poly) >= 1) {
    txt <- c(
      txt,
      '# define Q-matrix for 0.5 scoring of PCM',
      'Q <- matrix(1, nrow = length(items), ncol = 1)',
      'Q[items %in% poly, 1] <- 0.5',
      '',
      '# estimate partial credit model',
      paste0('mod <- TAM::tam.mml(resp = dat[, items], Q = Q, ',
             'irtmodel = "PCM2", pid = dat$ID_t)')
    )
  } else {
    txt <- c(
      txt,
      '# estimate Rasch model',
      paste0('mod <- TAM::tam.mml(resp = dat[, items], ',
             'irtmodel = "PCM2", pid = dat$ID_t)')
    )
  }
  txt <- c(
    txt,
    'summary(mod)',
    '',
    '# item fit',
    'TAM::tam.fit(mod)',
    '',
    '# WLE',
    'TAM::tam.wle(mod)'
  )
  if (tbl) {
    return(cat(knitr::knit_child(text = txt, quiet = TRUE), sep = "\n"))
  } else {
    return(txt)
  }

}

