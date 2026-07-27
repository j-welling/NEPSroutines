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

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Please install flextable!")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
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
    if (length(width) == 1) width <- rep(width, ncol(obj))
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

  # Set font family
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")

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
#' @param na.rm A logical to remove empty properties (rows) and empty groups (columns).
#' @param formats Long names of item properties to be displayed in the table.
#' @param ... Further arguments passed to [Tbl()]; `align` and `hline` are set
#' internally and will error if also passed here.
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams collapse_response_categories
#' @export
#' @examples
#' \dontrun{
#' data(ex2)
#' data(ex3)
#'
#' # Table with content areas for a single group
#' TblItemProps(
#'   ex2$vars,
#'   select = c("Number of items" = "mixed"),
#'   prop = "content",
#'   propname = "Content area"
#' )
#'
#' # Number of response format for three booklets with a footnote
#' ex3$vars$mixed1 <- ex3$vars$mixed & ex3$vars$booklet1
#' ex3$vars$mixed2 <- ex3$vars$mixed & ex3$vars$booklet2
#' ex3$vars$mixed3 <- ex3$vars$mixed & ex3$vars$booklet3
#' TblItemProps(
#'   ex3$vars,
#'   select = c("Booklet 1" = "mixed1", "Booklet 2" = "mixed2",
#'              "Booklet 3" = "mixed3"),
#'   prop = "type",
#'   propname = "Response format",
#'   footnote = "The study administered three difficulty-tiered booklets."
#' )
#' }
TblItemProps <- function(vars, select, prop, propname = "\x20", footnote = NULL,
                         na.rm = TRUE, size = 12, width = NULL,
                         formats = NULL, ...) {

    if (!(prop %in% names(vars))) stop("Unknown item property ", prop, "!")
    if (!is.factor(vars[[prop]])) vars[[prop]] <- as.factor(vars[[prop]])

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
    if (na.rm) {
      ff <- rowSums(freq_groups[, -1, drop = FALSE]) > 0
      freq_groups <- freq_groups[ff, , drop = FALSE]
      ff <- colSums(freq_groups[, -1, drop = FALSE]) > 0
      freq_groups <- freq_groups[, c(TRUE, ff), drop = FALSE]
      select <- select[ff]
    }

    # Sort table according to ordering in formats
    formatsvec <- GetPropLabels()
    if (!is.null(formats)) {
      formatsvec <- formatsvec[!(names(formatsvec) %in% names(formats))]
      formatsvec <- c(formatsvec, formats)
    }
    keyDF <- data.frame(r = names(formatsvec), w = 1:length(formatsvec))
    freq_groups <- merge(freq_groups, keyDF, by = "r", all.x = TRUE)
    freq_groups <- freq_groups[order(freq_groups$w), 1:(length(select) + 1)]

    # Rename variables and formats
    freq_groups$r <- formatsvec[freq_groups$r]
    colnames(freq_groups) <- c(propname, names(select))

    # Add total number of items
    sums <- colSums(freq_groups[, -1, drop = FALSE], na.rm = TRUE)
    freq_groups <- rbind(freq_groups, c("Total number of items", sums))

    # Create flextable
    hline <- if (nrow(freq_groups) > 1) nrow(freq_groups) - 1 else NULL
    ft <- Tbl(freq_groups, align = c("left", rep("center", length(select))),
              hline = hline, footnote = footnote,
              size = size, width = width, ...)
    return(ft)

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
#' @param ... Further arguments passed to [Tbl()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams collapse_response_categories
#' @inheritParams TblPars
#' @export
#' @examples
#' \dontrun{
#' data(ex2)
#'
#' # Table with content areas
#' TblItemFacets(
#'   ex2$vars,
#'   select = "mixed",
#'   facets = c("Content area" = "content")
#' )
#'
#' # Table with content areas, response formats, and item position
#' TblItemFacets(
#'   ex2$vars,
#'   select = "mixed",
#'   facets = c("Content area" = "content", "Response format" = "type"),
#'   position = "pos",
#'   footnote = "MC = simple multiple-choice, CMC = complex multiple-choice"
#' )
#' }
TblItemFacets <- function(vars, select, facets, position = NULL,
                          footnote = NULL, size = 12, width = 1.9,
                          lbl = NULL, rename_collapsed = TRUE, ...) {

  # Select variables
  cols <- c("item", facets)
  if (!is.null(position)) cols <- c(position, cols)
  tab <- vars[vars[[select]], cols]
  if (is.null(position)) tab <- cbind(position = seq(1, nrow(tab)), tab)
  tab <- tab[order(tab[, 1]), ]

  # Rename variables and formats
  lblvec <- GetPropLabels()
  if (!is.null(lbl)) {
    lblvec <- lblvec[!(names(lblvec) %in% names(lbl))]
    lblvec <- c(lblvec, lbl)
  }
  for (i in seq_along(facets)) {
    l <- lblvec[as.character(tab[[facets[i]]])]
    l[is.na(l)] <- as.character(tab[[facets[i]]])[is.na(l)]
    tab[[facets[i]]] <- l
  }
  if (is.null(names(facets))) names(facets) <- facets
  col_names <- c(ifelse(is.null(position), "No.", "Pos."), "Item", names(facets))
  colnames(tab) <- col_names
  if (rename_collapsed)
    tab$Item <- sub("_collapsed$", "", tab$Item)

  # Create flextable
  if (length(width) == 1) width <- c(0.3, rep(width, ncol(tab) - 1))
  ft <- Tbl(tab, footnote = footnote, width = width, size = size, ...)
  ft <- flextable::colformat_double(ft, j = 1, digits = 0)
  return(ft)

}



#' Create a table for missing responses by item
#'
#' @param obj A list with data frames with sheets from "mv_item.xlsx"
#' or a data frame with sheet "summary" from "mv_item.xlsx" created by
#' [mv_item()].
#' @param select An optional name of a specific group to select. Columns whose
#' names end in the group suffix (e.g. `"_mixed"`) are kept, along with the
#' `"Nr."` and `"item"` columns; the suffix is matched literally, not as a
#' regular expression.
#' @param footnote An optional table note.
#' @param sort A column name to indicate by which column to sort.
#' @param excl A vector of column names to exclude. Each value is matched
#' exactly (not as a regular expression or substring) against either the
#' internal column name or the label shown in the rendered table (e.g. both
#' `"N_administered"` and `"Total"` exclude the total-respondents column).
#' Columns are dropped before the widths are applied, so a vector `width` must
#' be sized and ordered to match the columns that remain after exclusion (a
#' single `width` value is recycled and is unaffected).
#' @param rename_collapsed A boolean to remove the "_collapsed" suffix from
#' item names.
#' @param ... Further arguments passed to [Tbl()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @export
#' @examples
#' \dontrun{
#' data(ex1)
#'
#' # Missing analyses by item
#' tmpdir <- tempdir()
#' mv_item(
#'  resp = ex1$resp,
#'  vars = ex1$vars,
#'  select = "dich",
#'  valid = "valid",
#'  mvs = c(OM = -97, NV = -95, NR = -94),
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
#' # Table with default columns
#' TblMvi(mvi)
#'
#' # Table with all missing responses and total number of respondents sorted
#' #  by number of valid responses
#' TblMvi(mvi, excl = NULL, sort = "N_valid")
#' }
TblMvi <- function(obj, select = NULL, footnote = NULL, sort = "position",
                   size = 12, width = NULL,
                   excl = c("N_administered", "ND", "ALL", "...1"),
                   rename_collapsed = TRUE, ...) {

  # Result table
  if (is.list(obj) & "list" %in% names(obj))
    obj <- obj[["list"]]
  tab <- obj

  # Correct empty column names
  colnames(tab)[1] <- "Nr."

  # Select results for group
  if (!is.null(select)) {
    keep <- colnames(tab) %in% c("Nr.", "item") |
      endsWith(colnames(tab), paste0("_", select))
    tab <- tab[, keep]
    # Strip the trailing group suffix only (anchored, matching the filter above)
    colnames(tab) <- sub(paste0("_", select, "$"), "", colnames(tab))
  }

  # Rename collapsed items
  if ("item" %in% colnames(tab) & rename_collapsed) {
    tab$item <- sub("_collapsed$", "", tab$item)
  }

  # Remove empty rows
  # Note: columns 'Nr.' and 'item' are always present (i.e. at least 2 columns)
  tab <- tab[!(rowSums(!is.na(tab)) == 2), ]

  # Sorting
  if (sort %in% names(tab)) {
    tab <- tab[order(as.numeric(tab[, sort])), ]
  }

  # Create numbering
  tab$"Nr." <- seq_len(nrow(tab))

  # Display labels (internal name -> label shown in the rendered table) so
  # `excl` can match either the internal name or the displayed header.
  lbl <- colnames(tab)
  lbl[lbl == "item"] <- "Item"
  lbl[lbl == "position"] <- "Pos."
  lbl[lbl == "N_administered"] <- "Total"
  lbl[lbl == "N_valid"] <- "N"

  # Exclude columns by exact name -- internal name OR display label
  # (also drops unnamed columns)
  keep <- nzchar(colnames(tab))
  if (!is.null(excl)) keep <- keep & !(colnames(tab) %in% excl | lbl %in% excl)
  tab <- tab[, keep]
  lbl <- lbl[keep]
  colnames(tab) <- lbl

  # Create footnote
  note <- list()
  if ("Pos." %in% colnames(tab))
    note <- append(note, "Pos. = Item position within the test. ")
  if ("Total" %in% colnames(tab)) {
    note <- append(note, "Total = Number of persons the item was administered. ")
  }
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
  if ("TA" %in% colnames(tab))
    note <- append(note,
                   "TA = Percentage of missing due to test abortion. "
    )
  if ("UM" %in% colnames(tab))
    note <- append(note,
                   "UM = Percentage of unspecific missing responses. "
    )
  if ("MD" %in% colnames(tab))
    note <- append(note,
                   "MD = Percentage of responses missing by design. "
    )
  if ("AZ" %in% colnames(tab))
    note <- append(note,
                   "AZ = Percentage of reset responses. "
    )
  if ("ALL" %in% colnames(tab))
    note <- append(note,
                   "ALL = Percentage of all types of missing responses. "
    )
  if (!is.null(footnote))
  note <- append(note, footnote)

  # Create flextable
  ft <- Tbl(tab, footnote = note, size = size, width = width, ...)
  if ("Pos." %in% colnames(tab))
    ft <- flextable::colformat_double(ft, j = "Pos.", digits = 0)
  if ("Total" %in% colnames(tab))
    ft <- flextable::colformat_double(ft, j = "Total", digits = 0)
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
#' [irt_analysis()].
#' @param rename_collapsed A boolean to remove the "_collapsed" suffix from
#' item names.
#' @param ... Further arguments passed to [Tbl()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
#' @examples
#' \dontrun{
#' data(ex1)
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
#' # Default parameter table
#' TblPars(pars)
#'
#' # Parameter table including number of respondents the item was administered
#' # and a footnote
#' TblPars(pars, excl = NULL, footnote = "Nothing to report.")
#' }
TblPars <- function(obj, footnote = NULL, excl = c("N_administered"),
                    size = 10, width = 0.5, rename_collapsed = TRUE, ...) {

  # Result table
  if (is.list(obj) & "summary" %in% names(obj))
    obj <- obj[["summary"]]
  tab <- obj

  # Correct empty column names
  colnames(tab)[1] <- "Nr."

  # Exclude columns by exact name -- internal name OR display label (the labels
  # mirror the rename block below; excl may use either form).
  if (!is.null(excl)) {
    lbl <- colnames(tab)
    lbl[lbl == "correct"] <- "Percentage\n correct"
    lbl[lbl == "N_administered"] <- "Total"
    lbl[lbl == "N_valid"] <- "N"
    lbl[lbl == "xsi"] <- "Difficulty"
    tab <- tab[, !(colnames(tab) %in% excl | lbl %in% excl)]
  }

  # Rename collapsed items
  if ("Item" %in% colnames(tab) & rename_collapsed) {
    tab$Item <- sub("_collapsed$", "", tab$Item)
  }

  # Model type
  pcm <- TRUE
  if (!is.null(tab$xsi) & sum(is.na(tab$xsi) == 0L)) pcm <- FALSE

  # Rename variables
  lbl <- colnames(tab)
  lbl[lbl == "correct"] <- "Percentage\n correct"
  lbl[lbl == "N_administered"] <- "Total"
  lbl[lbl == "N_valid"] <- "N"
  lbl[lbl == "xsi"] <- "Difficulty"
  colnames(tab) <- lbl

  # Create footnote
  note <- list()
  if ("Total" %in% colnames(tab))
    note <- append(note, "Total = Number of respondents the item was administered. ")
  if ("N" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("N")))
    note <- append(note, " = Number of observed responses. ")
  }
  if ("Difficulty" %in% colnames(tab)) {
    if (pcm) {
      note <- append(note, "Difficulty = Item difficulty / location. ")
    } else {
      note <- append(note, "Difficulty = Item difficulty. ")
    }
  }
  if ("SE" %in% colnames(tab)) {
    note <- append(note, list(flextable::as_i("SE")))
    note <- append(note,
                   " =  Standard error of item parameter. "
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
  if ("Discr." %in% colnames(tab)) {
    if (pcm) {
      note <- append(note,
                     "Discr. = Discrimination parameter of a generalized partial credit model. "
      )
    } else {
      note <- append(note,
                     "Discr. = Discrimination parameter of a two-parametric item response model. "
      )
    }
  }
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
                   " = Average absolute residual correlation for item (Yen, 1984). "
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
  ft <- Tbl(tab, size = size, width = width, footnote = note, ...)
  if ("Nr." %in% colnames(tab)) {
    ft <- flextable::colformat_double(ft, j = 1, digits = 0)
    ft <- flextable::width(ft, j = "Nr.", width = .3)
  }
  if ("Item" %in% colnames(tab))
    ft <- flextable::width(ft, j = "Item", width = max(nchar(tab$Item)) * .075)
  if ("Total" %in% colnames(tab))
    ft <- flextable::colformat_double(ft, j = "Total", digits = 0)
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
#' or sheet "steps" from "irt_poly.xlsx" created by [irt_analysis()].
#' @param digits A number for rounding.
#' @param ... Further arguments passed to [Tbl()].
#' @returns A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @inheritParams TblPars
#' @export
#' @examples
#' \dontrun{
#' data(ex2)
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
#' pars <- Import(tmpdir, "/irt_poly.xlsx")
#'
#' # Default parameter table
#' TblSteps(pars)
#'
#' # Parameter table with larger font size and footnote
#' TblSteps(pars, size = 12, footnote = "Nothing to note.")
#' }
TblSteps <- function(obj, footnote = NULL, size = 10, width = 1, digits = 2,
                     rename_collapsed = TRUE, ...) {

  # Result table
  if (is.list(obj) & "steps" %in% names(obj))
    obj <- obj[["steps"]]
  tab <- obj

  # Rename variables
  colnames(tab) <- c("Item", paste0("Step ", seq(1, ncol(tab) - 1)))

  # Rename collapsed items
  if ("Item" %in% colnames(tab) & rename_collapsed) {
    tab$Item <- sub("_collapsed$", "", tab$Item)
  }

  # Rounding
  for (i in seq_along(tab)[-1]) {
    for (j in seq_along(tab[[i]])) {
      thr <- regmatches(tab[j, i], regexpr("^\\-?\\d\\.\\d+", tab[j, i]))
      se <- regmatches(tab[j, i], regexpr("(?<=\\()\\d\\.\\d+", tab[j, i],
                       perl = TRUE))
      step <- NA
      if (length(thr == 1L)) {
        step <- rnd(as.numeric(thr), digits = digits)
        if (length(se) == 1L)
          step <- paste0(step, " (", rnd(as.numeric(se), digits = digits), ")")
      }
      tab[j, i] <- step
    }
  }

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
  ft <- Tbl(tab, footnote = note, size = size, width = width, ...)
  return(ft)

}


#' Validate a vector of dimension labels
#'
#' Labels are matched by name. An unnamed vector is the earlier form, where
#' labels were assigned by position; it is still honoured if it covers every
#' dimension, so that existing report scripts keep working, but is deprecated
#' because a wrong order mislabels results without any sign in the output.
#' Anything else that would leave a raw dimension name in the table is warned
#' about but kept, as a partly labelled table is still usable.
#'
#' @param labels The user-supplied vector of labels, or `NULL`.
#' @param dimensions The dimension names present in the table.
#' @param what Either "rowname" or "colname", used in the warning messages.
#' @return `labels`, named, or `NULL` if it cannot be used.
#' @noRd
check_dimnames <- function(labels, dimensions, what) {
  if (is.null(labels)) return(NULL)
  if (is.null(names(labels))) {
    if (length(labels) != length(dimensions)) {
      warning("Number of ", what, " elements does not match table dimensions.",
              call. = FALSE)
      return(NULL)
    }
    warning("Passing `", what, "s` without names is deprecated: the labels are ",
            "assigned by position, so a wrong order silently mislabels the ",
            "results. Name each element after its dimension, for example ",
            "c(\"", dimensions[1], "\" = \"", labels[1], "\").", call. = FALSE)
    # `dimensions` must be the dimensions in their original order: the caller
    # resolves the labels before any reordering, so position still corresponds
    # to dimension here.
    names(labels) <- dimensions
    return(labels)
  }
  if (anyDuplicated(names(labels))) {
    warning("Some ", what, " elements share the same name; only the first ",
            "of each is used.", call. = FALSE)
  }
  if (!all(names(labels) %in% dimensions)) {
    warning("Some ", what, " elements do not match a dimension in the table.",
            call. = FALSE)
  }
  if (!all(dimensions %in% names(labels))) {
    warning("Some dimensions are not covered by the supplied ", what,
            " elements; these keep their raw name, and the sequence given ",
            "here is not applied.", call. = FALSE)
  }
  labels
}


#' Replace dimension names by their labels
#'
#' @param dimensions The dimension names present in the table.
#' @param labels A named vector of labels, or `NULL`.
#' @return A character vector of labels, in the order of `dimensions`.
#' @noRd
replace_dimnames <- function(dimensions, labels) {
  if (is.null(labels)) return(dimensions)
  hit <- match(dimensions, names(labels))
  ifelse(is.na(hit), dimensions, labels[hit])
}


#' Create correlation table for multi-dimensional models
#'
#' @param obj A data frame with sheets from "dimensionality.xlsx"
#' created by [dim_analysis()].
#' @param model The model name, typically `uni` for the unidimensional
#' reference model and the name of the facet variable.
#' @param rownames An optional named vector of labels for the rows; if all
#' row elements are set, the rows and columns are reordered according to the
#' specified sequence (overrides any sequence specified in `colnames`). The
#' names must be the dimension names given by the dimension variable passed to
#' [dim_analysis()], not row positions; a name matching no dimension, or a
#' dimension left without a label, is reported by a warning. An unnamed vector
#' is deprecated: its labels are assigned by position, which mislabels the
#' results if the order does not match the dimensions in the data.
#' @param colnames An optional named vector of labels for the columns; if all
#' column elements are set, the rows and columns are reordered according to the
#' specified sequence. Named as in `rownames`. Note that supplying `colnames`
#' alone reorders the table but leaves the rows labelled "Dim 1", "Dim 2" and so
#' on, so set `rownames` as well to keep the rows identifiable.
#' @details An axis without labels is numbered "Dim 1", "Dim 2" and so on, and
#' while the columns are not fully labelled, both axes carry those numbers, so
#' that a column can be traced back to its row. The numbers are dropped only
#' once `colnames` labels every dimension: the columns then head themselves, and
#' the rows show their labels alone. Supplying `colnames` without `rownames`
#' therefore leaves the rows numbered rather than named.
#' @param width The column widths; if a single value is given, it refers to the
#' first column; otherwise the number of values must correspond to the number of
#' columns in `obj`.
#' @param ... Further arguments passed to [Tbl()]. The first column (dimension
#' labels) is always left-aligned, so any `align` passed here does not affect it.
#' @return A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @export
#' @examples
#' \dontrun{
#' data(ex2)
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
#' # Default correlation table
#' TblDim(dim, "content")
#'
#' # Correlation table with labels; names refer to the dimensions, so the
#' # sequence given here also determines the row and column order
#' TblDim(dim, "content",
#'        rownames = c("dim1" = "Units", "dim2" = "Change",
#'                     "dim3" = "Space", "dim4" = "Data"))
#' }

TblDim <- function(obj, model, rownames = NULL, colnames = NULL,
                   footnote = NULL, size = 12, width = 3, ...) {

  # Get table
  obj <- obj[[paste0("Cor-Var ", model)]]

  # Format table
  # Column 1 of `obj` holds the dimension names, hence the `j + 1` offset when
  # reading values and the `[, -1]` when sizing `tab`. `drop = FALSE` is
  # essential: the unidimensional model has a single value column, which would
  # otherwise collapse to a vector and make `ncol()` return NULL. Only the
  # lower triangle is filled; `d0 = i != j` keeps the leading zero on the
  # diagonal (variances) and drops it off-diagonal (correlations).
  values <- obj[, -1, drop = FALSE]
  tab <- matrix("", nrow = nrow(obj), ncol = ncol(values))
  for (i in seq(1, nrow(obj))) {
    for (j in seq(1, i)) {
      tab[i, j] <- rnd(obj[i, j + 1], digits = 2, d0 = i != j)
    }
  }
  # Carry the dimension names onto the matrix itself. These names -- not the
  # row positions -- are what `rownames`/`colnames` are matched against below,
  # which is what makes the labelling order-independent. The sheet is a square,
  # symmetric correlation matrix, so both axes take their names from the first
  # column. Its header is unusable as a second source: the unidimensional model
  # has no dimension name to write there, and `openxlsx` replaces blanks in any
  # other name by dots when the sheet is read back in, so a dimension called
  # "Change and relationships" would return as "Change.and.relationships".
  rownames(tab) <- colnames(tab) <- as.character(obj[, 1])
  tab <- as.data.frame(tab)

  # Resolve the labels before anything is reordered. An unnamed vector is bound
  # to the dimensions by position, so it must be resolved against the original
  # order -- doing this after a reorder would bind the labels to the new
  # sequence and mislabel every dimension.
  rownames <- check_dimnames(rownames, rownames(tab), "rowname")
  colnames <- check_dimnames(colnames, colnames(tab), "colname")

  # Reorder rows and columns
  # Reordering is opt-in: it happens only if the supplied vector covers every
  # dimension (`length(...) == ncol/nrow`) and every dimension name is one of
  # its names. Otherwise the vector is used for labelling alone and the table
  # keeps its original order. `rownames` is applied second so it wins.
  if (length(colnames) == ncol(tab) && all(colnames(tab) %in% names(colnames)))
    tab <- tab[names(colnames), names(colnames), drop = FALSE]
  if (length(rownames) == nrow(tab) && all(rownames(tab) %in% names(rownames)))
    tab <- tab[names(rownames), names(rownames), drop = FALSE]
  # Reordering permutes the filled lower triangle, so some values now sit above
  # the diagonal. Mirror them back down to restore a lower-triangular table.
  for (i in seq(1, nrow(tab))) {
    for (j in seq(1, i)) {
      if (tab[i, j] == "") {
        tab[i, j] <- tab[j, i]
        tab[j, i] <- ""
      }
    }
  }

  # Rename variables
  # Substitute in one vectorised step. An in-place loop would re-scan values it
  # had already replaced, so a label equal to another dimension's name would be
  # overwritten in turn.
  rowlabels <- replace_dimnames(rownames(tab), rownames)
  collabels <- replace_dimnames(colnames(tab), colnames)

  # The "Dim i" numbering is what lets a reader map a column back to its row, so
  # it is dropped only once every column carries a label of its own. A vector
  # that leaves any dimension unlabelled does not qualify: those columns would
  # be headed by their raw name from the results file, with nothing tying them
  # to the rows. Numbering follows the final (post-reorder) position, so a
  # number and its label always refer to the same dimension.
  labelled <- function(labels, dimensions) {
    !is.null(labels) && all(dimensions %in% names(labels))
  }
  if (is.null(rownames)) {
    rowlabels <- paste0("Dim ", seq(1, nrow(tab)))
  } else if (!labelled(colnames, colnames(tab))) {
    rowlabels <- paste0("Dim ", seq_along(rowlabels), ": ", rowlabels)
  }
  if (is.null(colnames)) {
    collabels <- paste0("Dim ", seq(1, ncol(tab)))
  } else if (!labelled(colnames, colnames(tab))) {
    collabels <- paste0("Dim ", seq_along(collabels), ": ", collabels)
  }
  colnames(tab) <- collabels

  # Add dimension column
  # Taken from `rowlabels` rather than from the row names of `tab`, which have
  # to be unique: without the "Dim i" prefix two rows may share a label, which
  # is the user's business as long as the column labels stay distinct -- those
  # become the table's column keys and must still be unique.
  tab <- cbind("Dimension" = rowlabels, tab)

  # Footnote
  note <- paste0("Variances of the dimensions are given in the ",
                 "diagonal; correlations are given in the ",
                 "off-diagonal. ")
  if (!is.null(footnote))
    note <- list(note, footnote)

  # Create flextable
  if (length(width) == 1) width <- c(width, rep(0.6, ncol(tab) - 1))
  ft <- Tbl(tab, footnote = note, size = size, width = width, ...)
  ft <- flextable::bold(ft, j = 1, part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  return(ft)

}


#' Create table for DIF analyses
#'
#' @param obj A list with data frames with sheets from "dif_poly_TR.xlsx"
#' or a data frame with sheet "estimates" from "dif_poly_TR.xlsx" created by
#' [dif_analysis()].
#' @param colnames1 A named vector of names for the DIF variables included in
#' the first line of the table heading; the names indicate the group name in
#' `obj`, while the values give th new headings.
#' @param colnames2 A named vector of groups for the DIF variables included in
#' the second line for the table heading; the names indicate the group name in
#' `obj`, while the values give the new headings.
#' @param excl A vector of column names to exclude, matched exactly against the
#' column names (e.g. `"mig.1-2"`); `.` and `-` are treated literally. Unlike
#' [TblMvi()], the displayed group labels (e.g. `"Migration"`) are not accepted
#' here, because one label maps to several columns.
#' @param width The widths of the columns; if a single values is given, it
#' corresponds to the first column; otherwise the number of values must
#' correspond to the number of columns in the rendered table (i.e. `obj` after
#' any columns removed by `excl`).
#' @param digits A number for rounding.
#' @param ... Further arguments passed to [Tbl()]; `hline` and `lbl` are set
#' internally and will error if also passed here.
#' @return A flextable.
#' @inheritParams Tbl
#' @inheritParams TblMvi
#' @inheritParams TblPars
#' @export
#' @examples
#' \dontrun{
#' data(ex1)
#'
#' # DIF analyses
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
#' # Default DIF table
#' TblDif(dif$TR, colnames2 = c("mig.1-3" = "without vs. missing",
#'                              "mig.2-3" = "with vs. missing"))
#'
#' # Excluding missing group for mig
#' TblDif(dif$TR, excl= c("mig.1-2", "mig.1-3"))
#' }
TblDif <- function(obj, footnote = NULL, excl = NULL,
                   colnames1 = NULL, colnames2 = NULL,
                   width = 1.4, size = 10, digits = 2,
                   rename_collapsed = TRUE, ...) {

  # Result table
  if (is.list(obj) & "estimates" %in% names(obj))
    obj <- obj[["estimates"]]
  tab <- obj

  # Exclude columns
  if (!is.null(excl)) {
    tab <- tab[, !(colnames(tab) %in% excl)]
  }

  # Rename collapsed items
  if ("item" %in% colnames(tab) & rename_collapsed) {
    tab$item <- sub("_collapsed$", "", tab$item)
  }

  # Rename variables
  lbl <- trimws(gsub("[\\.0-9\\-]", "", colnames(tab)))
  vals <- rep("", length(lbl))
  vals[lbl == "sex"] <- "men vs. women"
  vals[lbl == "mig"] <- "without vs. with"
  vals[lbl == "books"] <- "<100 vs. >100"
  vals[lbl %in% c("rotation", "position", "rot")] <- "first vs. second"
  for (i in seq_along(colnames2)) {
    vals[colnames(tab) == names(colnames2)[i]] <- colnames2[i]
    }
  lbl[1] <- "Item"
  lbl[lbl == "sex" & !(lbl %in% names(colnames1))] <- "Gender"
  lbl[lbl == "mig" & !(lbl %in% names(colnames1))] <- "Migration"
  lbl[lbl == "books" & !(lbl %in% names(colnames1))] <- "Books"
  lbl[lbl %in% c("rotation", "position", "rot") & !(lbl %in% names(colnames1))] <- "Test position"
  lbl[lbl %in% c("sc", "cohort") & !(lbl %in% names(colnames1))] <- "Starting cohort"
  for (i in seq_along(colnames1)) {
    lbl[lbl == names(colnames1)[i]] <- colnames1[i]
  }

  # Line breaks for main effects
  tab[nrow(tab), 1] <- gsub(" \\(", "\n (", tab[nrow(tab), 1])
  tab[nrow(tab) - 1, 1] <- gsub(" \\(", "\n (", tab[nrow(tab) - 1, 1])

  # Rounding
  for (i in seq_along(tab)[-1]) {
    for (j in seq_along(tab[[i]])) {
      ustd <- regmatches(tab[j, i], regexpr("^(-|\\s)?\\d\\.\\d+", tab[j, i]))
      std <- regmatches(tab[j, i], regexpr("(?<=\\()(-|\\s)?\\d\\.\\d+",
                                           tab[j, i], perl = TRUE))
      dif <- NA
      if (length(ustd == 1L)) {
        dif <- rnd(as.numeric(ustd), digits = digits)
        if (length(std) == 1L)
          dif <- paste0(dif, " (", rnd(as.numeric(std), digits = digits), ")")
      }
      tab[j, i] <- dif
    }
  }

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
            hline = nrow(tab) - 2, lbl = lbl, ...)
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
#' @param excl A vector of DIF variables that should be excluded. Each value is
#' matched exactly (not as a regular expression or substring) against either the
#' raw `DIF.variable` value (e.g. `"mig"`) or its display label (e.g.
#' `"Migration"`, including any custom `label`).
#' @param label A vector of names for the DIF variables.
#' @param width The widths of the columns; if a single value is given, it is
#' applied to all columns; otherwise the number of values must correspond to the
#' number of columns in the rendered table. `excl` removes rows here, so it does
#' not affect the column count.
#' @param ... Further arguments passed to [Tbl()]; `digits` is set internally and
#' will error if also passed here.
#' @return A flextable.
#' @inheritParams TblDif
#' @export
#' @examples
#' \dontrun{
#' data(ex1)
#'
#' # DIF analyses
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
#' # Default DIF fit table
#' TblDifFit(dif$TR)
#'
#' # Excluding results for sex, but with a new label for mig
#' TblDifFit(dif$TR, excl= "sex", label = c("mig" = "Migrant background"))
#' }
TblDifFit <- function(obj, footnote = NULL, excl = NULL, label = NULL,
                      size = 12, width = 0.9, ...) {

  # Result table
  if (is.list(obj) & "gof" %in% names(obj))
    obj <- obj[["gof"]]
  tab <- obj

  # Exclude rows by exact name -- raw DIF.variable value OR display label (the
  # labels mirror the rename block below; excl may use either form).
  if (!is.null(excl)) {
    lbl <- trimws(tab$'DIF.variable')
    lbl[lbl == "sex"] <- "Sex"
    lbl[lbl == "mig"] <- "Migration"
    lbl[lbl == "books"] <- "Books"
    lbl[lbl %in% c("rotation", "position")] <- "Test position"
    lbl[lbl %in% c("sc", "cohort")] <- "Starting cohort"
    for (i in seq_along(label)) {
      lbl[trimws(tab$'DIF.variable') == names(label)[i]] <- label[i]
    }
    tab <- tab[!(tab$'DIF.variable' %in% excl | lbl %in% excl), ]
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
            digits = 0, ...)
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
      pr_t = flextable::fp_text_default(underlined = TRUE, font.size = size)
    )
    ft <- flextable::style(
      ft, i = row_bic, j = "BIC",
      pr_t = flextable::fp_text_default(underlined = TRUE, font.size = size)
    )
  }

  return(ft)

}

#' @rdname TblDifFit
TblDIFFit <- TblDifFit


#' Create a table with the analysis code
#'
#' @param collapsed A data frame with the collapsed item categories generated
#' by [collapse_response_categories()].
#' @param tbl A logical to return the table (`TRUE`) instead of the code (`FALSE`).
#' @param special A logical for special schools (`TRUE`) or regular schools
#' (`FALSE`).
#' @returns A `knitr::knit_child()` object or a vector.
#' @inheritParams collapse_response_categories
#' @inheritParams TblPars
#' @export
#' @examples
#' \dontrun{
#' data(ex2)
#'
#' TblCode(ex2$vars, "mixed", tbl = FALSE)
#' }
TblCode <- function(vars, select, collapsed = NULL, tbl = TRUE,
                    special = FALSE, rename_collapsed = TRUE) {

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
  if (rename_collapsed)
    items <- sub("_collapsed$", "", items)
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
  if (rename_collapsed)
    poly <- sub("_collapsed$", "", poly)
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
                 'c(', gsub("=\\d", "", collapsed$scoring[i], perl = TRUE), '), ',
                 'c(', gsub("\\d=", "", collapsed$scoring[i], perl = TRUE), ')'),
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

