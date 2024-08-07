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
#' @param replace An optional named vector with substrings to replace in the
#' names of the imported files; the names identify the original string, while
#' the value gives the replacement.
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
                   rename = NULL,
                   replace = c("dif_poly_" = "", "dif_dich_" = "",
                               "mv_person_" = "")) {

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
      tbl <- list()
      for (s in sheets) {
        tbl[[s]] <- openxlsx::read.xlsx(xlsxFile = paste0(path, f), sheet = s)
      }
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
  if (!is.null(replace)) {
    lbl <- names(out)
    for (i in seq_along(replace))
      lbl <- gsub(names(replace)[i], replace[i], lbl)
    names(out) <- lbl
  }
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
