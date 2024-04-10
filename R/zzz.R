#'
#' Run when loading the package
#'
.onLoad <- function(libname, pkgname) {

    # Default settings for tables in technical reports
    options(knitr.kable.NA = "")
    if (requireNamespace("flextable", quietly = TRUE)) {
        flextable::set_flextable_defaults(
            font.size = 12,
            font.family = "Calibri",
            text.align = "center",
            big.mark = ",",
            decimal.mark = ".",
            digits = 2,
            theme_fun = flextable::theme_apa,
            padding = 6,
            background.color = "#FFFFFF"
        )
    }

    return(invisible())

}
