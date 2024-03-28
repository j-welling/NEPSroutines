
# if the package is stored on a remote server, do:-----------------------------
# https://stackoverflow.com/questions/40530968/overwriting-namespace-and-rd-with-roxygen2
# digest::digest

# Vorbereitungen

## Alte Version löschen
remove.packages("scaling")

## Pakete aktualisieren
update.packages()
# Wenn man einzelne Pakete über diesen Befehl nicht updaten kann, dann R neu starten,
# ggf. auch PC neu starten & per install.packages() manuell installieren


# Falls sich Dokumentation aktualisiert hat
mydigest <- function(object,
                     algo = c("md5", "sha1", "crc32", "sha256",
                              "sha512", "xxhash32", "xxhash64", "murmur32"),
                     serialize = TRUE,
                     file = FALSE, length = Inf, skip = "auto", ascii = FALSE,
                     raw = FALSE, seed = 0,
                     errormode = c("stop", "warn", "silent"))
{
    file.access <- R.utils::fileAccess
    algo <- match.arg(algo)
    errormode <- match.arg(errormode)
    .errorhandler <- function(txt, obj = "", mode = "stop") {
        if (mode == "stop") {
            stop(txt, obj, call. = FALSE)
        }
        else if (mode == "warn") {
            warning(txt, obj, call. = FALSE)
            return(invisible(NA))
        }
        else {
            return(invisible(NULL))
        }
    }
    if (is.infinite(length)) {
        length <- -1
    }
    if (is.character(file) && missing(object)) {
        object <- file
        file <- TRUE
    }
    if (serialize && !file) {
        object <- if ("nosharing" %in% names(formals(base::serialize)))
            base::serialize(object, connection = NULL, ascii = ascii,
                            nosharing = TRUE)
        else base::serialize(object, connection = NULL, ascii = ascii)
        if (any(!is.na(pmatch(skip, "auto")))) {
            if (ascii) {
                skip <- which(object[1:30] == as.raw(10))[4]
            }
            else {
                skip <- 14
            }
        }
    }
    else if (!is.character(object) && !inherits(object, "raw")) {
        return(.errorhandler(paste("Argument object must be of type character",
                                   "or raw vector if serialize is FALSE"),
                             mode = errormode))
    }
    if (file && !is.character(object))
        return(.errorhandler("file=TRUE can only be used with a character object",
                             mode = errormode))
    algoint <- switch(algo, md5 = 1, sha1 = 2, crc32 = 3, sha256 = 4,
                      sha512 = 5, xxhash32 = 6, xxhash64 = 7, murmur32 = 8)
    if (file) {
        algoint <- algoint + 100
        object <- path.expand(object)
        if (!file.exists(object)) {
            return(.errorhandler("The file does not exist: ",
                                 object, mode = errormode))
        }
        if (!isTRUE(!file.info(object)$isdir)) {
            return(.errorhandler("The specified pathname is not a file: ",
                                 object, mode = errormode))
        }
        if (file.access(object, 4)) {
            return(.errorhandler("The specified file is not readable: ",
                                 object, mode = errormode))
        }
    }
    if (is.character(skip))
        skip <- 0
    val <- .Call(digest_impl, object, as.integer(algoint), as.integer(length),
                 as.integer(skip), as.integer(raw), as.integer(seed))
    return(val)
}

library(digest)
R.utils::reassignInPackage('digest', 'digest', mydigest)

devtools::document(pkg = "./scaling")

# Installieren des Pakets
devtools::install(pkg = "./scaling")

# CRAN check brauchen wir eigentlich nicht, da wir Paket nicht in CRAN hochladen
# Check ist aber trotzdem nützlich, da es auf Fehler und Warnmeldungen testet
devtools::check(pkg = "scaling", args = c('--as-cran'))

# Erstellen des tar.gz-Verzeichnisses
pkgbuild::build(path = "./scaling", dest_path = ".")
