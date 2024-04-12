#'
#' Copy files for NEPS Survey Papers to local folder
#'


#' Setup the folder with the Quarto extension for the technical report
#'
#' @param path The path to the folder for the technical report.
#' @param ask A logical to ask for confirmation and some information (`TRUE`)
#' or provide the information as arguments without confirmation (`FALSE`).
#' @param sc The number of the starting cohort.
#' @param domain A two letter abbreviation for the competence domain.
#' @param study The number of the study.
#' @returns Returns `NULL`, invisibly.
#' @export
Setup <- function(path = getwd(), ask = TRUE, sc = 0, domain = "re",
                  study = "A000", wave = 0) {

  if (ask) {

    # Check path for setup
    prmt <- paste0("The technical report will be set up in ", path, ". ")
    if (dir.exists(path)) {
      prmt <- paste0(prmt, "Please note that existing files will be ",
                     "overwritten. ")
    } else {
      prmt <- paste0(prmt, "Please note that a new folder will be created. ")
    }
    prmt <- paste0(prmt, "Do you want to proceed?")
    prm <-
      utils::menu(
        c("Yes, set up the technical report.", "No, I' ve changed my mind."),
        title = prmt
      )
    if (prm %in% c(0, 2)) return(invisible())

    # Study number
    cat("\nPlease provide some information for the setup:\n\n")
    study <- base::readline("What is the number of the study (e.g., A104)? ")

    # Starting cohort
    sc <-
      utils::menu(
        c(1:6, 8),
        title = "What is the starting cohort of the study?"
      )
    sc <- ifelse(sc %in% c(1:6, 8), sc, 0)

    # Wave
    wave <- base::readline("What is the number of the study wave (e.g., 14)? ")

    # Domain
    domain <-
      utils::menu(
        c("Reading", "Mathematics"),
        title = "Which competence domain did you analyze?"
      )
    domain <- ifelse(domain %in% 2, "ma", "re")

  }

  # Validate input
  if (nchar(study) > 4) study <- toupper(substr(study, 1, 4))
  sc <- as.integer(substr(sc, 1, 1))
  if (is.na(sc)) sc <- 0
  wave <- as.integer(substr(wave, 1, 1))
  if (is.na(wave)) wave <- 0
  domain <- tolower(substr(domain, 1, 2))

  # Set variables
  domains <- c("re", "ma", "dc")
  domainname <- switch(
    domain,
    "ma" = "mathematics",
    "re" = "reading",
    "unknown"
  )
  nodomain <- domains[!(domains %in% domain)]
  scname <- switch(
    as.character(sc),
    "1" = "newborns",
    "2" = "kindergarten",
    "3" = "fifth grade",
    "4" = "ninth grade",
    "5" = "students",
    "6" = "adults",
    "8" = "fifth grade",
    "unknown"
  )

  # Copy extension
  if (!dir.exists(file.path(path, "_extensions")))
    dir.create(file.path(path, "_extensions"))
  file.copy(system.file("_extensions", package = "scaling"),
            path, recursive = TRUE, copy.date = TRUE)

  # Names for .bib and .qmd files
  name <- "TR"
  if (length(study) > 0L) name <- paste0(name, "_", study)
  if (length(sc) > 0L) name <- paste0(name, "_SC", sc)
  if (length(wave) > 0L) name <- paste0(name, "_W", wave)
  if (length(domain) > 0L) name <- paste0(name, "_", toupper(domain))

  # Copy bibtex file
  file.copy(system.file("skeletons/survey_paper.bib", package = "scaling"),
            path, copy.date = TRUE)
  file.rename(file.path(path, "survey_paper.bib"),
              file.path(path, paste0(name, ".bib")))

  # Copy Quarto file
  txt <- base::readLines(system.file("skeletons/survey_paper.qmd",
                                     package = "scaling"))
  txt <- gsub('bibliography: "survey_paper.bib"',
              paste0('bibliography: "', name, '.bib"'),
              txt)
  txt <- gsub("\\{\\{sc\\}\\}", sc, txt)
  txt <- gsub("\\{\\{wave\\}\\}", wave, txt)
  txt <- gsub("\\{\\{scname\\}\\}", scname, txt)
  txt <- gsub("\\{\\{domain\\}\\}", domainname, txt)
  txt <- gsub("\\{\\{Domain\\}\\}", tools::toTitleCase(domainname), txt)
  txt <- gsub(paste0("\\{\\{if", domain, "\\}\\}"), "", txt)
  txt <- gsub(paste0("\\{\\{\\/if", domain, "\\}\\}"), "", txt)
  txt <- paste0(txt, collapse = "\n")
  regexp <- paste0("\\{\\{if(",
                   paste(nodomain, collapse = "|"),
                   ")\\}\\}",
                   "(?:(?!\\{\\{/if(", paste(nodomain, collapse = "|"), ")\\}\\})(.|\\n))+",
                   "\\{\\{/if(",
                   paste(nodomain, collapse = "|"),
                   ")\\}\\}")
  txt <- gsub(regexp, "", txt, perl = TRUE)
  writeLines(txt, file.path(path, paste0(name, ".qmd")))

  message(paste0("\n The technical report has been setup in ", path, "."))

  return(invisible())

}


#' Update the Quarto extension in a folder
#'
#' @param path The path to the folder for the technical report.
#' @returns Returns `NULL`, invisibly.
#' @export
Update <- function(path = getwd()) {

  if (!dir.exists(file.path(path, "_extensions")))
    stop(paste0("Couldn't find the path ", file.path(path, "_extensions")))

  # Copy extension
  if (!dir.exists(file.path(path, "_extensions")))
    dir.create(file.path(path, "_extensions"))
  file.copy(system.file("_extensions", package = "scaling"),
            path, recursive = TRUE, copy.date = TRUE)

  message(paste0("\n The Quarto extension in ", file.path(path, "_extensions"),
                 " has been updated."))

  return(invisible())

}

