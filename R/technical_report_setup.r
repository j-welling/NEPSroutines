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
#' @param wave The number of the study wave.
#' @param overwrite A logical to overwrite existing files
#' @returns Returns `NULL`, invisibly.
#' @seealso
#' `vignette("technical_report", package = "NEPSroutines")`
#' @export
Setup <- function(path = getwd(), ask = TRUE, sc = 0, domain = "re",
                  study = "A000", wave = 0, overwrite = FALSE) {

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
        c(1:8),
        title = "What is the starting cohort of the study?"
      )

    # Wave
    wave <- base::readline("What is the number of the study wave (e.g., 14)? ")

    # Domain
    domain <-
      utils::menu(
        c("Reading", "Mathematics", "Digital competence"),
        title = "Which competence domain did you analyze?"
      )
    domain <- ifelse(domain %in% 2, "ma", ifelse(domain %in% 3, "dc", "re"))

  }

  # Validate input
  if (!grepl("^[0-9]+$", as.character(wave))) stop("wave must be numeric.")
  if (!domain %in% c("re", "ma", "dc")) stop("domain must be one of re, ma, dc.")
  if (!sc %in% 1:8) stop("sc must fall between 1 and 8.")

  # Set variables
  domains <- c("re", "ma", "dc")
  domainname <- switch(
    domain,
    "ma" = "mathematics",
    "re" = "reading",
    "dc" = "digital competence",
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
    "7" = "newborns",
    "8" = "fifth grade",
    "unknown"
  )

  # Copy extension
  if (!dir.exists(file.path(path, "_extensions")))
    dir.create(file.path(path, "_extensions"))
  res <- file.copy(system.file("_extensions", package = "NEPSroutines"),
                   path, recursive = TRUE, copy.date = TRUE, overwrite = overwrite)
  if (overwrite & !res) stop("Copying files to _extensions failed!")

  # Names for .bib and .qmd files
  name <- "TR"
  if (length(study) > 0L) name <- paste0(name, "_", study)
  if (length(sc) > 0L) name <- paste0(name, "_SC", sc)
  if (length(wave) > 0L) name <- paste0(name, "_W", wave)
  if (length(domain) > 0L) name <- paste0(name, "_", toupper(domain))

  # Copy bibtex file
  if (!file.exists(file.path(path, paste0(name, ".bib"))) | overwrite) {
    bibfile <- system.file("skeletons/survey_paper.bib", package = "NEPSroutines")
    if (!nzchar(bibfile)) stop("Could not find survey_paper.bib.")
    res <- file.copy(bibfile, path, copy.date = TRUE, overwrite = overwrite)
    if (overwrite & !res) stop("Copying survey_paper.bib failed!")
    file.rename(file.path(path, "survey_paper.bib"),
                file.path(path, paste0(name, ".bib")))
  }

  # Copy Quarto file
  template <- system.file("skeletons/survey_paper.qmd", package = "NEPSroutines")
  if (!nzchar(template)) stop("Could not find survey_paper.qmd template.")
  data <- list(name = name, sc = sc, wave = wave, scname = scname,
               domainname = domainname, domain = domain, nodomain = nodomain)
  txt <- SetupRenderTemplate(base::readLines(template), data)
  if (!file.exists(file.path(path, paste0(name, ".qmd"))) | overwrite)
    writeLines(txt, file.path(path, paste0(name, ".qmd")))

  # Copy additional files
  if (domain == "dc") {
    if (!file.exists(file.path(path, "Fig_DC_facets.png")) | overwrite) {
      fig_dc <- system.file("skeletons/Fig_DC_facets.png", package = "NEPSroutines")
      if (!nzchar(fig_dc)) stop("Could not find Fig_DC_facets.png!")
      res <- file.copy(fig_dc, path, copy.date = TRUE, overwrite = overwrite)
      if (overwrite & !res) stop("Copying Fig_DC_facets.png failed!")
    }
  }

  message(paste0("\n The technical report has been setup in ", path, "."))

  return(invisible())

}


#' Replace placeholders in template for Quarto report
#'
#' @param txt The template with placeholders
#' @param data A list with data to be included in the template
#' @returns The template without placeholders
SetupRenderTemplate <- function(txt, data) {

  txt <- gsub('bibliography: "survey_paper.bib"',
              paste0('bibliography: "', data$name, '.bib"'),
              txt)
  txt <- gsub("\\{\\{sc\\}\\}", data$sc, txt)
  txt <- gsub("\\{\\{wave\\}\\}", data$wave, txt)
  txt <- gsub("\\{\\{scname\\}\\}", data$scname, txt)
  txt <- gsub("\\{\\{domain\\}\\}", data$domainname, txt)
  txt <- gsub("\\{\\{Domain\\}\\}", tools::toTitleCase(data$domainname), txt)
  txt <- gsub(paste0("\\{\\{if", data$domain, "\\}\\}"), "", txt)
  txt <- gsub(paste0("\\{\\{\\/if", data$domain, "\\}\\}"), "", txt)
  txt <- paste0(txt, collapse = "\n")
  regexp <- paste0("\\{\\{if(",
                   paste(data$nodomain, collapse = "|"),
                   ")\\}\\}",
                   "(?:(?!\\{\\{/if(", paste(data$nodomain, collapse = "|"), ")\\}\\})(.|\\n))+",
                   "\\{\\{/if(",
                   paste(data$nodomain, collapse = "|"),
                   ")\\}\\}")
  txt <- gsub(regexp, "", txt, perl = TRUE)
  return(txt)

}


#' Update the Quarto extension in a folder
#'
#' @param path The path to the folder for the technical report.
#' @returns Returns `NULL`, invisibly.
#' @seealso
#' `vignette("technical_report", package = "NEPSroutines")`
#' @export
Update <- function(path = getwd()) {

  if (!dir.exists(file.path(path, "_extensions")))
    stop(paste0("Couldn't find the path ", file.path(path, "_extensions")))

  # Remove existing extension
  unlink(file.path(path, "_extensions", "neps-paper"), recursive = TRUE)

  # Copy extension
  dir.create(file.path(path, "_extensions", "neps-paper"))
  res <- file.copy(system.file("_extensions/neps-paper", package = "NEPSroutines"),
                   file.path(path, "_extensions"), recursive = TRUE, copy.date = TRUE)
  if (!res) stop("Copying files to _extensions failed!")

  message(paste0("\n The Quarto extension in ", file.path(path, "_extensions"),
                 " has been updated."))

  return(invisible())

}

