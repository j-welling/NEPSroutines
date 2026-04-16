#' Create SUF
#'
#' @param resp  data.frame; contains item responses with items as variables and
#'   persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#'   polytomous responses with k categories; missing values (default -999 to -1)
#'   are coded as NA internally; additionally includes ID_t as a person identifier
#'   and all variables that are further defined in the function arguments
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select  character; indicates the logical variable in vars which
#'   contains the item names designated for the SUF (original scored items
#'   before the first IRT analyses)
#' @param competence  character; name of tested competence for labeling the
#'   data
#' @param scores  data.frame; contains test scores (potentially with standard
#'   errors) as variables and persons as rows; additionally includes ID_t as a
#'   person identifier; return object of functions c
#' @param score_name  character; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param items_labels  vector; named vector for each item used for labeling
#'   the data
#' @param suf_name character; name of suf (defaults to 'suf')
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param path character; indicates where SUF shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param warn  logical; whether to print warnings
#'
#' @export
create_suf <- function(resp, vars, select, competence, scores = NULL, mvs = NULL,
                       score_name = NULL, items_labels = NULL, suf_name = 'suf',
                       name_group = NULL, path = 'suf',
                       save = TRUE, return = TRUE, warn = TRUE) {

  # Test data
  NEPSroutines:::check_logicals(vars, "vars", select, warn = warn)
  NEPSroutines:::check_items(vars$item[vars[[select]]])

  # Select items for SUF
  suf <- resp[, c("ID_t", vars$item[vars[[select]]])]

  # Determine unit non-response
  nr <- apply(suf, 1, function(x) { all(is.na(x) | x < 0) })

  # Add scores to data set
  if (!is.null(scores)) suf <- merge(suf, scores, by = "ID_t", all.x = TRUE)

  # Set unit non-response
  suf[nr, ] <- -56

  # Add labels to response items, ID_t, WLEs, and SEs
  suf <-
    NEPSroutines:::set_labels(
      suf = suf,
      vars = vars,
      select = select,
      competence = competence,
      score_name = score_name,
      mvs = mvs,
      items_labels = items_labels,
      test = FALSE
    )

  # Convert still present NA to -55 (not determinable)
  suf[is.na(suf)] <- -55

  # (save SUF in rds, sav and dta format)
  if (save) {
    name <- NEPSroutines:::create_name(suf_name, name_group)
    NEPSroutines:::save_suf(suf = suf, path = path, filename = name)
    message("SUF successfully saved!")
  }

  # Return SUF
  if (return) return(suf)
}


#' Set labels of items in SUF
#'
#' @param suf data.frame; SUF to be (re)labelled
#' @param vars  data.frame; contains information about items with items as rows;
#'   includes variable 'item' containing item names; additionally includes all
#'   variables that are further defined in the function arguments
#' @param select character; indicates the logical variable in vars which
#'   contains the item names designated for the SUF (original scored items
#'   before the first IRT analyses)
#' @param competence character; name of tested competence for labelling the
#'   data
#' @param score_name character; name of the test scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param items_labels  vector; named vector for each item used for labelling
#'   the data
#' @param warn  logical; whether to print warnings
#' @param test  logical; whether to test data structure (should be set to TRUE)
#'
#' @return suf with labels
#' @importFrom stats setNames
#' @export
set_labels <- function(suf, vars, select, competence, score_name = NULL,
                       mvs = NULL, items_labels = NULL, warn = TRUE, test = TRUE) {

  # Test data
  if (test) {
    NEPSroutines:::check_logicals(vars, "vars", select, warn = warn)
    NEPSroutines:::check_items(vars$item[vars[[select]]])
  }

  # Label for ID_t
  attr(suf$ID_t, "label") <- "Target-ID"

  # Set missing values
  if (is.null(mvs)) {
    mvs <- c("filterbedingt fehlend" = -99,
             "Angabe verweigert" = -97,
             "unplausibler Wert" = -95,
             "nicht erreicht" = -94,
             "Befragung abgebrochen" = -91,
             "nicht spezifisch fehlend" = -90,
             "nicht teilgenommen" = -56,
             "nicht ermittelbar" = -55,
             "designbedingt fehlend" = -54,
             "Abwesenheit am Testtag" = -22,
             "L1-Test nicht administriert (keine Sprachkenntnis)" = -21,
             "L1-Test nicht administriert (Ergebnis Screening)" = -20)
  }

  # Label items
  j <- 1
  for (i in vars$item[vars[[select]]]) {

    # Variable label
    lbl <- paste0(Hmisc::capitalize(competence), ": Item ", j)
    if (!is.null(items_labels)) lbl <- items_labels[i]

    # Dichotomous items
    if (max(suf[[i]], na.rm = TRUE) == 1) {

      suf[[i]] <-
        haven::labelled_spss(
          suf[[i]],
          label = lbl,
          labels = c("nicht gel\u00f6st" = 0, "gel\u00f6st" = 1, mvs)
        )

    # Polytomous items
    } else {

      # Value labels
      maxK <- max(suf[[i]], na.rm = TRUE)
      vallbl <-
        setNames(
          object = seq(0, maxK),
          nm = paste0(seq(0, maxK), " von ", maxK, " Punkten")
        )

      # Label item
      suf[[i]] <-
        haven::labelled_spss(
          suf[[i]],
          label = lbl,
          labels = c(vallbl, mvs)
        )
    }

    j <- j + 1

  }

  # Labels test scores
  scores <- names(suf)[!(names(suf) %in% c("ID_t", vars$item[vars[[select]]]))]
  for (i in scores) {

    # Set label based on suffix of variable name
      type <- substring(i, regexpr("(_sc[1-9])(a|b|u|a_pb|b_pb|a_cb|b_cb|a_wb|b_wb)?$|pr_sc3|cs_sc8|bs_sc9|cs_sc9", i))
      domain <- switch(paste0(Hmisc::capitalize(competence)),
                       "Fr\u00fche Lesekompetenz"         = "Fr\u00fches Lesen",
                       "Orthografie"                      = "Orthografie",
                       "Grammatik"                        = "Grammatik",
                       "H\u00f6rverstehen"                = "H\u00f6ren",
                       "H\u00f6rverstehen in Russisch"    = "Russisch",
                       "H\u00f6rverstehen in T\u00fcrkisch" = "T\u00fcrkisch",
                       "Wortschatz"                       = "Wortschatz",
                       "Lesekompetenz"                    = "Lesen",
                       "Mathematische Kompetenz"          = "Mathematik",
                       "Naturwissenschaftliche Kompetenz" = "Naturwissenschaften",
                       "ICT Kompetenz"                    = "ICT",
                       "Lesekompetenz in Englisch"        = "Englisch"
      )

      if ((type == "_sc5" || type ==  "_sc6" ||
           type ==  "_sc5a" || type == "_sc5b" ||
           type == "_sc6a" || type ==  "_sc6b")
          && is.null(domain)) {
          warning("Check the spelling of the competence domain in the 'competence' argument
                    to get the correct label for the Procedural Metacognition scores.
                Choose the correct spelling from the list below:
                'Fr\u00fche Lesekompetenz', 'Lesekompetenz', 'Mathematische Kompetenz',
                'Naturwissenschaftliche Kompetenz', 'ICT Kompetenz', 'Orthografie',
                'Grammatik', 'H\u00f6rverstehen', 'H\u00f6rverstehen in Russisch',
                'H\u00f6rverstehen in T\u00fcrkisch',
                'Wortschatz', 'Lesekompetenz in Englisch'.")
      }

    lbl <- switch(type,
                  "_sc1"  = paste0(Hmisc::capitalize(competence), ": ", "WLE (korrigiert)"),
                  "_sc2"  = paste0(Hmisc::capitalize(competence), ": ", "Standardfehler des WLE (korrigiert)"),
                  "_sc1a" = paste0(Hmisc::capitalize(competence), ": ", "WLE (korrigiert)"),
                  "_sc2a" = paste0(Hmisc::capitalize(competence), ": ", "Standardfehler des WLE (korrigiert)"),
                  "_sc1b" = paste0(Hmisc::capitalize(competence), ": ", "WLE (korrigiert)"),
                  "_sc2b" = paste0(Hmisc::capitalize(competence), ": ", "Standardfehler des WLE (korrigiert)"),
                  "_sc1u" = paste0(Hmisc::capitalize(competence), ": ",  "WLE (unkorrigiert)"),
                  "_sc2u" = paste0(Hmisc::capitalize(competence), ": ", "Standardfehler des WLE (unkorrigiert)"),
                  "_sc3"  = paste0(Hmisc::capitalize(competence), ": ", "Summe"),
                  "_sc3a" = paste0(Hmisc::capitalize(competence), ": ", "Summe"),
                  "_sc3b" = paste0(Hmisc::capitalize(competence), ": ", "Summe"),
                  "pr_sc3" = paste0(Hmisc::capitalize(competence), ": ", "Anzahl administrierter \u00dcbungsitems"),
                  "_sc4"  = paste0(Hmisc::capitalize(competence), ": ", "Mittelwert"),
                  "_sc5"  = paste0("Prozedurale Metakognition (", domain, "): Differenzma\u00df"),
                  "_sc6"  = paste0("Prozedurale Metakognition (", domain, "): Anteil korrekt"),
                  "_sc5a_pb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (papierbasiert)"),
                  "_sc5b_pb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (papierbasiert)"),
                  "_sc5a_cb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (computerbasiert)"),
                  "_sc5a_cb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (computerbasiert)"),
                  "_sc5a_wb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (internetbasiert)"),
                  "_sc5a_wb" = paste0(Hmisc::capitalize(competence), ": ", "Summe (internetbasiert)"),
                  "_sc5a" = paste0("Prozedurale Metakognition (", domain, "): Differenzma\u00df f\u00fcr statische Items"),
                  "_sc6a" = paste0("Prozedurale Metakognition (", domain, "): Anteil korrekt f\u00fcr statische Items"),
                  "_sc5b" = paste0("Prozedurale Metakognition (", domain, "): Differenzma\u00df f\u00fcr interaktive Items"),
                  "_sc6b" = paste0("Prozedurale Metakognition (", domain, "): Anteil korrekt f\u00fcr interaktive Items"),
                  "_sc7"  = paste0(Hmisc::capitalize(competence), ": ", "Person in L1-Zielpopulation"),
                  "_sc8"  = paste0(Hmisc::capitalize(competence), ": ", "Testabbruch"),
                  "_sc9"  = paste0(Hmisc::capitalize(competence), ": ", "Anzahl administrierter \u00dcbungsitems"),
                  "cs_sc8"  = paste0(Hmisc::capitalize(competence), ": ", "Deckenset"),
                  "bs_sc9"  = paste0(Hmisc::capitalize(competence), ": ", "Bodenset"),
                  "cs_sc9"  = paste0(Hmisc::capitalize(competence), ": ", "Deckenset")
                  )

    if (is.null(lbl)) next

    # Set score label
    suf[[paste0(score_name, type)]] <-
      haven::labelled_spss(
        suf[[i]],
        label = lbl,
        labels = mvs
      )

    if (i != paste0(score_name, type))
      suf[[i]] <- NULL

  }

  # Return SUF
  return(suf)
}



#' Save SUF in three formats
#'
#' @param suf  data.frame; SUF to be saved
#' @param filename  character; indicates final name of SUF according to study
#' @param path  character; indicates where SUF shall be saved
#' @noRd
save_suf <- function(suf, path, filename) {

  NEPSroutines::check_folder(path = path)
  saveRDS(suf, file = paste0(path, "/", filename, ".rds"))
  haven::write_dta(suf, path = paste0(path, "/", filename, ".dta"))
  haven::write_sav(suf, path = paste0(path, "/", filename, ".sav"))

}
