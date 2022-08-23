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
#' @param competence  character; name of tested competence for labelling the
#'   data
#' @param scores  data.frame; contains test scores (potentially with standard
#'   errors) as variables and persons as rows; additionally includes ID_t as a
#'   person identifier
#' @param score_name  character; name of the scores -- WITHOUT extension (e.g.,
#'   reg4 instead of reg4_sc1 or mag12 instead of mag12_sc1u)
#' @param mvs  named integer vector; contains user-defined missing values
#' @param items_labels  vector; named vector for each item used for labelling
#'   the data
#' @param filename character; indicates final name of SUF according to study
#' @param path_results character; indicates where SUF shall be saved
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#'
#' @export
create_suf <- function(resp, vars, select, competence,
                       scores = NULL, score_name = NULL,
                       mvs = NULL, items_labels = NULL,
                       filename = "suf", path_results = here::here('Results'),
                       save = TRUE, return = FALSE) {

  # Select items for SUF
  suf <- resp[, c("ID_t", vars$item[vars[[select]]])]

  # Determine unit non-response
  nr <- apply(suf, 1, function(x) { all(is.na(x) | x < 0) })

  # Add scores to data set
  if (!is.null(scores)) {
    suf <- merge(suf, scores, by = "ID_t", all.x = TRUE)
  }

  # Set unit non-response
  suf[nr, ] <- -56

  # Add labels to response items, ID_t, WLEs, and SEs
  suf <-
    set_labels(
      suf = suf,
      vars = vars,
      select = select,
      competence = competence,
      score_name = score_name,
      mvs = mvs,
      items_labels = items_labels
    )

  # Convert still present NA to -55 (not determinable)
  suf[is.na(suf)] <- -55

  # (save SUF in rds, sav and dta format)
  if (save) {
    save_suf(suf = suf, path_results = path_results, filename = filename)
    message("SUF successfully saved!")
  }

  # Return SUF
  if (return) {
    return(suf)
  }

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
#' @return suf with labels
#' @importFrom stats setNames
#' @export
set_labels <- function(suf, vars, select,
                       competence, score_name = NULL,
                       mvs = NULL, items_labels = NULL) {

  # Label for ID_t
  attr(suf$ID_t, "label") <- "Unique person identifier"

  # Set missing values
  if (is.null(mvs)) {
    mvs <- c("omitted" = -97,
             "not valid" = -95,
             "not reached" = -94,
             "test aborted" = -91,
             "unspecific missing" = -90,
             "not participated" = -56,
             "not determinable" = -55,
             "not administered" = -54,
             "reset response" = -21)
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
          labels = c("incorrect" = 0, "correct" = 1, mvs)
        )

    # Polytomous items
    } else {

      # Value labels
      maxK <- max(suf[[i]], na.rm = TRUE)
      vallbl <-
        setNames(
          object = seq(0, maxK),
          nm = paste0(seq(0, maxK), " correct of ", maxK)
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
    type <- substring(i, regexpr("(_sc[1-9])(a|b|u)?$", i))
    lbl <- switch(type,
                  "_sc1" = "Cross-sectional WLE",
                  "_sc2" = "Standard error of cross-sectional WLE",
                  "_sc1u" = "Longitudinal WLE",
                  "_sc2u" = "Standard error of longitudinal WLE",
                  "_sc3" = "Sum score",
                  "_sc3b" = "Sum score"
                )
    if (is.null(lbl)) next

    # Set score label
    suf[[paste0(score_name, type)]] <-
      haven::labelled_spss(
        suf[[i]],
        label = paste0(Hmisc::capitalize(competence), ": ", lbl),
        labels = mvs
      )

    if (i != paste0(score_name, type))
      suf[[i]] <- NULL

  }

  return(suf)

}



#' Save SUF in three formats
#'
#' @param suf  data.frame; SUF to be saved
#' @param filename  character; indicates final name of SUF according to study
#' @param path_results  character; indicates where SUF shall be saved
#' @noRd
save_suf <- function(suf, path_results, filename) {

  saveRDS(suf, file = paste0(path_results, "/", filename, ".rds"))
  haven::write_dta(suf, path = paste0(path_results, "/", filename, ".dta"))
  haven::write_sav(suf, path = paste0(path_results, "/", filename, ".sav"))

}
