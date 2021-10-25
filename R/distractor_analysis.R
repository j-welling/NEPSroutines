#' Distractor analysis
#'
#' @param resp      data.frame with all respondents as rows and all variables as columns,
#'                  needs to contain at least all competence variables
#' @param vars      data.frame with all variables as rows with at least following columns:
#'                  items: contains all item names (both scored and unscored)
#'                  scored: logical or binary integer, identifies all scored items
#'                  type: characer vector, identifies type of each item (e.g. "MC")
#'                  correct_response: integer, identifies correct response for each item
#' @param items     character. contains name of variable (boolean) in vars that
#'                    indicates which items to use for analysis.
#' @param valid     character string. defines name of boolean variable in dat,
#'                    indicating (in)valid cases.
#'
#' @return list of one data frame per item containing item-total correlations
#'   for each possible response; correct response is marked with an *
#'
#' @importFrom stats cor
#' @importFrom rlang .data
#' @export

distractor_analysis <- function(resp, vars, items, valid = NULL) {

    # prepare data
    resp <- only_valid(resp, valid = valid)
    MC <- dplyr::select(
        dplyr::filter(vars, vars$type == 'MC' & vars$raw == TRUE),
        .data$items, .data$correct_response
    )
    scored <- vars$items[vars[[items]]]
    var_names <- unique(c(MC$items, scored))
    resp <- convert_mv(resp = resp, variables = var_names)

    # Sum score across all items
    resp$score <- rowMeans(resp[, scored], na.rm = TRUE)

    # Correlations with distractors (for MC items)
    dis <- list()

    for (item in MC$items) {

        # Create for each item a dataframe with row for each response option
        dis[[item]] <- as.data.frame(table(resp[, item]),
                                     responseName = "N",
                                     stringsAsFactors = FALSE)
        names(dis[[item]])[1] <- item
        dis[[item]]$rit <- NA

        cscore <- resp$score - resp[, paste0(item, "_c")] / length(scored) # corrected total score

        # Correlation between each response option and corrected total score
        for (s in seq_len(nrow(dis[[item]]))) {

            # Create score vectors and compute correlation
            iscore <- ifelse(resp[, item] == dis[[item]][[item]][s], 1, 0)
            dis[[item]]$rit[s] <- round(cor(iscore, cscore, use = "complete.obs"), 2)
        }

        # Label correct response with *
        dis[[item]][[item]] <- ifelse(
            dis[[item]][[item]] == MC$correct_response[MC$items == item],
            paste0("*", dis[[item]][[item]]),
            dis[[item]][[item]]
        )
    }

    return(dis)

}

#' Summary of distractor analysis
#'
#' @param distractors return object of distractor_analysis() function (list of
#'   data frames containing item-total correlations for each item)
#' @param print logical indicating whether summary is printed to the console;
#'   prints minimum and maximum for distractors and correct responses as well as
#'   the respective item names
#' @param save_at character string; indicates the folder location where the
#'   summaries are stored on the hard drive (first, the distractor object is
#'   saved unchanged as Rdata; second, the distractor object is saved unchanged
#'   as an excel file with one sheet per item distractor analysis; third, one
#'   table for the distractors and one table for the correct responses is
#'   saved as an excel file with one sheet per table). Please note that the
#'   path is relative to the current working path set by here::i_am(). Defaults
#'   to NULL (not stored)
#' @param overwrite boolean; indicates whether to overwrite existing file when saving table.
#'
#' @return list of data frames
#'           correct : item-total correlations for correct responses
#'           distractor : item-total correlations for distractors
#'
#' @export

distractor_summary <- function(distractors, print = TRUE,
                                        save_at = NULL, overwrite = FALSE) {
    # data.frames containing information for distractors and correct responses,
    # respectively

    # Gather results in a name and a results vector
    nam <- unlist(lapply(distractors, function(x) {x[, 1]}))
    num <- unlist(lapply(distractors, function(x) {x$N}))
    cor <- unlist(lapply(distractors, function(x) {x$rit}))

    # Create two dataframes, one each for correct responses and distractors
    res <- data.frame(name = nam, N = num, corr = cor)
    sel <- substr(res$name, 1, 1) == "*"

    rc <- res[sel,]
    rd <- res[!sel,]


    if (print) {
        # short summary containing minimum and maximum for distractors and
        # correct responses as well as respective item names [print to console!]
        message("Item-total correlation for correct response: \nMin. = ",
                rc[which(rc$corr == min(rc$corr)), "corr"],
                " (", rownames(rc[which(rc$corr == min(rc$corr)),]),
                "), Max. = ", rc[which(rc$corr == max(rc$corr)), "corr"], " (",
                rownames(rc[which(rc$corr == max(rc$corr)),]), ")")
        message("Item-total correlation for distractor: \nMin. = ",
                rd[which(rd$corr == min(rd$corr)), "corr"],
                " (", rownames(rd[which(rd$corr == min(rd$corr)),]),
                "), Max. = ", rd[which(rd$corr == max(rd$corr)), "corr"], " (",
                rownames(rd[which(rd$corr == max(rd$corr)),]), ")")

        # auffällige Distraktoren / korrekte Antworten anzeigen
        message("\n",
                "Items with problematic item-total correlations for correct ",
                "response (r < 0.2): \n",
                paste0(rownames(rc[which(rc$corr < 0.2),]), collapse = ", "))
        message("\n",
                "Items with problematic item-total correlations for ",
                "distractors (r > 0.05): \n",
                paste(rownames(rd[which(rd$corr > 0.05),]), collapse = ", "),
                "\n")
    }

    if (!is.null(save_at)) {

        check_folder(save_at)

        save(distractors, file = here::here(paste0(save_at, "/distractors.Rdata")))
        openxlsx::write.xlsx(
            distractors,
            file = here::here(paste0(save_at, "/distractor_analysis.xlsx")),
            showNA = FALSE, overwrite = overwrite
        )
    }

    # Return list with results
    return(list(correct = rc, distractor = rd))
}
