#' Distractor analysis
#'
#' Checks how the distractors in multiple choice items functioned.
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variables 'items' (character) containing item names,
#'                    'raw' (logical) indicating unscored items,
#'                    'type' (character) containing item type (e.g. MC),
#'                    'correct_response' (integer) containing the correct
#'                      response for each item,
#' and all variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results string; indicates the folder location where the summaries
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param path_table string; indicates the folder location where the tables
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return list of one data frame per item containing item-total correlations
#'   for each possible response; correct response is marked with an *
#'
#' @importFrom stats cor
#' @importFrom rlang .data
#' @export

dis_analysis <- function(resp, vars, items, valid = NULL,
                         save = TRUE, print = TRUE, return = FALSE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         overwrite = FALSE) {

    distractors <- list()
    distractors$analysis <- conduct_dis_analysis(resp = resp,
                                                 vars = vars,
                                                 items = items,
                                                 valid = valid)
    distractors$summary <- dis_summary(distractors$analysis)

    if (print) print_dis_summary(distractors$summary)
    if (save) {
        save_results(distractors,
                     filename = "distractors.rds", path = path_results)
        save_table(distractors$analysis, overwrite = overwrite,
                   filename = "distractors.xlsx", path = path_table)
    }

    if (return) return(distractors)
}

#' Conduct distractor analysis
#'
#' Calculates the item-rest correlations of distractor and correct responses
#'
#' @param resp  data.frame; contains item responses with items as variables and
#' persons as rows; y in {0, 1} for binary data and y in {0, 1, ... k-1} for
#' polytomous responses with k categories; missing values (default -999 to -1)
#' are coded as NA internally; additionally includes ID_t as a person identifier
#' and all variables that are further defined in the function arguments
#' @param vars data.frame; contains information about items with items as rows;
#' includes variables 'items' (character) containing item names,
#'                    'raw' (logical) indicating unscored items,
#'                    'type' (character) containing item type (e.g. MC),
#'                    'correct_response' (integer) containing the correct
#'                      response for each item,
#' and all variables that are further defined in the function arguments
#' @param items  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @export
conduct_dis_analysis <- function(resp, vars, items, valid = NULL) {
    # prepare data
    resp <- only_valid(resp, valid = valid)
    MC <- dplyr::select(
        dplyr::filter(vars, vars$type == 'MC' & vars$raw == TRUE),
        .data$items, .data$correct_response
    )
    scored <- vars$items[vars[[items]]]
    var_names <- unique(c(MC$items, scored))
    resp <- convert_mv(resp = resp, items = var_names)

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

        cscore <- resp$score - resp[, paste0(item, "_c")] / sum(vars$raw)#length(scored) # corrected total score

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
#' @param distractors return object of dis_analysis() function (list of
#'   data frames containing item-total correlations for each item)
#'
#' @return list of data frames
#'           correct : item-total correlations for correct responses
#'           distractor : item-total correlations for distractors
#'
#' @export
dis_summary <- function(distractors) {
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

    # Return list with results
    return(list(correct = rc, distractor = rd))
}

#' Print summary of distractor analyses
#'
#' @param dist_sum return object of dis_summary()
#' @export
print_dis_summary <- function(dist_sum) {
        rc <- dist_sum$correct
        rd <- dist_sum$distractor
        # short summary containing minimum and maximum for distractors and
        # correct responses as well as respective item names [print to console!]
        message("\nItem-total correlation for correct response: \nMin. = ",
                rc[which(rc$corr == min(rc$corr)), "corr"],
                " (", rownames(rc[which(rc$corr == min(rc$corr)),]),
                "), Max. = ", rc[which(rc$corr == max(rc$corr)), "corr"], " (",
                rownames(rc[which(rc$corr == max(rc$corr)),]), ")")
        message("\nItem-total correlation for distractor: \nMin. = ",
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
