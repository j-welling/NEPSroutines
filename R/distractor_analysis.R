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
#' includes variables 'item' (character) containing item names,
#'                    'raw' (logical) indicating unscored items,
#'                    'type' (character) containing item type (e.g. MC),
#'                    'correct_response' (integer) containing the correct
#'                      response for each item,
#' and all variables that are further defined in the function arguments
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param select_raw string; defines name of logical variable in vars that
#' indicates all raw items that shall be used for the distractor analysis
#' @param select_score string; defines name of logical variable in vars that
#' indicates all items that shall be used to caculate total score
#' @param correct string; defines name of variable in vars that contains the
#' correct responses to the items
#' @param print  logical; whether results shall be printed to console
#' @param save  logical; whether results shall be saved to hard drive
#' @param return  logical; whether results shall be returned
#' @param path_results string; indicates the folder location where the summaries
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param path_table string; indicates the folder location where the tables
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param overwrite logical; whether to overwrite existing file when saving table
#' @param digits  integer; number of decimals for rounding
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @return list of one data frame per item containing item-total correlations
#'   for each possible response; correct response is marked with an *
#'
#' @importFrom stats cor
#' @importFrom rlang .data
#' @export

dis_analysis <- function(resp, vars, valid = NULL, mvs = NULL,
                         select_raw, select_score = 'dich',
                         correct = 'correct_response', name_group = NULL,
                         save = TRUE, print = TRUE, return = FALSE,
                         path_results = here::here('Results'),
                         path_table = here::here('Tables'),
                         overwrite = FALSE, digits = 3, warn = TRUE) {

    # Create list for results
    distractors <- list()

    # Conduct distratcor analysis
    distractors$analysis <- scaling:::conduct_dis_analysis(
      resp = resp,
      vars = vars,
      select_raw = select_raw,
      select_score = select_score,
      correct = correct,
      valid = valid,
      mvs = mvs,
      warn = warn,
      save = FALSE
    )

    distractors$summary <- scaling:::dis_summary(
        distractors$analysis,
        digits = digits,
        save = FALSE
    )

    # Print results
    if (print) scaling:::print_dis_summary(distractors$summary)

    # Save results
    if (save) {
        name <- scaling::create_ifelse(
            is.null(name_group),
            paste0("distractors"),
            paste0("distractors_", name_group)
        )
        scaling::save_results(
            distractors,
            filename = paste0(name, ".rds"),
            path = path_results
        )
        scaling::save_table(
            distractors$summary,
            overwrite = overwrite,
            filename = paste0(name, "_summary.xlsx"),
            path = path_table
        )
        scaling::save_table(
            distractors$analysis,
            overwrite = overwrite,
            filename = paste0(name, "_items.xlsx"),
            path = path_table
        )
    }

    # Return results
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
#' includes variables 'item' (character) containing item names,
#'                    'dich' (logical) indicating all dichotomously scored items
#'                    'raw' (logical) indicating all unscored items,
#'                    'type' (character) containing item type (e.g. MC),
#'                    'correct_response' (integer) containing the correct
#'                      response for each item,
#' and all variables that are further defined in the function arguments
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param mvs  named integer vector; contains user-defined missing values
#' @param select_raw string; defines name of logical variable in vars that
#' indicates all raw items that shall be used for the distractor analysis
#' @param select_score string; defines name of logical variable in vars that
#' indicates all items that shall be used to caculate total score
#' @param correct string; defines name of variable in vars that contains the
#' correct responses to the items
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; indicates the folder location where the results
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param warn  logical; whether to print warnings (should be set to TRUE)
#'
#' @export

conduct_dis_analysis <- function(resp, vars, valid = NULL,
                                 select_raw, select_score = 'dich',
                                 correct = 'correct_response',
                                 mvs = NULL, save = TRUE, name_group = NULL,
                                 path = here::here('Results'),
                                 warn = TRUE) {
    # Test data
    scaling::check_logicals(vars, "vars", c(select_raw, select_score), warn = warn)
    scaling::check_variables(vars, "vars", correct)
    scaling::check_variables(resp, "resp", valid)
    scaling::check_items(vars$item[vars[[select_raw]]])
    scaling::check_items(vars$item[vars[[select_score]]])

    if (warn) scaling::is_null_mvs_valid(mvs = mvs, valid = valid)

    # prepare data
    raw_items <- vars$item[vars[[select_raw]]]
    items_for_score <- vars$item[vars[[select_score]]]
    vars$keep_items <- vars[[select_raw]] | vars[[select_score]]
    resp <- scaling::prepare_resp(
      resp,
      vars,
      select = 'keep_items',
      use_only_valid = TRUE,
      valid = valid,
      convert = TRUE,
      mvs = mvs,
      warn = FALSE
    )

    # Check whether all items to be used for score generation are dichotomous
    scaling::check_numerics(resp, "resp", items_for_score, check_invalid = TRUE, dich = TRUE)

    # Sum score across all items
    resp$score <- rowMeans(resp[ , items_for_score], na.rm = TRUE)

    # Correlations with distractors (for unscored items)
    dis <- list()

    for (item in raw_items) {

        # Create for each item a dataframe with row for each response option
        dis[[item]] <- as.data.frame(
          table(resp[, item]),
          responseName = "N",
          stringsAsFactors = FALSE
        )
        names(dis[[item]])[1] <- item
        dis[[item]]$rit <- NA

        # Create corrected total score
        item_scored <- ifelse(
          resp[[item]] == vars[[correct]][vars$item == item], 1, 0
        )
        cscore <- resp$score - item_scored / length(items_for_score)

        # Correlation between each response option and corrected total score
        for (s in seq_len(nrow(dis[[item]]))) {

            # Create score vectors and compute correlation
            iscore <- ifelse(resp[, item] == dis[[item]][[item]][s], 1, 0)
            dis[[item]]$rit[s] <- round(cor(iscore, cscore, use = "complete.obs"), 2)
        }

        # Label correct response with *
        dis[[item]][[item]] <- ifelse(
            dis[[item]][[item]] == vars[[correct]][vars$item == item],
            paste0("*", dis[[item]][[item]]),
            dis[[item]][[item]]
        )
    }

    # Save results
    if (save) {
        name <- scaling::create_ifelse(
            is.null(name_group),
            paste0("distractors.rds"),
            paste0("distractors_", name_group, ".rds")
        )
        scaling::save_results(dis, filename = name, path = path)
    }

    # Return results
    return(dis)
}


#' Summary of distractor analysis
#'
#' @param distractors return object of dis_analysis() function (list of
#'   data frames containing item-total correlations for each item)
#' @param digits  integer; number of decimals for rounding
#' @param save  logical; whether results shall be saved to hard drive
#' @param path  string; indicates the folder location where the results
#' are stored on the hard drive; please note that the path is relative to the
#' current working path set by here::i_am()
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#' @param overwrite logical; whether to overwrite existing file when saving table
#'
#' @return list of data frames
#'           correct : item-total correlations for correct responses
#'           distractor : item-total correlations for distractors
#'
#' @export
dis_summary <- function(distractors, digits = 3, save = TRUE, name_group = NULL,
                        path = here::here('Tables'), overwrite = FALSE) {
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
    desc <- data.frame(
      correct = round(unlist(psych::describe(rc[, 3])), digits),
      distract = round(unlist(psych::describe(rd[, 3])), digits
    )
    )[c(3:5, 8:9), ]

    # Create list with results
    results <- list(correct = rc, distractor = rd, descriptives = desc)

    # Save results
    if (save) {
        name <- scaling::create_ifelse(
            is.null(name_group),
            paste0("distractors_summary.xlsx"),
            paste0("distractors_summary_", name_group, ".xlsx")
        )
        scaling::save_table(results, overwrite = overwrite, filename = name, path = path)
    }

    # Return list with results
    return(results)
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

    # Correct responses
    corr_min <- rc[which(rc$corr == min(rc$corr)), "corr"]
    corr_min_names <- rownames(rc[which(rc$corr == min(rc$corr)),])
    corr_max <- rc[which(rc$corr == max(rc$corr)), "corr"]
    corr_max_names <- rownames(rc[which(rc$corr == max(rc$corr)),])
    corr_med <- rc[which(rc$corr == median(rc$corr)), "corr"]
    corr_med_names <- rownames(rc[which(rc$corr == median(rc$corr)),])

    message("\nItem-total correlation for correct response: ",
            "\nMin. = ",  corr_min, " (", paste(corr_min_names, collapse = ", "), ")",
            "\nMax. = ",  corr_max, " (", paste(corr_max_names, collapse = ", "), ")",
            "\nMd. = ",  median(rc$corr), " (", paste(corr_med_names, collapse = ", "), ")")

    # Distractors
    dist_min <- rd[which(rd$corr == min(rd$corr)), "corr"]
    dist_min_names <- rownames(rd[which(rd$corr == min(rd$corr)),])
    dist_max <- rd[which(rd$corr == max(rd$corr)), "corr"]
    dist_max_names <- rownames(rd[which(rd$corr == max(rd$corr)),])
    dist_med <- rd[which(rd$corr == median(rd$corr)), "corr"]
    dist_med_names <- rownames(rd[which(rd$corr == median(rd$corr)),])

    message("\nItem-total correlation for distractor: ",
            "\nMin. = ",  dist_min, " (", paste(dist_min_names, collapse = ", "), ")",
            "\nMax. = ",  dist_max, " (", paste(dist_max_names, collapse = ", "), ")",
            "\nMd. = ",  median(rd$corr), " (", paste(dist_med_names, collapse = ", "),
            ")\n")

    # Auffällige Distraktoren und korrekte Antworten anzeigen
    problematic_correct_responses <- rownames(rc[which(rc$corr < 0.2),])
    problematic_distractors <- rownames(rd[which(rd$corr > 0.05),])

    if (length(problematic_correct_responses) >= 1) {
      message("\nItems with problematic item-total correlations for correct ",
            "response (r < 0.2): \n",
            paste0(problematic_correct_responses, collapse = ", "))
    }

    if (length(problematic_distractors) >= 1) {
      message("\nItems with problematic item-total correlations for ",
              "distractors (r > 0.05): \n",
              paste0(problematic_distractors, collapse = ", "), "\n")
    }
}
