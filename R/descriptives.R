#' Number of valid cases
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#'
#' @return   character string containing the numbers of valid and invalid cases.
#'
#' @export

n_valid <- function(resp, valid = NULL) {

    scaling:::check_logicals(resp, "resp", valid, warn = TRUE)

    if (!is.null(valid)) {
        n_inval <- sum(!resp[[valid]])
        n_val <- sum(resp[[valid]])
    } else {
        n_inval <- 0
        n_val <- nrow(resp)
        warn("No variable to identify (in)valid cases provided. Thus, all cases are counted as valid.")
    }

    message("There are ", n_val, " valid cases and ", n_inval, " invalid cases ",
    "in the dataset.")
}


#' Descriptives of continuous variables
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param digits  integer; number of decimals for rounding
#' @param print  logical; whether results shall be printed to console
#' @param return  logical; whether results shall be returned
#'
#' @return   list with descriptive statistics (n, mean, etc.) for each variable
#' defined in 'desc'.
#'
#' @export

desc_con <- function(resp, desc, valid = NULL, digits = 3,
                     print = TRUE, return = FALSE) {

    scaling:::check_variables(resp, "resp", c(desc, valid))

    resp <- scaling:::only_valid(resp, valid = valid)

    stats <- data.frame(sapply(resp[ , desc, drop = FALSE], function(x) {
             d <- data.frame(psych::describe(x))
             d[, -c(1:2)] <- format(round(d[, -c(1:2)], digits), nsmall = digits)
             d
             }))

    if (print) {
        message("\nSummary statistics of the continuous variables:\n")
        print(stats)
    }

    if (return) return(stats)
}


#' Descriptives of nominal variables
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param digits  integer; number of decimals for rounding of percentages
#' @param print  logical; whether results shall be printed to console
#' @param return  logical; whether results shall be returned
#'
#' @return   list containing a table with frequencies (absolute & relative)
#' for each variable defined in 'desc'.
#'
#' @export

desc_nom <- function(resp, desc, valid = NULL, digits = 2,
                     print = TRUE, return = FALSE) {

    scaling:::check_variables(resp, "resp", c(desc, valid))

    resp <- scaling:::only_valid(resp, valid = valid)

    descriptives <- list()
    descriptives$frequency_abs <- scaling:::desc_abs(
        resp,
        desc = desc,
        valid = valid,
        warn = FALSE
    )
    descriptives$frequency_perc_NA <- scaling:::desc_perc(
        resp,
        desc = desc,
        valid = valid,
        warn = FALSE,
        useNA = 'always',
        digits = digits
    )
    descriptives$frequency_perc_noNA <- scaling:::desc_perc(
        resp,
        desc = desc,
        valid = valid,
        warn = FALSE,
        useNA = 'no',
        digits = digits
    )

    if (print) {
        message("\nSample size by groups:\n")
        print(descriptives$frequency_abs)

        message("\nFrequency of groups including missing values:\n")
        print(descriptives$frequency_perc_NA)

        message("\nFrequency of groups excluding missing values:\n")
        print(descriptives$frequency_perc_noNA)
    }

    if (return) return(descriptives)
}


#' Show frequency (absolute) of answers of list of variables
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no parameter 'valid' provided
#'
#' @return   list (table) with  absolute frequencies for each variable in 'desc'.
#'
#' @export

desc_abs <- function(resp, desc, valid = NULL, warn = TRUE) {

    scaling:::check_variables(resp, "resp", c(desc, valid))

    resp <- scaling:::only_valid(resp, valid = valid, warn = warn)

    sapply(resp[ , desc, drop = FALSE], function(x) {table(x, useNA = "always")})
}


#' Show frequency (relative) of answers of list of variables
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @param valid  string; defines name of logical variable in resp that indicates
#' (in)valid cases
#' @param warn  logical; whether to warn if no parameter 'valid' provided
#' @param useNA  string; "no" if NAs should not be included in table, "always"
#' (default) if NAs should be included in table
#' @param digits  integer; number of decimals for rounding
#'
#' @return   list (table) with relative frequencies for each variable in 'desc'.
#'
#' @export

desc_perc <- function(resp, desc, valid = NULL, warn = TRUE, useNA = 'always',
                      digits = 2) {

    scaling:::check_variables(resp, "resp", c(desc, valid))

    resp <- scaling:::only_valid(resp, valid = valid, warn = warn)

    sapply(resp[ , desc, drop = FALSE], function(x) {
      d <- round(prop.table(table(x, useNA = useNA))*100, digits)
      paste0(d, "%")
      })
}


#' Show attributes of selected variables
#'
#' @param resp  data.frame; contains sociodemographic and booklet variables
#' @param desc  character vector; contains the name(s) of the variable(s) that
#' shall be used in analysis
#' @export

show_attributes <- function(resp, desc) {

    scaling:::check_variables(resp, "resp", desc)

    for (var in desc) {
        message("\nThe attributes for variable ", var, " are:\n")
        print(attributes(resp[[var]])$labels)
    }
}


#' Table with sample size by groups
#'
#' @param resp  data.frame; contains grouping variable
#' @param grouping_variable string; defines name of variable in resp that identifies groups
#' @param labels named character vector; links a label each value of groups
#' (e.g. labels = c(easy = 1, difficult = 2))
#' @param save  logical; whether to save the table in Excel
#' @param overwrite logical; whether to overwrite an existing table with the same name
#' @param path string; defines path for saving the table
#' @param name_group  string; defines name of group used in analysis (e.g. 'easy')
#'
#' @return data.frame with sample size by groups.
#' @export

sample_by_group <- function(resp, grouping_variable, labels = NULL, save = FALSE,
                            overwrite = FALSE, path = here::here("Tables"),
                            name_group = NULL) {

    # Check variable
    scaling:::check_variables(resp, "resp", grouping_variable)

    # Create table with results
    df <- as.data.frame.AsIs(table(resp[[grouping_variable]]))
    df <- rbind(df,sum(df))
    names(df) <- 'N'
    rownames(df)[nrow(df)] <- c("Total")

    # Add labels as row names
    if(!is.null(labels) | !is.null(attributes(vars[[grouping_variable]])$labels)) {
        lbls <- scaling:::create_ifelse(
            is.null(labels),
            attributes(vars[[grouping_variable]])$labels,
            labels
        )
        for (v in seq(nrow(df)-1)) {
            rownames(df)[v] <- names(which(rownames(df)[v] == lbls))
        }
    }

    # Convert results
    res <- data.frame(t(df))

    # Save results
    if (save) {
        name <- scaling:::create_name(
            paste0("sample.size.by.", grouping_variable),
            name_group,
            ".xlsx"
        )
        scaling:::save_table(
            res,
            filename = name,
            path = path,
            overwrite = overwrite,
            show_rownames = FALSE
        )
    }

    # Return results
    return(res)
}


#' Table with item properties by groups
#'
#' @param vars data.frame; contains information about the competence items
#' @param select  string; defines name of logical variable in vars that indicates
#' which items to use for the analysis
#' @param grouping  character vector; contains for each group a name of a logical
#' variable in vars that indicates to which group belongs a person or an item
#' @param properties character vector; defines name(s) of variable(s) in vars
#' that identify item properties
#' @param labels list; may contain for each variable defined in argument 'properties'
#' a named character vector with the labels for the variable categories, the name
#' of the vector must be identical with the name identified in 'properties'
#' (e.g. sex = c(male = 1, female = 2))
#' @param name_grouping  string; name of the grouping variable (e.g. 'test version')
#' @param save logical; whether to save the table in Excel
#' @param overwrite logical; whether to overwrite an existing table with the same name
#' @param path string; defines path for saving the table
#' @param name_group  string; defines name of group used in analysis (e.g. 'settingA')
#' @param warn logical; whether to print warnings (should be better set to true)
#'
#' @return data.frame with item properties by groups.
#' @export

props_by_group <- function(vars, select, groups, properties, labels = NULL,
                           name_grouping = 'version', save = FALSE,
                           overwrite = FALSE, path = here::here("Tables"),
                           name_group = NULL, warn = TRUE) {

    # Check variables
    scaling:::check_logicals(vars, "vars", c(select, groups), warn = warn)
    scaling:::check_variables(vars, "vars", properties)

    # Select only necessary items and check for duplicates
    vars <- subset(vars, vars[[select]])
    scaling:::check_items(vars$item)

    res <- list()
    if(is.null(labels)) labels <- list()

    for (props in properties) {

      # Create empty dataframe
      df <- data.frame(matrix(NA, length(unique(vars[[props]])) + 1, length(groups)))
      names(df) <- groups

      # Add property labels as row names
      if(!is.null(labels[[props]]) | !is.null(attributes(vars[[props]])$labels)) {
          lbls <- scaling:::create_ifelse(
              is.null(labels[[props]]),
              attributes(vars[[props]])$labels,
              labels[[props]]
          )
          for (v in seq(nrow(df)-1)) {
              rownames(df)[v] <- names(which(rownames(df)[v] == lbls))
          }
      }

      rownames(df)[nrow(df)] <- "Total number of items"

      # Fill dataframe
      for (g in groups){
        N <- as.data.frame.AsIs(table(vars[[props]][vars[[g]]]))
        N <- rbind(N,sum(N))
        df[[g]]<- N[seq(nrow(N)), ]
      }

      res[[props]] <- df
    }

    # Save results
    if (save) {
        name <- scaling:::create_name(
          paste0("item.properties.by.", name_grouping),
          name_group,
          ".xlsx"
        )
        scaling:::save_table(
            res,
            filename = name,
            path = path,
            overwrite = overwrite,
            show_rownames = TRUE
        )
    }

    # Return results
    return(res)
}
