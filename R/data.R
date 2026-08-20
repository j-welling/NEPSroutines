#' Test data: Dichotomous NEPS data without booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 15
#' dichotomous items presented in a single booklet.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 1,010 rows and 35 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{grk1..._c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{grk1...}{Unscored item responses for simple multiple-choice items
#'   with values of 1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#' }
#'
#' `vars` is a data frame with 30 rows and 8 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{pos}{A number with the item position in the test.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for simple multiple-choice items.}
#' }
"ex1"


#' Test data: Polytomous NEPS data without booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 13
#' dichotomous and 4 polytomous items presented in a single booklet.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 1,550 rows and 36 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{rotation}{Test position with 0 = first and 1 = second.}
#'   \item{mag12XXX_c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{mag12XXX}{Unscored item responses for simple multiple-choice items
#'   with values of 1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#' }
#'
#' `vars` is a data frame with 30 rows and 11 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{poly}{A logical identifying scored polytomous (`TRUE`) and other
#'   (`FALSE`) items.}
#'   \item{mixed}{A logical identifying scored (`TRUE`) and other (`FALSE`)
#'   items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{content}{The four content domains of the math test.}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{pos}{A number with the item position in the test.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for simple multiple-choice items.}
#' }
"ex2"


#' Test data: Polytomous NEPS data with three booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 16
#' dichotomous and 5 polytomous items presented in three booklets.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 2,110 rows and 46 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{rotation}{Test position with 0 = first and 1 = second.}
#'   \item{booklet}{Booklet with 0 = easy, 1 = medium, and 2 = difficult.}
#'   \item{reg7XXX_c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{reg7XXX}{Unscored item responses for simple multiple-choice items
#'   with values of 1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#'   \item{booklet1}{A logical identifying respondents for Booklet 1.}
#'   \item{booklet2}{A logical identifying respondents for Booklet 2.}
#'   \item{booklet3}{A logical identifying respondents for Booklet 3.}
#' }
#'
#' `vars` is a data frame with 37 rows and 16 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{poly}{A logical identifying scored polytomous (`TRUE`) and other
#'   (`FALSE`) items.}
#'   \item{mixed}{A logical identifying scored (`TRUE`) and other (`FALSE`)
#'   items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{texttype}{The five text types included in the reading test.}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{booklet1}{A logical identifying items included in Booklet 1.}
#'   \item{pos1}{A number with the item position in Booklet 1.}
#'   \item{booklet2}{A logical identifying items included in Booklet 2.}
#'   \item{pos2}{A number with the item position in Booklet 2.}
#'   \item{booklet2}{A logical identifying items included in Booklet 3.}
#'   \item{pos3}{A number with the item position in Booklet 3.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for simple multiple-choice items.}
#' }
"ex3"


#' Dichotomous NEPS data without booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 15
#' dichotomous items presented in a single booklet.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 1,010 rows and 43 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{books}{Binary book count with 0 = less than 100, 1 = more than 100.}
#'   \item{age}{Age in years.}
#'   \item{t34005a}{Raw school variable with categories 1 to 6.}
#'   \item{t400500}{Raw migration variable with categories 1 to 10.}
#'   \item{testy}{Test year.}
#'   \item{testm}{Test month.}
#'   \item{birthy}{Birth year.}
#'   \item{birthm}{Birth month.}
#'   \item{grk1..._c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{grk1...}{Unscored item responses for simple multiple-choice items
#'   with values of 1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#' }
#'
#' `vars` is a data frame with 30 rows and 8 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{pos}{A number with the item position in the test.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for simple multiple-choice items.}
#' }
"dat1"


#' Polytomous NEPS data without booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 13
#' dichotomous and 4 polytomous items presented in a single booklet.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 1,550 rows and 82 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{books}{Binary book count with 0 = less than 100, 1 = more than 100.}
#'   \item{age}{Age in years.}
#'   \item{t34005a}{Raw school variable with categories 1 to 6.}
#'   \item{t400500}{Raw migration variable with categories 1 to 10.}
#'   \item{testy}{Test year.}
#'   \item{testm}{Test month.}
#'   \item{birthy}{Birth year.}
#'   \item{birthm}{Birth month.}
#'   \item{rotation}{Test position with 0 = first and 1 = second.}
#'   \item{mag12XXX_c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{mag12XXX}{Unscored item responses for each (sub)item with values of
#'   1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#' }
#'
#' `vars` is a data frame with 68 rows and 11 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{poly}{A logical identifying scored polytomous (`TRUE`) and other
#'   (`FALSE`) items.}
#'   \item{mixed}{A logical identifying scored (`TRUE`) and other (`FALSE`)
#'   items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{content}{The four content domains of the math test.}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{pos}{A number with the item position in the test.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for each unscored (sub)item.}
#' }
"dat2"


#' Polytomous NEPS data with three booklets
#'
#' The simulated data resembles the NEPS data structure for a test with 16
#' dichotomous and 5 polytomous items presented in three booklets.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 2,110 rows and 103 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{books}{Binary book count with 0 = less than 100, 1 = more than 100.}
#'   \item{age}{Age in years.}
#'   \item{t34005a}{Raw school variable with categories 1 to 6.}
#'   \item{t400500}{Raw migration variable with categories 1 to 10.}
#'   \item{testy}{Test year.}
#'   \item{testm}{Test month.}
#'   \item{birthy}{Birth year.}
#'   \item{birthm}{Birth month.}
#'   \item{rotation}{Test position with 0 = first and 1 = second.}
#'   \item{booklet}{Booklet with 0 = easy, 1 = medium, and 2 = difficult.}
#'   \item{reg7XXX_c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{reg7XXX}{Unscored item responses for each (sub)item with values of
#'   1 to 4 representing the selected response option.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#'   \item{easy}{A logical identifying respondents for the easy booklet.}
#'   \item{medium}{A logical identifying respondents for the medium booklet.}
#'   \item{difficult}{A logical identifying respondents for the difficult booklet.}
#' }
#'
#' `vars` is a data frame with 85 rows and 16 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{poly}{A logical identifying scored polytomous (`TRUE`) and other
#'   (`FALSE`) items.}
#'   \item{mixed}{A logical identifying scored (`TRUE`) and other (`FALSE`)
#'   items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{texttype}{The five text types included in the reading test.}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{booklet1}{A logical identifying items included in Booklet 1.}
#'   \item{pos1}{A number with the item position in Booklet 1.}
#'   \item{booklet2}{A logical identifying items included in Booklet 2.}
#'   \item{pos2}{A number with the item position in Booklet 2.}
#'   \item{booklet2}{A logical identifying items included in Booklet 3.}
#'   \item{pos3}{A number with the item position in Booklet 3.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for each unscored (sub)item.}
#' }
"dat3"

#' Polytomous NEPS data with multi-stage design
#'
#' The simulated data resembles the NEPS data structure for a multi-sateg test with 26
#' dichotomous and 8 polytomous items presented in three stages.
#'
#' @format A list with two data frames including the simulated responses
#' (`resp`) and the characteristics of the variables (`vars`).
#'
#' `resp` is a data frame with 3,710 rows and 145 columns:
#' \describe{
#'   \item{ID_t}{Unique identifier for respondent.}
#'   \item{sex}{Sex with 0 = male and 1 = female.}
#'   \item{mig}{Migrant background with 0 = no and 1 = yes.}
#'   \item{school}{School type with 0 = lower, 1 = intermediate, and 1 = upper.}
#'   \item{books}{Binary book count with 0 = less than 100, 1 = more than 100.}
#'   \item{age}{Age in years.}
#'   \item{t34005a}{Raw school variable with categories 1 to 6.}
#'   \item{t400500}{Raw migration variable with categories 1 to 10.}
#'   \item{testy}{Test year.}
#'   \item{testm}{Test month.}
#'   \item{birthy}{Birth year.}
#'   \item{birthm}{Birth month.}
#'   \item{rotation}{Test position with 0 = first and 1 = second.}
#'   \item{sca14XXX_c}{Scored item responses 0 = incorrect and 1 = correct.}
#'   \item{sca14XXX}{Unscored item responses for each (sub)item with values of
#'   1 to 4 representing the selected response option.}
#'   \item{stage1}{A logical identifying respondents who were administered the first stage.}
#'   \item{stage2}{A logical identifying respondents who were administered the second stage.}
#'   \item{stage3}{A logical identifying respondents who were administered the third stage.}
#'   \item{valid}{A logical vector identifying valid respondents.}
#' }
#'
#' `vars` is a data frame with 128 rows and 17 columns:
#' \describe{
#'   \item{item}{Name of the item.}
#'   \item{raw}{A logical distinguishing scored (`FALSE`) and unscored (`TRUE`)
#'   items.}
#'   \item{dich}{A logical identifying scored dichotomous (`TRUE`) and
#'   other (`FALSE`) items.}
#'   \item{poly}{A logical identifying scored polytomous (`TRUE`) and other
#'   (`FALSE`) items.}
#'   \item{mixed}{A logical identifying scored (`TRUE`) and other (`FALSE`)
#'   items.}
#'   \item{type}{A string identifying the item type as simple multiple-choice
#'    (`MC`) or complex multiple-choice (`CMC`).}
#'   \item{scoring}{A number with the scoring weight in the item response
#'   analyses.}
#'   \item{num_cat}{A number with the number of response categories.}
#'   \item{component}{Contains the two components KOS and KAS.}
#'   \item{correct}{A number between 1 and 4 representing the correct response
#'   option for each unscored (sub)item.}
#'   \item{stage1}{A logical identifying the items administered in the first stage.}
#'   \item{stage2}{A logical identifying the items administered in the second stage.}
#'   \item{stage3}{A logical identifying the items administered in the third stage.}
#'   \item{level1}{A logical identifying the items corresponding to level 1.}
#'   \item{level2}{A logical identifying the items corresponding to level 2.}
#'   \item{level3}{A logical identifying the items corresponding to level 3.}
#'   \item{pos}{A number with the item position in the test.}
#' }
"dat4"
