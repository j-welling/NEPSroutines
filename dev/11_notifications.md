# NEPSroutines — Comprehensive Notification Reference

This document lists every **message**, **warning**, and **error** (`stop()`) that
the NEPSroutines package can emit at runtime.  For each notification, the
following information is provided:

- **Type** — `message` (informational), `warning` (potentially problematic, but
  execution continues), or `error` (execution is aborted).
- **Source** — the R source file and the function that emits the notification.
- **Trigger** — the condition that causes the notification to fire.
- **Effect** — the practical consequence for the user / the running analysis.

---

## Table of contents

1. [Data validation utilities (`utils.R`)](#data-validation-utilities-utilsr)
2. [Data preparation (`data_preparation.R`)](#data-preparation-data_preparationr)
3. [IRT analyses (`irt_analyses.R`)](#irt-analyses-irt_analysesr)
4. [DIF analysis (`dif_analysis.R`)](#dif-analysis-dif_analysisr)
5. [Dimensionality analysis (`dimensionality_analysis.R`)](#dimensionality-analysis-dimensionality_analysisr)
6. [Linking (`linking.R`)](#linking-linkingr)
7. [Missing-value analysis — items (`mv_item.R`)](#missing-value-analysis--items-mv_itemr)
8. [Missing-value analysis — persons (`mv_person.R`)](#missing-value-analysis--persons-mv_personr)
9. [Distractor analysis (`distractor_analysis.R`)](#distractor-analysis-distractor_analysisr)
10. [Descriptives (`descriptives.R`)](#descriptives-descriptivesr)
11. [Score creation (`create_scores.R`)](#score-creation-create_scoresr)
12. [SUF creation (`create_suf.R`)](#suf-creation-create_sufr)
13. [Technical report — setup (`technical_report_setup.r`)](#technical-report--setup-technical_report_setupr)
14. [Technical report — import (`technical_report_import.r`)](#technical-report--import-technical_report_importr)
15. [Technical report — tables (`technical_report_tables.r`)](#technical-report--tables-technical_report_tablesr)
16. [Technical report — figures (`technical_report_figures.r`)](#technical-report--figures-technical_report_figuresr)
17. [Technical report — get (`technical_report_get.r`)](#technical-report--get-technical_report_getr)

---

## Data validation utilities (`utils.R`)

### `only_valid()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 1 | **message** | *"No variable with valid cases provided. All cases are used for analysis."* | `valid = NULL` is passed to `only_valid()` and `warn = TRUE`. | No case filtering is performed; the full dataset is carried forward. |

### `convert_mv()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 2 | **message** | *"No user defined missing values provided for item responses. Default of '-999 to -1' is used."* | `mvs = NULL` is passed to `convert_mv()` and `warn = TRUE`. | When `warn = TRUE`, the package first emits this message and then recodes values in the range −999 to −1 as `NA`; when `warn = FALSE`, the recoding happens silently. If the actual data uses a different coding, missing values will **not** be converted correctly. |

### `prepare_resp()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 3 | **error** | *"To create a data frame (resp) with only the indicated items, please also provide vars."* | `select` is specified but `vars` is `NULL`. | Execution stops. The `resp` data frame cannot be subset because the item list is unavailable. |
| 4 | **message** | *"No variable provided indicating the items to keep. All items are kept."* | `select = NULL` and `warn = TRUE`. | All columns in `resp` are used in the analysis (no item selection). |

### `is_null_mvs_valid()` (internal helper)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 5 | **message** | *"No user defined missing values provided. Default of '-999 to -1' is used."* | `mvs = NULL`. | Values −999 to −1 are treated as missing (see #2). |
| 6 | **message** | *"No variable with valid cases provided. All cases are used for analysis."* | `valid = NULL`. | No case filtering (see #1). |

### `check_folder()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 7 | **message** | *"The location \<path\> did not exist. New folder created."* | The requested output directory does not exist. | A new directory is created automatically; the file-save operation then proceeds. |

### `check_pid()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 8 | **error** | *"There are duplicates in the person identifiers."* | The person ID vector contains repeated values. | Execution stops. Duplicate IDs would lead to ambiguous person-level results. |
| 9 | **warning** | *"There are missing values in the person identifiers."* | At least one `NA` exists in the person ID vector. | Execution continues but persons with `NA` IDs may be incorrectly matched or silently dropped in downstream merge operations. |

### `check_items()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 10 | **error** | *"There are duplicates in the item names."* | The item name vector contains repeated values. | Execution stops. Duplicate item names would corrupt the item-parameter matrix. |
| 11 | **error** | *"There are missing values in the item names."* | At least one `NA` is present in the item name vector. | Execution stops. Items without names cannot be identified in output tables. |

### `check_variables()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 12 | **error** | *"Data.frame \<name_df\> does not include any variable with the name '\<var\>'. Please check again."* | A requested variable name is absent from the target data frame. | Execution stops. The requested variable cannot be accessed. |

### `check_logicals()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 13 | **error** | *"Variable '\<var\>' in data.frame \<name_df\> is no logical. Please check again."* | A variable that should be logical (TRUE/FALSE) is of a different type. | Execution stops. Non-logical selector variables would yield incorrect item or case selection. |
| 14 | **warning** | *"Logical variable '\<var\>' in data.frame \<name_df\> contains other values than TRUE or FALSE (e.g., NA). Please check again."* | A logical selector variable contains `NA` values (`warn = TRUE`). | Execution continues. `check_logicals()` does not modify the data; downstream subsetting or logical indexing may behave unexpectedly or error if `NA`s are not handled before use. |

### `check_numerics()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 15 | **error** | *"Variable '\<var\>' in data.frame \<name_df\> is no numeric variable. Please check again."* | An item column is not of numeric type (e.g., character or factor). | Execution stops. IRT routines require numeric input. |

### `check_invalid_values()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 16 | **error** | *"Data.frame \<name_df\> contains invalid values (< 0) in specified items: \<values\>. Please check again and be sure to include all user defined missing values in the vector mvs."* | Item response data still contains negative values after missing-value conversion. | Execution stops. Negative values in item responses are invalid for IRT models and indicate that `convert_mv()` was not called or that `mvs` was incomplete. |

### `check_dich()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 17 | **error** | *"Variable '\<var\>' in data.frame \<name_df\> contains values greater than 1, although specified as dichotomous. Please check again."* | An item designated as dichotomous has response values > 1. | Execution stops. Dichotomous IRT models assume only 0/1 responses; polytomous items must use the PCM/GPCM path. |

### `meht()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 18 | **error** | *"Please provide an effect size in eta2 or delta!"* | Both `eta2` and `delta` are `NULL`. | Execution stops. The minimum-effect hypothesis test cannot be computed without a reference effect size. |
| 19 | **message** | Nil-hypothesis test statistics (F critical, p value). | `verbose = TRUE`. | Informational: prints the classical nil-hypothesis F-test results to the console. |
| 20 | **message** | Minimum-effect hypothesis test statistics. | `verbose = TRUE`. | Informational: prints the minimum-effect test results to the console. |

### `reached_maxiter()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 21 | **warning** | *"Maximum number of iterations were reached for the IRT model \<name_model\>! Model did not converge."* | The TAM convergence iteration counter equals or exceeds `maxiter`. | Execution continues but parameter estimates should be treated as unreliable. Consider increasing `control$maxiter` or revising the model / item set. |

### `create_q()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 22 | **warning** | *"No variable name for scoring factor for polytomous analysis provided. Therefore no loading matrix is used for analysis."* | `scoring = NULL` and the response data contain polytomous items (`poly = TRUE`). | Execution continues. The Q (loading) matrix defaults to `NULL`, which may alter the scale of person parameters for polytomous models. |

### `order_xsi_fixed()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 23 | **error** | *"Items in xsi_fixed do not match items in \<irtmodel\> model!"* | Item names in `xsi_fixed` are not found among the estimated item parameters of the TAM model. | Execution stops. Fixed item parameters cannot be applied if the items are missing from the model. |

---

## Data preparation (`data_preparation.R`)

### `duplicate_items()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 24 | **error** | *"Item/s '\<items\>' is/are not included in vars! Please check again."* | Items listed for duplication/renaming do not exist in the `vars` data frame. | Execution stops. The duplication/renaming cannot proceed without valid source items in `vars`. |

### `pc_scoring()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 25 | **error** | *"The argument 'poly_items' must be a list. Please check your input."* | `poly_items` is not an R list object. | Execution stops. |
| 26 | **error** | *"The argument 'treshold' must be numeric in the interval between 0 and 1. Please check your input."* | `threshold` is non-numeric or outside [0, 1]. | Execution stops. |
| 27 | **message** | *"\<pc_name\>: Variable name should contain a subitem marker like 's', e.g. '[item]s_c'."* | A polytomous item name does not follow the expected naming convention. | Informational. Analysis continues, but downstream functions that rely on the naming pattern may fail or produce incorrect results. |
| 28 | **warning** | *"No missing values provided. c(-99:-1) used as default."* | `mvs = NULL`. | Missing values in the range −99 to −1 are recoded as `NA`. Values outside this range are retained as valid responses. |
| 29 | **message** | *"When scoring polytomous items, missing values of subitems are imputed …"* | `impute = TRUE` and `verbose = TRUE`. | Informational: describes the imputation strategy. The original `resp` data frame is not modified; imputation affects only the internal scoring. |
| 30 | **warning** | *"Recoding of subitems into indicator variables failed. Please contact the package developers."* | The internal conversion of subitems to 0/1 indicator variables produced unexpected results (all values outside {0, 1}). | Execution continues but polytomous scoring will be incorrect. Contact the package developers. |
| 31 | **error** | *"Number of 'sumMV' variables does not match the number of 'pc_items'. Please contact the package developers."* | An internal consistency check fails: the count of `_sumMV` columns does not equal the number of items in `poly_items`. | Execution stops. This indicates an internal bug; contact the developers. |
| 32 | **message** | Overview of absolute and relative frequencies of imputed missing values. | Always printed when imputation is performed. | Informational: supports quality-control review of how many missing subitems were imputed. |
| 33 | **message** | Overview of cases with imputed missing values per polytomous item. | Always printed when imputation is performed. | Informational: supports quality-control review at the person level. |

### `pc_imputation()` (internal helper for `pc_scoring()`)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 34 | **error** | *"The imputation of missing values on subitems, requires a data.frame with missing indicators …"* | `indicators` is `NULL` or not a data frame. | Execution stops. Usually indicates `pc_scoring()` was not called with `impute = TRUE` first. |
| 35 | **error** | *"The number of respondents 'resp' does not match the number of respondents in 'indicators'. …"* | Row counts of `resp` and `indicators` differ. | Execution stops. The mismatch prevents reliable row-level joining. |
| 36 | **error** | *"The imputation of missing values on subitems, requires a data.frame containing information on the competence items …"* | `vars` is `NULL` or not a data frame. | Execution stops. |
| 37 | **error** | *"The imputation of missing values on subitems requires the name of a logical variable in vars …"* | `select = NULL`. | Execution stops. |
| 38 | **error** | *"The subitems defined in 'poly_items' are not included in 'indicators'. …"* | Subitem columns are absent from the `indicators` data frame. | Execution stops. |
| 39 | **error** | *"The subitems defined in 'poly_items' are not included in the selected item set. …"* | Subitems listed in `poly_items` are not flagged by the `select` variable in `vars`. | Execution stops. |
| 40 | **warning** | *"ID_ts in original data.frame and in data.frame with predicted responses are different. Please contact the package developer."* | After prediction, the set of valid respondent IDs in `resp` and `pred_resp` diverges. | Execution continues but the imputed responses may be assigned to wrong persons. Contact the developers. |

### `collapse_response_categories()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 41 | **message** | *"The following items resulted in less than two response categories with more than \<per_cat\> cases …"* | Some items have sparse response categories that cannot be collapsed further. | Informational. The listed items are left unchanged; review them manually before IRT analysis. |
| 42 | **message** | *"Dichotomous items were not considered for collapsing. The following items have less than three response categories: …"* | Dichotomous items are found among the set of items passed to `collapse_response_categories()`. | Informational. Dichotomous items require no collapsing. |
| 43 | **message** | *"The following items have been collapsed: …"* | At least one polytomous item had categories successfully merged. | Informational: lists collapsed items and the new category structure. |
| 44 | **message** | *"No items have been collapsed."* | No items met the collapsing criterion. | Informational. |

### `min_val()` 

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 45 | **warning** | *"No valid (=> 0) number of minimum valid responses per person (min.val) provided. Default of 3 valid responses applies."* | `min.val` is `NULL` or negative. | Execution continues using `min.val = 3`. Persons with fewer than 3 valid item responses are treated as having insufficient data. |

### `calculate_age()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 46 | **message** | *"\<n\> missing value(s) in birth year were replaced by the sample median."* | `NA`s exist in the birth year variable. | Median imputation is applied; the affected person count is reported. Age calculations for those persons are approximate. |
| 47 | **message** | *"\<n\> missing value(s) in birth month were replaced by the sample median."* | `NA`s exist in the birth month variable. | Analogous to #46. |
| 48 | **message** | *"\<n\> missing value(s) in test year were replaced by the sample median."* | `NA`s exist in the test year variable. | Analogous to #46. |
| 49 | **message** | *"\<n\> missing values in test month were replaced by the sample median."* | `NA`s exist in the test month variable. | Analogous to #46. |

---

## IRT analyses (`irt_analyses.R`)

### `irt_analyses()` (multi-group wrapper)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 50 | **message** | *"\<i\> IRT ANALYSIS (\<poly/dich\>) FOR GROUP '\<g\>':"* | Printed at the start of each group's analysis when multiple groups are processed. | Informational progress indicator. |

### `irt_analysis()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 51 | **error** | *"Invalid irtmodel. Please provide one of the following: '1PL', '2PL', 'PCM2', 'GPCM'."* | `irtmodel` is not one of the four valid strings. | Execution stops. |
| 52 | **message** | IRT summary table header and content. | `print = TRUE`. | Informational: prints the item-parameter summary to the console. |
| 53 | **message** | Model fit table header and content. | `print = TRUE`. | Informational: prints AIC/BIC and other fit indices. |
| 54 | **message** | Steps analysis table header and content. | `print = TRUE` and `irt_type == 'poly'`. | Informational: prints threshold (step) parameters for polytomous items. |
| 55 | **message** | *"SUMMARY FOR TR"* followed by text summary. | `print = TRUE`. | Informational: prints a prose summary suitable for pasting into a technical report. |

### `irt_model()` (internal)

Called by `irt_analysis()` for each model type (1PL, 2PL, PCM2, GPCM).

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 55a | **error** | *"The following items in resp\[\<group info\>\] have a maximum observed score of 0 … \<item names\>"* | At least one item's maximum observed response is 0 (all values are 0 or `NA` after MV conversion). Fires before any TAM function is called. | Execution stops. Without this check TAM crashes internally with an uninformative `dimnames` error. The message lists the offending items and suggests excluding them or revising the `mvs` specification. |

### `model_fit_table()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 56 | **error** | *"No valid irt_type provided. Possible are 'dich' for dichotomous analysis or 'poly' for polytomous analysis."* | `irt_type` is neither `'dich'` nor `'poly'`. | Execution stops. |

### `print_irt_summary()` (internal)

Messages 57–67 are all `message()` calls emitted when `verbose = TRUE` (the default). They are informational; they summarise model output and do **not** stop or alter the analysis.

| # | Type | Message text (abbreviated) | Trigger |
|---|------|---------------------------|---------|
| 57 | **message** | Percentage-correct statistics (min, max, mean, median, SD). | Always when called. |
| 58 | **message** | Item difficulty statistics (min, max, mean, median, SD). | Always when called. |
| 59 | **message** | SE of item difficulties (max). | Always when called. |
| 60 | **message** | WMNSQ fit statistics (range, mean, SD, median). | Always when called. |
| 61 | **message** | Items with WMNSQ ≥ 1.15 (or "No item"). | Always when called. |
| 62 | **message** | WMNSQ t-value statistics. | Always when called. |
| 63 | **message** | Items with \|t\| ≥ 8 (or "No item"). | Always when called. |
| 64 | **message** | Item–total correlation statistics. | Always when called. |
| 65 | **message** | Model variance. | Always when called. |
| 66 | **message** | EAP/PV reliability and WLE reliability. | Always when called. |
| 67 | **message** | Item discrimination statistics. | Always when called (2PL / GPCM only). |
| 68 | **message** | Threshold statistics (polytomous items only). | Always when called and thresholds are available. |

---

## DIF analysis (`dif_analysis.R`)

### `dif_analysis()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 69 | **warning** | *"No scoring variable provided. All items are scored with 1."* | `scoring = NULL` for a polytomous DIF analysis (`irt_type == 'poly'`) and `warn = TRUE`. | Execution continues. Without a scoring matrix, item responses are implicitly weighted 1, which may be incorrect for constructed-response polytomous items. |

### `validate_dif_var()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 70 | **warning** | *"\<n\> invalid values (< 0) were found in the DIF variable \<dif_var\>. The corresponding cases were replaced by NAs."* | Negative values exist in the DIF grouping variable (e.g., user-defined missing codes). | Execution continues with those cases excluded. The effective sample size decreases. |

### `warn_excluded_missings()` / `warn_included_missings()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 71 | **warning** | *"\<n\> missing values were found in the DIF variable \<dif_var\>. The corresponding cases were excluded from the analysis."* | `NA`s exist in the DIF variable and `na_groups = FALSE` (or equivalent exclusion logic). | Execution continues with reduced sample size. DIF estimates may be affected if missingness is systematic. |
| 72 | **warning** | *"\<n\> missing values were found in the DIF variable \<dif_var\>. The corresponding cases were included in the analysis as an extra group."* | `NA`s exist in the DIF variable and `na_groups = TRUE`. | An additional "NA" group is added to the facets model. This changes degrees of freedom and may alter parameter estimates. |

### `remove_sparse_items()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 73 | **warning** | *"At least one group from the DIF variable '\<dif_var\>' does not have the minimum number of valid responses (\<min_val\>) on item(s) \<items\>. The corresponding items were excluded from the analysis."* | A DIF group has too few valid responses on one or more items. | Execution continues. The listed items are dropped from the DIF model; DIF cannot be assessed for them. |

### `print_dif_summary()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 74 | **message** | *"RESULTS FOR THE DIF VARIABLE '\<dif_var\>':…"* followed by facet counts, information criteria, main effects, and items with problematic DIF. | Called from `dif_analysis()` when `print = TRUE`. | Informational: structured DIF output printed to the console. |

### `test_dif_data()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 75 | **error** | *"Please check 'select' and 'dif_vars'. At least one of them does not match the intended analysis."* | Multiple `select` variables are provided but their count differs from the count of `dif_vars`. | Execution stops. The analysis specification is ambiguous. |

---

## Dimensionality analysis (`dimensionality_analysis.R`)

### `dim_analysis()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 76 | **message** | *"Finished unidimensional reference model."* | The unidimensional TAM model has converged (or reached `maxiter`). | Informational progress indicator. |
| 77 | **message** | *"Finished \<d\> model."* | Each multidimensional model has finished estimation. | Informational progress indicator for each requested dimensionality level. |

### `print_dim_summary()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 78 | **message** | *"RESULTS:"* followed by named list elements (factor correlations, fit indices). | Called when `print = TRUE`. | Informational. |

---

## Linking (`linking.R`)

### `prepare_longitudinal_resp()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 79 | **error** | *"No anchor items found!"* | After filtering, the anchor item matrix has fewer than 2 rows, i.e., no (or only one) shared item remains. | Execution stops. Linking requires at least two anchor items. |

### `calculate_link_parameters()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 80 | **error** | *"Anchor items in 'anchors' do not match item names in the IRT models. Please check that all anchor items exist in both the previous and current datasets."* | User-supplied `anchors` data frame references item names absent from either the previous or current IRT result objects. | Execution stops. |

### `link_item_parameters()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 81 | **warning** | *"No variable name for scoring factor for polytomous analysis provided. Scoring is set to 1 for all items."* | `scoring = NULL` while polytomous items are present in the link data. | Execution continues. All item loadings default to 1, which may be suboptimal for constructed-response items. |

### `print_link_summary()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 82 | **message** | Sample sizes (longitudinal subsample and link study). | Always printed. | Informational. |
| 83 | **message** | *"DIF estimates for anchor items:"* and accompanying table. | Always printed. | Informational. |
| 84 | **message** | DIF threshold legend (marks `+` and `*`). | Always printed. | Clarifies the annotation in the DIF table. |
| 85 | **message** | Critical F-values for DIF between time points. | Always printed. | Informational. |
| 86 | **message** | Min./max. absolute DIF estimate. | Always printed. | Informational. |
| 87 | **message** | *"Linking constant: \<c\>\nLinking error: \<e\>"* | Always printed. | Reports the main linking results. |
| 88 | **message** | Dimensionality analysis results (factor correlations, fit indices) for the link study and/or previous/current test. | Always printed. | Informational. Provides evidence for (or against) unidimensionality of the linked construct. |

### `test_linking_data()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 89 | **error** | *"Dataframe 'anchors' includes items that are not defined as linking items in the 'select' arguments. Please check again."* | Items named in `anchors` are absent from the item sets defined by `select_prev`/`select_curr` (or `select_link`). | Execution stops. |

---

## Missing-value analysis — items (`mv_item.R`)

### `mvi_analysis()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 90 | **message** | *"Table 1 shows missing values by item position and missing type. All other tables show summary statistics over all items."* | `print = TRUE`. | Informational header before printed tables. |
| 91 | **message** | *"Summary for TR"* followed by prose summary. | `print = TRUE`. | Prose summary ready for inclusion in a technical report. |
| 92 | **warning** | *"NAs found in resp! These values are ignored."* | `resp` contains raw `NA` values not listed in `mvs`, and `warn = TRUE`. | Execution continues. Genuine `NA`s (e.g., "missing by design" not coded numerically) are ignored in the missing-type breakdown and are not counted in any missing-value category. |

### `test_mvi_analysis()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 93 | **error** | *"No grouping. Please provide only one position variable."* | `grouping = NULL` but multiple position variables are supplied. | Execution stops. Multiple positions without grouping are ambiguous. |
| 94 | **warning** | *"Only one position variable provided. The items of each group are therefore set to the same positions."* | `grouping` is supplied but only a single `position` variable is given. | Execution continues. All groups are assumed to have items at the same positions. |
| 95 | **error** | *"Position and grouping variables do not match. Please provide either only one position variable … or matching position and grouping variables …"* | `length(position) != length(grouping)` and `length(position) > 1`. | Execution stops. |
| 96 | **error** | *"NEPSroutines package cannot account for multiple stages and multiple item positions at once. Please conduct missing values analysis manually."* | Both `stages` and multiple `position` variables with `grouping` are specified simultaneously. | Execution stops. This combination is not implemented. |

### `create_mvlist()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 97 | **error** | *"Number of items in dataframe responses, in vector item and in vector position do not match. Please provide matching arguments to function create_mvlist()."* | Lengths of `item`, `position`, and the number of columns in `responses` are inconsistent. | Execution stops. |

### `mvi_plots()` / `check_color()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 98 | **warning** | *"Number of provided legend labels does not correspond to number of groups. Group labels are used instead."* | `labels_legend` has a different length than the number of groups. | Execution continues using the group names as legend labels instead. |
| 99 | **message** | *"Missing plot \<i\> created."* | `verbose = TRUE` and each plot finishes rendering. | Informational progress indicator. |
| 100 | **error** | *"The number of provided colors does not match the number of groups (\<n\>)."* | `color` vector length ≠ number of groups. | Execution stops. A color must be assigned to every group. |

### `print_mvi_results()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 101 | **message** | Prose summary of missing-value proportions per missing type (min, max, items). | Always called when `print = TRUE`. | Informational. |

### `test_mvi_data()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 102 | **error** | *"Please provide mv_i with all specified missing values."* | The `mv_i` result object does not contain entries for all missing-value types defined in `mvs`. | Execution stops. The output cannot be summarised when data for some missing-value types is absent. |

---

## Missing-value analysis — persons (`mv_person.R`)

### `mvp_analysis()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 103 | **warning** | *"NAs found in resp! These values are ignored."* | `resp` contains raw `NA` values and `warn = TRUE`. | Analogous to #92. |

### `mvp_plots()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 104 | **error** | *"The number of provided colors does not match the number of groups (\<n\>)."* | `color` vector length ≠ number of groups. | Execution stops (analogous to #100). |
| 105 | **error** | *"Please provide labels for each missing value type specified in mv_p."* | `labels_mvs` does not cover all missing-value types in `mv_p`. | Execution stops. Labels are required to render informative plot legends. |
| 106 | **warning** | *"Number of provided legend labels does not correspond to number of groups. Group labels are used instead."* | `labels_legend` length ≠ number of groups. | Analogous to #98. |
| 107 | **message** | *"Missing plot \<i\> created."* | `verbose = TRUE`. | Informational (analogous to #99). |

### `print_mvp_results()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 108 | **message** | *"Table with results:"* followed by summary table(s). | Always called when `print = TRUE`. | Informational. |

### `test_mvp_data()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 109 | **error** | *"Please provide mv_p with all specified missing values."* | The `mv_p` result object does not contain all missing-value types from `mvs`. | Execution stops. Analogous to #102. |

---

## Distractor analysis (`distractor_analysis.R`)

### `print_distractor_summary()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 110 | **message** | Item–total correlation statistics for the correct response (min, max, median). | Always called. | Informational. |
| 111 | **message** | Item–total correlation statistics for distractors (min, max, median). | Always called. | Informational. |
| 112 | **message** | *"Items with problematic item-total correlations for correct response (r < 0.2): \<items\>"* | At least one item has a correct-response correlation below 0.2. | Identifies potentially non-discriminating items that may require revision. |
| 113 | **message** | *"Items with problematic item-total correlations for distractors (r > 0.05): \<items\>"* | At least one distractor shows a positive item–total correlation above 0.05. | Identifies distractors that are answered more often by high-ability respondents — possibly an item-key error or an attractive wrong option. |

---

## Descriptives (`descriptives.R`)

### `valid_cases()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 114 | **message** | *"No variable to identify (in)valid cases provided. Thus, all cases are counted as valid."* | No `valid` variable is passed. | All rows are counted as valid; no exclusion occurs. |
| 115 | **message** | *"There are \<n_val\> valid cases and \<n_inval\> invalid cases in the dataset."* | Always. | Informational case-count report. |

### `continuous_descriptives()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 116 | **message** | *"Summary statistics of the continuous variables:"* and table. | `print = TRUE`. | Informational. |

### `group_descriptives()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 117 | **message** | *"Sample size by groups:"* and table. | `print = TRUE`. | Informational. |
| 118 | **message** | *"Frequency of groups including missing values:"* and table. | `print = TRUE`. | Informational. |
| 119 | **message** | *"Frequency of groups excluding missing values:"* and table. | `print = TRUE`. | Informational. |

### `variable_attributes()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 120 | **message** | *"The attributes for variable \<var\> are:"* and label table. | For each variable in `desc`. | Informational: shows value labels from haven-imported data. |

---

## Score creation (`create_scores.R`)

### `create_scores()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 121 | **message** | *"No variable 'sum_select' provided for sum scores. All items as specified in variable '\<select\>' are used instead."* | `sum_select = NULL`. | Execution continues using the same `select` variable used for the IRT analysis. The full selected item set is summed. |
| 122 | **error** | *"No argument 'meta_variable' provided."* | `meta_variable = NULL`. | Execution stops. The meta-score cannot be computed without a variable identifying metacognition items. |
| 123 | **message** | *"No variable 'meta_select' provided for meta scores. All items as specified in variable '\<select\>' are used instead."* | `meta_select = NULL`. | Analogous to #121. |

### `estimate_rotated_wles()` (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 124 | **warning** | *"Please provide the item parameters to ensure the correct results in the WLE estimation."* | `xsi_fixed = NULL`. | Execution continues without fixed item parameters. WLE scores are estimated from the data alone, which may differ from scores based on calibrated/normed parameters. This should only be used for exploratory purposes. |

---

## SUF creation (`create_suf.R`)

### `create_suf()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 125 | **message** | *"SUF successfully saved!"* | `save = TRUE` and the file was written without error. | Informational confirmation. |

### Label lookup (internal)

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 126 | **warning** | *"Check the spelling of the competence domain in the 'competence' argument to get the correct label for the Procedural Metacognition scores. Choose the correct spelling from the list below: …"* | A metacognition score type is detected but `domain = NULL` (or the domain string does not match any known value). | Execution continues but the SUF variable label for the metacognition score may be generic/incorrect. Verify the `competence` argument spelling. |

---

## Technical report — setup (`technical_report_setup.r`)

### `Setup()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 127 | **message** | *"The technical report has been setup in \<path\>."* | Always on success. | Informational confirmation. |

### `Update()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 128 | **error** | *"Couldn't find the path \<path\>/_extensions"* | The `_extensions` directory is absent from `path`. | Execution stops. The Quarto NEPS extension must be installed first (via `Setup()`). |
| 129 | **message** | *"The Quarto extension in \<path\>/_extensions has been updated."* | Always on success. | Informational confirmation. |

---

## Technical report — import (`technical_report_import.r`)

### `Import()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 130 | **error** | *"Please provide argument 'filename' or 'regexp'."* | Both `filename` and `regexp` are `NULL`. | Execution stops. At least one file identifier is required. |
| 131 | **warning** | *"The argument 'filename' was ignored because 'regexp' was set."* | Both `filename` and `regexp` are provided simultaneously. | Execution continues using `regexp` only. The explicit filename is silently ignored; verify this is the intended behaviour. |

---

## Technical report — tables (`technical_report_tables.r`)

### `Tbl()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 132 | **error** | *"Please install flextable!"* | The `flextable` package is not installed. | Execution stops. `flextable` is required for Word-format table rendering. |
| 133 | **error** | *"Please install officer!"* | The `officer` package is not installed. | Execution stops. `officer` is required for advanced Word formatting. |

### Dimension table helpers

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 134 | **warning** | *"Number of rowname elements does not match table dimensions."* | `rownames` vector length ≠ number of rows in the table. | Execution continues; auto-generated row names ("Dim 1", "Dim 2", …) are used instead. |
| 135 | **warning** | *"Number of colname elements does not match table dimensions."* | `colnames` vector length ≠ number of columns in the table. | Execution continues; auto-generated column names are used instead. |

---

## Technical report — figures (`technical_report_figures.r`)

### `Fig()`

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 136 | **error** | *"Please install magick!"* | The `magick` package is not installed but figure manipulation (footnote, crop, resize) was requested. | Execution stops. `magick` is required for image post-processing; install it or omit the optional arguments. |

---

## Technical report — get (`technical_report_get.r`)

### `GetPars()` / `GetDif()` and related helpers

| # | Type | Message text (abbreviated) | Trigger | Effect |
|---|------|---------------------------|---------|--------|
| 137 | **error** | *"Unknown stat function."* (in `GetPars()` `excl` branch) | An exclusion condition string does not start with `=`, `<`, or `>`. | Execution stops. Only simple comparison operators are supported in the `excl` argument. |
| 138 | **error** | *"Unknown stat function."* (in `GetPars()` `stat` branch) | A statistic condition string does not start with `=`, `<`, or `>`. | Execution stops. Analogous to #137. |
| 139 | **error** | *"Allowed values for argument main are 'std' and 'ustd'."* (in `GetDif()` `main` argument validation) | `main` is not `"std"` or `"ustd"`. | Execution stops. Only standardised and unstandardised effect-size variants are implemented. |
| 140 | **error** | *"Unknown stat function."* (in `GetDif()` `dif` branch) | A DIF condition string uses an unsupported operator. | Execution stops. Analogous to #137. |

---

## Summary table

| Notification type | Count |
|-------------------|------:|
| **error** (`stop()`) | 46 |
| **warning** | 24 |
| **message** | 70 |
| **Total** | **140** |

> **Note:** Counts reflect all distinct notification call-sites in the package source at the time of writing.  The same underlying condition may be triggered multiple times during a single analysis run (e.g., once per group or per item).
