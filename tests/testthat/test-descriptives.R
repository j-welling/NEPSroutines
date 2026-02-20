library(testthat)

################################################################################

test_that("sample_by_group() works", {

  # Load data
  data(b129)
  result_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/sample.size.by.difficulty.xlsx")

  # Run function
  sample_by_group(resp = resp,
                  grouping_variable = "difficulty",
                  save = TRUE, overwrite = TRUE,
                  path = "tests/testthat/fixtures/tables_curr")
  result_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/sample.size.by.difficulty.xlsx")


  #---------------------------------------
  # Compare the numeric results directly
  expect_equal(
    colnames(result_new), colnames(result_old),
               info = "Column names differ between old and new versions"
    )

  expect_equal(
    result_new, result_old,
    info = "Values in summary.size.by.difficulty differ between old and new versions"
    )
})

################################################################################

test_that("props_by_group() works", {

  # Load data
  data(b129)
  text_type_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/item.properties.by.version.xlsx",
                                      sheet = "text_type")
  kog_type_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/item.properties.by.version.xlsx",
                                      sheet = "kog_type")
  type_old <- readxl::read_excel("tests/testthat/fixtures/tables_prev/item.properties.by.version.xlsx",
                                      sheet = "type")
  # Run function
  props_by_group(vars = vars,
                 select = "mixed",
                 grouping = c("easy", "difficult"),
                 properties = c("text_type", "kog_type", "type"),
                 labels = list(
                   text_type = c(
                     Advertising = 1,
                     Information = 2,
                     Instruction = 3,
                     Literary = 4,
                     Commenting = 5
                     ),
                   kog_type = c(
                     Finding_information = 1,
                     Drawing_conclusions = 2,
                     Reflecting = 3
                     )
                   ),
                 save = TRUE, overwrite = TRUE,
                 path = "tests/testthat/fixtures/tables_curr",
                 warn = TRUE)
  text_type_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/item.properties.by.version.xlsx",
                                      sheet = "text_type")
  kog_type_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/item.properties.by.version.xlsx",
                                     sheet = "kog_type")
  type_new <- readxl::read_excel("tests/testthat/fixtures/tables_curr/item.properties.by.version.xlsx",
                                 sheet = "type")

  #---------------------------------------
  # Compare the numeric results directly
  expect_equal(
    colnames(text_type_new), colnames(text_type_old),
    info = "Column names in text_type sheet differ between old and new versions"
  )
  expect_equal(
    colnames(kog_type_new), colnames(kog_type_old),
    info = "Column names in kog_type sheet differ between old and new versions"
  )
  expect_equal(
    colnames(type_new), colnames(type_old),
    info = "Column names in type sheet differ between old and new versions"
  )

  expect_equal(
    text_type_new, text_type_old,
    info = "Values in text_type sheet differ between old and new versions"
  )
  expect_equal(
    kog_type_new, kog_type_old,
    info = "Values in kog_type sheet differ between old and new versions"
  )
  expect_equal(
    type_new, type_old,
    info = "Values in type sheet differ between old and new versions"
  )
})

