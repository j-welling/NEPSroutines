
test_that("create table with item properties works", {

  skip_if_not_installed("flextable")
  data("ex2")

  tbl <- try({
    TblItemProps(vars = ex2$vars, select = "mixed", prop = "type",
                 propname = "Response formats")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))

})


test_that("create table with item facets works", {

  skip_if_not_installed("flextable")
  data("ex2")

  tbl <- try({
    TblItemFacets(vars = ex2$vars, select = "mixed",
                  facets = c("Dimension" = "content"),
                  position = "pos")
  })
  expect_false(inherits(tbl, "try-error"))
  expect_true(inherits(tbl, "flextable"))

})

