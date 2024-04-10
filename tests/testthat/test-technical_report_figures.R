
test_that("create figure for missing values works", {

  skip_if_not_installed("magick")

  img <- try({
    FigMv(test_path("fixtures/ex1/plots/missing/by_item/Missing_responses_by_item (NR).png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

  img <- try({
    FigMv(
      test_path("fixtures/ex1/plots/missing/by_item/Missing_responses_by_item (NR).png"),
      footnote = "A pretty picture."
    )
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

})


test_that("create Wright map works", {

  skip_if_not_installed("magick")
  img <- try({
    FigWrightMap(test_path("fixtures/ex1/plots/Wright_Maps/Wright_map_for_1PL.png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

})

