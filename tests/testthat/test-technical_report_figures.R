
test_that("create figure for missing values works", {

  skip_if_not_installed("magick")

  img <- try({
    FigMv(test_path("fixtures/ex1/plots/missing/by_item/Missing_responses_by_item_NR.png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

  # footnote path uses image_graph() + ggplot2 text rendering, which requires
  # FreeType fonts. These are unavailable on macOS ARM64 CI runners and cause
  # a process crash (SIGABRT) rather than a catchable R error.
  skip_on_os("mac")
  img <- try({
    FigMv(
      test_path("fixtures/ex1/plots/missing/by_item/Missing_responses_by_item_NR.png"),
      footnote = "A pretty picture."
    )
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

})


test_that("create Wright map works", {

  skip_if_not_installed("magick")
  # FigWrightMap always adds a footnote, which uses image_graph() + text rendering.
  # On macOS ARM64 CI runners, missing FreeType fonts cause a process crash.
  skip_on_os("mac")
  img <- try({
    FigWrightMap(test_path("fixtures/ex1/plots/Wright_Maps/Wright_map_for_1PL.png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

})

