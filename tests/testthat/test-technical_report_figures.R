
# Helper: check if magick can render ggplot2 text (requires FreeType fonts)
.can_render_magick_text <- function() {
  if (!requireNamespace("magick", quietly = TRUE)) return(FALSE)
  tryCatch({
    dev <- magick::image_graph(100, 100, bg = "white")
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.new()
    graphics::text(0.5, 0.5, "test")
    grDevices::dev.off()
    TRUE
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    FALSE
  })
}

test_that("create figure for missing values works", {

  skip_if_not_installed("magick")

  img <- try({
    FigMv(test_path("fixtures/ex1/plots/missing/by_item/Missing_responses_by_item_NR.png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

  # footnote path uses image_graph() + ggplot2 text - requires fonts
  skip_if(!.can_render_magick_text(), "ImageMagick cannot render text (fonts unavailable)")
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
  # FigWrightMap always includes a footnote, which requires image_graph() + fonts
  skip_if(!.can_render_magick_text(), "ImageMagick cannot render text (fonts unavailable)")
  img <- try({
    FigWrightMap(test_path("fixtures/ex1/plots/Wright_Maps/Wright_map_for_1PL.png"))
  })
  expect_false(inherits(img, "try-error"))
  expect_true(inherits(img, "magick-image"))

})

