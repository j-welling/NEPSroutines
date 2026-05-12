
test_that("Fig() works when no image processing is requested", {

  file <- test_path(
    "fixtures", "ex1", "plots", "missing", "by_item",
    "Missing_responses_by_item_NR.png"
  )
  img <- Fig(file)

  expect_s3_class(img, "knit_image_paths")
  expect_s3_class(img, "knit_asis")
  expect_equal(unclass(img), file)

})


test_that("Fig() crops and resizes images deterministically", {

  skip_if_not_installed("magick")

  file <- test_path(
    "fixtures", "ex1", "plots", "missing", "by_item",
    "Missing_responses_by_item_NR.png"
  )
  cropped <- Fig(file, crop = "500x400+0+0")
  cropped_info <- magick::image_info(cropped)

  expect_s3_class(cropped, "magick-image")
  expect_equal(cropped_info$format, "PNG")
  expect_equal(c(cropped_info$width, cropped_info$height), c(500, 400))

  resized_width <- Fig(file, width = 500)
  resized_width_info <- magick::image_info(resized_width)
  expect_equal(c(resized_width_info$width, resized_width_info$height), c(500, 300))

  resized_height <- Fig(file, height = 300)
  resized_height_info <- magick::image_info(resized_height)
  expect_equal(c(resized_height_info$width, resized_height_info$height), c(500, 300))

  resized_both <- Fig(file, width = 500, height = 400)
  resized_both_info <- magick::image_info(resized_both)
  expect_equal(c(resized_both_info$width, resized_both_info$height), c(500, 300))

})


test_that("Fig() appends a note area below processed images", {

  skip_if_not_installed("magick")
  skip_if_not_installed("ggplot2")

  # Footnote rendering uses image_graph() and text rendering. On macOS ARM64 CI,
  # unavailable FreeType fonts can abort the R process rather than signal an R error.
  skip_on_os("mac")

  file <- test_path(
    "fixtures", "ex1", "plots", "missing", "by_item",
    "Missing_responses_by_item_NR.png"
  )

  plain <- Fig(file, width = 500)
  noted <- Fig(file, width = 500, footnote = "A pretty picture.")
  multiline <- Fig(file, width = 500, footnote = "Line one.\nLine two.")

  plain_info <- magick::image_info(plain)
  noted_info <- magick::image_info(noted)
  multiline_info <- magick::image_info(multiline)

  expect_equal(noted_info$width, plain_info$width)
  expect_equal(multiline_info$width, plain_info$width)
  expect_equal(noted_info$height, plain_info$height + 120)
  expect_equal(multiline_info$height, plain_info$height + 170)
  expect_true(noted_info$matte)
  expect_true(multiline_info$matte)

})

test_that("FigMV() works when no image processing is requested", {

  skip_if_not_installed("magick")

  file <- test_path(
    "fixtures", "ex1", "plots", "missing", "by_item",
    "Missing_responses_by_item_NR.png"
  )

  img <- FigMv(file)
  img_info <- magick::image_info(img)

  expect_s3_class(img, "magick-image")
  expect_equal(img_info$format, "PNG")
  expect_equal(c(img_info$width, img_info$height), c(1900, 1064))
  expect_equal(
    magick::image_info(FigMv(file))[, c("format", "width", "height")],
    magick::image_info(FigMV(file))[, c("format", "width", "height")]
  )

})

test_that("FigMv() includes a footnote when requested", {

  skip_if_not_installed("magick")
  skip_if_not_installed("ggplot2")
  skip_on_os("mac")

  file <- test_path(
    "fixtures", "ex1", "plots", "missing", "by_item",
    "Missing_responses_by_item_NR.png"
  )

  plain <- FigMv(file)
  noted <- FigMv(file, footnote = "A pretty picture.")

  plain_info <- magick::image_info(plain)
  noted_info <- magick::image_info(noted)

  expect_equal(noted_info$width, plain_info$width)
  expect_equal(noted_info$height, plain_info$height + 120)
  expect_true(noted_info$matte)

})


test_that("FigWrightMap() works when no image processing is requested", {

  skip_if_not_installed("magick")
  skip_if_not_installed("ggplot2")
  skip_on_os("mac")

  file <- test_path(
    "fixtures", "ex1", "plots", "Wright_Maps", "Wright_map_for_1PL.png"
  )
  img <- FigWrightMap(file)
  img_info <- magick::image_info(img)

  expect_s3_class(img, "magick-image")
  expect_equal(img_info$format, "PNG")
  expect_equal(c(img_info$width, img_info$height), c(1500, 2256))
  expect_true(img_info$matte)
  expect_equal(
    magick::image_info(FigWright_Map(file))[, c("width", "height")],
    magick::image_info(FigWrightMap(file))[, c("width", "height")]
  )

})


test_that("FigWrightMap() adds polytomous footnote", {

  skip_if_not_installed("magick")
  skip_if_not_installed("ggplot2")
  skip_on_os("mac")

  file <- test_path(
    "fixtures", "ex2", "plots", "Wright_Maps", "Wright_map_for_PCM2.png"
  )

  img <- FigWrightMap(file, tbl = 9)
  img_info <- magick::image_info(img)

  expect_s3_class(img, "magick-image")
  expect_equal(img_info$format, "PNG")
  expect_equal(c(img_info$width, img_info$height), c(1500, 2356))
  expect_true(img_info$matte)

})


