#'
#' Generate figures in NEPS Survey Papers
#'


#' Load image from file, add note, and print it
#'
#' @param file The file path for the image.
#' @param width The width of the resized image.
#' @param height The height of the resized image
#' @param crop A string indicating how to crop the image in the format
#' "<width>x<height>{+-}<xoffset>{+-}<yoffset>"
#' (see http://www.imagemagick.org/Magick++/Geometry.html)
#' @param dpi The dots per inch.
#' @param footnote An optional note to be displayed below the figure.
#' @param note.offset The height of the additional field below  the original
#' image that will include the note.
#' @param note.bgcol The background color of the note field.
#' @param note.col The font color of the note.
#' @param note.x The left position of the note.
#' @param note.y The top position of the note.
#' @param note.size The font size of the note.
#' @returns An image.
#' @export
Fig <- function(file, width = NULL, height = NULL, crop = NULL, dpi = 96,
                footnote = NULL, note.offset = NULL,
                note.bgcol = "white", note.col = "black",
                note.x = 30, note.y = 70, note.size = 14) {

  # return image without note
  if (is.null(footnote) & is.null(width) & is.null(height) & is.null(crop)) {
    return(knitr::include_graphics(file))
  } else if(!requireNamespace("magick", quietly = TRUE)) {
    stop("Please install magick!")
  }

  # load plot
  img <- magick::image_read(file)
  img_info <- magick::image_info(img)

  # crop image
  if (!is.null(crop)) {
    img <- magick::image_crop(img, crop)
    img_info <- magick::image_info(img)
  }

  # resize image
  if (!is.null(width) | !is.null(height)) {
    if (is.null(width)) {
      img <- magick::image_scale(img, paste0("x", height))
    } else if (is.null(height)) {
      img <- magick::image_scale(img, width)
    } else {
      img <- magick::image_resize(img, paste0(width, "x", height))
    }
    img_info <- magick::image_info(img)
  }

  # skip footnote
  if (is.null(footnote))
    return(img)

  # determine note size
  if (isTRUE(!grepl("\\n", footnote))) {
    n <- gsub("\\n", "", footnote)
    l <- base::strsplit(n, " ")[[1]]
    notes <- c()
    maxw <- img_info$width - 2 * note.x - 120
    while (TRUE) {
      if (length(l) == 0) break

      for (i in seq_along(l)) {
        curw <- graphics::strwidth(paste0(l[seq_len(i)], collapse = " "),
                         font = note.size, units = "inches")
        curw <- curw * 230 * (note.size / 12) # convert to pixel
        if (i == length(l)) {
          notes <- c(notes, paste0(l[seq_len(i)], collapse = " "))
          l <- l[-seq_len(i)]
        } else if (curw > maxw) {
          notes <- c(notes, paste0(l[seq_len(i - 1)], collapse = " "))
          l <- l[-seq_len(i - 1)]
          break
        }
      }
    }

  } else {
    notes <- base::strsplit(footnote, "\n")[[1]]
  }
  note.offset <- note.y + 50 * length(notes)

  # device that will contain variable data
  fig <-
    magick::image_graph(
      width = img_info$width,
      height = img_info$height + note.offset,
      bg = "transparent"
    )

  # empty plot that will contain note
  p <-
    ggplot2::ggplot() +
    ggplot2::xlim(0, img_info$width) +
    ggplot2::ylim(0, img_info$height + note.offset) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.background =
        ggplot2::element_rect(fill = note.bgcol, color = note.bgcol)
    ) +
    ggplot2::annotate(
      "text",
      label = "Note. ",
      fontface = "italic",
      x = note.x,
      y = note.offset - note.y,
      size = note.size,
      hjust = 0,
      color = note.col
    )
  for (i in seq_along(notes)) {
    p <-
      p +
      ggplot2::annotate(
        "text",
        label = notes[i],
        x = note.x + 120,
        y = note.offset - note.y - (i - 1) * 50,
        size = note.size,
        hjust = 0,
        color= note.col
      )
  }
  print(p)
  dev.off()

  # combine images
  img_comb <-
    magick::image_composite(
      fig,
      img,
      operator = 'Over',
      gravity ="NorthWest"
    )

  return(img_comb)

}


#' Crop plot for missing values to remove title
#'
#' @param file A file path for the image.
#' @param footnote An additional note to be displayed below the figure.
#' @returns An image.
#' @export
FigMv <- function(file, footnote = NULL) {

  return(Fig(file, width = 1900, crop = "2900x2900+0+80", footnote = footnote))

}

#' @rdname FigMv
FigMV <- FigMv


#' Crop plot with Wright map and include footnote
#'
#' @param file A file path for the image.
#' @param tbl The table number for the footnote.
#' @returns An image.
#' @export
FigWrightMap <- function(file, tbl = 5) {

  return(Fig(
    file = file,
    crop = "800x1010+5+50",
    width = 1500,
    note.y = 150,
    footnote =
      paste0("The distribution of the person abilities in the ",
             "sample is given on the left-hand side of the graph. ",
             "The category thresholds of the items are given on ",
             "the right-hand side of the graph. Each number ",
             "represents one threshold with the first part (before ",
             "the dot) corresponding to the item numbers given in ",
             "Table ", tbl, " and the second part indicating the ",
             "threshold.")
  ))

}

#' @rdname FigWrightMap
FigWright_Map <- FigWrightMap

