#' Convert a ggplot object to a flextable image paragraph
#'
#' Saves a ggplot object as a PNG file and returns it as a
#' flextable-compatible paragraph containing an embedded image.
#' This is useful when inserting ggplots directly into flextable
#' cells using functions such as `mk_par()`.
#'
#' @param ggplot_obj A ggplot object to save and insert.
#' @param file_path Character string specifying the output PNG file path.
#'   Defaults to `"./images/Rplot.png"`.
#' @param height Numeric height of the saved image in inches.
#' @param width Numeric width of the saved image in inches.
#'
#' @return A flextable paragraph object created with
#'   `flextable::as_paragraph()` containing the saved image.
#'
#' @details
#' The plot is first written to disk using `ggplot2::ggsave()`
#' and then converted to a flextable image using
#' `flextable::as_image()`. The returned object can be supplied
#' directly to functions such as `mk_par()` or `compose()`.
#'
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' img <- gg2_ft(
#'   ggplot_obj = p,
#'   file_path = "scatter.png",
#'   width = 5,
#'   height = 3
#' )
#'
#' ft <- flextable(data.frame(plot = ""))
#' ft <- mk_par(ft, i = 1, j = 1, value = img)
#'
#' @export
gg2_ft = function(ggplot_obj,file_path = "./images/Rplot.png",height,width){

  ggsave(
    filename = file_path,
    device = "png",
    plot = ggplot_obj,
    width = width,
    height = height,
    create.dir = FALSE
  )

  as_paragraph(as_image(src=file_path,width=width,height=height)) %>%
    return()

}
