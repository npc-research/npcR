#' NPC Research Custom Theme
#'
#' This function allows you to easily and consistenly alter the style of your ggplot
#' data viz in the style of NPC Research.
#' @keywords npc_style
#' @export

npc_style <- function() {
  font <- "Calibri"
  npc_blue <- "#001E5B"

  ggplot2::theme(
    # Format text
    # This sets the font, size, and color for the chart's title
    plot.title = ggplot2::element_text(family = font,
                                       size = 18,
                                       face = "bold",
                                       color = npc_blue,
                                       hjust = 0.5),
    # This sets the font, size, and color for the chart's subtitle (if you have one)
    plot.subtitle = ggplot2::element_text(family = font,
                                          size = 11,
                                          color = npc_blue,
                                          margin = ggplot2::margin(5,0,5,0),
                                          hjust = 0.5),
    # This sets the font, size, color, and position of the chart's caption (if you have one)
    plot.caption = ggplot2::element_text(family = font,
                                         size = 11,
                                         color = npc_blue,
                                         margin = ggplot2::margin(5,0,0,0)),

    # Legend format
    # This formats the legend (position and alignment, sets formatting for any text within the legend)
    legend.position = "bottom",
    legend.text.align = 0, # left aligns the text
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = font,
      size = 11,
      color = npc_blue),

    # Axis format
    # This formats the axes (size, color, margins, removes lines and ticks - can always add back later)
    axis.title = ggplot2::element_blank(), # removes axis title (e.g., "island" and "n"), instead use subtitle, don't have to do this
    axis.text = ggplot2::element_text(family = font,
                                      size = 11,
                                      color = npc_blue),
    axis.ticks = ggplot2::element_blank(), # removes ticks
    axis.line = ggplot2::element_blank(), # removes axis lines

    # Grid lines format
    # This removes all major and minor gridlines (can add back later)
    panel.grid.minor = ggplot2::element_blank(), # removes all minor gridlines
    panel.grid.major = ggplot2::element_blank(), # removes all major gridlines

    # Background format
    # sets background as blank
    panel.background = ggplot2::element_blank(), # gets rid of standard gray background

    # Multi-faceted charts background format
    # Removes standard gray background to white for when you're making small multiples + changes title text size for small multiples
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(size = 11,
                                       hjust = 0))

}
