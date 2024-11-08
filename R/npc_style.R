npc_style <- function() {
#' NPC Research Custom Theme
#' This function allows you to easily and consistently alter the style of your ggplot
#' data viz in the style of NPC Research.
#'
#' The npc_style function can be added to any plot to change basic styling elements to match NPC Research's styling preferences. This function makes the following changes to your plot (if applicable):
#'
#' - Changes font to Calibri and color to NPC Blue (#001E5B) for all text
#' - For the title, it changes the font size to 18, bold, and center aligns it
#' - For the subtitle, it changes the font size to 11, center aligns it, and adds a little bit of spacing above and below the subtitle
#' - For the caption, it changes the font size to 11, and adds some spacing around the caption
#' - Moves the legend to the bottom of the plot, left aligns the text, removes the legend title and background, and changes font size to 11
#' - For the axes, it removes the titles, removes ticks, and removes axis lines
#' - Removes grid lines
#' - Gets rid of standard gray background
#' - Gets rid of standard gray background for small multiples
#' - Changes font size to 11 for small multiples
#'
#' The intent of this function is to make the basics of NPC's style easy to create in R, while still leaving room for unique decisions to be made for each individual plot. Any aspect of npc_style() can be easily overridden by adding any changes AFTER npc_style() has been applied (see example below).
#'
#' @keywords npc_style
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(npcR)
#'
#' # Create a simple plot
#' car_plot <- mtcars %>% # mtcars is a preloaded dataset
#'   ggplot(aes(x = mpg, y = cyl)) + # Set the x and y axis for your plot
#'   geom_point() + # Choose which type of plot you want (this is a scatter plot)
#'   labs(title = "A Plot About Cars", # Add a title, subtitle, and/or caption (completely optional)
#'        subtitle = "Created to demonstrate the npc_style() function",
#'        caption = "Dataviz by Emily Katz, Source: mtcars dataset")
#'
#' # Run this to see what your basic plot looks like
#' car_plot
#'
#' # Add npc_style() to any plot
#' car_plot + npc_style()
#'
#' # If you want to override any of the style settings in npc_style(), add those AFTER you add npc_style()
#' car_plot + npc_style() +
#'   theme(plot.title = element_text(hjust = 0), # Left aligns the title
#'         plot.subtitle = element_text(hjust = 0)) # Left aligns the subtitle

  font <- "Calibri"
  npc_blue <- "#001E5B"

  ggplot2::theme(
    # Format text
    # This sets the font, size, and color for the chart's title
    plot.title = ggplot2::element_text(family = font,
                                       # size = 18,
                                       face = "bold",
                                       color = npc_blue,
                                       hjust = 0.5),
    # This sets the font, size, and color for the chart's subtitle (if you have one)
    plot.subtitle = ggplot2::element_text(family = font,
                                          # size = 11,
                                          color = npc_blue,
                                          margin = ggplot2::margin(5,0,5,0),
                                          hjust = 0.5),
    # This sets the font, size, color, and position of the chart's caption (if you have one)
    plot.caption = ggplot2::element_text(family = font,
                                         # size = 11,
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
      # size = 11,
      color = npc_blue),

    # Axis format
    # This formats the axes (size, color, margins, removes lines and ticks - can always add back later)
    axis.title = ggplot2::element_blank(), # removes axis title (e.g., "island" and "n"), instead use subtitle, don't have to do this
    axis.text = ggplot2::element_text(family = font,
                                      # size = 11,
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
    strip.text = ggplot2::element_text(
                          # size = 11,
                          hjust = 0))
}
