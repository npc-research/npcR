
#' Create a horizontal proportion bar chart
#'
#' Creates a horizontal bar chart for displaying one or more
#' proportions. Each bar is drawn against a reference bar
#' representing 100 percent and labeled directly with its
#' percentage value.
#'
#' @param plot_data Data frame containing the variables to plot.
#'
#' @details
#' The input data must contain:
#'
#' \describe{
#'   \item{group}{Category label displayed on the y-axis.}
#'   \item{value}{Proportion value between 0 and 1.}
#' }
#'
#' Labels are placed inside the bar when the value is at least
#' 50 percent and outside the bar otherwise.
#'
#' @return
#' A \code{ggplot2} object.
#'
#' @examples
#' df <- data.frame(
#'   group = c("White", "Black", "Asian"),
#'   value = c(0.62, 0.12, 0.18)
#' )
#'
#' prop_bar_chart(df)
#'
#' @export
prop_bar_chart = function(plot_data){

  ggplot(plot_data, aes(x = value, y = group)) +
    geom_col(
      aes(x=1,y=group),
      fill = NA,
      color = colors$lightblue2,
      linetype = "dotted",
      linewidth = 0.4,
      width = 0.7
    ) +
    geom_col(fill = colors$lightblue2, width = 0.7) +
    geom_text(
      aes(
        label = paste0(group, ": ", scales::percent(value, accuracy = 0.1)),
        hjust = ifelse(value >= 0.5, 1, 0)
      ),
      nudge_x = ifelse(plot_data$value >= 0.5, -0.02, 0.01),
      color = ifelse(plot_data$value >= 0.5, "white", colors$darkblue),
      size = 3.5
    ) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank()
    )
}
