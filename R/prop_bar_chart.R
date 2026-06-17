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
