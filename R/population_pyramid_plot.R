#' Create a population pyramid plot
#'
#' Creates a population pyramid displaying male and female population
#' counts by age group for a selected geography.
#'
#' The input data is expected to contain sex-specific population
#' estimates by age group, such as output from
#' \code{get_age_census_data()}.
#'
#' @param geo_name Character string identifying the geography to plot.
#'   Must match a value in the \code{name} column of
#'   \code{pyramid_df}.
#' @param pyramid_df Data frame containing age-specific population
#'   estimates.
#' @param size_mult Numeric scaling factor applied to plot text and
#'   annotation sizes. Default is \code{1}.
#'
#' @details
#' Male populations are displayed to the right of the center axis and
#' female populations are displayed to the left. Age categories are
#' displayed along a central label column.
#'
#' Required columns in \code{pyramid_df}:
#'
#' \describe{
#'   \item{name}{Geographic name}
#'   \item{age_bin}{Age-group label}
#'   \item{sex}{Male or Female}
#'   \item{population}{Population estimate}
#' }
#'
#' Population values are aggregated within age group and sex prior to
#' plotting.
#'
#' @return
#' A \code{ggplot2} / \code{patchwork} plot object.
#'
#' @examples
#' \dontrun{
#' age_data <- get_age_census_data(
#'   geography = "county",
#'   state_code = "OR",
#'   age_bins = age_bin_xwalk$age_bins
#' )
#'
#' population_pyramid_plot(
#'   geo_name = "Multnomah",
#'   pyramid_df = age_data
#' )
#' }
#'
#' @export
population_pyramid_plot = function(geo_name, pyramid_df, size_mult = 1){

  age_bin_xwalk = data.frame(
    age_bins =
      c(
        "Under 5 years","5 to 9 years","10 to 14 years","15 to 17 years","18 and 19 years",
        "20 to 24 years","25 to 29 years","30 to 34 years","35 to 44 years","45 to 54 years",
        "55 to 64 years","65 to 74 years","75 to 84 years","85 years and over"
      ),
    labels = c(
      "0-4","5-9","10-14","15-17","18-19","20-24",
      "25-29","30-34","35-44","45-54","55-64",
      "65-74","75-84","85+"
    )
  )

  pyramid_df = pyramid_df %>%
    filter(name==geo_name) %>%
    rename(age_bins=age_bin) %>%
    group_by(age_bins,sex) %>%
    summarise(population = sum(population), .groups = "drop_last") %>%
    left_join(age_bin_xwalk, by = "age_bins") %>%
    mutate(age_bin = factor(labels, levels = age_bin_xwalk$labels))

  female_df <- pyramid_df %>%
    filter(sex == "Female") %>%
    mutate(
      value = -population,
      age_bin = factor(labels)
    )

  male_df <- pyramid_df %>%
    filter(sex == "Male") %>%
    mutate(
      value = population,
      age_bin = factor(labels)
    )

  # Find max population for symmetric x-axis
  population_pyramid_limit <- max(pyramid_df$population, na.rm = TRUE)

  pyramid_theme =
    theme_void(base_size = 10 * size_mult) +
    theme(
      axis.text.x = element_text(
        color = "darkgray",
        size = 9 * size_mult,
        margin = margin(3, 0, 0, 0, "pt")
      ),
      axis.text.y = element_blank(),
      panel.grid.major.x = element_line(color = "lightgray")
    )

  # ---- Middle: Age labels ----
  age_labels_plot <- ggplot(pyramid_df, aes(x = 0, y = age_bin, label = age_bin)) +
    geom_text(
      color = "darkgray",
      size = 3.5 * size_mult
    ) +
    scale_x_continuous(limits = c(-1, 1), expand = c(0, 0)) +
    theme_void(base_size = 10 * size_mult)

  make_population_pyramid = function(gender_to_filter){

    if (gender_to_filter == "Men") {
      plot_data = male_df
      gender_color <- colors$darkblue
      text_box_text_color <- "#ffffff"
      reverse_value <- 1
      x_limits <- c(0, population_pyramid_limit * reverse_value)
      gender_hjust = 0.5
    }

    if (gender_to_filter == "Women") {
      plot_data = female_df
      gender_color <- colors$lightblue2
      text_box_text_color <- "white"
      reverse_value <- -1
      x_limits <- rev(c(0, population_pyramid_limit * reverse_value))
      gender_hjust = 0.3
    }

    ggplot(plot_data, aes(x = value, y = age_bin)) +
      geom_col(fill = gender_color, width = 0.7) +
      # geom_text(label = age_bins) +
      annotate(
        geom = "label",
        x = reverse_value * population_pyramid_limit * 0.6,
        y = 15-1.5,
        label = gender_to_filter,
        size = 2.5 * size_mult,
        fontface = "bold",
        color = text_box_text_color,
        fill = gender_color,
        # label.size = 0,
        label.r = unit(0.15, "lines"),
        label.padding = unit(0.35, "lines"),
        hjust = gender_hjust,
        vjust = 0.5
      ) +
      scale_x_continuous(
        limits = x_limits,
        breaks = pretty(x_limits, n = 3),
        labels = function(x) percent(accuracy = 1, scale = 0.001, abs(x)),
      ) +
      coord_cartesian(clip = "off") +
      theme(
        plot.margin = margin(t = 15, r = 5, b = 5, l = 5)
      ) +
      pyramid_theme

  }

  # combine plots
  male_plot = make_population_pyramid(gender_to_filter = "Men")
  female_plot = make_population_pyramid(gender_to_filter = "Women")

  p =
    female_plot +
    age_labels_plot +
    male_plot +
    patchwork::plot_layout(widths = c(3.5, 1, 3.5))

  return(p)
}
