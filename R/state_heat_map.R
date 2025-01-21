state_heat_map = function(state_data,legend_title = NULL){
  #' Function for Creating Heat Map of US States
  #'
  #' @param data A class of data.frame that has variables state_abbr and count
  #' @param legend_title A class of "string" that is what you want the legend title to display.
  #'
  #' @return ggplot heat map
  #' @export
  #'
  #' @examples
  #'
  #'library(usmap)
  #'
  #'data = haven::read_sav("P:/6. Projects Active/Harm Reduction in Tx Cts/Survey/Data/Alchemer Exports/Harm Reduction Survey_working.sav") %>%
  #' sjlabelled::as_label() %>%
  #'   select(state_abbr = Q1.3_4, type = Q1.1) %>%
  #'   # filter(type == "Adult Treatment Court") %>%
  #'   group_by(state_abbr) %>%
  #'   summarise(count = n())
  #'
  #'state_heat_map(data)
  #'

  library(usmap)

  if(is.null(legend_title)) legend_title = "Number of programs"

  if(!"state_abbr" %in% names(state_data)) warning("Make sure state_abbr is a character variable in yout state_data input.")
  if(!"count" %in% names(state_data)) warning("Make sure count state_abbr is a character variable in yout state_data input.")

  state_abbrev = data.frame(
    abbr = state.abb,
    state = tolower(state.name)
  )

  # Join state abbreviations to full state names
  state_data <- left_join(state_data, state_abbrev, by = c("state_abbr" = "abbr"))

  # Get map data for US states
  states_map <- us_map(regions = "state")

  # Joining summarized data with the map data
  map_data <- left_join(states_map, state_data, by = c("abbr" = "state_abbr"))

  # Plot the density map
  ggplot(map_data, aes(geometry = geom, fill = count)) +
    geom_sf(color = "black") +  # Draw state boundaries
    scale_fill_continuous(
      name = legend_title,
      labels = scales::number_format(accuracy = 1),
      low = "lightgray",
      high = "#305496",
      na.value = "white",
      guide = "colorbar"
      ) +  # Gray for missing states
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),  # Remove axis labels for cleaner map
      axis.title.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
      )

}
