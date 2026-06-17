get_race_data = function(geography,state_code,age_bins){

  letters_vec <- LETTERS[1:9]

  race_letters = c(
    "A" = "White Alone",
    "B" = "Black or African American Alone",
    "C" = "American Indian and Alaska Native Alone",
    "D" = "Asian Alone",
    "E" = "Native Hawaiian and Other Pacific Islander Alone",
    "F" = "Some Other Race Alone",
    "G" = "Two or More Races",
    "H" = "White Alone, Not Hispanic or Latino",
    "I" = "Hispanic or Latino"
  )

  data = do.call(
    rbind,
    lapply(letters_vec, function(x) {
      get_age_census_data(
        geography = geography,
        state_code = state_code,
        table_number = "B01001",
        table_letter = x,
        age_bins = age_bins
      )
    })
  )

  if(is.null(age_bins)){
    data = data %>%
      filter(str_detect(variable,paste0("B01001","[",letters_vec[1],"-",letters_vec[length(letters_vec)],"]","_001"))) %>%
      mutate(age_bin = "all ages")
  }

  if(geography == "place"){
    data = data %>%
      rename(name = place_name)
  }

  if(geography == "county"){
    data = data %>%
      rename(name = county_name)
  }

  if(geography == "state"){
    data = data %>%
      mutate(state_name = str_c(state_name," State")) %>%
      rename(name = state_name)
  }

  return(data)
}
