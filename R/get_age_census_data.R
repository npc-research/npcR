get_age_census_data = function(
    geography = "county",
    state_code,
    counties = NULL,
    census_year = NULL,
    table_number = "B01001",
    table_letter = "A",
    age_bins = NULL
){
  #' Function finding Census Age Data for Counties in specified State
  #' @param geography input to get_acs() function, "state", "county", "place", "tract"
  #' @param table_number Census Table of data of interested
  #' @param table_letter letter for which race you are collecting data on, see example for xwalk
  #'
  #' @return data.frame
  #' @export
  #'
  #' @examples
  #' age_populations = rbind(
  #'  get_age_census_data(table_letter = "A", age_bins = age_bins), # White Alone
  #'  get_age_census_data(table_letter = "B", age_bins = age_bins), # Black or African American Alone
  #'  get_age_census_data(table_letter = "C", age_bins = age_bins), # American Indian and Alaska Native Alone
  #'  get_age_census_data(table_letter = "D", age_bins = age_bins), # Asian Alone
  #'  get_age_census_data(table_letter = "E", age_bins = age_bins), # Native Hawaiian and Other Pacific Islander Alone
  #'  get_age_census_data(table_letter = "F", age_bins = age_bins), # Some Other Race Alone
  #'  get_age_census_data(table_letter = "G", age_bins = age_bins), # Two or More Races
  #'  get_age_census_data(table_letter = "H", age_bins = age_bins), # White Alone, Not Hispanic or Latino
  #'  get_age_census_data(table_letter = "I", age_bins = age_bins)  # Hispanic or Latino
  #' )


  # age_bins = c(
  #   "18 and 19 years",
  #   "20 to 24 years",
  #   "25 to 29 years",
  #   "30 to 34 years",
  #   "35 to 44 years",
  #   "45 to 54 years",
  #   "55 to 64 years"
  # )

  age_sex_xwalk = tibble::tribble(
    ~table,~sub_table_label,~desc,          ~sex,    ~age_bin,
    "B01001", "001", "Estimate!!Total:", NA,NA,
    "B01001", "002", "Estimate!!Total:!!",   "Male:", NA,
    "B01001", "017", "Estimate!!Total:!!",   "Female:", NA,
    "B01001", "003", "Estimate!!Total:!!",   "Male", "Under 5 years",
    "B01001", "004", "Estimate!!Total:!!",   "Male", "5 to 9 years",
    "B01001", "005", "Estimate!!Total:!!",   "Male", "10 to 14 years",
    "B01001", "006", "Estimate!!Total:!!",   "Male", "15 to 17 years",
    "B01001", "007", "Estimate!!Total:!!",   "Male", "18 and 19 years",
    "B01001", "008", "Estimate!!Total:!!",   "Male", "20 to 24 years",
    "B01001", "009", "Estimate!!Total:!!",   "Male", "25 to 29 years",
    "B01001", "010", "Estimate!!Total:!!",   "Male", "30 to 34 years",
    "B01001", "011", "Estimate!!Total:!!",   "Male", "35 to 44 years",
    "B01001", "012", "Estimate!!Total:!!",   "Male", "45 to 54 years",
    "B01001", "013", "Estimate!!Total:!!",   "Male", "55 to 64 years",
    "B01001", "014", "Estimate!!Total:!!",   "Male", "65 to 74 years",
    "B01001", "015", "Estimate!!Total:!!",   "Male", "75 to 84 years",
    "B01001", "016", "Estimate!!Total:!!",   "Male", "85 years and over",
    "B01001", "018", "Estimate!!Total:!!", "Female", "Under 5 years",
    "B01001", "019", "Estimate!!Total:!!", "Female", "5 to 9 years",
    "B01001", "020", "Estimate!!Total:!!", "Female", "10 to 14 years",
    "B01001", "021", "Estimate!!Total:!!", "Female", "15 to 17 years",
    "B01001", "022", "Estimate!!Total:!!", "Female", "18 and 19 years",
    "B01001", "023", "Estimate!!Total:!!", "Female", "20 to 24 years",
    "B01001", "024", "Estimate!!Total:!!", "Female", "25 to 29 years",
    "B01001", "025", "Estimate!!Total:!!", "Female", "30 to 34 years",
    "B01001", "026", "Estimate!!Total:!!", "Female", "35 to 44 years",
    "B01001", "027", "Estimate!!Total:!!", "Female", "45 to 54 years",
    "B01001", "028", "Estimate!!Total:!!", "Female", "55 to 64 years",
    "B01001", "029", "Estimate!!Total:!!", "Female", "65 to 74 years",
    "B01001", "030", "Estimate!!Total:!!", "Female", "75 to 84 years",
    "B01001", "031", "Estimate!!Total:!!", "Female", "85 years and over"
  )


if (is.null(age_bins)) {
  age_sex_xwalk <- age_sex_xwalk %>%
    filter(is.na(age_bin))
} else {
  age_sex_xwalk <- age_sex_xwalk %>%
    filter(age_bin %in% age_bins)
}

  if(is.null(census_year)) census_year = latest_acs_year(frequency = "annually")

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

  data = get_acs(
    geography = geography,
    variables = paste0(
      table_number,
      table_letter,
      "_",
      age_sex_xwalk$sub_table_label
    ),
    year = census_year,
    state = state_code,
    survey = "acs5"
  )

  if(geography == "county" & !is.null(counties)) data = data %>% filter(GEOID %in% counties$GEOID)

  data = data %>%
    rename(name = NAME) %>%
    mutate(
      sub_table_label = str_remove(variable, paste0(table_number,table_letter,"_")),
      table = table_number,
      race = race_letters[table_letter]
    ) %>%
    left_join(
      age_sex_xwalk %>% select(table,sub_table_label,age_bin,sex),
      by = c("table","sub_table_label")
    ) %>%
    select(variable,name,population = estimate,age_bin,sex,race)

  state_name = data.frame(state.abb,state.name) %>%
      filter(state.abb==state_code) %>%
      pull(state.name)

  if(geography == "tract") {
    data = data %>%
      # TODO clean up tract_name
      # mutate(name = str_remove(name, paste0(", ", state_name))) %>%
      rename(tract_name = name)
  }

  if(geography == "place") {
    data = data %>%
      mutate(name = str_remove(name, paste0(", ", state_name))) %>%
      rename(place_name = name)
  }

  if(geography == "county") {
    data = data %>%
      mutate(name = str_remove(name, paste0(" County, ", state_name))) %>%
      rename(county_name = name)
  }

  if(geography == "state") {
    data = data %>% rename(state_name = name)
  }

  return(data)
}
