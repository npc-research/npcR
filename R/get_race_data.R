#' Retrieve ACS race population estimates by geography
#'
#' Retrieves race-specific population estimates from ACS table B01001A-I
#' and combines them into a single data frame. Results may be returned
#' for states, counties, places, or census tracts and optionally grouped
#' by age category.
#'
#' This function is a convenience wrapper around
#' \code{get_age_census_data()}, querying all race-specific variants of
#' Census table B01001 and stacking the results.
#'
#' @param geography Geographic level passed to
#'   \code{get_age_census_data()}. Common values include
#'   \code{"state"}, \code{"county"}, \code{"place"}, and
#'   \code{"tract"}.
#' @param state_code Two-letter state abbreviation.
#' @param age_bins Optional character vector of age categories to return.
#'   When \code{NULL}, only total population counts by race are returned.
#'
#' @details
#' The function retrieves data from the following ACS tables:
#'
#' \itemize{
#'   \item B01001A - White Alone
#'   \item B01001B - Black or African American Alone
#'   \item B01001C - American Indian and Alaska Native Alone
#'   \item B01001D - Asian Alone
#'   \item B01001E - Native Hawaiian and Other Pacific Islander Alone
#'   \item B01001F - Some Other Race Alone
#'   \item B01001G - Two or More Races
#'   \item B01001H - White Alone, Not Hispanic or Latino
#'   \item B01001I - Hispanic or Latino
#' }
#'
#' When \code{age_bins = NULL}, only table totals are retained and
#' \code{age_bin} is set to \code{"all ages"}.
#'
#' @return A data frame containing:
#' \describe{
#'   \item{name}{Geographic name.}
#'   \item{race}{Race category.}
#'   \item{population}{Estimated population count.}
#'   \item{age_bin}{Age category or \code{"all ages"}.}
#'   \item{sex}{Sex associated with the estimate when age groups are requested.}
#'   \item{variable}{ACS variable identifier.}
#' }
#'
#' @examples
#' \dontrun{
#' race_totals <- get_race_data(
#'   geography = "county",
#'   state_code = "OR"
#' )
#'
#' working_age <- c(
#'   "18 and 19 years",
#'   "20 to 24 years",
#'   "25 to 29 years",
#'   "30 to 34 years",
#'   "35 to 44 years",
#'   "45 to 54 years",
#'   "55 to 64 years"
#' )
#'
#' race_by_age <- get_race_data(
#'   geography = "county",
#'   state_code = "OR",
#'   age_bins = working_age
#' )
#' }
#'
#' @export
get_race_data = function(geography,state_code,age_bins){

  race_table_letters <- LETTERS[1:9]

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
    lapply(race_table_letters, function(x) {
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
      filter(str_detect(variable,paste0("B01001","[",race_table_letters[1],"-",race_table_letters[length(race_table_letters)],"]","_001"))) %>%
      mutate(age_bin = "all ages")
  }

  if(geography == "tract"){
    data = data %>%
      rename(name = tract_name)
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
