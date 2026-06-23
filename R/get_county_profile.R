#' Retrieve demographic and economic profile metrics for a geography
#'
#' Creates a profile dataset using ACS 5-year estimates and Decennial
#' Census data. The resulting dataset includes population, unemployment,
#' poverty, housing value, sex composition, race composition, and
#' urban/rural population characteristics.
#'
#' @param state_code Two-letter state abbreviation.
#' @param geography_type Geographic level to retrieve. Common values
#'   include \code{"county"}, \code{"place"}, \code{"tract"}, and
#'   \code{"state"}.
#' @param census_year ACS year to retrieve. Defaults to the most recent
#'   available ACS release.
#'
#' @details
#' ACS estimates are obtained from the American Community Survey 5-year
#' dataset and combined with urban and rural population measures from
#' the most recent Decennial Census Detailed Housing Characteristics
#' (DHC) summary file.
#'
#' Derived metrics include:
#'
#' \itemize{
#'   \item Unemployment rate
#'   \item Poverty rate
#'   \item Proportion male
#'   \item Race proportions
#'   \item Urban population percentage
#'   \item Rural population percentage
#' }
#'
#' @return A data frame containing one row per geography with:
#'
#' \describe{
#'   \item{GEOID}{Census geographic identifier}
#'   \item{NAME}{Geographic name}
#'   \item{geography}{Requested geography level}
#'   \item{total_pop}{Total population}
#'   \item{unemployment_rate}{Unemployed labor force proportion}
#'   \item{median_home_value}{Median owner-occupied home value}
#'   \item{poverty_rate}{Population below poverty level proportion}
#'   \item{proportion_male}{Male share of population}
#'   \item{prop_white}{White alone population proportion}
#'   \item{prop_black}{Black population proportion}
#'   \item{prop_native}{American Indian/Alaska Native proportion}
#'   \item{prop_asian}{Asian population proportion}
#'   \item{prop_pacific}{Native Hawaiian/Pacific Islander proportion}
#'   \item{prop_other}{Other race proportion}
#'   \item{prop_multi}{Two or more races proportion}
#'   \item{urban_pct}{Urban population proportion}
#'   \item{rural_pct}{Rural population proportion}
#' }
#'
#' @examples
#' \dontrun{
#' county_profiles <- get_county_profile(
#'   state_code = "OR"
#' )
#'
#' place_profiles <- get_county_profile(
#'   state_code = "OR",
#'   geography_type = "place"
#' )
#' }
#'
#' @export
get_county_profile <- function(state_code, geography_type = "county",census_year = NULL) {

  if(is.null(census_year)) census_year = latest_acs_year(frequency = "annually")

  acs_data <- get_acs(
    geography = geography_type,
    state = state_code,
    year = census_year,
    variables = c(
      population = "B01003_001",
      unemployed = "B23025_005",
      labor_force = "B23025_003",
      median_home_value = "B25077_001",
      total_poverty = "B17001_001",
      below_poverty = "B17001_002",
      male = "B01001_002",
      female = "B01001_026",

      # Race Variables
      total_pop = "B02001_001",
      white = "B02001_002",
      black = "B02001_003",
      native = "B02001_004",
      asian = "B02001_005",
      pacific = "B02001_006",
      other = "B02001_007",
      multi = "B02001_008"

    ),
    geometry = FALSE
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      geography = geography_type,
      unemployment_rate = unemployed / labor_force,
      # CHECK this math with KK
      poverty_rate = below_poverty / total_poverty,
      proportion_male = male / (male + female),
      prop_white = white / total_pop,
      prop_black = black / total_pop,
      prop_native = native / total_pop,
      prop_asian = asian / total_pop,
      prop_pacific = pacific / total_pop,
      prop_other = other / total_pop,
      prop_multi = multi / total_pop
    )

  acs_data <- acs_data %>%
    select(
      GEOID,
      NAME,
      geography,
      total_pop,
      unemployment_rate,
      median_home_value,
      poverty_rate,
      proportion_male,
      starts_with("prop_")
    ) %>%
    left_join(
      get_decennial(
        geography = geography_type,
        variables = c(
          total_pop = "H2_001N", # Total
          urban_pop = "H2_002N", # Urban
          rural_pop = "H2_003N"  # Rural
        ),
        # NOTE urban/rural data is from get_decennial data
        year = latest_acs_year(frequency = "decennial"),
        sumfile = "dhc"
      ) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        mutate(
          urban_pct = urban_pop / total_pop,
          rural_pct = rural_pop / total_pop,
        ) %>%
        select(GEOID,urban_pct,rural_pct),
      by = "GEOID"
    ) %>%
    left_join(
      get_acs(
        geography = geography_type,
        state = state_code,
        year = census_year,
        variables = c(population = "B01003_001"),
        geometry = TRUE
      ) %>%
        select(GEOID),
      by = "GEOID"
    )

  return(acs_data)
}
