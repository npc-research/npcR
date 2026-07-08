#' Find the most recent available ACS year
#'
#' Searches backward from the current calendar year to find the latest year for
#' which ACS variables can be loaded for the specified product. This is useful
#' when you want to automatically detect the newest available ACS release
#' instead of hard-coding a year.
#'
#' The function checks up to 15 years in the past and returns the first year
#' for which `load_variables()` succeeds. For decennial frequency, only years
#' divisible by 10 are tested.
#'
#' @param product Character scalar. ACS product to test. Common values include
#'   `"acs1"` and `"acs5"`.
#' @param frequency Character scalar. ACS frequency to test. Supported values
#'   include `"yearly"`, `"annually"`, `"fiveyears"`, `"quinquennial"`, and
#'   `"decennial"`.
#'
#' @return An integer giving the most recent ACS year available for the
#'   requested `product` and `frequency`.
#'
#' @details
#' The function attempts to call `load_variables(year, product, cache = TRUE)`
#' for each candidate year, starting from the current year and moving backward.
#' The first year that loads successfully is returned.
#'
#' @examples
#' \dontrun{
#' latest_acs_year()
#' latest_acs_year(product = "acs5")
#' latest_acs_year(product = "acs5", frequency = "decennial")
#' }
#'
#' @export
latest_acs_year <- function(product = "acs5", frequency = "yearly") {

  current_year <- as.numeric(format(Sys.Date(), "%Y"))

  period_lookup <- c(
    yearly       = 1,
    annually     = 1,
    fiveyears    = 5,
    quinquennial = 5,
    decennial    = 10
  )

  for (yr in seq(current_year, current_year - 15, by = -1)) {

    # Optional: enforce valid years for decennial
    if (frequency == "decennial" && yr %% 10 != 0) next

    result <- tryCatch({
      load_variables(yr, product, cache = TRUE)
      return(yr)
    }, error = function(e) NULL)

    if (!is.null(result)) {
      return(result)
    }
  }

  stop("No ACS data found in tested range.")
}
