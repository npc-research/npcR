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
