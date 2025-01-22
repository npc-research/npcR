alchemer_surveys = function(survey_ids){
  #' Viewing Alchemer Surveys
  #' 
  #' @param survey_ids optional argument of class character vector. If bp_surveys is false you must enter a character in this argument on which survey ID numbers you are interested in loading data for. 
  #'
  #' @return data frame of list of all surveys available on Alchemer.
  #' @export
  #'
  #' @examples
  #' data = alchemer_surveys(survey_ids = c("6726093","7311939"))
  #' data = alchemer_surveys(survey_ids = "7311939")
  
  # Set up ----
  library(ODataQuery)
  
  # pull in basic data for all surveys I have access to
  surveys = retrieve_data(
    paste0(
      "https://api.alchemer.com/v5/survey",
      "?api_token=",Sys.getenv("al_api_key"),
      "&api_token_secret=",Sys.getenv("al_api_secret_key"),
      "&resultsperpage=500"
    )
  )
  
  surveys = surveys$data
  
  surveys %<>%
    # select necessary vars
    select(-c("team", "type", "status")) %>%
    filter(id %in% survey_ids) %>% 
    unnest(c(statistics, links)) %>% # this opens up more vars that are nested
    mutate(
      # format date time vars
      modified_on = as.POSIXct(modified_on, format = "%Y-%m-%d %H:%M:%S"),
      created_on = as.POSIXct(created_on, format = "%Y-%m-%d %H:%M:%S"),
    )
  
  cat("Finished Loading all survey data at",Sys.time() %>% as.character(),"\n")
  
  return(surveys)
}
