read_alchemer = function(
    survey_ids = 7589884,
    end_date = Sys.Date(),
    start_date = Sys.Date() - days(14)
    ){
#' Reading Alchemer Data
#'
#' @param survey_ids argument of class character vector. If bp_surveys is false you must enter a character in this argument on which survey ID numbers you are interested in loading data for.
#' @param end_date argument of class string, that should be specified as a date for when you want to extract survey data
#' @param start_date argument of class string, that should be specified as a date for when you want to extract survey data
#'
#' @return data frame of selected survey's survey responses with survey name start date, submit date, reference number and assessment id.
#' @export
#'
#' @examples
#' data = read_alchemer(survey_ids = c("6726093","7311939"))

  # survey_ids = "7457171" # Ohio
  # survey_ids = "7564298" # NY
  # survey_ids = "7589884" # 2024
  # survey_ids = "7589884" # NPC Treatment Court Assessment 2024

  # survey_id_number = "7589884" # NPC Treatment Court Assessment 2024
  # survey_id_number = "7676066" # Ohio Specialized Dockets - Certification

  # survey_ids = "7676066"
  # end_date = Sys.Date()
  # start_date = "2024-05-29"

# Set up ----
library(ODataQuery)

if(as.Date(start_date)>as.Date(end_date)){print("Error, make sure your start date is the same as your end date or before.");stop()}

surveys = alchemer_surveys(survey_ids = survey_ids)

# function that helps loop through every survey based on survey id number
appending_survey_data = function(survey_id_number){

  # creating link to make api request
  url <- paste0(
    "https://api.alchemer.com/v5/survey/",
    survey_id_number,
    # "7457171", # Ohio
    # "7564298", # NY
    # "7589884", # NPC Treatment Court Assessment 2024

    "/surveyresponse?",

    # defining filters with url variables https://apihelp.alchemer.com/help/filters-v5

    paste(
      # defining start date
      "filter[field][0]=date_submitted",
      "filter[operator][0]=>=",
      paste0("filter[value][0]=", start_date, "+00:00:00"),

      # defining end date
      "filter[field][1]=date_submitted",
      "filter[operator][1]=<=",
      paste0("filter[value][1]=", end_date, "+23:59:59"),
      sep = "&"
    ),

    "&api_token=",Sys.getenv("al_api_key"),"&api_token_secret=",Sys.getenv("al_api_secret_key")

    # , "&resultsperpage=500" # getting error code: 500 for some reason with this
    # https://apihelp.alchemer.com/help/filters#:~:text=to%20the%20limit.-,Browsing%20Examples,-To%20view%20a
  )

  survey = retrieve_data(url, httr_args = list(httr::timeout(60)))
  n_total_responses = survey[["total_count"]]

  # creating sub function that helps loop through an api requests.
  # parameter we index through is basically "&page=n"
  # I've been having some difficulties requesting all the alchemer survey
  # responses using resultsperpage url variable so we're going to loop
  # through page=1-max page number

pagination = function(url_p){

  # url_p = url_pages[2]

  survey_p = retrieve_data(url_p, httr_args = list(httr::timeout(60)))
  df_p = survey_p$data

  # data structure varies across api request. To correctly bind refno
  # I test what the data structures are below and query it as shown below
  if(!is.null(survey_p$data$url_variables$refno$value)){
    df_p$refno = survey_p$data$url_variables$refno$value
  }else{
    df_p$refno = sapply(
            1:nrow(df_p),
            function(i) {
              obj = survey_p[["data"]][["url_variables"]][[i]][["refno"]][["value"]]
              if(is.null(obj)) return("MISSING") # recoding missing refnos as MISSING to keep them
              else return(obj)
              }
            )
  }

  # keep necessary columns and reformatting time variables
  df_p %<>%
    # select(id, date_started, date_submitted, status, region, city, refno) %>%
    mutate(
      date_submitted = as.POSIXct(date_submitted, format = "%Y-%m-%d %H:%M:%S"),
      date_started   = as.POSIXct(date_started,   format = "%Y-%m-%d %H:%M:%S"),
    )

  return(df_p)
}

if(n_total_responses>0){

# defining url links across "&page=n"
url_pages = sapply(1:ceiling(n_total_responses/50), # finding number of pages all survey data is on
  function(p)
  paste0(
    "https://api.alchemer.com/v5/survey/",
    survey_id_number,
    # "7457171", # Ohio
    # "7564298", # NY
    # "7589884", # NPC Treatment Court Assessment 2024

    "/surveyresponse?",

    # defining filters with url variables https://apihelp.alchemer.com/help/filters-v5
    paste(
      # defining start date
      "filter[field][0]=date_submitted",
      "filter[operator][0]=>=",
      paste0("filter[value][0]=", start_date, "+00:00:00"),

      # defining end date
      "filter[field][1]=date_submitted",
      "filter[operator][1]=<=",
      paste0("filter[value][1]=", end_date, "+23:59:59"),
      sep = "&"
    ),

    "&api_token=",Sys.getenv("al_api_key"),"&api_token_secret=",Sys.getenv("al_api_secret_key"),
    "&page=",p
  )
)

url_pages[1] = url_pages[1] %>% str_remove("&page=1$")
# removing page=1 from first link
# I don't think this is necessary but this doesn't hurt to do.

# storing pages as list of lists
survey_i_list <- lapply(
    # 1L,
    1:length(url_pages),
    function(i) {
      pagination(url_pages[i])})
# Sys.sleep(45)

df <- do.call(rbind, survey_i_list) # binding pages together

# appending survey title column
df = cbind(
  title = surveys %>%
    select(id,title) %>%
    filter(id==survey_id_number) %>%
    # filter(id=="7457171") %>%
    pull(title),
  survey_id = surveys %>%
    select(id,title) %>%
    filter(id==survey_id_number) %>%
    # filter(id=="7457171") %>%
    pull(id),
  df
  )

  # if(nrow(df)==150){beep(2);print("increase results per page")}

  # Note: API limit is 240 requests per minute
  # https://apihelp.alchemer.com/help/api-request-limits
  # Because of this if we have a large amount of survey's updated we
  # will want to have our computer take a break in between requests
  # if(nrow(surveys)>150){Sys.sleep(2)}

  print(survey_id_number)

  return(df)
  }else{df = NULL}

}

# Sys.sleep(20)
Sys.time() %>% print()

result <- lapply(
  1:nrow(surveys),
  function(i) {
    output = (surveys %>%
       select(id) %>%
       pull())[i] %>%
       appending_survey_data()

    if(!is.null(output)) {
      output %>%
       select(-matches("survey_data"),-matches("url_variables"),-matches("data_quality")) %>%
        return()
    }
})
result = Filter(Negate(is.null), result)
Sys.time() %>% print()

# Combine the resulting data frames into a single data frame
result_df <- do.call(rbind, result)

  return(result_df)

}
