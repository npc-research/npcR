read_airtable = function(table,base = "Assessment Tracker"){
  #' Function for reading in Air Table tables
  #'
  #' @param base A class of "string" that is the AirTable Base with my table located in it.
  #' @param table A class of "string" that is the name of table I am interested in reading in.
  #'
  #' @return Air Table table
  #' @export
  #'
  #' @examples
  #' 
  #' 
  #' assessments = read_airtable(table = "Assessments",base = "Assessment Tracker")
  #' courts = read_airtable(table = "Courts",base = "Assessment Tracker")
  #' survey_responses = read_airtable(table = "Survey Responses",base = "Assessment Tracker")
  
  # base = "Assessment Tracker" # Assessment Tracker
  # table = "Courts"
  # table = "Assessments"
  # table = "Survey Responses"

  # you will need to add your own script called R/tba.R 
  # to keep your personal access token hidden. Once you create it it will not 
  # be uploaded to git hub after you push your newest changes because I have 
  # included the file path of it in the .gitignore file
  
  bases = "https://api.airtable.com/v0/meta/bases" %>% 
    GET(., add_headers("Authorization" = paste("Bearer", Sys.getenv("at_pa_tkn")))) %>% 
    content(., "text") %>% 
    jsonlite::fromJSON() %>% 
    data.frame() %>% 
    rename_with(~gsub("^bases\\.", "", .), everything()) %>% 
    mutate(table_data = map(id, ~{
      base_id <- .
      paste0(
        "https://api.airtable.com/v0/meta/bases/",
        base_id,
        "/tables") %>% 
        GET(., add_headers("Authorization" = paste("Bearer", Sys.getenv("at_pa_tkn")))) %>% 
        content(., "text") %>% 
        jsonlite::fromJSON() %>% 
        data.frame() %>% 
        rename_with(~gsub("^tables\\.", "", .), everything())
    })) %>% 
    janitor::clean_names()
  
  base_id = bases %>%
    filter(name=={{base}}) %>%
    pull(id)
  
  table_id = bases %>% 
      filter(id==base_id) %>% 
      pull(table_data) %>% 
      data.frame() %>% 
      filter(name=={{table}}) %>% 
      pull(id)
  
  fields_data = bases %>% 
    filter(id==base_id) %>%
    pull(table_data) %>%
    data.frame() %>% 
    filter(name=={{table}}) %>%
    pull(fields) %>% 
    data.frame()
  
  column_names = fields_data %>% 
    pull(name) %>% 
    c("id", "createdTime", .)
  
  data = data.frame(matrix(nrow = 0, ncol = length(column_names))) 
  colnames(data) = column_names
  
  # Airtable only lets us get 100 records at a time, so we will need to write a function to get all the pages
  api_url = paste0("https://api.airtable.com/v0/",base_id,"/",table_id,"?")
  
  offset_flag <- TRUE # Set our flag to true (we will change this in the function)
  offset <- "" # Create an empty string for our offset value (we pull this in the function)
  
  # Create a "while" function
  # This means the function keeps going only while whatever is in parentheses is true
  while(offset_flag == TRUE) {
    
    response <- httr::GET(api_url, add_headers("Authorization" = paste("Bearer", Sys.getenv("at_pa_tkn")))) # get the raw data
    content <- httr::content(response, as = "text")
    raw_data = jsonlite::fromJSON(content)
    
    # making list of cols that are not in this offsets data that need to be for rbind correctly
    add_cols_local = setdiff(
      names(data),
      raw_data$records %>% unnest(cols = everything()) %>% names()
    )
    
    # making a list of cols that are in this current offset, but not in our data yet
    add_cols_global = setdiff(
      raw_data$records %>% unnest(cols = everything()) %>% names(),
      names(data)
    )
    
    data =
      raw_data$records %>% 
      unnest(cols = everything()) %>%
      bind_cols(set_names(map(add_cols_local, ~ NA), add_cols_local)) %>% 
      rbind(
        # adding cols that aren't in previous offsets for this data that need to be to rbind
        data %>% bind_cols(set_names(map(add_cols_global, ~ NA), add_cols_global)),
        .
      )
    
    # TODO add R/lookup_keys_to_values.R to recode lookup values
    
    offset = raw_data$offset # grab our offset value (which tells airtable to grab the next page)
    
    # this is an if else statement that says if offset is empty, change our flag to false, which stops the loop
    # if it's not empty, add the offset value into our api url
    if(is.null(offset)) {
      offset_flag = FALSE
    }else{
      api_url = paste0("https://api.airtable.com/v0/",base_id,"/",table_id,"?","offset=",offset)
    }
    
    
  } # end while loop
  
  paste(
    "Table loaded from Airtable successfully!\n", 
    "Base:", base, "\n",
    "Table:", table,
    "\n",
    nrow(data) %>% format(big.mark = ","), "records\n",
    ncol(data) %>% format(big.mark = ","), "fields"
  ) %>% cat()
  
  return(data)
  
} # end of function