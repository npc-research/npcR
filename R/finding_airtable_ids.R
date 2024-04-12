finding_airtable_ids <- function(table,base = "Assessment Tracker") {
  #' Function for Finding AirTable IDs
  #'
  #' @param base A class of "string" that is the AirTable Base with my table located in it.
  #' @param table A class of "string" that is the name of table I am interested in reading in.
  #'
  #' @return IDs for our Base and Table
  #' @export
  #'
  #' @examples
  #' finding_airtable_ids(table = "Logins",base = "Assessment Tracker")

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

  return(
    list(
      "base_id" = base_id,
      "table_id" = table_id,
      "fields_data" = fields_data
      )
    )
}
