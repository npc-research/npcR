delete_all_airtable_records <- function(table,base) {
  #' Function for Deleting all rows in an AirTable table
  #'
  #' @param data A class of "string" that is the AirTable Base you are deleting data from.
  #' @param base A class of "string" that is the AirTable Base you are deleting data from.
  #'
  #' @return HTTPs response on if the rows were deleted successfully or not. Message 200 means that the rows were deleted correctly.
  #' @export
  #'
  #' @examples
  #'delete_all_airtable_records(base = "Assessment Tracker", table = "Contact List")

  # base = "Assessment Tracker"
  # table = "Contact List"

  at_ids = finding_airtable_ids(table=table,base=base)

  base_id  = at_ids$base_id
  table_id = at_ids$table_id

  ids_to_remove = npcR::read_airtable(table=table,base=base) %>% pull(id)

  sapply(ids_to_remove, function(record_id){

    record_url <- paste0("https://api.airtable.com/v0/",base_id,"/",table_id,"/",record_id)

    # Make the DELETE request to delete records
    response <- DELETE(
      record_url,
      httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("at_pa_tkn"))),
      encode = "json"
      )
    }
  )
}
