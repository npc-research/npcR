create_airtable_rows <- function(data,table,base,
                                 single_select_fields = character(),
                                 link_fields = character(),
                                 attachment_fields = character(),
                                 first_row_i,
                                 last_row_i
                                 ) {
  #' Function for Creating AirTable Rows
  #'
  #' @param data A class of "data.frame" that is the AirTable Base you are uploading data to.
  #' @param base A class of "string" that is the AirTable Base you are uploading data to.
  #' @param table A class of "string" that is the name of table I am wanting to upload to AirTable.
  #' @param single_select_fields A class of "character vector" that is a list of the single select columns.
  #' @param link_fields A class of "character vector" that is a list of the class "url" columns.
  #' @param attachment_fields A class of "character vector" that is a list of the class "attachment" columns.
  #'
  #' @return HTTPs response on if the rows were uploaded successfully or not. Message 200 means that the rows were uploaded correctly.
  #' @export
  #'
  #' @examples
  #'
  #' row_indices = data.frame(
  #'  first_row_i = seq(1, nrow(df), 10),
  #'  last_row_i  = c(seq(min(10, nrow(df)), nrow(df), 10),nrow(df))
  #'  )
  #'
  #'Sys.sleep(30)
  #'sapply(
  #'  1:nrow(row_indices),
  #'  function(index) {
  #'    create_airtable_rows(
  #'      data = df,
  #'      table = "Contact List",
  #'      base = "Assessment Tracker",
  #'      single_select_fields = character(),
  #'      link_fields = character(),
  #'      attachment_fields = character(),
  #'      first_row_i = row_indices[index,"first_row_i"],
  #'      last_row_i = row_indices[index,"last_row_i"])
  #'  }
  #')

  # data = df
  # first_row_i = 131
  # last_row_i = 140
  # single_select_fields = c("Primary Language","Other Languages")
  # single_select_fields = character()
  # link_fields = c("Email Address","Work Phone")
  # attachment_fields = c("Report","Assessment PDF")

    data = data %>% slice(first_row_i:last_row_i)

    format_fields <- function(data, single_select_fields = character(), link_fields = character(), attachment_fields = character()) {
      records <- lapply(1:nrow(data), function(i) {
        fields <- lapply(names(data), function(col) {
          if (col %in% single_select_fields) {

            # For single select fields, format value as an array with a single value
            if (!is.null(data[[col]][i])) {
              setNames(list(list(data[[col]][i])), col)
            } else {
              setNames(list(NULL), col)
            }
          } else if (col %in% link_fields) {

            # For link columns, format value as a list containing a URL
            if (!is.null(data[[col]][i])) {
              setNames(list(list(list(url = data[[col]][i]))), col)
            } else {
              setNames(list(NULL), col)
            }
          } else if (col %in% attachment_fields) {

            # For attachment columns, format value as a list containing a URL to an attachment
            if (!is.null(data[[col]][i])) {
              setNames(list(list(list(url = data[[col]][i]))), col)
            } else {
              setNames(list(NULL), col)
            }
          } else {

            # For other fields, format value directly
            if (!is.null(data[[col]][i])) {
              setNames(list(data[[col]][i]), col)
            } else {
              NULL
            }
          }
        })
        list(fields = Filter(Negate(is.null), do.call(c, fields)))
      })
      return(records)
    }

    formatted_data = sapply(1:nrow(data), function(row) {

      data %>%
        slice(row) %>%
        select(where(~ !any(is.na(.)))) %>% # dropping columns that have an NA in them
        format_fields(.,
                      single_select_fields = single_select_fields,
                      link_fields = link_fields,
                      attachment_fields = attachment_fields
        )

    })

    formatted_data %>%
      list(records = .,typecast = TRUE) %>%
      RJSONIO::toJSON(., auto_unbox = TRUE) %>%
      write("data/post_request.json")

    # browseURL(paste("file://", file.path(getwd(), "data/post_request.json"), sep=""))
    # cat("Uploading ", length(formatted_data), " records\n")

    at_ids = finding_airtable_ids(base=base,table=table)

    base_id  = at_ids$base_id
    table_id = at_ids$table_id

    response <- POST(
      paste("https://api.airtable.com/v0", base_id, table_id, sep = "/"),
      httr::add_headers("Authorization" = paste("Bearer", Sys.getenv("at_pa_tkn"))),
      body = upload_file("data/post_request.json"),
      encode = "json",
      timeout(20)
    )

    Sys.sleep(1)

    http_status(response)$message %>% print()

    cat("Uploading records ",first_row_i," to ", last_row_i,"\n")

}
