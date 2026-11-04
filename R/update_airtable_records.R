update_airtable_records = function(data,fields,key,table,base = "Assessment Tracker",
                                   single_select_fields = character(),
                                   link_fields = character(),
                                   attachment_fields = character()
                                   ){
  #' Function for Creating AirTable Rows
  #'
  #' @param data A class of "data.frame" that is the AirTable Base you are uploading data to.
  #' @param fields A class of "character vector" that is a list of the fields you are planning on updating.
  #' @param key A class of "string" that is variable to join on and link data you're uploading, to data in AirTable.
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
  #'create_airtable_rows(
  #'  data = df,
  #'  table = "Contact List",
  #'  base = "Assessment Tracker",
  #'  single_select_fields = character(),
  #'  link_fields = character(),
  #'  attachment_fields = character()
  #'  )

  # data = data.frame(
  #   character_name = c("Cruella","Jasper","Horace"),
  #   name = c("Emma","Joel","Paul")
  #   # name = c("Emma Stone","Joel Fry","Paul Walter")
  # )
  # data = tibble(
  #   `Resource Name` = str_c("Resource ",LETTERS),
  #   `HTTP Error` = c(
  #     rep("404 Page does not exist",3),
  #     rep("",23)
  #   ),
  #   Notes = c(
  #     # "test",
  #     "new notes from 11/4",
  #     rep("",25)
  #   )
  # ) %>%
  #   filter(str_detect(`Resource Name`,str_c(str_c("Resource ",LETTERS[1:7]),collapse = "|")))
  # table = "Table 5"
  # base = "Zapier Test"
  # key = "Resource Name"
  # # key = "id"
  # fields = c("HTTP Error","Notes")
  # # key = "character_name"
  # single_select_fields = character()
  # link_fields = c("Resource Link")
  # attachment_fields = c("Report","Assessment PDF")
  # attachment_fields = c("File")

  if (Sys.getenv("at_pa_tkn")=="") {
    stop("API key not found. Make sure you have your API key in a hidden variable named `at_pa_tkn` by running Sys.setenv(at_pa_tkn ='your_api_key_here').")
  }
  # source("U:/api_keys/api_keys.R")

  airtable_data = npcR::read_airtable(table=table,base=base)

  df_all = airtable_data %>%
    right_join(
      data %>% select(key,matches(fields)),
      suffix = c(".airtable", ".upload_data"),
      by = key)

  add_overwrite_flags <- function(df, fields) {
    for (field in fields) {
      airtable_col <- paste0(field, ".airtable")
      upload_col <- paste0(field, ".upload_data")
      flag_col <- paste0("overwrite_", field)

      df <- df %>%
        # checking if airtable data and upload data are both not missing
        mutate(
          !!flag_col := !is.na(.data[[airtable_col]]) &
                        !is.na(.data[[upload_col]]) &
                        .data[[airtable_col]] != .data[[upload_col]]
          # TODO update this to factor for attachments
          )
    }

    return(df)
  }

  confirm_overwrite <- function(df, fields) {

    stopifnot("id" %in% names(df))
    # TODO add a unit test for variable names matching

    # initialize confirmation columns
    for (field in fields) {
      confirm_col <- paste0("overwrite_", field, "_confirmed")
      df[[confirm_col]] <- FALSE
    }

    for (i in seq_len(nrow(df))) {

      for (field in fields) {

        flag_col <- paste0("overwrite_", field)
        confirm_col <- paste0("overwrite_", field, "_confirmed")

        if (isTRUE(df[[flag_col]][i])) {

          if (field %in% attachment_fields) {

            file_url <- utils::URLencode(
              df[[paste0(field, ".airtable")]][i][[1]][["url"]]
            )

            file_name <- utils::URLencode(
              df[[paste0(field, ".airtable")]][i][[1]][["filename"]]
            )

          } else {

            old_val <- df[[paste0(field, ".airtable")]][i]
            new_val <- df[[paste0(field, ".upload_data")]][i]

          }

          cat("\n============================\n")
          cat("Row ID:", df$id[i], "\n")
          cat("Key:", df[[key]][i], "\n")
          cat("Field:", field, "\n")

          if (field %in% attachment_fields) {

            cli::cli_text(
              paste0(
                "Old: {.href ",
                "", file_url, "}\n"
              )
            )

            cli::cli_text(
              paste0(
                "New: {.url ",
                df[[paste0(field, ".upload_data")]][i],
                "}\n"
              )
            )

          } else {

            cat("Old:", old_val, "\n")
            cat("New:", new_val, "\n")

          }

          response <- readline("Overwrite this value? (Y/n): ")

          df[[confirm_col]][i] <-
            tolower(response) %in% c("", "y")

          cat(
            if (df[[confirm_col]][i]) {
              "✅ Overwritten.\n"
            } else {
              "⏭️ Skipped.\n"
            }
          )
        }
      }
    }

    df
  }

  finalize_overwrites <- function(df, fields) {
    # For each field, keep rows where:
    #  - overwrite not needed, OR
    #  - overwrite was confirmed
    for (field in fields) {
      flag_col <- paste0("overwrite_", field)
      confirm_col <- paste0(flag_col, "_confirmed")

      df <- df %>%
        filter(!.data[[flag_col]] | (.data[[flag_col]] & .data[[confirm_col]]))
    }

    # Select id + renamed columns from .upload_data
    select_cols <- c("id", setNames(paste0(fields, ".upload_data"), fields))

    df %>%
      select(any_of(select_cols))
  }

  df_all = df_all %>%
    add_overwrite_flags(fields) %>%
    confirm_overwrite(fields)

  # TODO add a safe guard here like press esc or enter stop to prevent changes from occurring
  data <- finalize_overwrites(df_all, fields)

  # making table that recognizes how to upload rows in batches of 10
  first_row_indices = seq(1, nrow(data), 10)
  last_row_indices = c(first_row_indices[-length(first_row_indices)]+9,nrow(data))

  row_indices = data.frame(
    first_row_i = first_row_indices,
    last_row_i  = last_row_indices
  )

  # uploading data
  sapply(
    1:nrow(row_indices),
    function(index) {

    first_row_i = row_indices[index,"first_row_i"]
    last_row_i = row_indices[index,"last_row_i"]

    data = data %>% slice(first_row_i:last_row_i)

  format_fields_patch <- function(
    data, id_column = "id", single_select_fields = character(),
    link_fields = character(), attachment_fields = character()) {

    records <- lapply(1:nrow(data), function(i) {
      fields <- lapply(names(data), function(col) {
        if (col == id_column) return(NULL)  # Don't include ID in fields

        if (col %in% single_select_fields) {
          if (!is.null(data[[col]][i])) {
            setNames(list(list(data[[col]][i])), col)
          } else {
            setNames(list(NULL), col)
          }
        } else if (col %in% link_fields || col %in% attachment_fields) {
          if (!is.null(data[[col]][i])) {
            setNames(list(list(list(url = data[[col]][i]))), col)
          } else {
            setNames(list(NULL), col)
          }
        } else {
          if (!is.null(data[[col]][i])) {
            setNames(list(data[[col]][i]), col)
          } else {
            NULL
          }
        }
      })

      list(
        id = data[[id_column]][i],
        fields = Filter(Negate(is.null), do.call(c, fields))
      )
    })

    return(records)
  }

  formatted_data <- lapply(1:nrow(data), function(row) {
    row_data <- data %>%
      slice(row) %>%
      select(where(~ !any(is.na(.))), !!sym("id"))  # keep ID even if NA in other fields

    format_fields_patch(
      row_data,
      id_column = "id",
      single_select_fields = single_select_fields,
      link_fields = link_fields,
      attachment_fields = attachment_fields
    )[[1]]
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

  response <- PATCH(
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
  ) # end of sapply
}
