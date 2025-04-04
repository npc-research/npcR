create_bookmarks_html <- function(urls, titles = NULL, file_path = NULL) {
  #' create_bookmarks_html
  #'
  #' @param urls a variable of class character vector of url links
  #' @param titles an optional variable of class character vector of what you would like your bookmarks to be named
  #' @param file_path an optional variable of class character that is a file path of where you want your html file to be saved to. Default is C:/Users/USER/Documents/bookmarks.html
  #' @return a saved file of an bookmarks as an html file you can upload to chrome
  #' @export
  #' @examples
    #' urls = paste0("https://app.alchemer.com/explorer/export-summary/id/",10000:10001) # define surveys you want to open
    #' create_bookmarks_html(urls, titles = c("Survey 1", "Survey 2"), file_path = "P:\...surveys.html")

  if(is.null(file_path)) file_path = paste0("C:/Users/",Sys.info()["user"],"/Documents/bookmarks.html")
  if(is.null(titles)) titles = urls

  bookmarks <- paste0("<DT><A HREF=\"", urls, "\">", titles, "</A>")
  html_content <- c(
    "<!DOCTYPE NETSCAPE-Bookmark-file-1>",
    "<!-- This is an automatically generated file. -->",
    "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=UTF-8\">",
    "<TITLE>Bookmarks</TITLE>",
    "<H1>Bookmarks</H1>",
    "<DL><p>",
    paste(bookmarks, collapse = "\n"),
    "</DL>"
  )
  writeLines(html_content, con = file_path)

  numer_of_bookmarks = length(urls)

  paste0("saving html file of ", numer_of_bookmarks, " bookmarks here: ", file_path) %>%
    print()
}
