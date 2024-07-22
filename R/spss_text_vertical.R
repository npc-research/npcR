spss_text_vertical <- function(file = "~/text.txt") {
  #' Function for taking spss syntax and making it vertial with line breaks.
  #'
  #' @param file path to a text file.
  #'
  #' @return output of spss variable names with line broke after every name.
  #' @export
  #'
  #' @examples
  #'
  #'writeLines(text = "var1 var2
  #'var3", con = "~/text.txt")
  #'
  #'spss_text_vertical("~/text.txt")
  #'
  #'var1
  #'var2
  #'var3

  read_file(file) %>%
    str_replace_all("\\s+", ",") %>%
    str_split(",", simplify = TRUE) %>%
    as.character() %>%
    cat(sep = "\n") %>%
    return()
}
