concatenating_percentages = function(){
  #' Function for finding percentages to the right of fractions(n/N)
  #'
  #'
  #' @return html view in the Viewer pane
  #' @export

  trib_call <- vector_construct()
  df = eval(parse(text = trib_call))

  df %>%
    data.frame(text = .) %>%
    mutate(
      num = str_extract(text, "\\d+(?=/)"),
      den = str_extract(text, "(?<=/)\\d+"),
      num = as.numeric(num),
      den = as.numeric(den),
      pct = round(100*num/den, 0),
      pct = paste0(pct, "%"),
      ans = if_else(is.na(num),
                    "",
                    paste0(num, "/", den, " (", pct, ")")
      ),
    ) %>%
    select(ans) %>%
    flextable() %>%
    delete_part() %>%
    border_remove() %>%
    fontsize(size = 12) %>%
    color(color = "#000000") %>%
    font(fontname = "Calibri")
}
