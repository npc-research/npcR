pnames = function(input, var_names = TRUE){
  #' Function for printing strings in colsole in a easy to read format
  #'
  #' @param data A class of "data.frame" or "string" based on our second argument var_names
  #' @param var_names A class of "logical", depending on what class our input value is
  #'
  #' @return output in console of string printed with line breaks
  #' @export
  #'
  #' @examples
  #'pnames(iris)
  #'Sepal.Length
  #'Sepal.Width
  #'Petal.Length
  #'Petal.Width
  #'Species
  #'
  #'pnames(fruit[1:3],F)
  #'apple
  #'apricot
  #'avocado

  pretty_print_names = function(text){
    text %>%
      cat(sep = "\n") %>%
      return()
  }

  if(var_names){
    input %>%
      names() %>%
      pretty_print_names() %>%
      return()
  }else{
    # input is string
    input %>%
      pretty_print_names() %>%
      return()
  }
}
