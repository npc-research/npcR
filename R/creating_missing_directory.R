creating_missing_directory = function(path){
  #' Creating a missing Directory
  #'
  #' @param path argument of class character
  #'
  #' @return a print statement on the status of the directory you want created.
  #' @export
  #'
  #' @examples
  #' creating_missing_directory(paste0("C:/Users/lastname/Documents/",Sys.Date()))

  if(!file.exists(path)){
    dir.create(path)
    print(paste("Created Driectory", path))
  }else{print(paste("Driectory", path, "Already Exisits"))}

}
