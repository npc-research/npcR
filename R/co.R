#' @title Git Commit *NPC Style*
#' @description Automatically commits and updates any changes you have saved to the pdrive and git hub repository. You will be asked to enter your commit message into the consol and press Enter.
#'
#' @return summary of the five most recent commits
#'
#' @examples
#' > co()
#' > Enter commit message: sc
#' "small changes"
#'
#' > Enter commit message: ls
#'
#' > Enter commit message:
#' > ""
#' > "No commits made or files staged"
#'
#' @export

co = function(){

  if (!require("pacman")) install.packages("pacman")
  library(pacman)
  p_load(tidyverse,beepr,gert)

  npc_employees = tribble(
    ~first,     ~last,       ~email,
    "cyrus",    "tadjiki",   "tadjiki@npcresearch.com"
  )

  # The syntax below finds out who's running this by looking at the users and
  # maps the user name to your github signature
  user_last_name = intersect(npc_employees$last,list.files("C:\\Users"))
  user_info = npc_employees %>% filter(last == user_last_name)


  git_sig = git_signature(
    name = str_c(
      str_to_sentence(user_info$first), # cyrus → Cyrus
      str_to_sentence(user_info$last),  # tadjiki → Tadjiki
      sep = " "),
    email = user_info$email)            # tadjiki@npcresearch.com

  commit_message <- readline("Enter commit message: ")

  if(commit_message == "sc") {
    commit_message <- "small changes"
    git_add(git_status()$file)
  }else if(commit_message == "ls") {

    git_status() %>% print()

    parse_input <- function(input) {
      parsed <- try(eval(parse(text = input)), silent = TRUE)
      if (class(parsed) == "try-error") {
        return(input)  # Treat input as text if parsing fails
      } else {
        return(parsed)  # Return parsed expression
      }
    }

    files = readline("Please enter an array of which files to stage or regex base on file names: ")
    files <- parse_input(files)

    if(is.numeric(files)) {
      git_status() %>%
        slice(files) %>%
        pull(file) %>%
        git_add()

      print("The following files have been staged")
      git_status() %>% filter(staged==TRUE)
    }else{
      git_status() %>%
        filter(str_detect(file,files)) %>%
        pull(file) %>%
        git_add()

      print("The following files have been staged")
      git_status() %>% filter(staged==TRUE)
    }

    commit_message <- readline("Enter commit message: ")

  }else if(commit_message == "") {print("No commits made or files staged")
  }else{
    git_add(git_status()$file) # stage every file
  }

  if(commit_message != "") git_commit(message = commit_message,author = git_sig);print(commit_message)

  rm(
    npc_employees,
    user_info,
    user_last_name,
    git_sig,
    commit_message
  )

}
