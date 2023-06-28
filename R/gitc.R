  #' @title Git Commit *NPC Style*
  #' @description Automatically commits and updates any changes you have saved to the pdrive and git hub repository. You will be asked to enter your commit message into the consol and press Enter.
  #'
  #' @param files optional list argument of the of files you wish to commit changes to. Default is all files with changes saved.
  #'
  #' @return summary of the five most recent commits
  #'
  #' @examples
  #' > gitc()
  #' > Made report title larger font
  #'
  #' > gitc("cleaning_data.R")
  #' > finished joining tc data sets
  #'
  #' @export

gitc = function(files = TRUE, beep = TRUE){

  if (!require("pacman")) install.packages("pacman")
  # if (!require("libgit2")) install.packages("libgit2")
  library(pacman)
  p_load(flextable,tidyverse,beepr,gert)

  # files = c(".gitignore","R/gitc.R")

  users = list.files(path = "C:\\Users")

  users = users[!(users %in% c(
    "administrator",
    "All Users",
    "Default",
    "Default User",
    "desktop.ini",
    "Public"))]

  info = inner_join(
    users %>% # searches for last name in users
      # ↑this follows the weak assumption that their last name their email
      as_tibble() %>%
      rename(last = value),

    tribble(
      ~first,     ~last,       ~email,
      "maria",    "dale",      "dale@npcresearch.com",
      "emily",    "katz",      "katz@npcresearch.com",
      "kate",     "kissick",   "kissick@npcresearch.com",
      "brian",    "lee",       "lee@npcresearch.com",
      "cyrus",    "tadjiki",   "tadjiki@npcresearch.com",
      "jessica",  "dahlgren",  "dahlgren@npcresearch.com",
      "erica",    "boyce",     "boyce@npcresearch.com",
      "colin",    "holloway",  "holloway@npcresearch.com",
      "laura",    "hunter",    "hunter@npcresearch.com",
      "kate",     "mackey",    "mackey@npcresearch.com",
      "charlene", "zilius",    "zilius@npcresearch.com",
      "jennifer", "aborn",     "aborn@npcresearch.com",
      "theresa",  "allen",     "allen@npcresearch.com",
      "jade",     "croome",    "croome@npcresearch.com",
      "jenna",    "duncan",    "duncan@npcresearch.com",
      "lisa",     "lucas",     "lucas@npcresearch.com",
      "david",    "reinitz",   "reinitz@npcresearch.com",
      "leslie",   "robertson", "robertson@npcresearch.com",
      "sarah",    "rowse",     "rowse@npcresearch.com",
      "lyndsey",  "smith",     "smith@npcresearch.com",
      "leanza",   "walker",    "walker@npcresearch.com"
    ),
    by = "last"
  )

  commit_message <- readline("Enter commit message: ")


  if(is.logical(files) & all(files==TRUE)){
    git_add(git_status()$file) # this stages every file with changes made and saved
  }else{
    git_add(files)
    }

  git_commit(
    message = commit_message,
    author = git_signature(
      name = str_c(
        str_to_sentence(info$first), # cyrus → Cyrus
        str_to_sentence(info$last),  # tadjiki → Tadjiki
        sep = " "),
      email = info$email)            # tadjiki@npcresearch.com
  )

  git_pull()
  git_push()


  # backing up files to pdrive
  path_split = strsplit(getwd(), "/")[[1]][]
  project_name = strsplit(getwd(), "/")[[1]][length(path_split)]

  if(list.files(path = "C:\\Users")[7] == "tadjiki"){

  source("C:/Users/tadjiki/Documents/projects.R")

  project_file_path =
    projects %>%
      filter(project == project_name) %>%
      select(file_path)

  if(nrow(project_file_path)==0){message1
  }else{
    shell(cmd = str_c(
      "cd /d ",
      project_file_path,
      " && git pull")
         )

    if(beep){beep()}
    message2
    }

  }

  git_log(max = 5) %>% select(-c(commit, merge))
}


