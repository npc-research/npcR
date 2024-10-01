initialize_project <- function(mail_merge = TRUE) {
  # 1. Create necessary directories
  directories <- c(
    "R",
    "docs",
    "images",
    "data",
    "output",
    "links",
    "api_keys"
  )

  sapply(directories, dir.create, showWarnings = FALSE)

  # 2. Create necessary R files (excluding loading_packages.R since it will be copied)
  r_files <- c("todor_setup")
  if(mail_merge == TRUE) r_files = c(r_files, "mail_merge")

  sapply(paste0("R/", r_files, ".R"), file.create)

  # 3. Create API key directory file
  sapply(paste0("api_keys/api_keys.R"), file.create)

  r_files =
    dir(full.names = TRUE, recursive = TRUE) %>%
    keep(~ str_detect(., "\\.R$"))

  sapply(r_files, function(file){
    fileConn<-file(file, "a")
    writeLines(c("source(\"R/loading_packages.R\")"), fileConn)
    close(fileConn)
    rm(fileConn)
    }
  )

  ignore_files = c("data","links","api_keys")
  fileConn<-file(".gitignore", "a")
  writeLines(ignore_files, fileConn)
  close(fileConn)
  rm(fileConn)

  # 4. Copy loading_packages.R from internal package to the new project
  # Locate the file inside the package's `inst` directory

  # TODO have this code run for all files in inst/templates
  # TODO test this with ~/git_repos_cloned/GitHub-Help

  tibble(
    full_path = system.file("templates/", package = "npcR") %>%
      dir(full.names = TRUE),
    name = system.file("templates/", package = "npcR") %>%
      dir()
  )


  template_path <- system.file("templates/loading_packages.R", package = "npcR")

  if (template_path != "") {
    file.copy(template_path, "R/loading_packages.R", overwrite = TRUE)
    message("loading_packages.R has been copied.")
  } else {
    warning("Template loading_packages.R not found in package.")
  }

  message("Project setup complete!")
}


