tree = function(
  #' @title Interactive Tree Map
  #' @description Outputs an interactive html file of a project directory
  #'
  #' @param file_path string of a file path you have access to. It is
  #' recommended to use forward slashes in between files. Two back slashes
  #' is also an option.
  #' @param exclude list of files you wish to exclude from the tree.
  #' @param collapse if FALSE, entire tree is collapsed, Default is TRUE.
  #'
  #' @return Returns interactive html file
  #'
  #' @references Adeel Khan
  #' \pkg{collapsibleTree}:
  #'  <https://github.com/AdeelK93/collapsibleTree>
  #'
  #'
  #' @examples
  #' path <- "P:/6. Projects Active/Example/Syntax"
  #' tree(path)
  #' tree("P:/6. Projects Active/Example", exclude = ".RData")
  #' tree(
  #'   file_path = "P:/6. Projects Active/Example/Data",
  #'   exclude = c("data.csv", "data.sav"),
  #'   collapse = TRUE)
  #'
  #' @export
  file_path,
  exclude,
  collapse
  ){

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   plyr,
#   data.tree,
#   collapsibleTree,
#   htmltools,
#   dplyr
#   )

# source(other.R)

# exclude = c(
#   "template_files",
#   "lib",
#   "renv",
#   "renv.lock",
#   "archived",
#   "BeST.Rproj",
#   "template.html",
#   "README.html",
#   "template.pdf")

# TODO comment out lines below
file_path = "C:/Users/tadjiki/Documents/Root"
exclude = c("Data","Archive")

files_listed = list.files(file_path, full.names = TRUE, recursive = TRUE)
df = lapply(strsplit(files_listed, "/"), function(z) as.data.frame(t(z))) %>%
  rbind.fill()
df$pathString <- apply(df, 1, function(df) paste(trimws(na.omit(df)), collapse="/"))

# finding what root should be
n_unnecessary_par_dir = strsplit(file_path, "/")[[1]] %>% length()
root = strsplit(file_path, "/")[[1]][n_unnecessary_par_dir]


# excluding files that clutter the tree
if(missing(exclude)) { # making this argument optional
  # do nothing
} else {
files_excluded = exclude

df <- df %>%
  filter(if_all(
    .cols = everything(),
    .fns = ~ !. %in% files_excluded
  ))

}


df %<>%
  select((n_unnecessary_par_dir+1):ncol(df)) %>%
  # filter(!grepl("\\~\\$", pathString)) %>%
  mutate(is_directory = FALSE)


directories =
  lapply(strsplit(list.dirs(file_path)[-1], "/"), function(z) as.data.frame(t(z))) %>%
  plyr::rbind.fill() %>%
  select(n_unnecessary_par_dir:nrow(df)) %>%
  select(-1) %>%
  lapply(., unique) %>%
  unlist() %>%
  na.omit() %>%
  as.vector() %>%
  data.frame(directory = .)

color_table = directories %>%
  cbind(
    matrix(
      NA,
      nrow = nrow(directories),
      ncol = ncol(df)-3),
    file_path = list.dirs(file_path)[-1],
    dir = TRUE
    ) %>%
  rename_with(~ names(df), everything()) %>%
  rbind(.,df) %>%
  arrange(pathString)



# adding color to nodes
color_vec = 1:nrow(color_table)
i = 4

adding_color = function(i){
  pathString_split = strsplit(color_table$pathString[i], "/")
  file_name = pathString_split[[1]][length(pathString_split[[1]])]
  removed_periods = strsplit(file_name, "\\.")
  file_type = removed_periods[[1]][length(removed_periods[[1]])]

  if(color_table$is_directory[i]) {result = "yellow"
  }else if(file_type == "Rmd"){result = "#ef6232"
  }else if(file_type == "R"||file_type =="sps"){result = "#4b729d"

  # data files
  }else if(
    file_type == "xlsx"
    ||file_type =="csv"
    ||file_type =="sav"){result = "#1c5b34"
  }else if(file_type == "spv"){result = "lightblue"
  }else if(file_type == "docx"){result = "darkblue"
  }else if(file_type == "png"||file_type =="jpeg"){result = "purple"
  }else if(file_type == "html"||file_type =="MD"){result = "red"
  }else if(file_type == "pdf"){result = "#AE3535"
  }else if(file_type == "RHistory"){result = "gray"
  }else if(file_type == "RData"){result = "#7cb494"
  }else if(file_type == "gitignore"){result = "#B96C37"
    # else if(file_type == ""){""}
  # else if(file_type == ""){""}
  }else{result = "black"}

  color_vec[i] = result

}

rm(i)
sapply(1:nrow(color_table), adding_color)

color_vec = sapply(1:nrow(color_table), adding_color)


# color_vec = c(
#   rep("yellow", 15),
#   "pink",
#   color_vec[-1]
# )

if(missing(collapse)) { # making this argument optional
  collapse = TRUE
  } else if(collapse == TRUE){
  collapse = TRUE
  } else{collapse = FALSE}

tree_output = color_table %>%
  # Traverse(.,traversal = "post-order") %>% view()
  select(-pathString,-is_directory) %>%
  collapsibleTree(
    hierarchy = names(.),
    root = root,
    attribute = "leafCount",
    # aggFun = sum,
    # fill = color_vec,
    # fill = c(color_vec,rep(
    #   c(
    #     "red",
    #     "orange",
    #     "yellow",
    #     "blue",
    #     # "green",
    #     "purple"
    #   )
    #   ,1)),
    fillByLevel = TRUE,
    linkLength = NULL,
    fontSize = 12,
    tooltip = FALSE,
    nodeSize = NULL,
    collapsed = collapse,
    zoomable = TRUE,
    width = 1500,
    height = 1000
  )

tree_output
return(tree_output)
}




# docstring::docstring(tree)


#
#
# tree = df %>%
#   mutate(pathString = str_remove(pathString, "")) %>%
#   data.tree::as.Node()
#
# get_value_by_folder <- function(tree) {
#
#   res <- rep(NA_real_, tree$totalCount)
#
#   i <- 0
#   myApply <- function(node) {
#     i <<- i + 1
#     force(k <- i)
#     res[k] <<- node$Value + `if`(node$isLeaf, 0, sum(sapply(node$children, myApply)))
#   }
#
#   myApply(tree)
#   res
# }
#
# get_value_by_folder(tree)
#
# tree$parent()
#
# parent(tree)
#
#
#
# library(treemap)
# tree$BeST$docs$Doc1.docx
# tree$`C:`$Users
#
# as.data.frame(tree$`C:`$Users$tadjiki$Documents$BeST$data)
#
# %>% save_html("tree.html")

# shiny::runApp(system.file("examples/02shiny", package = "collapsibleTree"))

# plot(mytree)
#
#
# tree$height


tree("P:/6. Projects Active/CMC Care Link", exclude = "Data", collapse = TRUE)
