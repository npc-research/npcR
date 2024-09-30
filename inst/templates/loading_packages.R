# checking for pacman 
if(!require("pacman")) install.packages("pacman")

# installing packages from CRAN
pacman::p_load(
  skimr, tidyverse, knitr, beepr, officedown, flextable, 
  janitor, haven, sf, rmarkdown, todor, officer, readxl,
  httr, npcR)

# installing packages from GitHub
if(!require("devtools"))   install.packages("devtools")
if(!require("npcR"))       devtools::install_github("npc-research/npcR")
if(!require("sjlabelled")) devtools::install_github("strengejacke/sjlabelled")