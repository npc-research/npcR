# todor only works if you save the file first
# You can also click on addins in the ribbon and scroll down the TODOr section

# Default Tags
default_tags = c(
  "FIXME","TODO","CHANGED","IDEA","HACK","NOTE","REVIEW","BUG","QUESTION","COMBAK","TEMP"
)

# Custom Tags and Authors
custom_tags = c(
  # "YEARLY", # only include this when scanning for yearly changes
  # "QUARTERLY", # only include this when scanning for quarterly changes
  # "TRANSLATE", 
  "CT",
  "MD",
  "SR",
  "KK",
  "BL"
)

options(todor_patterns = c(default_tags,custom_tags))
rm(default_tags,custom_tags)