# npcR
Functions for styling and workflow at NPC Research

# Installing npcR
To install npcR, you must use `devtools`. If you don't already have `devtools` installed, do that first. If you already have `devtools` installed, simply load the library before running the second line of code. 

```R
# install.packages("devtools")  
devtools::install_github("npc-research/npcR")
```
# Using AirTable functions
To use any functions that interact with AirTable you will need to store your API Personal Access Tokens in a hidden environment named `at_pa_tkn`.
```
Sys.setenv(
  # AIRTABLE API KEYS
  at_pa_tkn = "AIRTABLE_PERSONAL_ACCESS_TOKEN"
)
```
