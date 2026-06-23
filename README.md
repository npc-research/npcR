# npcR
Functions for styling and workflow at NPC Research

# Installing npcR
To install npcR, you must use `pak`. If you don't already have `pak` installed, do that first. If you already have `pak` installed, simply load the library before running the second line of code. 

```R
# install.packages("pak")  
pak::pak("npc-research/npcR")
```
## Updating package
To update package, create R script here `./R` and run `devtools::document()` to create .Rd file and then commit changes to git.

## Using AirTable functions
To use any functions that interact with AirTable you will need to store your API Personal Access Tokens in a hidden environment named `at_pa_tkn`.
```
Sys.setenv(
  # AIRTABLE API KEYS
  at_pa_tkn = "AIRTABLE_PERSONAL_ACCESS_TOKEN"
)
```
