# npcR
Functions for styling and workflow at NPC Research

# Installing npcR
To install npcR, you must use `devtools`. If you don't already have `devtools` installed, do that first. If you already have `devtools` installed, simply load the library before running the second line of code.    
```R
# install.packages('devtools')  
devtools::install_github('npc-research/npcR')
```

# npc_style() Function Overview
The npc_style function can be added to any plot to change basic styling elements to match NPC Research's styling preferences. This function makes the following changes to your plot (if applicable):
- Changes font to Calibri and color to NPC Blue (#001E5B) for all text
- For the title, it changes the font size to 18, bold, and center aligns it
- For the subtitle, it changes the font size to 11, center aligns it, and adds a little bit of spacing above and below the subtitle
- For the caption, it changes the font size to 11, and adds some spacing around the caption
- Moves the legend to the bottom of the plot, left aligns the text, removes the legend title and background, and changes font size to 11
- For the axes, it removes the titles, removes ticks, and removes axis lines
- Removes grid lines
- Gets rid of standard gray background
- Gets rid of standard gray background for small multiples
- Changes font size to 11 for small multiples

The intent of this function is to make the basics of NPC's style easy to create in R, while still leaving room for unique decisions to be made for each individual plot. Any aspect of npc_style() can be easily overridden by adding any changes AFTER npc_style() has been applied (see example below). 

# npc_style() Example

```R
# Install Packages
library(tidyverse)
library(npcR)

# Create a simple plot
car_plot <- mtcars %>% # mtcars is a preloaded dataset
  ggplot(aes(x = mpg, y = cyl)) + # Set the x and y axis for your plot
  geom_point() + # Choose which type of plot you want (this is a scatter plot)
  labs(title = "A Plot About Cars", # Add a title, subtitle, and/or caption (completely optional)
       subtitle = "Created to demonstrate the npc_style() function",
       caption = "Dataviz by Emily Katz, Source: mtcars dataset")

# Run this to see what your basic plot looks like
car_plot 

# Add npc_style() to any plot
car_plot + npc_style()

# If you want to override any of the style settings in npc_style(), add those AFTER you add npc_style()
car_plot + npc_style() +
  theme(plot.title = element_text(hjust = 0), # Left aligns the title
        plot.subtitle = element_text(hjust = 0)) # Left aligns the subtitle`
```
