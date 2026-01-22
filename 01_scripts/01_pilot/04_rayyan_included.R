# 04_rayyan_included.R
# take the results from Rayyan screening and filter to only gather those articles for inclusion in data extractions

# load in libraries
library(tidyverse)

# load in rayyan data
rayyan <- read.csv(file = "02_data/02_pilot/rayyan_pilot.csv")

# filter to only include those which are included
included <- rayyan %>% 
  filter(notes == 'RAYYAN-INCLUSION: {"Alexander"=>"Included"}')

# save the included articles to get the PDFs
write.csv(included, file = "02_data/02_pilot/included_pilot.csv")
