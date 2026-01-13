
# flow charts for protocol document outlining data collection process
# created 2024-07-30

# load relevant packages
library(DiagrammeR)

# creating the main data collection flowchart

  grViz("digraph {
  
  graph[layout = dot, 
        rankdir = TB,
        overlap = true,
        fontsize = 10]
  node [shape = rectangle,
  fixedsize = false,
  width = 3.5]
  
  'PubMed Search' ->
  'Filter to Q1 Journals' ->
  'Screen Title and Abstracts' ->
  'Full-text Data Collection'
  
}")

  
  
# creating the pilot collection flowchart
  
  grViz("digraph {
  graph[layout = dot,
        rankdir = TB,
        overlap = true,
        fontsize = 10]
  node [shape = rectangle,
  fixedsize = true,
  width = 3.5]

  'PubMed Search' ->
  'Randomly Sort Articles' ->
  'Screen 20 Articles' ->
  'Full-text Data Collection' ->
  'Potential Protocl Changes'

}")
  


  
  

