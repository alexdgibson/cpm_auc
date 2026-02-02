
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
  
  'PubMed Search (n = 63,954)' ->
  'Filter to Q1 Journals (n = 26,018)' ->
  'Random Sample of 10% (n = 2,602)' ->
  '2025 ePublication Date (n = 2,200)' ->
  'Screen Title and Abstracts (to be done)' ->
  'Full-text Data Collection (to be done)'
  
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
  


  
  

