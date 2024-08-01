
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
  
  'MEDLINE search' ->
  'Abstrackr screen' ->
  'Randomly sort articles' ->
  'Full-text examination (n = 500)'
  
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
  
  'MEDLINE search' ->
  'Abstrackr' ->
  'Randomly sort articles' ->
  'Full-text examination (n = 10) (AG)' -> 'Cross-check criteria (AB)'
  'Full-text examination (n = 10) (AG)' -> 'Cross-check examination (NW)'
}")
