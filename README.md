# RUMdesignSimulator
The package RUMdesignSimulator proposes convenient tools for generating synthetic data for decision theory.

# Installation:
devtools::install_github("AntoineDubois/RUMdesignSimulator")

# Developpement
Some user may need more tools than the actual ones. Anticipating future needs, we 
organized the code files so that only one file need to be altered.<br/>  
To add new distributions: 
* open the file distribution.R
* add a new distribution
* reference the new distribution into the function *generation*, give it a relevant name for calling the new distribution

To add new designs:
* open the file designs.R
* implement a new design
* reference the new design into the function *call_design*, give it a relevant name for calling the new design
