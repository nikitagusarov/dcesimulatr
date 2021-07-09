# RUMdesignSimulator
The package RUMdesignSimulator proposes convenient tools for generating synthetic data for decision theory.<br/>
Firstly, Alternatives, Decision Makers and Preference Coefficients are easily generated. Then, experimental designs are generated in format long or wide. In addition, the effect of each variable can be visualized in a 3D graph.


## Installation:
devtools::install_github("AntoineDubois/RUMdesignSimulator")


# Tutorial
The files **tutorial.Rmd** or **tutorial.pdf** presents the RUMdesignSimulator package.

 
# Developpement

## Files
* The file **tutorial.Rmd** contains the tutorial above for reproductibility.
* The folder **R** contains the R files. Inside this folder, the file **experiment.R** is the main file. Moreover, the utility of the other files is explicitly given by their name.
* The folder **man** contains the documentation about the functions of the package

## Adding new features
Some user may need more tools than the actual ones. Anticipating future needs, we 
organized the R files so that only one file need to be altered.<br/>  
To add new distributions: 
* open the file distribution.R
* add a new distribution
* reference the new distribution into the function *generation*, give it a relevant name for calling

To add new designs:
* open the file designs.R
* implement a new design
* reference the new design into the function *call_design*, give it a relevant name for calling

For more information, do not hesitate to contact me at <antoine.dubois.fr@gmail.com>
