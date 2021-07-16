# SDCv2

The package SDCv2 proposes convenient tools for generating synthetic data for decision theory.
Firstly, Alternatives, Decision Makers and Preference Coefficients are easily generated. 
Then, experimental designs are generated in format long or wide. 
In addition, the effect of each variable can be visualized in a 3D graph.



# Installation

devtools::install_github("AntoineDubois/sdcv2")
devtools::install_github("nikitagusarov/sdcv2")


 
# Development

## Files

* The folder **R** contains the R files. Inside this folder, the file **experiment.R** is the main file. 
Moreover, the utility of the other files is explicitly given by their name.

* The folder **man** contains the documentation about the functions of the package

## Adding new features

Some user may need more tools than the actual ones. 
Anticipating future needs, we organized the R files so that only one file need to be altered for adding new features.

![Capture_d_écran_2021-07-12_à_17.53.50](/uploads/b19d8f4a5ad26f48efd8e5e131b24503/Capture_d_écran_2021-07-12_à_17.53.50.png)

To add new distributions: 

* open the file distribution.R
* add a new distribution
* reference the new distribution into the function *generation*, give it a relevant name for calling

To add new designs:

* open the file designs.R
* implement a new design
* reference the new design into the function *call_design*, give it a relevant name for calling

For more information, do not hesitate to contact me at <antoine.dubois.fr@gmail.com> or <nikita.gusarov@univ-grenoble-alpes.fr> (new maintainer starting from 01/10/2021).