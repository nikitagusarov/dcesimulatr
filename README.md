# `dcesimulatr`

The package `dcesimulatr` provide a flexible controlled environment for discrete choice experiment simulation. 

At this moment, the package supports only minimal number of behavioural theories and preset experiment configuration. 
We hope that in near future, the number of available functionnalities will increase. 



# Installation

For now, the package resides in an old repository, keeping the previous naming convention, which may rise confusion:

```
devtools::install_github("nikitagusarov/sdcv2")
```

Alternatively, you may use the old repository:

```
devtools::install_github("AntoineDubois/sdcv2")
```


 
# Developpement

<!--

## Files

* The folder **R** contains the R files. Inside this folder, the file **experiment.R** is the main file. 
Moreover, the utility of the other files is explicitly given by their name.

* The folder **man** contains the documentation about the functions of the package

## Adding new features

Some user may need more tools than the actual ones. Anticipating future needs, we 
organized the R files so that only one file need to be altered.

To add new distributions: 

* open the file distribution.R
* add a new distribution
* reference the new distribution into the function *generation*, give it a relevant name for calling

To add new designs:
* open the file designs.R
* implement a new design
* reference the new design into the function *call_design*, give it a relevant name for calling

-->

For more information, do not hesitate to contact me at <nikita.gusarov@univ-grenoble-alpes.fr> or <antoine.dubois.fr@gmail.com> (the previous maintainer and main developper before the 01/10/2021). 
