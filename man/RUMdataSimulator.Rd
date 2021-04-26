\name{RUMdataSimulator}
\alias{RUMdataSimulator}
\title{RUMdataSimulator}
\description{
RUMdataSimulator provides convenient tools for generating synthetic data for Random Utility Model evaluation.
Below, please find a complete example of how to use the package RUMdataSimulator.
}

\author{
  Antoine Dubois
}
\examples{
library("RUMdataSimulator")
library("clusterGeneration") # This packages provides functions generating random covariance matrices
##############################
# Generation of synthetic dataset
##############################
N <- 10
DM_att_names <- list("A", "B", "C")
AT_names <- list("good1", "good2")
AT_att_names <- list("D", "E", "F")
no_choice <- TRUE
J <- length(AT_names)+no_choice
FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N, no_choice=no_choice)

################################################################################
# Example 1: step-by-step generation

#Generation of S whose components law's are different
FD$gen_DM_attributes("discrete_uniform", a=0, b=1, which="A")
FD$gen_DM_attributes("normal", which=c("B","C"))
FD$gen_DM_attributes("normal", mu=1, sd=2, which=c("B","C"))
FD$S

#Generation of X whose components law's are identical
sigma <- genPositiveDefMat(3)$sigma
FD$gen_AT_attributes(mu=c(-1,2,0), sd=sigma)
FD$X

#Generation of beta whose components law's are different
FD$gen_preference_coefficients("student", heterogeneity=TRUE, location=-100,  scale=1, df=4, which=c(1:4))
FD$gen_preference_coefficients("normal", heterogeneity=TRUE, mu=0, sd=2, which=c(5))
FD$gen_preference_coefficients("discrete_uniform", heterogeneity=TRUE, a=1, b=5, which=6)
FD$beta
#Computation of V
f <- function(s, x){
 return(sum(s)+sum(x))
}

FD$representative_utility()# The agents' utility function is by default linear
FD$representative_utility(func=f)# look for "foreach" package for fast loop
FD$V

#Generation of Epsilon whose components law's are different
FD$total_utility()
FD$total_utility("normal", mu=1, sd=2) # It is possible to have correlation between alternatives preference (for both student and normal distributions)
FD$total_utility("student", location=1, scale=2, df=4)
FD$total_utility("discrete_uniform")

FD$Epsilon
FD$U
FD$choice_order

FD$best_choice()
FD$choice

# Drawing a 3D preference mapping
FD$map("D", "E")
FD$map("A", "E")
FD$map("A", "B")


################################################################################
#Example 2: Straightforward generation with default distributions
N <- 10
DM_att_names <- list("A", "B", "C")
AT_names <- list("good1", "good2")
AT_att_names <- list("D", "E", "F")
no_choice <- TRUE
FD <- FactorialDesign(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,N=N, no_choice=no_choice)

FD$factorial_design()
FD$choice_order
FD$map("D", "E")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
