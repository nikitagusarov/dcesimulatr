#############################################################
## This file characterizes and computes the utility function of a decision maker.
## The notations are taken from Train 2003.
## antoine.dubois.fr@gmail.com - March 2021
#############################################################

##############################
# 1 - Experiment
##############################


#' @title Experiment
#'
#' @name Experiment
#'
#' @description The reference class whose methods generate an Experiment
#'
#' @param AT_names List of the alternatives' names
#'
#' @param AT_att_names List of the names of the alternatives' attributes
#'
#' @param N Number of decision makers
#'
#' @param DM_att_names List of the name of the decision makers' attributes
#'
#' @param normalize whether or not to normalize the utility
#'
#' @param J The number of alternatives
#'
#' @param q The number of attributes of each alternatives
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#'

Experiment <- setRefClass("Experiment", fields = list(
  normalize="logical", no_choice="logical",
  DM_att_names="list", AT_att_names="list", AT_names="list", groups="numeric",
  N="numeric", p="numeric", J="numeric", q="numeric",
  S="data.frame", S_clustered="data.frame", S_category="data.frame",
  X="data.frame", X_clustered="data.frame", X_category="data.frame",
  beta="data.frame",
  Epsilon="data.frame", func="function", V="data.frame", U="data.frame", choice_order="data.frame", choice="data.frame",
  design_expe="data.frame", info="list")
)


Experiment$methods(initialize = function(AT_names, AT_att_names, groups, DM_att_names, normalize=TRUE, no_choice=FALSE){
    .self$normalize <<- normalize
    .self$no_choice <<- no_choice
    if(.self$no_choice){.self$AT_names <<- append("no_choice", AT_names)}
    else{.self$AT_names <<- AT_names}
    .self$DM_att_names <<- DM_att_names
    .self$AT_att_names <<- AT_att_names
    .self$groups <<- groups
    .self$N <<- sum(.self$groups)
    .self$p <<- length(.self$DM_att_names)
    .self$J <<- length(.self$AT_names)
    .self$q <<- length(.self$AT_att_names)

    S <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$p)); colnames(S) <<- .self$DM_att_names
    S_clustered <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$p)); colnames(S_clustered) <<- .self$DM_att_names
    S_category <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$p)); colnames(S_category) <<- .self$DM_att_names

    X <<- data.frame(matrix(NA, nrow=.self$J, ncol=.self$q)); colnames(X) <<- .self$AT_att_names; row.names(X) <<- .self$AT_names
    X_clustered <<- data.frame(matrix(NA, nrow=.self$J, ncol=.self$q)); colnames(X_clustered) <<- .self$AT_att_names; row.names(X) <<- .self$AT_names
    X_category <<- data.frame(matrix(NA, nrow=.self$J, ncol=.self$q)); colnames(X_category) <<- .self$AT_att_names; row.names(X) <<- .self$AT_names

    Epsilon <<- data.frame(matrix(NA, nrow=.self$N, ncol=.self$J)); colnames(Epsilon) <<- .self$AT_names
    V <<- data.frame(matrix(NA, nrow = .self$N, ncol = .self$J)); colnames(V) <<- .self$AT_names
    U <<- data.frame(matrix(NA, nrow = .self$N, ncol = .self$J)); colnames(U) <<- .self$AT_names
  })


#' @title gen_DM_attributes
#'
#' @name gen_DM_attributes
#'
#' @description The method of the Experiment class which generates some decision makers' attributes
#'
#' @param law The name of the distribution generating the attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @param nb_levels The vector of number of levels of the attributes designated in argument which
#'
#' @param group The group to generate values. By default, every groups are concerned by the generation.
#'
#' @param observation The observation function. Takes a formula as argument.
#'
#' @optional_parameter characteristics of the chosen distribution
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$gen_DM_attributes("discrete_uniform", a=0, b=1, which="S1")
#' FD$gen_DM_attributes("normal", which=c("S2", "S3"), group=1)
#' FD$gen_DM_attributes("normal", mu=1, sd=2, which=c("S2","S3"), group=2)
#' FD$S
#'

Experiment$methods(gen_DM_attributes = function(law="normal", which=c(1:.self$p), group, observation,...){
    param <- list(...)
    if(missing(observation)){
    if(missing(group)){group <- c(1:.self$N)}
    else if(group==1){group <- c(1:.self$groups[1])}
    else if(group>length(.self$groups)){stop("There is/are only ", length(.self$groups), " groups")}
    else{group <- c(.self$groups[group-1]+1:.self$groups[group])}
    ob_DM_att <- ob_decision_makers_att(N=.self$N, p=.self$p)
    S[group, which] <<- ob_DM_att$gen(law=law, m=length(which), param=param)

    if(any(is.na(S))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}

    }else{
      S <<- model.frame(observation, data=S)
      S_col_names <- colnames(S)
      S <<- data.frame(matrix(unlist(S), ncol=ncol(S)))
      colnames(S) <<- S_col_names
    }

})


#' @title gen_AT_attributes
#'
#' @name gen_AT_attributes
#'
#' @description The method of the Experiment class which generates some alternatives' attributes
#'
#' @param law The name of the distribution generating the attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @param nb_levels The vector of number of levels of the attributes designated in argument which
#'
#' @optional_parameter characteristics of the chosen distribution
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2", "good3", "good4")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names, groups=groups)
#' FD$gen_AT_attributes("discrete_uniform", b=2, which=c("X1","X2"))
#' FD$gen_AT_attributes("normal", which=c("X3"), nb_levels=3)
#' FD$X; FD$X_clustered; FD$X_category
#'

Experiment$methods(gen_AT_attributes = function(law="normal", which=c(1:.self$q), observation,...){
    param <- list(...)
    if(missing(observation)){
      ob_AT_att <- ob_alternatives_att(J=.self$J, q=.self$q)
      X[, which] <<- ob_AT_att$gen(law=law, m=length(which), param=param)
      if(.self$no_choice){X[1, ] <<- 0}

    if(any(is.na(X))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}
  }else{
    X <<- model.frame(observation, data=X)
    X_col_names <- colnames(X)
    X <<- data.frame(matrix(unlist(X), ncol=ncol(X)))
    colnames(X) <<- X_col_names
    rownames(X) <<- .self$AT_names
  }
})


#' @title gen_preference_coefficients
#'
#' @name gen_preference_coefficients
#'
#' @description The method of the Experiment class which generates some preference coefficients' attributes
#'
#' @param which The number of the columns which should be generated
#'
#' @optional_parameter distribution The distribution which generates the chosen columns.
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$gen_preference_coefficients("student", heterogeneity=TRUE, location=-100,  scale=1, df=4, which=c(1:4), group=1)
#' FD$gen_preference_coefficients("student", heterogeneity=TRUE, location=100,  scale=1, df=4, which=c(1:4), group=2)
#' FD$gen_preference_coefficients("normal", heterogeneity=TRUE, mu=0, sd=2, which=5)
#' FD$gen_preference_coefficients("discrete_uniform", heterogeneity=TRUE, a=1, b=5, which=6)
#' FD$beta
#'

Experiment$methods(gen_preference_coefficients = function(law="normal", heterogeneity=FALSE, which=c(1:(ncol(S)+ncol(X))), group, ...){
    if(nrow(beta)==0 | ncol(beta)==0){
    beta <<- data.frame(matrix(NA, nrow=.self$N, ncol=(ncol(S)+ncol(X)))); colnames(beta) <<- c(colnames(S), colnames(X))
    }
    param <- list(...)
    if(missing(group)){group <- c(1:.self$N)}
    else if(group==1){group <- c(1:.self$groups[1])}
    else if(group>length(.self$groups)){stop("There is/are only ", length(.self$groups), " groups")}
    else{group <- c(.self$groups[group-1]+1:.self$groups[group])}
    pref_coef <- preference_coef(N=.self$N, p=ncol(S), q=ncol(X))
    beta[group, which] <<- pref_coef$gen(law=law, m=length(which), heterogeneity=heterogeneity, param=param)

    if(any(is.na(beta))) {warning("There remain some NA values in the decision makers' attributes matrix", call. = FALSE)}})


#' @title representative_utility
#'
#' @name representative_utility
#'
#' @description The method of the Experiment class which computes the representative utility
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$gen_DM_attributes()
#' FD$gen_AT_attributes()
#' FD$gen_preference_coefficients()
#' FD$representative_utility()
#' FD$V
#'

Experiment$methods(representative_utility = function(){

  beta2 <- data.matrix(FD$beta)
  A <- data.matrix(FD$S)
  B <- data.matrix(FD$X)

  func <<- function(s, x, i){return(c(s,x)%*% matrix(beta2[i,], ncol = 1))}
  # func_U <<- function(s, x){return(sum(s)+sum(x))} # will be useful for $map method
  for(j in 1:.self$J){
    for( i in 1:.self$N){
      V[i,j] <<- func(A[i,], B[j,], i)
    }
  }

  if(.self$normalize){
    for(i in .self$J:1){
      V[,i] <<- V[,i] -V[,1]
    }
  }
})


#' @title utility
#'
#' @name utility
#'
#' @description The method of the Experiment class which generates the utility of the agents for each good
#'
#' @optional_parameter distribution The distribution which generates the perturbations
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$gen_AT_attributes()
#' FD$gen_DM_attributes()
#' FD$gen_preference_coefficients()
#' FD$utility()
#' FD$Epsilon; FD$U; FD$choice_order
#' FD$utility("student", mu=3, df=4) # Some examples
#' FD$utility("discrete_uniform") # Some examples
#' FD$Epsilon
#' FD$U
#' FD$choice_order
#' FD$Epsilon
#' FD$U
#' FD$choice_order
#'

Experiment$methods(utility = function(law="gumbel", ...){
    param <- list(...)
    perturbation <- noise(N=.self$N, J=.self$J)
    Epsilon <<- data.frame(perturbation$gen(law=law, param=param))
    colnames(Epsilon) <<- .self$AT_names
    if(.self$normalize){
      for(i in .self$J:1){
        Epsilon[i] <<- Epsilon[i] -Epsilon[1]
      }
    }
    representative_utility()
    U <<- V + Epsilon
    if(.self$normalize){
      U <<- normalization(U, U)
      V <<- normalization(V, U)
      Epsilon <<- normalization(Epsilon, U)
    }
    sort_index_decr <- function(x){
      return(sort(x, decreasing=TRUE, index.return=TRUE)$ix)}
    choice_order <<- data.frame(t(apply(U, 1, sort_index_decr)))
    colnames(choice_order) <<- .self$AT_names
    best_choice()
  })


#' @title best_choice
#'
#' @name best_choice
#'
#' @description The method of the Experiment class which computes the optimal decision makers' choice
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$gen_AT_attributes()
#' FD$gen_DM_attributes()
#' FD$gen_preference_coefficients()
#' FD$utility()
#' FD$Epsilon; FD$U; FD$choice_order
#' FD$utility("student", mu=3, df=4) # Some examples
#' FD$utility("discrete_uniform") # Some examples
#' FD$Epsilon
#' FD$U
#' FD$choice_order
#' FD$Epsilon
#' FD$U
#' FD$choice_order
#' FD$choice

Experiment$methods(best_choice = function(){
  which_best <- function(x){
    return(.self$AT_names[which.min(x)])
  }
  choice <<- data.frame(unlist(apply(choice_order, 1, which_best)))
  colnames(choice) <<- "optimal choice"
})


#' @title design
#'
#' @name design
#'
#' @description The method of the Experiment class which generate a full factorial design
#'
#' @param name The name of the experimental design to implement.
#'
#' @param choice_set_size The number of alternative per choice set.
#'
#' @param clustered Determines the way the data is represented in the experimental design table.
#' 0 if the matrices of decision makers' characteristics and alternatives' attributes are row.
#' 1 if the matrices of decision makers' characteristics and alternatives' attributes are clustered.
#' 2 if the matrices of decision makers' characteristics and alternatives' attributes are categorized.
#'
#' @param format If "long" (default) the design is expressed in long format and wide format otherwise.
#'
#' @return an Experimental Design as well as a some pieces of information such as the D-score, defined as the determinant of the covariance matrix of the preference parameter (a good D-score should be small), and an estimation of the preference parameters.
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2", "good3", "good4")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names, groups=groups)
#' FD$gen_AT_attributes()
#' FD$gen_DM_attributes()
#' FD$gen_preference_coefficients()
#' FD$utility()
#' FFD <- FD$design(choice_set_size=2, clustered=0) # generation of the full factorial design with row data
#' #by default, name="FuFD", choice_set_size = nb_alternatives
#' FFD1 <- FD$design(name="FuFD",choice_set_size=2, clustered=1, nb_levels_DM=c(3, 3, 4, 2), nb_levels_AT=c(3, 2, 2, 4), format="wide") # generation of the full factorial design with glustered data
#' FFD2 <- FD$design(choice_set_size=2, clustered=2, nb_levels_DM=c(2, 3, 4, 2), nb_levels_AT=c(2, 2, 2, 2)) # generation of the full factorial design with categorical data
#' FFD3 <- FD$design(name="FrFD", choice_set_size=2, clustered=2, nb_levels_DM=c(2, 3, 4, 2), nb_levels_AT=c(2, 2, 2, 2), nb_questions = 2) # Generation a a random fractional factorial design with categorical data
#' FFD4 <- FD$design(name="FrFD", choice_set_size=2, clustered=2, nb_levels_DM=c(2, 3, 4, 2), nb_levels_AT=c(3, 3, 3, 3), nb_questions = 2, format="wide") # Yet, we want to express this design in wide format


Experiment$methods(design = function(name="FuFD", choice_set_size=(.self$J-.self$no_choice),
                                     clustered = 0, nb_levels_DM, nb_levels_AT, nb_questions=NULL, format="long"){
  if(choice_set_size > (.self$J-.self$no_choice) | choice_set_size<0){stop("Unconsistent choice set size")}
  if(clustered==0){A <- S; B <- X}
  else if(clustered==1){
    clustering <- categorization(S, nb_levels = nb_levels_DM)
    DF <- data.frame(clustering$clustered); colnames(DF) <- colnames(S)
    S_clustered <<- DF
    is.duplicate <- duplicated(S_clustered)
    if(any(is.duplicate)){
      duplicates <- list(Decision_makers_duplicates=rownames(S)[is.duplicate])
      warning("Decision makers have ", sum(is.duplicate)," duplicates.")
      print(duplicates)
    }

    DF <- data.frame(clustering$category); colnames(DF) <- colnames(S)
    S_category <<- DF

    clustering <- categorization(X, nb_levels = nb_levels_AT)

    DF <- data.frame(clustering$clustered); colnames(DF) <- colnames(X)
    X_clustered <<- DF
    is.duplicate <- duplicated(X_clustered)
    if(any(is.duplicate)){
      if(sum(is.duplicate)==1){
        warning("Alternative ", .self$AT_names[is.duplicate], " is a duplicate.")
      }else{
        warning("Alternative ", list(.self$AT_names[is.duplicate]), " are duplicates.")}
    }

    DF <- data.frame(clustering$category); colnames(DF) <- colnames(X)
    X_category <<- DF

    A <- S_clustered; B <- X_clustered}
  else if(clustered==2){
    clustering <- categorization(S, nb_levels = nb_levels_DM)
    DF <- data.frame(clustering$clustered); colnames(DF) <- colnames(S)
    S_clustered <<- DF
    DF <- data.frame(clustering$category); colnames(DF) <- colnames(S)
    S_category <<- DF
    is.duplicate <- duplicated(S_category)
    if(any(is.duplicate)){
      duplicates <- list(Decision_makers_duplicates=rownames(S)[is.duplicate])
      warning("Decision makers have ", sum(is.duplicate)," duplicates.")
      print(duplicates)
    }

    clustering <- categorization(X, nb_levels = nb_levels_AT)
    DF <- data.frame(clustering$clustered); colnames(DF) <- colnames(X); rownames(DF) <- rownames(X)
    X_clustered <<- DF
    DF <- data.frame(clustering$category); colnames(DF) <- colnames(X); rownames(DF) <- rownames(X)
    X_category <<- DF

    is.duplicate <- duplicated(X_category)
    if(any(is.duplicate)){
      if(sum(is.duplicate)==1){
        warning("Alternative ", .self$AT_names[is.duplicate], " is a duplicate.")
      }else{
        warning("Alternative ", .self$AT_names[is.duplicate], " are duplicates.")}
    }
    A <- S_category; B <- X_category
  }else{cat("The variable 'clustered' may be equal to 0 for building a design with raw data,
           to 1 with clustered data and to 2 with categorical data")}

  Design_long <- call_design(name=name, A=A, B=B, U=U, choice_set_size=choice_set_size,
                     J=.self$J, no_choice=.self$no_choice, nb_questions=nb_questions, format="long")
  Design_wide <- call_design(name=name, A=A, B=B, U=U, choice_set_size=choice_set_size,
                        J=.self$J, no_choice=.self$no_choice, nb_questions=nb_questions, format="wide")
  info <<- infoDesign(name=name, experimental_design_long=Design_long, experimental_design_wide=Design_wide,
                      AT_names=.self$AT_names, choice_set_size=choice_set_size, J=.self$J,
             no_choice=.self$no_choice, DM_att_names=colnames(S), AT_att_names=colnames(X), beta)
  print(info)
  if(format=="long"){
    Design <- Design_long
  } else if(format=="wide"){
    Design <- Design_wide
  }else{
    stop("The two formats are 'long' and 'wide'")
  }

  return(Design)
})


#' @title map
#'
#' @name map
#'
#' @description The method of the Factorial Experiment class which draws a preference mapping
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2", "good3")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$experiment()
#' FD$map("S1", "X2")
#'
#' @import scatterplot3d
#' @import RColorBrewer

Experiment$methods(map = function(dim1, dim2){
  dim1_id <- which(colnames(X)==dim1)
  A <- X
  dim1_in_x <- TRUE
  if(length(dim1_id)==0){dim1_id <- which(colnames(S)==dim1); A <- S; dim1_in_x <- FALSE}
  if(length(dim1_id)==0){stop("dim1 unknown attribute")}

  dim2_id <- which(colnames(X)==dim2)
  B <- X
  dim2_in_x <- TRUE
  if(length(dim2_id)==0){dim2_id <- which(colnames(S)==dim2); B <- S; dim2_in_x <- FALSE}
  if(length(dim2_id)==0){stop("dim2 unknown attribute")}

  colors_list <- RColorBrewer::brewer.pal(n = .self$J, name = "Set1")
  mat <- c()
  colors <- c()
  for(i in 1:.self$J){
    which_best <- choice_order[i]==1
    nb_best <- sum(which_best)
    if(dim1_in_x){x1 <- rep(A[i, dim1_id], nb_best)}
    else{x1 <- A[which_best, dim1_id]}
    if(dim2_in_x){x2 <- rep(B[i, dim2_id], nb_best)}
    else{x2 <- B[which_best, dim2_id]}
    mat <- rbind(mat, cbind(x1, x2, U[which_best, i]))
    colors <- c(colors, rep(colors_list[i], nb_best))
  }

  mat <- data.frame(mat)
  colnames(mat) <- c(dim1, dim2, "Utility")
  shapes <- c()
  for(i in 1:length(.self$groups)){
    shapes <- c(shapes, rep(16 + 2*i, .self$groups[i]))
  }
  legend_list <- c()
  pch_list <- c()
  for(i in 1:length(.self$groups)){
    pch_list <- c(pch_list, rep(16 + 2*i, length(.self$AT_names)+1))
    legend_list <- c(legend_list, paste("Groupe", i, ":", sep=" "), .self$AT_names)
  }
  s3d <- scatterplot3d::scatterplot3d(mat, color=colors,  pch = shapes, main="Utility map", box=TRUE)

  legend("bottomright", legend = legend_list,  xpd = TRUE, horiz = FALSE,# inset = -0.2,
         col =  rep(c("white", colors_list), 2), pch = pch_list, cex=0.5, inset=c(0,-0.3))

  func_U2 <- function(a, b){
    beta_av <- as.vector(apply(beta, 2, mean))
    s_av <- as.vector(apply(S, 2, mean))
    x_av <- as.vector(apply(X, 2, mean))
    listofpoints <- c()
    for(i in 1:length(a)){
      if(dim1_in_x){x_av[dim1_id] <- a[i]}
      else{s_av[dim1_id] <- a[i]}

      if(dim2_in_x){x_av[dim2_id] <- b[i]}
      else{s_av[dim2_id] <- b[i]}

      listofpoints <- c(listofpoints, c(s_av, x_av) %*% beta_av)#I have changed func_U to func
    }
    return(listofpoints)
  }

  s3d$contour3d(func_U2)
})


#' @title experiment
#'
#' @name experiment
#'
#' @description The method of the Experiment class which generates a Experiment with default distributions
#'
#' @examples DM_att_names <- list("S1", "S2", "S3")
#' AT_att_names <- list("X1", "X2", "X3")
#' AT_names <- list("good1", "good2")
#' groups <- c(10, 20)
#' FD <- Experiment(DM_att_names=DM_att_names, AT_att_names=AT_att_names, AT_names=AT_names,groups=groups)
#' FD$experiment()
#' FD$U
#' FD$choice_order
#' FD$map("A", "D")
#'

Experiment$methods(experiment = function(){
  gen_DM_attributes()
  gen_AT_attributes()
  gen_preference_coefficients()
  utility()
})
