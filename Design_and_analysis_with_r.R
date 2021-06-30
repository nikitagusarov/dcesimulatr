################################################################################
##
## Examples and exercises of Design and Analysis of Experiments with R
##
################################################################################

## Chapter 2:CRD wtih one factor ###############################################
set.seed(7638)

#simple factorial design
f <- factor( rep( c(35, 40, 45 ), each = 4))#each gives an order of replication
fac <- sample(f, 12)#random sample from f, the possible outputs
eu <- 1:12
plan <- data.frame(loaf=eu, time=fac)

#matricial form of the linear model
bread <- data.frame(matrix(NA, 12, 2))
colnames(bread) <-  c("time", "height")
bread["time"] <- rep(c(35, 40, 45), each=4)
bread["height"] <- c(4.5, 5.0, 5.5, 6.75, 6.5, 6.5, 10.5, 9.5, 9.75, 8.75, 6.5, 8.25)
library("daewr")
mod0 <- lm(height~time, data = bread)
summary(mod0)

#
library("gmodels")
fit.contrast(mod0, "time", c(1, -1, 0))

mod1 <- aov( height ~ time, data = bread )
summary(mod1)

# build Row Column Design

f <- factor( c(1,2,3,4) )
b1t <- sample(f,4)
b2t <- sample(f,4)
b3t <- sample(f,4)
b4t <- sample(f,4)
block <- factor( rep(c("carnation", "daisy", "rose", "tulip"), each=4))
flnum <- rep(f,4)
plan<-data.frame(TypeFlower = block, FlowerNumber = flnum)


library(agricolae)
treat<-c(1,2,3,4)
outdesign <- design.rcbd(treat, 4, seed = 11)
rcb <- outdesign$book
levels(rcb$block) <- c("carnation", "daisy", "rose", "tulip")

# Use a logit optimizer
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

summary(mydata)
sapply(mydata, sd)
## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

## We convert rank to a factor to indicate that rank should be treated as a categorical variable.
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

## Odds ratios only
exp(coef(mylogit))
## Odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))












