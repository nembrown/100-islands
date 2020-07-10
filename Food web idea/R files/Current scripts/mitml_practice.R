#Multiple imputation practice
#from: https://cdn.rawgit.com/simongrund1/mitml/abdc7d58/vignettes/Introduction.html

library(mitml)
data(studentratings)
summary(studentratings)

#Assume interested in relationship between two variables:
#ReadAchiev ~ 1 + ReadDis + (1|ID)
# the relation between ReadDis and ReadAchiev is represented by a single fixed effect of ReadDis, 
#and a random intercept is included to account for the clustered structure of the data and the group-level variance in ReadAchiev that is not explained by ReadDis.


fml <- ReadAchiev + ReadDis + SchClimate ~ 1 + (1|ID)
#Note that, in this specification of the imputation model. all variables are included on the left-hand side of the model, whereas the right-hand side is left “empty”. 
#This model allows for all relations between variables at Level 1 and 2 and is thus suitable for most applications of the multilevel random intercept model 


#wrapper to pan: 
#include all variables part of model of interest, related to variables in model or related to wheteher the variables in model are missing
#The imputation procedure is then run for 5,000 iterations (burn-in), after which 100 imputations are drawn every 100 iterations.
imp <- panImpute(studentratings, formula=fml, n.burn=5000, n.iter=100, m=100)

#assess convergence of the imputation procedure:
summary(imp)

#or by diagnostic plots: 
#These plots consist of a trace plot, an autocorrelation plot, and some additional information about the posterior distribution. 
#Convergence can be assumed if the trace plot is stationary (i.e., does not “drift”), and the autocorrelation is within reasonable bounds 
#for the chosen number of iterations between imputations

plot(imp, trace="all", print="beta", pos=c(1,2))



#Then complete the data:
#In order to work with and analyze the imputed data sets, the data sets must be completed with the imputations generated in the previous steps. 
#To do so, mitml provides the function mitmlComplete.

implist <- mitmlComplete(imp, "all")
#This resulting object is a list that contains the 100 completed data sets.

#In order to obtain estimates for the model of interest, the model must be fit separately to each of the completed data sets, 
#and the results must be pooled into a final set of estimates and inferences. 

#The mitml package offers the with function to fit various statistical models to a list of completed data sets.

#In this example, we use the lmer function from the R package lme4 to fit the model of interest.
library(lme4)
fit <- with(implist, lmer(ReadAchiev ~ 1 + ReadDis + (1|ID)))
#The resulting object is a list containing the 100 fitted models. 

#To pool the results of these models into a set of final estimates and inferences, mitml offers the testEstimates function.
testEstimates(fit, var.comp=TRUE)


############### Missing data at both levels
data(leadership)
summary(leadership)

#COHES is data at the higher level (i.e. unq_isl)
fml <- list( JOBSAT + NEGLEAD + WLOAD ~ 1 + (1|GRPID) , # Level 1
             COHES ~ 1 )                                # Level 2
#We use jomoImpute instead of Pan b/c missing values at both levels.

imp <- jomoImpute(leadership, formula=fml, n.burn=5000, n.iter=250, m=20)
summary(imp)
implist <- mitmlComplete(imp, "all")
### i .e. it's going to inpute the same inputed data at the same level. 