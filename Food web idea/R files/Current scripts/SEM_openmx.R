library(OpenMx)

#https://vipbg.vcu.edu/vipbg/OpenMx2/docs//OpenMx/latest/NewBeginnersGuide.html


myModel<-mxModel()
#saves the result of mxModel as an R object

myModelRun<-mxRun(myModel)
#save the result of mxRun the model, same class as above but updated with results from model fitting

myAmatrix<-mxMatrix(type="Full", nrow=1, ncol=1, values=4, name="Amatrix")
myAmatrix$values

# Why do we need two names? The matrix name (here, “Amatrix”) is used within 
#OpenMx when performing an operation on this matrix using algebra (see below) 
#or manipulating/using the matrix in any way within a model. When you want to 
#manipulate/use/view the matrix outside of OpenMx, or build a model by building 
#each of the elements as R objects in the ‘piecewise’ approach, you use the R object 
#name (here, myAmatrix). Let’s clarify this with an example.

#Model creation
myModel1<- mxModel(mxMatrix(type="Full", nrow=1, ncol=1, values=4, name="Amatrix"))

#Model execution
#this runs a model through the optimizer
myModel1Run  <- mxRun(myModel1)

#explore the arguments/list all the matrices
myModel1Run$matrices
#or just to get the specific values from the specific matrix
myModel1Run$Amatrix$values

myModel1Run$output


#Alternative using iterative coding
myModel2<-mxModel(myAmatrix, name="model2")
myModel2Run<-mxRun(myModel2)

#Algebra creation
myBmatrix<-mxAlgebra(expression=Amatrix+1, name="Bmatrix")

myModel3<-mxModel(myAmatrix, myBmatrix, name="model3")
myModel3Run<-mxRun(myModel3)
myModel3Run$Amatrix$values
myModel3Run$Bmatrix$result

#### SAme as above just all together:
#Piecewise style
#build all the pieces and then put them together in a model
myAmatrix    <- mxMatrix(type="Full", nrow=1, ncol=1, values=4, name="Amatrix")
myBmatrix    <- mxAlgebra(expression=Amatrix+1, name="Bmatrix")

myModel3     <- mxModel(myAmatrix, myBmatrix, name="model3")
myModel3Run  <- mxRun(myModel3)

#Stepwise style: 
myModel1     <- mxModel( mxMatrix(type="Full", nrow=1, ncol=1, values=4, name="Amatrix") )
myModel1Run  <- mxRun(myModel1)
#start with a model

#then build a new model starting from the first
myModel4<- mxModel(myModel1,
                  mxAlgebra(expression=Amatrix+1, name="Bmatrix"),
                   name="model4")
 #give it a name to not overwrite the prev model model1
myModel4Run  <- mxRun(myModel4)

#Classic style: build all the argument explictly within one mxModel
#harder to debug
myModel5     <- mxModel(
  mxMatrix(type="Full", nrow=1, ncol=1, values=4, name="Amatrix"),
  mxAlgebra(expression=Amatrix+1, name="Bmatrix"),
  name="model5"
)
myModel5Run  <- mxRun(myModel5)


### Now practice with real data:
data("demoOneFactor")
require(psych)
describe(demoOneFactor)
#the data is in a matrix form with x1 etc as the y variables

#mxData() constrcucts a data source for themodel
#OpenMx can work with summary statiscx and raw data
#eg. covariance matrices, means and correlation matrices. 
#Raw data: columns represnt variables and rows represent subjects (long)


#start with summary data - covariance matrix
exampleDataCov <- mxData(observed=cov(demoOneFactor), type="cov", numObs=500)

# or cov and the means too
exampleDataCovMeans <- mxData(observed=cov(demoOneFactor),
                              means=colMeans(demoOneFactor), type="cov", numObs=500)

### raw data 
exampleDataRaw <- mxData(observed=demoOneFactor, type="raw")

#Variable creation
#latent = not directly observed
#manifests = observed variables

manifests <- c("x1","x2","x3","x4","x5")
latents <- c("G")

manifestVars = manifests
latentVars = latents

#or more succinctly:
manifestVars = names(demoOneFactor)
latentVars = c("G")

#path creation
#using the mxPath function
#from (source) and to (sinks), if to is missing it's on itself
#connect - the type of course to sink connect, one of five types
#arrows - unidirectional or bidirectional
#free - is the path free or fixed?
#values - starting value of the path
#labels - paths with same label are contrainsed to be equal


#for the simple model with five manifest variables and one latent G
#we need three sets of paths
causalPaths  <- mxPath(from=latents, to=manifests)
residualVars <- mxPath(from=manifests, arrows=2)
factorVars   <- mxPath(from=latents, arrows=2, free=FALSE, values=1.0)




