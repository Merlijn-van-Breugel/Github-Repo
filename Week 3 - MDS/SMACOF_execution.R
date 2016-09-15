#Title:     Exection of SMACOF-function
#Author:    Group 1
#Date:      15-09-2016

#Load calibrate package for labelling data points in plot
#install.packages("calibrate")
#library(calibrate)

#Load standard package
#install.packages('smacof')
#library('smacof')

#Convert similarity matrix 'basket' to dissimilarity matrix
basket_dissim <- function_similarity_to_dissimilarity(as.matrix(basket, colnames.force = TRUE))
#Define epsilon: minimal error for convergence
eps <- 10^-6
#Define number of dimensions
p <- 2
#Define weight matrix, here we use equal weights, so W is set to 1
W <- matrix(1,dim(basket_dissim)[1],dim(basket_dissim)[2])       

#Compute X-matrix, with p dimensions, taken from init configuration from standard package
res_standard_init <- mds(basket_dissim,ndim=p,weightmat=W,init="random",type="ratio",itmax=1,eps = eps)
X <- as.matrix(do.call(rbind.data.frame, res_standard_init[5]))
rownames(X)=colnames(basket_dissim) #Correct rownames

#Compare output to standard package
#Run standard package
res_standard <- mds(delta=basket_dissim,ndim=p,weightmat=W,init=X,type="ratio",eps = eps)
#Run own SMACOF function
SMACOF_output <- function_SMACOF(basket_dissim,X,eps,W)
#Unlist output and store in useable format
X_final <- as.matrix(do.call(rbind.data.frame, SMACOF_output["X"]))
Stress <- as.matrix(do.call(rbind.data.frame, SMACOF_output["Stress"]),rownames.force = NA)
Stress_normal <- as.matrix(do.call(rbind.data.frame, SMACOF_output["Stress_normal"]))
Stress_normal_dif <- t(as.matrix(do.call(rbind.data.frame, SMACOF_output["Stress_normal_dif"])))

X_standard <- as.matrix(do.call(rbind.data.frame, res_standard[5]))
plot(X_standard,asp=1)
textxy(X_standard[,1],X_standard[,2],rownames(X),offset = 0.65)

#Check why the plots are on a different scale
#We expect the MDS-function to use some kind of normalization or scaling
#We indeed found a normalizing subfunction used in MDS
basket_dissim_norm <- basket_dissim/sqrt(sum(W*basket_dissim^2))*sqrt(length(basket_dissim))
SMACOF_output_norm <- function_SMACOF(basket_dissim_norm,X,eps,W)
#No we get identical plots

#Do MDS with ordinal transformation
res_standard_ordinal <- mds(delta=basket_dissim,ndim=p,weightmat=W,init=X,type="ordinal",eps = eps)
diss_shepard <- unlist(res_standard_ordinal["delta"])
dhat_shepard <- unlist(res_standard_ordinal["dhat"])
distance_shepard <- unlist(res_standard_ordinal["confdiss"])
data_shepard <- cbind(diss_shepard,dhat_shepard,distance_shepard)

#Sort data_shepard
data_shepard_ordered <- as.data.frame(data_shepard[order(diss_shepard),])
ggplot(data_shepard_ordered, aes(diss_shepard,dhat_shepard))+geom_point()+geom_point(aes(diss_shepard,distance_shepard))

plot(data_shepard_ordered,type="o",col='blue')
