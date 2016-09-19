#Title:     Exection of SMACOF-function
#Author:    Group 1
#Date:      15-09-2016

#Load calibrate package for labelling data points in plot
#install.packages("calibrate")
#library(calibrate)
#Load standard package
#install.packages('smacof')
#library('smacof')
#Load ggplot
#install.packages('ggplot')
#library('ggplot2')
#install.packages('calibrate')
#library('calibrate')

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
rownames(X)=rownames(basket_dissim) #Correct rownames

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
plot(X_standard,asp=1,xlab='',ylab='',col='red',pch=16)
textxy(X_standard[,1],X_standard[,2],rownames(X),offset = 0.65)

#Check why the plots are on a different scale
#We expect the MDS-function to use some kind of normalization or scaling
#We indeed found a normalizing subfunction used in MDS
basket_dissim_norm <- basket_dissim/sqrt(sum(W*basket_dissim^2))*sqrt(length(basket_dissim))
SMACOF_output_norm <- function_SMACOF(basket_dissim_norm,X,eps,W)
#Now we get identical plots

#Do MDS with ordinal transformation
res_standard_ordinal <- mds(delta=basket_dissim,ndim=p,weightmat=W,init=X,type="ordinal",eps = eps)
diss_shepard <- unlist(res_standard_ordinal["delta"])
dhat_shepard <- unlist(res_standard_ordinal["dhat"])
distance_shepard <- unlist(res_standard_ordinal["confdiss"])
data_shepard <- cbind(diss_shepard,dhat_shepard,distance_shepard)

#Sort data_shepard
data_shepard_ordered <- as.data.frame(data_shepard[order(diss_shepard),])
ggplot(data_shepard_ordered, aes(diss_shepard,dhat_shepard))+
    geom_line(color="black")+
    geom_point(aes(diss_shepard,dhat_shepard),color="blue")+
    geom_point(aes(diss_shepard,distance_shepard),color="red")+
    xlab("Dissimilarities")+
    ylab("Distances and d-hats")+
    theme_bw()

#Make bubble plot of stress per point 
spp_standard <- t(as.matrix(do.call(rbind.data.frame, res_standard_ordinal["spp"])))
conf_standard <- as.matrix(do.call(rbind.data.frame, res_standard_ordinal["conf"]),rownames.force = TRUE)
data_bubble <- as.data.frame(cbind(conf_standard[,1],conf_standard[,2],spp_standard))
rownames(data_bubble) <- rownames(basket_dissim) 
colnames(data_bubble) <- c("x_coordinate","y_coordinate","Stress_per_point")
ggplot(data_bubble, aes(x_coordinate,y_coordinate))+
    geom_point(aes(x_coordinate,y_coordinate,size=Stress_per_point),color="blue",show.legend = FALSE)+
    geom_text (aes(x_coordinate,y_coordinate),label=rownames(data_bubble),size=3,
               nudge_x = 0.1, nudge_y = 0.1,check_overlap = TRUE)+
    theme_bw()
    
plot(conf_standard)
