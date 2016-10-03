homals_MCA <- function (data,ndim){
  #test
  #data <- matrix(c(1,2,3,4,5,6), ncol = 2)
  data <- titanic
  ndim <- 2
  
  #initialize
  n <- nrow(data) #number of observations
  k <- ncol(data) #number of variables 
  p <- ndim #dimensions of MCA
  
  #Generate a Random X where X is nxp
  X <- matrix(runif((n*p),0,1), nrow = n, ncol=p)
  
  #set X in deviation of column mean
  X_colmeans <- t(matrix(rep(colMeans(X),n),ncol=n))
  X <- X - X_colmeans
  
  #Compute SVD
  s <- svd(X)
  
  #in terms of slide 15
  P <- s$u
  Phi <- diag(s$d)
  Q <- s$v
  
  #Check (onnodig)
  X_svd <- P %*%  Phi %*% t(Q)
  
  #Choose X = sqrt(n)*P
  X <- sqrt(n) * P
  
  #Compute Indicator Matrix
  G_ind <- NULL #initialize
  Cat <- NULL   #initialize
  G <-as.dummy(data)
  G_ind<- as.matrix(G[[1]])
  Cat <- as.matrix(G[[2]])
  
  #Compute D
  D <- t(G_ind)%*%G_ind
  
  #Compute update of Yk given X (first time)
  Y <- NULL
  start <- 1
  for ( l in 1:k)
  {
    end <- sum(Cat[1:l])
    Dk <- D[start:end,start:end]
    Gk <- G_ind[,start:end]  
    Yk <- solve(Dk) %*% t(Gk) %*% X #calculate Yk
    Y <-rbind(Y,Yk) #insert Yk into Y
    
    start <- start + Cat[l]
  }
  
  #Update X0 to the update of X given Y
  XQ <- update_X(Y)
  X0 <- as.matrix(do.call(rbind.data.frame, XQ[1]))
  Q  <- as.matrix(do.call(rbind.data.frame, XQ[2]))
  
  #Set Y0 to Y rotated
  Y0 <- rotate_Y(Y,Q)
  

  #Set X0 to the update of X given the Yk (t=0)
  #LossFunction(X,Y)

  Xt <- X0
  Yt <- Y0
  t<-0
  LossFunction_t_1 <- Inf
  LossFunction_t <- LossFunction(X0,Y0)
  
  while(LossFunction_t_1 - LossFunction_t > 10^(-6)){
    t = t + 1
    Xt_1 <- Xt
    Yt_1 <- Yt
    Xt <- NULL
    Yt <- NULL
    Qt <- NULL
    
    #update of Y given Xt-1
    Yt <-update_Y(Xt_1)
    
    #Set Xt to the update of X given the Yk
    XQt <- update_X(Yt)
    Xt <- as.matrix(do.call(rbind.data.frame, XQt[1]))
    Qt  <- as.matrix(do.call(rbind.data.frame, XQt[2]))
    
    #set Yt to Y rotated to principal components
    Yt <- rotate_Y(Yt,Qt)
    
    LossFunction_t_1 <- LossFunction_t
    LossFunction_t <- LossFunction(Xt,Yt)
  }
  return(Yt_1)
  
  plot(Yt_1, plot.type = "jointplot", asp = 1)                  # biplot of objects and categories  
  plot(Yt_1, plot.type = "objplot"  , asp = 1)                  # Plot of the object scores
  plot(Yt_1, plot.type = "labplot"  , var.subset = 1, asp = 1)  # object score plot labeled by a12 (doctors are murderers)
  #summary(res)
}

#rm(Cat,D,Y0,data,Dk,G_ind,Gk,P,Phi,Q,X,X_colmeans,X_svd,X0,Xt,Xt_1,Y,X0,Yk,Yt,Yt_1,end,G,k,l,n,ndim,p,s,start,t,XQ)


