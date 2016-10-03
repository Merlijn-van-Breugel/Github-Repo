HomalsMCA <- function (data,ndim,X){
  
  #initialize
  n <- nrow(data)       #number of observations
  k <- ncol(data)       #number of variables 
  p <- ndim             #dimensions of MCA
  
  #If not given as arg, compose random X according to SVD
  if (is.null(X)){       
      X <- matrix(runif((n*p),0,1), nrow = n, ncol=p)
      X_colmeans <- t(matrix(rep(colMeans(X),n),ncol=n))
      X <- X - X_colmeans   #set X in deviation of column mean
      s <- svd(X)           #Compute SVD
      #Singular value composition of X
      P <- s$u
      Phi <- diag(s$d)
      Q <- s$v
      #Choose X = sqrt(n)*P
      X <- sqrt(n) * P
  }
  #Compute Indicator Matrix
  #initialize
  G_ind <- NULL 
  Cat   <- NULL
  G     <- as.dummy(data)
  G_ind <- as.matrix(G[[1]])
  Cat   <- as.matrix(G[[2]])
  #Compute D
  D <- t(G_ind)%*%G_ind
  #Compute update of Yk given X (first time)
  Y <- NULL
  start <- 1
  for (l in 1:k)
  {
    end <- sum(Cat[1:l])            #Set end to total number of categories passed
    Dk <- D[start:end,start:end]
    Gk <- G_ind[,start:end]  
    Yk <- solve(Dk) %*% t(Gk) %*% X #calculate Yk
    Y  <-rbind(Y,Yk) #insert Yk into Y
    
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
  t  <- 0
  LossFunction_t_1 <- Inf
  LossFunction_t <- LossFunction(X0,Y0)
  
  while(LossFunction_t_1 - LossFunction_t > 10^(-6)){
    t  <- t + 1
    Xt_1 <- Xt
    Yt_1 <- Yt
    Xt <- NULL
    Yt <- NULL
    Qt <- NULL
    #update of Y given Xt-1
    Yt <-update_Y(Xt_1)
    #Set Xt to the update of X given the Yk
    XQt <- update_X(Yt)
    Xt  <- as.matrix(do.call(rbind.data.frame, XQt[1]))
    Qt  <- as.matrix(do.call(rbind.data.frame, XQt[2]))
    #set Yt to Y rotated to principal components
    Yt <- rotate_Y(Yt,Qt)
    
    LossFunction_t_1 <- LossFunction_t
    LossFunction_t <- LossFunction(Xt,Yt)
  }
  
  #Compute output elements
  #Discrimination measures
  start     <- 1
  end       <- 0 
  discrim   <- NULL
  for (l in 1:k){
      end   <- sum(Cat[1:l])
      Dk    <- D[start:end,start:end]
      Yk    <- Yt[start:end,] 
      eta_k <- diag(t(Yk)%*%Dk%*%Yk)
      discrim <- rbind(discrim,eta_k)
      start <- start + Cat[l]
  }
  #Eigenvalues
  eigenvalues <- (1/k)*colSums(discrim)
  
  #Make list with output elements
  #Return centroids coordinates, object scores, eigenvalues and discrimination measures
  output        <- list(cat.centroid=Yt,objscores=Xt,eigenvalues=eigenvalues,discrim=discrim)
  return(output)
  
  plot(Yt_1, plot.type = "jointplot", asp = 1)                  # biplot of objects and categories  
  plot(Yt_1, plot.type = "objplot"  , asp = 1)                  # Plot of the object scores
  plot(Yt_1, plot.type = "labplot"  , var.subset = 1, asp = 1)  # object score plot labeled by a12 (doctors are murderers)
  #summary(res)
}


