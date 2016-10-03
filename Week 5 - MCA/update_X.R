
update_X <- function(Y_use){
  #compute Z
  start <- 1
  sumGY <- 0
  end <- 0 #initialize
  
  for (l in 1:k){
    end<- sum(Cat[1:l])
    Gk <- G_ind[,start:end]
    Yk <- Y_use[start:end,]
    start <- start + Cat[l]
    sumGY <- sumGY + (Gk %*% Yk)
  }
  Z <- (1/m)*sumGY
  
  #set Z in deviation of column mean
  Z_colmeans <- t(matrix(rep(colMeans(Z),n),ncol=n))
  Z <- Z - Z_colmeans
  
  #Compute SVD
  s <- svd(Z)
  
  #in terms of slide 15
  P <- s$u
  Phi <- diag(s$d)
  Q <- s$v
  
  Z_svd <- P %*%  Phi %*% t(Q)
  
  #Choose X = sqrt(n)*P
  X <- sqrt(n) * P
  XQ <- list(X,Q)
  return(XQ)
}