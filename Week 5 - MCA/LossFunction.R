LossFunction <- function(X_t,Y_t)
{
  #initialize
  LF    <-0
  m     <- nrow(Cat)
  start <- 1
  end   <- 0 
  
  for(k in 1:m){
    end <- sum(Cat[1:k])
    Gk  <- G_ind[,start:end]
    Yk  <- Y_t[start:end,]
    A   <- t(X_t - Gk %*% Yk)%*%(X_t-Gk%*%Yk)
    LF  <- LF + sum(diag(A))
    start <- start + Cat[k]
  }
  LF <- 1/m * LF
  return(LF)
  #rm(start,end,Gk,Yk,A,LF)
}
