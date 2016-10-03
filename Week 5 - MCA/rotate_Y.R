rotate_Y <- function(Y,Q)
  {
  Y_rot <- NULL
  start <- 1
  end   <- 0 #initialize
  
  for (l in 1:k){
    end     <- sum(Cat[1:l])
    Yk      <- Y[start:end,]
    start   <- start + Cat[l]
    Y_rot_k <- (1/sqrt(n))*Yk %*% Q
    Y_rot   <- rbind(Y_rot,Y_rot_k)
  }
  
  return(Y_rot)
}