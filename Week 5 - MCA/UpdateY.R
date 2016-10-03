update_Y <- function(X_use){
    #Compute update of Yk given X
    Y_new   <- NULL
    start   <- 1
    end     <- 0 #initialize
    for (l in 1:k){
      end <- sum(Cat[1:l])
      Dk <- D[start:end,start:end]
      Gk <- G_ind[,start:end]  
      Yk <- solve(Dk) %*% t(Gk) %*% X_use   #calculate Yk
      Y_new <-rbind(Y_new,Yk)               #insert Yk into Y
      start <- start + Cat[l]
    }
    return (Y_new)
}
