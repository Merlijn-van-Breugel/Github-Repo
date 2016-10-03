test <- function(){
    start <- 1
    end   <- 0 
    discr <- NULL
    for (l in 1:k){
        end   <- sum(Cat[1:l])
        Dk    <- D[start:end,start:end]
        Yk    <- Yt_1[start:end,start:end] 
        eta_k <- diag(t(Yk)*Dk*Yk)
        discr <- rbind(discr,eta_k)
        start <- start + Cat[l]
    }
}