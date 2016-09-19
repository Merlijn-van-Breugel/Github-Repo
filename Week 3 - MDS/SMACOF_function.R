
function_SMACOF <- function(D,X,eps,W){

    
    #Check whether D satisfies symmetry, nonnegativity and zero diagonal
    if (isSymmetric.matrix(D) == 1 && min(D)>=0 && sum(diag(D)) == 0) {
        #okay, continue
    }
    else {
        stop("Matrix D does not satisfy dissimilarity properties")
    }
    #initialize 
    labels <- rownames(X)
    k <- 0
    Z <- X
    F <- matrix(0,dim(D)[1],dim(D)[2])
    F_1 <- matrix(0,dim(D)[1],1)
    Z_dist <- function_euclidean_distance(Z)            #Euclidian distance of Z
    Stress <- sum(W * ((D - Z_dist)*(D - Z_dist)))/2 #Compute raw Stress
    #Compute new coordinate matrix for new Stress-value
    while (k==0 || (Stress[k]-Stress[k+1])>=eps){
        k <- k + 1                                      #Update counter
        #Compute matrix B
        for (i in 1:dim(D)[1]){                         #Compute F element wise
            for (j in 1:dim(D)[1]){
                if (Z_dist[i,j]==0){
                    F[i,j] <- 0
                    }
                else {
                    F[i,j] <- D[i,j]/Z_dist[i,j]
                    }
            }
        }
        F_1 <- rowSums(F)                               #F_1 as vector of rowSums of F
        B <- diag(F_1) - F                              #Compute B, part of cross-product term of Stress
        X <- dim(X)[1]^-1 * B %*% Z                     #Update new coordinate matrix
        Z <- X                                          #Update Z 
        Z_dist <- function_euclidean_distance(Z)        #Get new Euclidian distance of Z
        Stress <- rbind(Stress,sum(W * ((D - Z_dist)*(D - Z_dist)))/2) #Add new raw Stress to Stress vector
        plot(X,asp=1,xlab='',ylab='',col='red',pch=16)
        textxy(X[,1],X[,2],labels,offset = 0.65)
        Sys.sleep(0.07)
        #print(Stress[k+1]/(sum(D*D)/2))                #Print new normalized Stress value, also given in output
    }
    #Normalise the raw Stress 
    Stress_normal <- Stress/(sum(D*D)/2)
    Stress_normal_dif <- Stress_normal[1:nrow(Stress_normal)-1,1] - Stress_normal[2:nrow(Stress_normal),1]
    output_list <- list(X=X, Stress=Stress, Stress_normal=Stress_normal,Stress_normal_dif=Stress_normal_dif)
    return(output_list)
}
