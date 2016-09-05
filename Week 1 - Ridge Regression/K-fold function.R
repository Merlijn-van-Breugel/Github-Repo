
# Title:    Week 1 Ridge Regression - K-fold function
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     05-09-2016

k_fold_function <- function(k,X,y,lambda){

B <- NULL
R_sq_holdout<- NULL
y_centered_est<- NULL

#Start looping over all k folds
for (i in 1:k) {
    
    X_train         <- X[X[,dim(X)[2][1]]!=i,][,1:ncol(X)-1]
    X_holdout       <- X[X[,dim(X)[2][1]]==i,][,1:ncol(X)-1]
    y_train         <- cbind(y[y[,dim(y)[2][1]]!=i,][,1:ncol(y)-1])
    y_holdout       <- cbind(y[y[,dim(y)[2][1]]==i,][,1:ncol(y)-1])
    
    X_train_scaled  <- scaling_function(X_train, TRUE, TRUE)
    X_holdout_scaled<- scaling_function(X_holdout, TRUE, TRUE)

    y_train_centered  <- scaling_function(y_train, TRUE, FALSE)
    
    #Store estimates in coefficient matrix B
    result          <- as.data.frame(ols_ridge_function(X_train_scaled,y_train_centered,lambda,FALSE,FALSE))
    b_estimates     <- as.matrix(result$estimates)
    B               <- cbind(B,b_estimates)
    colnames(B)[ncol(B)] <- paste("Fold #",toString(i))
    #Estimate y_holdout
    y_centered_est_k<- X_holdout_scaled %*% b_estimates
    y_centered_est  <- rbind(y_centered_est,y_centered_est_k)
    
}

y_centered      <- scaling_function(y[,1], TRUE, FALSE)
error           <- y_centered - y_centered_est
e_sq            <- t(error) %*% error
SST             <- t(y_centered - mean(y_centered)) %*% (y_centered - mean(y_centered))   #no need to take the diff from mean, as this is set to zero
R_sq            <- 1 - e_sq/SST

return(R_sq)
}



    
    
    
 



