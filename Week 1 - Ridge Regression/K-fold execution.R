
# Title:    Week 1 Ridge Regression - K-fold execution
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016


#Define a range of lambda you want so use:
lambda_range <- seq(
    from = 1, 
    to = 10, 
    by = 0.1)

#Give number of folds
k <- 5

#Choose Matrices X and y
X <- dataX
y <- datay

#Add column with fold-categories
selection_col <- sample(1:k,dim(X)[1][1],replace=TRUE)
X_foldcat <- cbind(X,selection_col)
y_foldcat <- cbind(y,selection_col)



#Loop over your lambda range
for (m in lambda_range){
    
    #We use the Ridge-function, we have to store results in a matrix 
    
    #Initialize a matrix for estimates
    b_estimates <- NULL
    R_sq_holdout<- NULL
    
    #Start looping over all k folds
    for (i in 1:k) {
    
        X_train         <- X_foldcat[X_foldcat[,dim(X_foldcat)[2][1]]!=i,][,1:ncol(X_foldcat)-1]
        X_holdout       <- X_foldcat[X_foldcat[,dim(X_foldcat)[2][1]]==i,][,1:ncol(X_foldcat)-1]
        y_train         <- cbind(y_foldcat[y_foldcat[,dim(y_foldcat)[2][1]]!=i,][,1:ncol(y_foldcat)-1])
        y_holdout       <- cbind(y_foldcat[y_foldcat[,dim(y_foldcat)[2][1]]==i,][,1:ncol(y_foldcat)-1])
    
        X_holdout_scaled <- scaling_function(X_holdout, TRUE, TRUE)
        y_holdout_scaled <- scaling_function(y_holdout, TRUE, TRUE)
        
        b_estimates     <- cbind(b_estimates,do.call(rbind.data.frame, ols_ridge_function(X_train,y_train,m,TRUE)[1])[,2])
        colnames(b_estimates)[ncol(b_estimates)] <- paste("Fold #",toString(i))
        
        e_holdout       <- y_holdout_scaled - X_holdout_scaled %*% b_estimates[,ncol(b_estimates)]
        e_sq            <- t(e_holdout) %*% e_holdout
        SST             <- t(y_holdout_scaled) %*% (y_holdout_scaled)
        R_sq_holdout    <- cbind(R_sq_holdout,1 - e_sq/SST)
        colnames(R_sq_holdout)[ncol(R_sq_holdout)] <- paste("Fold #",toString(i))
        
    }    

}

