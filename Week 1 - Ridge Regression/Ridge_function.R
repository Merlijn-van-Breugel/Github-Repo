
# Title:    Week 1 Ridge Regression - Ridge Function
# Course:   Multivariate Statistics
# Authors:  Merlijn van Breugel
# Date:     01-09-2016

ols_ridge_function <- function(X,y,lambda,standardize){
    
    scaling_factor_X <- 
        if (standardize==TRUE) {
            sqrt(apply(X,2,var))} else {
                FALSE
            }
    scaling_factor_y <- 
        if (standardize==TRUE) {
            sqrt(apply(y,2,var))} else {
                FALSE
            }
    X <- scale(X, center=colMeans(X), scale=scaling_factor_X)
    y <- scale(y, center=colMeans(y), scale=scaling_factor_y)
    
    size_X  <- dim(X)
    size_y  <- dim(y)
    I_lambda<- matrix(0,size_X[2][1],size_X[2][1])
    diag(I_lambda) <- sqrt(lambda)
    X_ridge <- rbind(X,I_lambda)
    y_zeros <- matrix(0,size_X[2][1],1)
    y_ridge <- rbind(y,y_zeros)          
    
    b <- solve(t(X_ridge) %*% X_ridge) %*% t(X_ridge) %*% y_ridge
    e <- y - X %*% b
    e_sq <- t(e) %*% e
    s_sq <- c(e_sq/(size_X[1][1]-size_X[2][1]))
    inv_X_lambda <- solve(t(X) %*% X + I_lambda)
    b_var <- s_sq * inv_X_lambda %*% t(X) %*% X %*% inv_X_lambda 
    b_stdev <- sqrt(diag(b_var))
    SST <- (t(y) %*% y)
    R_sq <- 1 - e_sq/SST
    
    hat_matrix <- X %*% inv_X_lambda %*% t(X)   
    df <- sum(diag(hat_matrix))  
    
    BIC_ridge <- size_X[1][1]*log(e_sq) + 2*df*log(size_X[1][1])
    AIC_ridge <- size_X[1][1]*log(e_sq) + 2*df
    
    ols_ridge <- lm(y_ridge ~ 0 + X_ridge)
    B <- cbind(b,b_stdev)
    colnames(B) = c('estimates','S.E.')
    #colnames(b) = c('estimates')
    #colnames(b_stdev) = c('S.E.')
    S <- cbind(R_sq,BIC_ridge,AIC_ridge)
    colnames(S) = c('R^2','BIC_ridge','AIC_ridge')
    result <- list(B,S)
    return(result)
    
    #return(summary(ols_ridge))
    #Use do.call(rbind.data.frame, ols_ridge_function(dataX,datay,10,TRUE)[1]) to get matrix B
    #Use do.call(rbind.data.frame, ols_ridge_function(dataX,datay,10,TRUE)[1])[,2] to get estimates etc.
}

